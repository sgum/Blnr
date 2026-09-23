# R/auth_ad.R
#
# Типовая авторизация ЦД для Blnr: одна форма «Email или доменный логин» +
# пароль, проверка сначала в Active Directory (LDAPS simple bind под самим
# пользователем), затем — локальный bcrypt-пароль. Финансовый стенд, поэтому
# поверх аутентификации — БЕЛЫЙ СПИСОК (по умолчанию s.gumerov, v.alad):
# валидный доменный пользователь вне списка внутрь не пускается.
#
# Метод и переменные окружения — dt-auth-skill (references/method.md).
# Секреты (реквизиты AD, локальные хеши) — только в .Renviron, не в git.

# --- Белый список -----------------------------------------------------------

blnr_allowed_users <- function() {
  raw <- Sys.getenv("BLNR_ALLOWED_USERS", unset = "s.gumerov,v.alad")
  tolower(trimws(strsplit(raw, "[,;]")[[1]]))
}

# Короткий логин в нижнем регистре — единый ключ (s.gumerov@dtwin.ru -> s.gumerov).
normalize_login <- function(login) tolower(sub("@.*$", "", trimws(as.character(login))))

user_allowed <- function(login) normalize_login(login) %in% blnr_allowed_users()

# --- Кандидаты для bind -----------------------------------------------------

# Короткий логин простой bind не принимает: пробуем login@UPN и NETBIOS\login.
# Почта пробуется как есть, затем локальная часть с доменом леса.
ad_bind_candidates <- function(login) {
  upn     <- Sys.getenv("AD_UPN_SUFFIX", unset = "@ad.dtwin.ru")
  netbios <- Sys.getenv("AD_NETBIOS",    unset = "AD")
  login   <- trimws(login)
  if (!grepl("@", login)) {
    unique(c(paste0(login, upn), paste0(netbios, "\\", login)))
  } else {
    local <- sub("@.*$", "", login)
    unique(c(login, paste0(local, upn)))
  }
}

# --- LDAPS bind внешней командой -------------------------------------------

# Возвращает код результата LDAP: 0 — пустил, 49 — неверные креды,
# 255/прочее — контроллер недоступен/ошибка. Пароль передаётся ТОЛЬКО файлом
# 0600 (иначе виден в `ps`).
ad_bind <- function(bind_user, password) {
  url     <- Sys.getenv("AD_LDAP_URL", unset = "ldaps://ad.dtwin.ru:636")
  timeout <- Sys.getenv("AD_TIMEOUT",  unset = "5")
  ca      <- Sys.getenv("AD_CA_CERT",  unset = "")

  pwfile <- tempfile()
  file.create(pwfile); Sys.chmod(pwfile, "0600")
  con <- file(pwfile, open = "wb")
  writeBin(charToRaw(enc2utf8(password)), con)  # без завершающего \n
  close(con)
  on.exit(unlink(pwfile), add = TRUE)

  envv <- if (nzchar(ca)) paste0("LDAPTLS_CACERT=", ca) else "LDAPTLS_REQCERT=allow"
  if (!nzchar(ca)) message("[AUTH] WARN LDAPS без AD_CA_CERT — подмена контроллера не проверяется")

  res <- tryCatch(
    system2("ldapwhoami",
            args   = c("-x", "-H", shQuote(url), "-D", shQuote(bind_user),
                       "-y", shQuote(pwfile), "-o", paste0("nettimeout=", timeout)),
            stdout = TRUE, stderr = TRUE, env = envv),
    error = function(e) NULL
  )
  if (is.null(res)) return(255L)                    # нет ldapwhoami / не запустился
  code <- attr(res, "status")
  if (is.null(code)) 0L else as.integer(code)       # без status = exit 0 = успех
}

# Состояние предохранителя: после «домен не ответил» не опрашиваем AD_COOLDOWN сек.
.AD_STATE <- new.env(parent = emptyenv())
.AD_STATE$cooldown_until <- 0

# Доменная проверка. Статусы: ok | rejected | down | disabled | cooldown.
ad_authenticate <- function(login, password) {
  if (!identical(toupper(Sys.getenv("AD_ENABLED", unset = "FALSE")), "TRUE")) {
    return(list(status = "disabled"))
  }
  if (as.numeric(Sys.time()) < .AD_STATE$cooldown_until) {
    return(list(status = "cooldown"))
  }
  saw_down <- FALSE
  for (bu in ad_bind_candidates(login)) {
    code <- ad_bind(bu, password)
    if (code == 0L)  return(list(status = "ok", bind_user = bu))
    if (code == 49L) next          # неверные креды для этой формы имени — пробуем следующую
    saw_down <- TRUE               # 255/прочее — проблема контроллера
  }
  if (saw_down) {
    cd <- as.numeric(Sys.getenv("AD_COOLDOWN", unset = "60"))
    .AD_STATE$cooldown_until <- as.numeric(Sys.time()) + cd
    message("[AUTH] WARN AD недоступен — переход на локальную проверку")
    return(list(status = "down"))
  }
  list(status = "rejected")
}

# --- Локальный пароль (bcrypt) ---------------------------------------------

# BLNR_LOCAL_USERS = "login:$2a$...;login2:$2a$..." — bcrypt-хеши, не открытый
# текст. Нужен как резерв на случай недоступности AD и для локальной отладки.
local_authenticate <- function(login, password) {
  raw <- Sys.getenv("BLNR_LOCAL_USERS", unset = "")
  if (!nzchar(raw)) return(FALSE)
  key <- normalize_login(login)
  for (rec in strsplit(raw, ";")[[1]]) {
    pos <- regexpr(":", rec, fixed = TRUE)
    if (pos < 1) next
    u <- tolower(trimws(substr(rec, 1, pos - 1)))
    h <- trimws(substr(rec, pos + 1, nchar(rec)))
    if (u == key) {
      return(tryCatch(bcrypt::checkpw(password, h), error = function(e) FALSE))
    }
  }
  FALSE
}

# --- Единая точка входа -----------------------------------------------------

# Возвращает list(ok, login). Сообщение об ошибке НЕ различает «нет пользователя»
# и «неверный пароль» — вызывающий код показывает одну строку.
auth_check <- function(login, password) {
  if (!nzchar(trimws(as.character(login))) || !nzchar(password)) return(list(ok = FALSE))
  if (!user_allowed(login)) return(list(ok = FALSE))   # финансовый стенд: только белый список
  ad <- ad_authenticate(login, password)
  if (identical(ad$status, "ok")) {
    return(list(ok = TRUE, login = normalize_login(login)))
  }
  if (isTRUE(local_authenticate(login, password))) {
    return(list(ok = TRUE, login = normalize_login(login)))
  }
  list(ok = FALSE)
}
