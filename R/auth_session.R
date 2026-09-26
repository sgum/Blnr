# R/auth_session.R
#
# Запоминание входа: человек, однажды набравший пароль, не набирает его при
# каждом обновлении страницы.
#
# УСТРОЙСТВО. Кука с ПОДПИСЬЮ, а не с признаком «вошёл». Внутри — логин, срок
# и HMAC-SHA256 на серверном секрете. Подделать нельзя: подпись не сходится.
# Никаких «session_id» в памяти процесса: воркер Shiny перезапускается при
# каждой выкатке и по простою, и хранимые в нём сессии умирают вместе с ним —
# человек снова у формы, то есть ровно та беда, ради которой всё затевалось.
#
# ЧЕГО ЭТА КУКА НЕ ДАЁТ. Она удостоверяет ТОЛЬКО личность. Право входа на
# стенд (белый список) и право распоряжаться счётом (BLNR_TRADERS) проверяются
# заново при каждом восстановлении и при каждом действии: человека могли убрать
# из списка, пока кука жила.
#
# ПОЧЕМУ НЕ HttpOnly. Куку ставит браузер из JS, потому что Shiny отдаёт
# страницу по вебсокету и заголовками ответа не распоряжается. Плата за это —
# кука видна скриптам страницы. Риск ограничен тем, что стенд не отображает
# пользовательский ввод и подставить на него чужой скрипт неоткуда; но если
# стенд начнёт показывать присланный контент, куку надо переносить на nginx.
#
# Секрет не задан — запоминание ВЫКЛЮЧЕНО целиком (отказ в безопасную сторону),
# а не работает с пустым ключом: подпись на пустом ключе подделывает кто угодно.

library(openssl)

BLNR_SESSION_COOKIE <- "dt_stand_auth"

blnr_session_secret <- function() Sys.getenv("BLNR_SESSION_SECRET", unset = "")

blnr_session_enabled <- function() {
  s <- blnr_session_secret()
  nzchar(s) && nchar(s) >= 32
}

# Срок жизни запомненного входа. Скользящий: при каждом восстановлении кука
# перевыпускается, поэтому активный пользователь пароль не набирает, а
# забытая на чужой машине вкладка протухает сама.
blnr_session_ttl_sec <- function() {
  h <- suppressWarnings(as.numeric(Sys.getenv("BLNR_SESSION_TTL_HOURS", unset = "12")))
  if (!is.finite(h) || h <= 0) h <- 12
  as.integer(h * 3600)
}

.session_sign <- function(payload) {
  paste(openssl::sha256(charToRaw(payload),
                        key = charToRaw(blnr_session_secret())), collapse = "")
}

# Токен вида v1.<логин>.<истекает>.<подпись>. Логин кодируется, чтобы точка в
# нём не ломала разбор.
session_token_make <- function(login, now = as.integer(Sys.time())) {
  if (!blnr_session_enabled()) return(NA_character_)
  l <- normalize_login(login)
  if (!nzchar(l)) return(NA_character_)
  exp <- now + blnr_session_ttl_sec()
  body <- paste0("v1.", openssl::base64_encode(charToRaw(l)), ".", exp)
  paste0(body, ".", .session_sign(body))
}

# Логин из токена или NA. Проверяются: формат, подпись, срок.
session_token_login <- function(token, now = as.integer(Sys.time())) {
  if (!blnr_session_enabled()) return(NA_character_)
  if (is.null(token) || is.na(token) || !nzchar(token)) return(NA_character_)
  parts <- strsplit(token, ".", fixed = TRUE)[[1]]
  if (length(parts) != 4L || !identical(parts[1], "v1")) return(NA_character_)
  body <- paste(parts[1:3], collapse = ".")
  # Сравнение подписи постоянного времени не нужно: ответ стенда не зависит от
  # того, на каком символе разошлось, и измерить это снаружи нечем. Но формат
  # проверяем ДО подписи, чтобы не считать HMAC от мусора.
  if (!identical(parts[4], .session_sign(body))) return(NA_character_)
  exp <- suppressWarnings(as.integer(parts[3]))
  if (!is.finite(exp) || exp <= now) return(NA_character_)
  l <- tryCatch(rawToChar(openssl::base64_decode(parts[2])),
                error = function(e) "")
  if (!nzchar(l)) return(NA_character_)
  normalize_login(l)
}

# Достать значение куки из заголовков запроса. Сервер читает ЗАГОЛОВОК, а не
# спрашивает у страницы: то, что прислал браузер, подделывается ровно так же,
# как и то, что прислал бы JS, но заголовок доступен сразу, без обмена
# сообщениями, и восстановление происходит до первой отрисовки.
session_cookie_value <- function(shiny_session, name = BLNR_SESSION_COOKIE) {
  raw <- tryCatch(shiny_session$request$HTTP_COOKIE, error = function(e) NULL)
  if (is.null(raw) || !nzchar(raw)) return(NA_character_)
  for (chunk in strsplit(raw, ";", fixed = TRUE)[[1]]) {
    kv <- sub("^\\s+", "", chunk)
    i <- regexpr("=", kv, fixed = TRUE)
    if (i < 1) next
    if (identical(substr(kv, 1, i - 1), name)) {
      return(substr(kv, i + 1, nchar(kv)))
    }
  }
  NA_character_
}

# JS, ставящий куку. Secure — только по https: на http (локальная отладка)
# браузер куку с этим флагом молча не сохранит, и запоминание «не работает»
# без единого сообщения.
session_cookie_set_js <- function(token, ttl = blnr_session_ttl_sec(),
                                  name = BLNR_SESSION_COOKIE) {
  sprintf(
    "document.cookie='%s='+%s+'; Path=/; Max-Age=%d; SameSite=Strict'+(location.protocol==='https:'?'; Secure':'');",
    name, jsonlite::toJSON(token, auto_unbox = TRUE), as.integer(ttl))
}

session_cookie_clear_js <- function(name = BLNR_SESSION_COOKIE) {
  sprintf("document.cookie='%s=; Path=/; Max-Age=0; SameSite=Strict';", name)
}
