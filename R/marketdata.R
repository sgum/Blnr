# R/marketdata.R
#
# Клиент marketdata.app — источник котировок стенда (заменил Yahoo).
#
# Почему не Yahoo: с сервера приложений (Petr) Yahoo отдаёт 401 на каждый
# запрос — «фактическая» половина дашборда молча оказывалась пустой, а стенд
# при этом выглядел исправным. marketdata.app доступен с Petr (проверено
# 24.09.2026) и это тот же источник, что кормит рабочую таблицу и внешнюю
# модель прогноза — значит дашборд и прогноз считаются на одних данных.
#
# Токен — только из окружения (MARKETDATA_TOKEN), в git не попадает.
# Все наблюдаемые инструменты — акции и ETF, то есть эндпоинт один:
# /v1/stocks/... Индексы из реестра убраны, источник их на нашем тарифе не
# отдаёт (см. R/watchlist.R).

library(httr)
library(jsonlite)

MD_BASE <- "https://api.marketdata.app/v1"

md_token <- function() Sys.getenv("MARKETDATA_TOKEN", unset = "")
md_has_token <- function() nzchar(md_token())

# --- Кэш -------------------------------------------------------------------
# Shiny поднимает воркер на каждую сессию и дёргает котировки по всему
# watchlist; без кэша это сжигает квоту API и добавляет секунды к отрисовке.
# Котировки живут 60 с, дневные свечи — до конца суток (они всё равно
# обновляются раз в день).
MD_CACHE_DIR <- Sys.getenv("BLNR_CACHE_DIR",
                            unset = file.path(dirname(SNAPSHOT_LOG_PATH), "md-cache"))

.md_cache_get <- function(key, ttl_sec) {
  f <- file.path(MD_CACHE_DIR, paste0(key, ".rds"))
  if (!file.exists(f)) return(NULL)
  age <- as.numeric(difftime(Sys.time(), file.mtime(f), units = "secs"))
  if (age > ttl_sec) return(NULL)
  tryCatch(readRDS(f), error = function(e) NULL)
}

.md_cache_put <- function(key, value) {
  tryCatch({
    dir.create(MD_CACHE_DIR, showWarnings = FALSE, recursive = TRUE)
    saveRDS(value, file.path(MD_CACHE_DIR, paste0(key, ".rds")))
  }, error = function(e) invisible(NULL))
  invisible(value)
}

# Сколько секунд осталось до конца суток — TTL дневных свечей.
.md_ttl_today <- function() {
  as.numeric(difftime(as.POSIXct(paste(Sys.Date() + 1, "00:05:00")), Sys.time(),
                      units = "secs"))
}

# --- Низкоуровневый запрос -------------------------------------------------
# Никогда не бросает исключение: возвращает список с полем `error`, который
# вызывающий код обязан проверить (иначе пустой ответ тихо превращается в NA
# и стенд выглядит исправным с пустыми колонками).
md_get <- function(path, query = list()) {
  if (!md_has_token()) return(list(error = "marketdata_no_token"))
  resp <- tryCatch(
    httr::GET(paste0(MD_BASE, path),
              httr::add_headers(Authorization = paste("Bearer", md_token())),
              query = query, httr::timeout(25)),
    error = function(e) e
  )
  if (inherits(resp, "condition")) {
    return(list(error = "marketdata_request_failed", message = conditionMessage(resp)))
  }
  if (httr::status_code(resp) >= 400) {
    return(list(error = "marketdata_http_error", status = httr::status_code(resp),
                message = substr(httr::content(resp, as = "text", encoding = "UTF-8"), 1, 300)))
  }
  out <- tryCatch(
    jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8")),
    error = function(e) list(error = "marketdata_bad_json")
  )
  # API отвечает 200 и s:"no_data"/"error" — это НЕ успех.
  if (!is.null(out$s) && !identical(out$s, "ok")) {
    return(list(error = "marketdata_no_data", status_field = out$s,
                message = if (!is.null(out$errmsg)) out$errmsg else NA_character_))
  }
  out
}

# --- Состояние источника ----------------------------------------------------
# Почему это вообще есть. У аккаунта marketdata.app конечный лимит кредитов, и
# при его исчерпании API отвечает 200/429 с {"s":"error"} по КАЖДОМУ запросу.
# Если такой ответ превращать просто в NA, витрина показывает не ошибку, а
# правдоподобные $0 и −100% — то есть врёт уверенно. Ровно это случилось
# 25.09.2026 (и ровно про это уже была запись в docs/dev.md про Yahoo).
# Поэтому причина последней неудачи хранится и поднимается в интерфейс.
.MD_STATE <- new.env(parent = emptyenv())
.MD_STATE$last_error <- NULL

md_note_error <- function(res) {
  .MD_STATE$last_error <- list(
    code = res$error,
    status = res$status %||% NA_integer_,
    message = res$message %||% NA_character_,
    at = Sys.time()
  )
  invisible(NULL)
}

md_last_error <- function() .MD_STATE$last_error

# Человеческая причина для шапки стенда. NULL, если сбоев не было.
md_status_text <- function() {
  e <- md_last_error()
  if (is.null(e)) return(NULL)
  if (identical(e$code, "marketdata_store_empty")) {
    return("хранилище рядов пусто — ночная загрузка ещё не отработала")
  }
  if (identical(e$code, "marketdata_no_token")) return("не задан MARKETDATA_TOKEN")
  if (identical(e$status, 429L) || identical(e$status, 429) ||
      (!is.na(e$message) && grepl("credit limit", e$message, ignore.case = TRUE))) {
    return("исчерпан лимит кредитов marketdata.app")
  }
  if (!is.na(e$status)) return(sprintf("marketdata.app ответил %s", e$status))
  e$code
}

# --- Котировка -------------------------------------------------------------
# Последняя цена = закрытие последней дневной свечи, а НЕ отдельный запрос
# /quotes. Причина — лимит кредитов: один запрос свечей на инструмент в день
# обслуживает сразу и текущую цену, и цену на любую прошлую дату, и график.
# Отдельные котировки жгли по запросу на бумагу при каждом открытии стенда и
# выедали суточный лимит за несколько перезагрузок.
# Возвращает NA_real_, когда данных нет; причина — в md_last_error().
md_last_price <- function(ticker) {
  cnd <- md_candles(ticker)
  if (nrow(cnd) == 0) return(NA_real_)
  cnd[nrow(cnd), close]
}

# Дата, на которую известна «текущая» цена: последняя торговая сессия в свечах.
md_last_price_date <- function(ticker) {
  cnd <- md_candles(ticker)
  if (nrow(cnd) == 0) return(as.Date(NA))
  cnd[nrow(cnd), date]
}

# Котировки по вектору тикеров — data.table(ticker, last).
md_last_prices <- function(tickers) {
  data.table::rbindlist(lapply(tickers, function(tk) {
    data.table::data.table(ticker = tk, last = md_last_price(tk))
  }))
}

# --- Дневные свечи ---------------------------------------------------------
# Ретроспектива на `days` торговых дней (countback — API сам отсчитывает
# назад от последней сессии, не требуя календарной арифметики и не промахиваясь
# на выходных/праздниках).
# Возвращает data.table(date, open, high, low, close, volume) или пустую.
# В интернет ходит ТОЛЬКО загрузчик (scripts/fetch_marketdata.R), который
# запускается заданием Jenkins раз в сутки. Стенд читает хранилище и при
# пустом хранилище честно говорит «нет данных», а не выжигает лимит запросов
# на каждом открытии экрана.
md_online_allowed <- function() {
  identical(toupper(Sys.getenv("BLNR_ALLOW_ONLINE", unset = "false")), "TRUE")
}

md_candles <- function(ticker, days = WATCHLIST_RETRO_DAYS,
                       online = md_online_allowed()) {
  st <- store_read_candles(ticker)
  if (nrow(st) > 0) return(utils::tail(st, days))
  if (!online) {
    md_note_error(list(error = "marketdata_store_empty"))
    return(empty_candles())
  }

  key <- paste0("c_", ticker, "_", days, "_", format(Sys.Date()))
  hit <- .md_cache_get(key, ttl_sec = .md_ttl_today())
  if (!is.null(hit)) return(hit)

  res <- md_get(sprintf("/stocks/candles/D/%s/", ticker),
                query = list(countback = days))
  empty <- empty_candles()
  if (!is.null(res$error)) {
    md_note_error(res)
    message(sprintf("[MD] WARN свечи %s: %s (%s)", ticker, res$error,
                    res$message %||% ""))
    return(empty)
  }
  n <- length(res$t)
  if (is.null(n) || n == 0) return(empty)
  out <- data.table::data.table(
    date   = as.Date(as.POSIXct(as.numeric(res$t), origin = "1970-01-01", tz = "UTC")),
    open   = as.numeric(res$o),
    high   = as.numeric(res$h),
    low    = as.numeric(res$l),
    close  = as.numeric(res$c),
    volume = if (!is.null(res$v)) as.numeric(res$v) else NA_real_
  )
  data.table::setorder(out, date)
  .md_cache_put(key, out)
  out
}

# Цена закрытия на дату (или ближайший предыдущий торговый день) — опора для
# расчёта роста «от базы». NA, если истории нет.
# ВНИМАНИЕ: аргумент называется on_date, а не date. Внутри `[.data.table`
# имя `date` резолвится в СТОЛБЕЦ, поэтому `cnd[date <= as.Date(date)]` всегда
# истинно и функция молча возвращает последнюю цену вместо цены на дату —
# «рост от базы» тогда сравнивает текущую цену с самой собой и даёт ~0.
# Ровно эти грабли уже были в forecast_for_date(), см. docs/dev.md.
md_close_on_date <- function(ticker, on_date, days = WATCHLIST_RETRO_DAYS,
                             online = md_online_allowed()) {
  cnd <- md_candles(ticker, days = days, online = online)
  if (nrow(cnd) == 0) return(NA_real_)
  target <- as.Date(on_date)
  sub <- cnd[date <= target]
  if (nrow(sub) == 0) return(NA_real_)
  sub[nrow(sub), close]
}

# Цена на дату И на предыдущую сессию за один проход по ряду — для показателя
# «за день» на выбранном моменте времени. Отдельными вызовами это два чтения
# одного и того же файла на каждую бумагу.
# ВНИМАНИЕ: аргумент снова НЕ называется date (см. предупреждение выше).
md_close_with_prev <- function(ticker, on_date, days = WATCHLIST_RETRO_DAYS,
                               online = md_online_allowed()) {
  empty <- list(close = NA_real_, prev = NA_real_, session = as.Date(NA))
  cnd <- md_candles(ticker, days = days, online = online)
  if (nrow(cnd) == 0) return(empty)
  target <- as.Date(on_date)
  sub <- cnd[date <= target]
  n <- nrow(sub)
  if (n == 0) return(empty)
  list(
    close   = sub[n, close],
    prev    = if (n >= 2) sub[n - 1L, close] else NA_real_,
    session = sub[n, date]
  )
}

# Проверка, что источник вообще знает такой тикер. ОДИН запрос — и он
# осознанный: реестр с несуществующим инструментом ронял бы ночную загрузку
# каждую ночь (она «всё или ничего»), а ловить это раз в сутки по красной
# сборке дороже, чем проверить при добавлении.
md_probe_ticker <- function(ticker) {
  if (!md_has_token()) return(list(ok = FALSE, message = "не задан MARKETDATA_TOKEN"))
  res <- md_get(sprintf("/stocks/candles/D/%s/", toupper(trimws(ticker))),
                query = list(countback = 5))
  if (!is.null(res$error)) {
    md_note_error(res)
    return(list(ok = FALSE, message = md_status_text() %||% res$error))
  }
  n <- length(res$c %||% NULL)
  if (n == 0) return(list(ok = FALSE, message = "источник вернул пустой ряд"))
  list(ok = TRUE, message = sprintf("получено %d свечей", n), rows = n)
}
