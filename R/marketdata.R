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
# Эндпоинты: акции и ETF — /v1/stocks/..., индексы — /v1/indices/...
# (см. R/watchlist.R, поле type).

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

# --- Котировка -------------------------------------------------------------
# Последняя цена по инструменту. Возвращает NA_real_ при любой ошибке —
# вызывающий код отличает «нет данных» по NA, а причина уходит в журнал.
md_last_price <- function(ticker, type = instrument_type(ticker)) {
  key <- paste0("q_", type, "_", ticker)
  hit <- .md_cache_get(key, ttl_sec = 60)
  if (!is.null(hit)) return(hit)

  seg <- if (identical(type, "index")) "indices" else "stocks"
  res <- md_get(sprintf("/%s/quotes/%s/", seg, ticker))
  if (!is.null(res$error)) {
    message(sprintf("[MD] WARN котировка %s: %s", ticker, res$error))
    return(NA_real_)
  }
  px <- suppressWarnings(as.numeric(res$last[1]))
  if (is.na(px) && !is.null(res$mid)) px <- suppressWarnings(as.numeric(res$mid[1]))
  .md_cache_put(key, px)
  px
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
md_candles <- function(ticker, days = WATCHLIST_RETRO_DAYS,
                       type = instrument_type(ticker)) {
  key <- paste0("c_", type, "_", ticker, "_", days, "_", format(Sys.Date()))
  hit <- .md_cache_get(key, ttl_sec = .md_ttl_today())
  if (!is.null(hit)) return(hit)

  seg <- if (identical(type, "index")) "indices" else "stocks"
  res <- md_get(sprintf("/%s/candles/D/%s/", seg, ticker),
                query = list(countback = days))
  empty <- data.table::data.table(
    date = as.Date(character()), open = numeric(), high = numeric(),
    low = numeric(), close = numeric(), volume = numeric()
  )
  if (!is.null(res$error)) {
    message(sprintf("[MD] WARN свечи %s: %s", ticker, res$error))
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
    # у индексов объёма нет — колонка остаётся NA, а не падает
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
                             type = instrument_type(ticker)) {
  cnd <- md_candles(ticker, days = days, type = type)
  if (nrow(cnd) == 0) return(NA_real_)
  target <- as.Date(on_date)
  sub <- cnd[date <= target]
  if (nrow(sub) == 0) return(NA_real_)
  sub[nrow(sub), close]
}
