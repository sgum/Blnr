# R/exante_api.R
#
# Клиент для Exante REST API (https://api-docs.exante.eu).
#
# ВАЖНО про аутентификацию. Реальные API-креды Exante, выданные в личном
# кабинете (раздел «API»), — это ПАРА значений: идентификатор приложения
# ("api", UUID) и «секретный ключ». Такая пара работает по схеме HTTP Basic:
#   Authorization: Basic base64("<api_id>:<shared_key>")
# на боевом контуре https://api-live.exante.eu.
#
# JWT-схема (client_id + app_id + shared_key, три значения) для этих кред
# НЕ проходит — проверено вживую 22.09.2026: JWT давал 401, Basic — 200.
# Поэтому клиент использует Basic. Подробности и раскладка эндпоинтов —
# в docs/EXANTE_API.md, креды — в .Renviron (см. .Renviron.example).
#
# Учётные данные читаются ТОЛЬКО из переменных окружения и никогда не
# должны попадать в код или коммит.

library(httr)
library(jsonlite)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Базовый URL API. EXANTE_ENV=demo переключает на тестовый контур,
# по умолчанию используется боевой (live). ВНИМАНИЕ: один и тот же ключ,
# как правило, действует только в одном контуре (боевые креды к demo не
# подходят, и наоборот).
exante_base_url <- function() {
  if (identical(Sys.getenv("EXANTE_ENV", unset = "live"), "demo")) {
    "https://api-demo.exante.eu"
  } else {
    "https://api-live.exante.eu"
  }
}

# Учётные данные приложения Exante для Basic-аутентификации: идентификатор
# приложения и секретный ключ. Основные имена переменных — EXANTE_API_ID и
# EXANTE_SHARED_KEY; для совместимости со старым .Renviron идентификатор
# берётся также из EXANTE_APP_ID / EXANTE_CLIENT_ID. Возвращает NULL, если
# чего-то не хватает, чтобы приложение прозрачно откатилось на резервный
# источник данных.
exante_credentials <- function() {
  api_id <- Sys.getenv("EXANTE_API_ID")
  if (api_id == "") api_id <- Sys.getenv("EXANTE_APP_ID")
  if (api_id == "") api_id <- Sys.getenv("EXANTE_CLIENT_ID")
  shared_key <- Sys.getenv("EXANTE_SHARED_KEY")

  if (api_id == "" || shared_key == "") {
    return(NULL)
  }

  list(api_id = api_id, shared_key = shared_key)
}

exante_has_credentials <- function() {
  !is.null(exante_credentials())
}

# Значение заголовка Authorization для Basic-аутентификации.
# base64_enc может переносить строку на 76-м символе — убираем любые
# пробелы/переносы, иначе заголовок Authorization ломается (HTTP 401).
exante_auth_header <- function(creds) {
  token <- jsonlite::base64_enc(charToRaw(paste0(creds$api_id, ":", creds$shared_key)))
  token <- gsub("[[:space:]]", "", token)
  httr::add_headers(Authorization = paste("Basic", token))
}

# Низкоуровневый GET-запрос к Exante API. Никогда не бросает исключение —
# при отсутствии учётных данных или ошибке запроса возвращает список с
# полем `error`, которое вызывающий код должен проверять.
exante_get <- function(path, query = list()) {
  creds <- exante_credentials()
  if (is.null(creds)) {
    return(list(error = "exante_not_configured"))
  }

  resp <- tryCatch(
    httr::GET(
      paste0(exante_base_url(), path),
      exante_auth_header(creds),
      query = query,
      httr::timeout(30)
    ),
    error = function(e) e
  )

  if (inherits(resp, "error") || inherits(resp, "condition")) {
    return(list(error = "exante_request_failed", message = conditionMessage(resp)))
  }

  if (httr::status_code(resp) >= 400) {
    return(list(
      error   = "exante_http_error",
      status  = httr::status_code(resp),
      message = httr::content(resp, as = "text", encoding = "UTF-8")
    ))
  }

  jsonlite::fromJSON(
    httr::content(resp, as = "text", encoding = "UTF-8"),
    simplifyVector = FALSE
  )
}

# Торговые счета, доступные приложению. Возвращает список объектов вида
# list(accountId = "VMZ3002.002", status = "Full").
exante_get_accounts <- function() {
  exante_get("/md/2.0/accounts")
}

# Сводка по счёту: позиции, остатки, стоимость портфеля в заданной валюте.
exante_get_account_summary <- function(account_id, currency = "USD") {
  exante_get(sprintf("/md/2.0/summary/%s/%s", account_id, currency))
}

# Текущие позиции по счёту (список объектов; symbolId лежит в поле `id`).
exante_get_positions <- function(account_id, currency = "USD") {
  summary <- exante_get_account_summary(account_id, currency)
  if (!is.null(summary$error)) {
    return(summary)
  }
  summary$positions %||% list()
}

# История ордеров (Exante v3.0 работает с Basic-аутентификацией). limit —
# сколько последних ордеров вернуть. Полезно для сверки «куда делись позиции».
exante_get_orders <- function(account_id = NULL, limit = 500) {
  query <- list(limit = limit)
  if (!is.null(account_id)) query$accountId <- account_id
  exante_get("/trade/3.0/orders", query)
}

# История транзакций по счёту (сделки, дивиденды, комиссии, переводы).
# from_date/to_date — ISO-8601 (например "2026-09-01T00:00:00.000Z").
exante_get_transactions <- function(account_id, from_date = NULL,
                                     to_date = NULL, limit = 1000) {
  query <- list(accountId = account_id, limit = limit)
  if (!is.null(from_date)) query$fromDate <- from_date
  if (!is.null(to_date))   query$toDate   <- to_date
  exante_get("/md/2.0/transactions", query)
}

# Последняя котировка по инструменту через market data API.
exante_get_last_quote <- function(symbol_id) {
  exante_get(sprintf("/md/2.0/feed/%s/last", symbol_id))
}

# Тикер из symbolId Exante: "NVDA.NASDAQ" -> "NVDA", "GS.NYSE" -> "GS".
# Опционы вида "AMD.CBOE.20G2026.C220" сводятся к базовому тикеру "AMD".
exante_symbol_to_ticker <- function(symbol_id) {
  sub("\\..*$", "", as.character(symbol_id))
}

# Преобразует список позиций Exante в data.table для отображения в таблице.
# В реальном ответе /md/2.0/summary поля позиции: id (symbolId), symbolType,
# quantity, price, averagePrice, value, convertedValue, pnl/convertedPnl,
# currency — проверено 22.09.2026.
exante_positions_to_dt <- function(positions) {
  if (is.null(positions) || length(positions) == 0) {
    return(data.table::data.table(
      symbolId = character(), quantity = numeric(),
      averagePrice = numeric(), price = numeric(),
      convertedValue = numeric(), pnl = numeric()
    ))
  }

  rows <- lapply(positions, function(p) {
    symbol <- p$symbolId %||% p$id %||% NA_character_
    data.table::data.table(
      symbolId       = symbol,
      quantity       = as.numeric(p$quantity %||% NA),
      averagePrice   = as.numeric(p$averagePrice %||% NA),
      price          = as.numeric(p$price %||% NA),
      convertedValue = as.numeric(p$convertedValue %||% p$value %||% NA),
      pnl            = as.numeric(p$pnl %||% p$convertedPnl %||% NA)
    )
  })

  data.table::rbindlist(rows, fill = TRUE)
}
