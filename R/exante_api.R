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

# Опцион ли это. У акции symbolId из двух частей ("GS.NYSE"), у опциона —
# из четырёх ("AMD.CBOE.20G2026.C220").
exante_is_option <- function(symbol_id) {
  lengths(strsplit(as.character(symbol_id), ".", fixed = TRUE)) > 2L
}

# Базовый тикер инструмента: "NVDA.NASDAQ" -> "NVDA", "GS.NYSE" -> "GS".
# Для ОПЦИОНА возвращается symbolId целиком, а не базовая бумага. Иначе
# "AMD.CBOE.20G2026.C220" превратился бы в "AMD" и опционная позиция молча
# слилась бы с акционной, а оценивалась бы по цене акции — числа остались бы
# правдоподобными и стали бы неверными. На счёте владельца опционы торгуются
# (найдены в истории 25.09.2026), так что это не гипотетический случай.
exante_symbol_to_ticker <- function(symbol_id) {
  sid <- as.character(symbol_id)
  data.table::fifelse(exante_is_option(sid), sid, sub("\\..*$", "", sid))
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

# Транзакции счёта в виде таблицы реестра (см. R/ledger.R). Поля ответа
# проверены на боевом счёте 25.09.2026: valueDate — дата зачисления,
# `when` — миллисекунды, `sum` приходит строкой, `asset` различает денежную
# ногу ("USD") и ногу инструмента (symbolId), transactionPrice — цена
# исполнения, orderId связывает ноги одной сделки и её комиссию.
exante_transactions_dt <- function(account_id, limit = 5000) {
  tx <- exante_get_transactions(account_id, limit = limit)
  if (!is.null(tx$error)) return(tx)
  if (length(tx) == 0) return(empty_ledger())
  out <- data.table::rbindlist(lapply(tx, function(t) {
    data.table::data.table(
      id         = as.integer(t$id %||% NA),
      value_date = as.Date(t$valueDate %||% NA),
      type       = as.character(t$operationType %||% ""),
      symbol     = as.character(t$symbolId %||% ""),
      asset      = as.character(t$asset %||% ""),
      amount     = suppressWarnings(as.numeric(t$sum %||% NA)),
      price      = suppressWarnings(as.numeric(t$transactionPrice %||% NA)),
      order_id   = as.character(t$orderId %||% "")
    )
  }), fill = TRUE)
  out <- out[!is.na(value_date)]
  data.table::setorder(out, value_date, id)
  out[]
}

# Денежный остаток и чистые активы счёта в валюте отчёта.
exante_account_cash <- function(account_id, currency = "USD") {
  s <- exante_get_account_summary(account_id, currency)
  if (!is.null(s$error)) return(s)
  cash <- NA_real_
  for (c in s$currencies %||% list()) {
    if (identical(c$code, currency)) cash <- as.numeric(c$convertedValue %||% c$value)
  }
  list(
    cash  = cash,
    nav   = as.numeric(s$netAssetValue %||% NA),
    free  = as.numeric(s$freeMoney %||% NA),
    currency = currency
  )
}

# Счёт, на котором реально есть движение: у владельца несколько субсчетов, и
# первый нередко пуст. Берём тот, где есть позиции или ненулевой остаток.
exante_primary_account <- function() {
  accounts <- exante_get_accounts()
  if (!is.null(accounts$error) || length(accounts) == 0) return(NULL)
  ids <- vapply(accounts, function(a) a$accountId %||% a$id %||% NA_character_,
                character(1))
  ids <- ids[!is.na(ids)]
  for (id in ids) {
    s <- exante_get_account_summary(id)
    if (!is.null(s$error)) next
    nav <- suppressWarnings(as.numeric(s$netAssetValue %||% 0))
    if (length(s$positions %||% list()) > 0 || (is.finite(nav) && nav > 1)) return(id)
  }
  NULL
}

# --- Торговые поручения -----------------------------------------------------
#
# ГРАНИЦА ОТВЕТСТВЕННОСТИ. Эта функция отправляет РЕАЛЬНОЕ поручение на боевой
# счёт. По умолчанию apply = FALSE: она только собирает и возвращает payload,
# ничего не отправляя. apply = TRUE выставляется ТОЛЬКО из обработчика кнопки
# подтверждения, то есть по явному действию владельца счёта. Ни один
# автоматический путь — таймер, реактив, стартовый observe — не имеет права
# вызывать её с apply = TRUE.
#
# Поручение рыночное и внутридневное (duration = day): отложенных и
# стоп-заявок стенд не ставит сознательно — они живут дольше сессии и требуют
# отдельного управления отменой, которого здесь нет.
exante_place_order <- function(account_id, symbol_id, side, quantity,
                               apply = FALSE, order_type = "market",
                               duration = "day") {
  side <- tolower(trimws(side))
  if (!side %in% c("buy", "sell")) {
    return(list(error = "bad_side", message = "Сторона сделки — только buy или sell."))
  }
  qty <- suppressWarnings(as.numeric(quantity))
  if (!is.finite(qty) || qty <= 0) {
    return(list(error = "bad_quantity", message = "Количество должно быть положительным."))
  }
  payload <- list(
    accountId  = account_id,
    symbolId   = symbol_id,
    side       = side,
    quantity   = format(qty, scientific = FALSE, trim = TRUE),
    orderType  = order_type,
    duration   = duration
  )
  if (!isTRUE(apply)) {
    return(list(dry_run = TRUE, payload = payload))
  }

  creds <- exante_credentials()
  if (is.null(creds)) return(list(error = "no_credentials"))
  resp <- tryCatch(
    httr::POST(paste0(exante_base_url(), "/trade/3.0/orders"),
               exante_auth_header(creds),
               httr::content_type_json(),
               body = jsonlite::toJSON(payload, auto_unbox = TRUE),
               httr::timeout(30)),
    error = function(e) e
  )
  if (inherits(resp, "condition")) {
    return(list(error = "request_failed", message = conditionMessage(resp)))
  }
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  if (httr::status_code(resp) >= 400) {
    return(list(error = "http_error", status = httr::status_code(resp),
                message = substr(txt, 1, 400), payload = payload))
  }
  list(ok = TRUE, status = httr::status_code(resp),
       response = tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                           error = function(e) txt),
       payload = payload)
}

# symbolId для тикера: биржу берём из уже известных позиций счёта, а если
# бумаги в портфеле нет — из истории операций. Гадать суффикс нельзя:
# "GS.NYSE" и "GS.NASDAQ" — разные инструменты, и поручение ушло бы не туда.
exante_symbol_for_ticker <- function(ticker, ledger = NULL, positions = NULL) {
  tk <- toupper(trimws(ticker))
  cand <- character()
  if (!is.null(positions) && length(positions) > 0) {
    cand <- c(cand, vapply(positions, function(p) as.character(p$id %||% ""), character(1)))
  }
  if (!is.null(ledger) && nrow(ledger) > 0) {
    cand <- c(cand, unique(ledger$symbol))
  }
  cand <- cand[nzchar(cand)]
  cand <- cand[!exante_is_option(cand)]
  hit <- cand[toupper(exante_symbol_to_ticker(cand)) == tk]
  if (length(hit) == 0) return(NA_character_)
  hit[1]
}
