# R/exante_api.R
#
# Клиент для Exante REST API v3.0 (https://api-docs.exante.eu).
# Учётные данные читаются ТОЛЬКО из переменных окружения и никогда не
# должны попадать в код или коммит — см. docs/EXANTE_API.md и
# .Renviron.example.

library(httr)
library(jsonlite)
library(jose)

`%||%` <- function(a, b) if (is.null(a)) b else a

# Базовый URL API. EXANTE_ENV=demo переключает на тестовый контур,
# по умолчанию используется боевой (live).
exante_base_url <- function() {
  if (identical(Sys.getenv("EXANTE_ENV", unset = "live"), "demo")) {
    "https://api-demo.exante.eu"
  } else {
    "https://api-live.exante.eu"
  }
}

# Учётные данные приложения Exante: EXANTE_CLIENT_ID, EXANTE_APP_ID,
# EXANTE_SHARED_KEY. Возвращает NULL, если хоть одна переменная не задана,
# чтобы приложение могло прозрачно откатиться на резервный источник данных.
exante_credentials <- function() {
  client_id  <- Sys.getenv("EXANTE_CLIENT_ID")
  app_id     <- Sys.getenv("EXANTE_APP_ID")
  shared_key <- Sys.getenv("EXANTE_SHARED_KEY")

  if (client_id == "" || app_id == "" || shared_key == "") {
    return(NULL)
  }

  list(client_id = client_id, app_id = app_id, shared_key = shared_key)
}

exante_has_credentials <- function() {
  !is.null(exante_credentials())
}

# Подписанный JWT (HS256) для заголовка Authorization: Bearer <token>.
exante_build_jwt <- function(creds,
                              scopes = c("symbols", "feed", "change",
                                         "crossrates", "summary",
                                         "accounts", "orders",
                                         "transactions"),
                              ttl_seconds = 60) {
  now <- as.integer(Sys.time())
  claim <- jose::jwt_claim(
    iss = creds$client_id,
    sub = creds$app_id,
    aud = as.list(scopes),
    iat = now,
    exp = now + ttl_seconds
  )
  jose::jwt_encode_hmac(claim, secret = creds$shared_key)
}

# Низкоуровневый GET-запрос к Exante API. Никогда не бросает исключение —
# при отсутствии учётных данных или ошибке запроса возвращает список с
# полем `error`, которое вызывающий код должен проверять.
exante_get <- function(path, query = list()) {
  creds <- exante_credentials()
  if (is.null(creds)) {
    return(list(error = "exante_not_configured"))
  }

  token <- exante_build_jwt(creds)

  resp <- tryCatch(
    httr::GET(
      paste0(exante_base_url(), path),
      httr::add_headers(Authorization = paste("Bearer", token)),
      query = query,
      httr::timeout(15)
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

# Торговые счета, доступные приложению.
exante_get_accounts <- function() {
  exante_get("/trade/3.0/accounts")
}

# Сводка по счёту: позиции, остатки, стоимость портфеля в заданной валюте.
exante_get_account_summary <- function(account_id, currency = "USD") {
  exante_get(sprintf("/trade/3.0/summary/%s/%s", account_id, currency))
}

# Текущие позиции по счёту (список объектов с symbolId/quantity/price/...).
exante_get_positions <- function(account_id) {
  summary <- exante_get_account_summary(account_id)
  if (!is.null(summary$error)) {
    return(summary)
  }
  summary$positions %||% list()
}

# Последняя котировка по инструменту через market data API.
exante_get_last_quote <- function(symbol_id) {
  exante_get(sprintf("/md/3.0/feed/%s/last", symbol_id))
}

# Преобразует список позиций Exante в data.table для отображения в таблице.
exante_positions_to_dt <- function(positions) {
  if (is.null(positions) || length(positions) == 0) {
    return(data.table::data.table(
      symbolId = character(), quantity = numeric(),
      averagePrice = numeric(), price = numeric(),
      convertedValue = numeric(), pnl = numeric()
    ))
  }

  rows <- lapply(positions, function(p) {
    data.table::data.table(
      symbolId       = p$symbolId %||% NA_character_,
      quantity       = as.numeric(p$quantity %||% NA),
      averagePrice   = as.numeric(p$averagePrice %||% NA),
      price          = as.numeric(p$price %||% NA),
      convertedValue = as.numeric(p$convertedValue %||% NA),
      pnl            = as.numeric(p$pnl %||% NA)
    )
  })

  data.table::rbindlist(rows, fill = TRUE)
}
