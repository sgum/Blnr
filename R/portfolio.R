# R/portfolio.R
#
# Бизнес-логика вкладки "Портфель Exante": получение текущих позиций
# (через Exante API, если настроены переменные окружения; иначе — по
# вручную заданному портфелю portfolio_holdings из global.R) и расчёт
# роста от даты покупки.

# Позиции портфеля: ticker, quantity, source ("exante" | "manual").
get_portfolio_positions <- function() {
  if (exante_has_credentials()) {
    accounts <- exante_get_accounts()
    if (is.null(accounts$error) && length(accounts) > 0) {
      account_id <- accounts[[1]]$id
      positions  <- exante_get_positions(account_id)
      if (is.null(positions$error)) {
        dt <- exante_positions_to_dt(positions)
        if (nrow(dt) > 0) {
          dt[, source := "exante"]
          data.table::setnames(dt, "symbolId", "ticker")
          return(dt[, .(ticker, quantity, source)])
        }
      }
    }
  }

  dt <- data.table::copy(portfolio_holdings)
  dt[, source := "manual"]
  dt[, .(ticker, quantity, source)]
}

# Цена закрытия тикера на дату (или ближайший предыдущий торговый день) —
# точка входа для расчёта роста.
get_close_on_date <- function(ticker, date) {
  data <- tryCatch(
    quantmod::getSymbols(ticker, src = "yahoo",
                          from = date - 7, to = date + 1,
                          auto.assign = FALSE),
    error = function(e) NULL
  )
  if (is.null(data) || nrow(data) == 0) return(NA_real_)
  as.numeric(quantmod::Cl(data)[nrow(data)])
}

# Последняя доступная цена закрытия тикера.
get_last_close <- function(ticker) {
  data <- tryCatch(
    quantmod::getSymbols(ticker, src = "yahoo",
                          from = Sys.Date() - 10, to = Sys.Date() + 1,
                          auto.assign = FALSE),
    error = function(e) NULL
  )
  if (is.null(data) || nrow(data) == 0) return(NA_real_)
  as.numeric(quantmod::Cl(data)[nrow(data)])
}

# Полная таблица метрик портфеля: количество, цена входа, текущая цена,
# стоимость, абсолютный рост (%), вес в портфеле, P&L.
build_portfolio_metrics <- function(positions = get_portfolio_positions(),
                                     entry_date = min(portfolio_holdings$purchase_date)) {
  dt <- data.table::copy(positions)

  dt[, entry_price   := sapply(ticker, get_close_on_date, date = entry_date)]
  dt[, current_price := sapply(ticker, get_last_close)]

  dt[, entry_value   := quantity * entry_price]
  dt[, current_value := quantity * current_price]
  dt[, growth_pct    := (current_price / entry_price - 1) * 100]
  dt[, pnl           := current_value - entry_value]

  total_current <- sum(dt$current_value, na.rm = TRUE)
  dt[, weight_pct := current_value / total_current * 100]

  dt[]
}

# Сводные метрики по всему портфелю.
summarize_portfolio <- function(metrics) {
  entry_value   <- sum(metrics$entry_value, na.rm = TRUE)
  current_value <- sum(metrics$current_value, na.rm = TRUE)
  list(
    entry_value   = entry_value,
    current_value = current_value,
    pnl           = current_value - entry_value,
    growth_pct    = (current_value / entry_value - 1) * 100
  )
}
