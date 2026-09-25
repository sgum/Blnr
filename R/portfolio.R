# R/portfolio.R
#
# Бизнес-логика вкладки "Портфель Exante": получение текущих позиций
# (через Exante API, если настроены переменные окружения; иначе — по
# вручную заданному портфелю portfolio_holdings из global.R) и расчёт
# роста от даты покупки.

# Позиции портфеля: ticker, quantity, entry_price, current_price, source
# ("exante" | "manual"). Цена входа и текущая цена берутся напрямую из
# Exante API, если он настроен (это фактическая средняя цена исполнения и
# последняя цена по счёту); иначе используются данные из
# portfolio_holdings (реальные цены исполнения ордеров) и текущая цена
# заполняется позже котировкой с Yahoo (см. build_portfolio_metrics()).
get_portfolio_positions <- function() {
  if (exante_has_credentials()) {
    accounts <- exante_get_accounts()
    if (is.null(accounts$error) && length(accounts) > 0) {
      # Поле счёта — accountId (не id). Берём первый счёт, где реально есть
      # позиции: у пользователя несколько суб-счетов, и первый нередко пуст.
      account_ids <- vapply(accounts, function(a) a$accountId %||% a$id %||% NA_character_,
                            character(1))
      account_ids <- account_ids[!is.na(account_ids)]
      for (account_id in account_ids) {
        positions <- exante_get_positions(account_id)
        if (!is.null(positions$error)) next
        dt <- exante_positions_to_dt(positions)
        if (nrow(dt) > 0) {
          dt[, ticker := exante_symbol_to_ticker(symbolId)]
          dt[, source := "exante"]
          data.table::setnames(dt,
            c("averagePrice", "price"),
            c("entry_price", "current_price")
          )
          return(dt[, .(ticker, quantity, entry_price, current_price, source)])
        }
      }
    }
  }

  dt <- data.table::copy(portfolio_holdings)
  dt[, current_price := NA_real_]
  dt[, source := "manual"]
  dt[, .(ticker, quantity, entry_price, current_price, source)]
}

# Источник котировок — marketdata.app (R/marketdata.R), НЕ Yahoo.
# Yahoo с сервера приложений (Petr) отдаёт 401 на каждый запрос: «фактическая»
# половина дашборда молча оказывалась пустой ($0 / −100%), при этом стенд
# выглядел исправным. См. docs/dev.md.

# Цена закрытия тикера на дату (или ближайший предыдущий торговый день) —
# точка входа для расчёта роста.
get_close_on_date <- function(ticker, date) {
  md_close_on_date(ticker, date)
}

# Последняя доступная цена тикера.
get_last_close <- function(ticker) {
  md_last_price(ticker)
}

# Полная таблица метрик портфеля: количество, цена входа, текущая цена,
# стоимость, абсолютный рост (%), вес в портфеле, P&L. Цена входа/текущая
# цена берутся из positions (Exante API или portfolio_holdings), когда
# известны; недостающие значения досчитываются через marketdata.app.
#
# Дополнительно считается growth_from_base_pct — рост от base_date
# (по умолчанию FORECAST_BASELINE_DATE, 11.09.2026), а не от цены покупки:
# именно от этой даты считается прогноз в файле пользователя (см.
# R/forecast.R), так что сравнивать факт с прогнозом нужно на одной базе.
build_portfolio_metrics <- function(positions = get_portfolio_positions(),
                                     entry_date = min(portfolio_holdings$purchase_date),
                                     base_date = FORECAST_BASELINE_DATE) {
  dt <- data.table::copy(positions)

  dt[is.na(entry_price),   entry_price   := sapply(ticker, get_close_on_date, date = entry_date)]
  dt[is.na(current_price), current_price := sapply(ticker, get_last_close)]
  dt[, base_price := sapply(ticker, get_close_on_date, date = base_date)]

  dt[, entry_value   := quantity * entry_price]
  dt[, current_value := quantity * current_price]
  dt[, growth_pct     := (current_price / entry_price - 1) * 100]
  dt[, growth_from_base_pct := (current_price / base_price - 1) * 100]
  dt[, pnl           := current_value - entry_value]

  total_current <- sum(dt$current_value, na.rm = TRUE)
  dt[, weight_pct := current_value / total_current * 100]

  dt[]
}

# Сводные метрики по всему портфелю.
#
# na.rm = TRUE здесь НЕЛЬЗЯ: если источник котировок отдал ошибку, все
# current_price приходят NA, сумма с na.rm даёт 0, и витрина показывает
# «стоимость $0, рост −100%» — уверенную неправду вместо «нет данных».
# Именно так стенд выглядел 25.09.2026, когда marketdata.app упёрся в лимит
# кредитов. Поэтому: нет цены хотя бы по одной позиции — итог NA, а интерфейс
# обязан показать прочерк и причину.
summarize_portfolio <- function(metrics) {
  entry_value   <- sum(metrics$entry_value)
  current_value <- sum(metrics$current_value)
  list(
    entry_value   = entry_value,
    current_value = current_value,
    pnl           = current_value - entry_value,
    growth_pct    = (current_value / entry_value - 1) * 100,
    priced        = sum(is.finite(metrics$current_price)),
    total         = nrow(metrics)
  )
}
