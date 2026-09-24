# R/portfolio_functions.R
# Логика вкладки "Мониторинг портфеля": сравнение факта с прогнозом темпов роста.

portfolio_holdings <- data.table(
  ticker   = c("GS", "GE", "AMD", "GOOG", "NVDA"),
  company  = c("Goldman Sachs", "General Electric", "AMD", "Google", "Nvidia"),
  quantity = c(5, 16, 8, 3, 17)
)

purchase_date      <- as.Date("2026-09-14")
forecast_base_date <- as.Date("2026-09-11")

# Псевдонимы, по которым тикер можно опознать в произвольном файле прогноза
ticker_aliases <- list(
  GS   = c("gs", "goldman sachs", "goldman", "голдман сакс", "голдман"),
  GE   = c("ge", "general electric", "дженерал электрик"),
  AMD  = c("amd", "advanced micro devices"),
  GOOG = c("goog", "googl", "google", "alphabet", "гугл", "алфабет", "алфавит"),
  NVDA = c("nvda", "nvidia", "энвидиа", "нвидиа")
)

match_ticker <- function(name) {
  key <- tolower(trimws(as.character(name)))
  for (tk in names(ticker_aliases)) {
    if (key %in% ticker_aliases[[tk]]) return(tk)
  }
  NA_character_
}

# Читает файл прогноза: широкий формат (дата + столбец на каждую акцию).
# Значения могут быть заданы как доли (0.02), проценты (2) или уровни цены —
# масштаб определяется автоматически по каждому столбцу отдельно, и всё
# приводится к темпу роста (доля) от forecast_base_date.
read_forecast_xlsx <- function(path) {
  raw <- openxlsx::read.xlsx(path, detectDates = TRUE)
  if (nrow(raw) == 0) stop("Файл прогноза пуст")

  is_date_col <- sapply(raw, function(x) inherits(x, "Date") || inherits(x, "POSIXct"))
  date_col <- if (any(is_date_col)) names(raw)[is_date_col][1] else names(raw)[1]
  raw[[date_col]] <- as.Date(raw[[date_col]])

  value_cols <- setdiff(names(raw), date_col)
  ticker_map <- setNames(vapply(value_cols, match_ticker, character(1)), value_cols)
  value_cols <- value_cols[!is.na(ticker_map[value_cols])]
  if (length(value_cols) == 0) {
    stop("Не удалось сопоставить ни один столбец файла прогноза с тикерами портфеля (GS, GE, AMD, GOOG, NVDA)")
  }

  long <- rbindlist(lapply(value_cols, function(col) {
    data.table(
      date   = raw[[date_col]],
      ticker = ticker_map[[col]],
      value  = suppressWarnings(as.numeric(raw[[col]]))
    )
  }))
  long <- long[!is.na(value)]

  long[, forecast_growth := {
    m <- median(abs(value), na.rm = TRUE)
    if (is.na(m)) value
    else if (m <= 1.5) value                       # уже доли, напр. 0.02 = +2%
    else if (m <= 100) value / 100                  # проценты, напр. 2 = +2%
    else value / value[which.min(date)] - 1         # уровни цены -> темп роста от первой даты
  }, by = ticker]

  long[order(ticker, date), .(date, ticker, forecast_growth)]
}

# Фактические котировки и темп роста от forecast_base_date (Yahoo Finance)
fetch_actual_growth <- function(tickers, from = forecast_base_date, to = Sys.Date()) {
  actual <- rbindlist(lapply(tickers, function(tk) {
    px <- tryCatch(
      quantmod::getSymbols(tk, src = "yahoo", from = from - 10, to = to + 1, auto.assign = FALSE),
      error = function(e) NULL
    )
    if (is.null(px)) return(NULL)
    cl <- quantmod::Cl(px)
    dt <- data.table(date = as.Date(index(cl)), price = as.numeric(coredata(cl)))
    dt[, ticker := tk]
    dt
  }), fill = TRUE)

  if (is.null(actual) || nrow(actual) == 0) return(data.table())

  actual <- actual[order(ticker, date)]
  actual[, base_price     := price[which.min(abs(date - forecast_base_date))], by = ticker]
  actual[, purchase_price := price[which.min(abs(date - purchase_date))], by = ticker]
  actual[, actual_growth  := price / base_price - 1]
  actual[date >= forecast_base_date]
}

# Накопленная ошибка = сумма модулей расхождений темпа роста между соседними
# датами (т.е. накопленная невязка прогнозных и фактических приращений).
add_cumulative_error <- function(dt, group_col) {
  dt <- dt[order(get(group_col), date)]
  dt[, d_actual   := actual_growth - shift(actual_growth), by = group_col]
  dt[, d_forecast := forecast_growth - shift(forecast_growth), by = group_col]
  dt[is.na(d_actual), d_actual := 0]
  dt[is.na(d_forecast), d_forecast := 0]
  dt[, cum_error := cumsum(abs(d_actual - d_forecast)), by = group_col]
  dt[, c("d_actual", "d_forecast") := NULL]
  dt
}

# Собирает данные для дашборда: по каждой акции и агрегированно по портфелю
# (веса — стоимость позиции на дату покупки, quantity * purchase_price).
compute_dashboard <- function(forecast_long, actual_long, holdings = portfolio_holdings) {
  merged <- merge(actual_long, forecast_long, by = c("ticker", "date"))
  if (nrow(merged) == 0) {
    stop("Даты в файле прогноза и фактические котировки не пересекаются")
  }
  merged <- merge(merged, holdings, by = "ticker")
  merged[, weight := quantity * purchase_price]
  merged <- add_cumulative_error(merged, "ticker")

  portfolio <- merged[, .(
    actual_growth   = sum(weight * actual_growth) / sum(weight),
    forecast_growth = sum(weight * forecast_growth) / sum(weight)
  ), by = date]
  portfolio[, ticker := "PORTFOLIO"]
  portfolio <- add_cumulative_error(portfolio, "ticker")

  list(by_ticker = merged[order(ticker, date)], portfolio = portfolio[order(date)])
}
