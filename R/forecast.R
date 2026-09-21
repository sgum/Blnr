# R/forecast.R
#
# Разбор Excel-файла с прогнозом относительного роста по дням (от уровня,
# достигнутого 11.09.2026) и сравнение прогноза с фактом. Файл читается
# ЛОКАЛЬНО, там, где реально запущено Shiny-приложение (эта логика не
# выполняется в песочнице, где собирался код, — путь к файлу существует
# только на машине пользователя).
#
# Ожидаемый формат файла: первый столбец — дата (торговый день), остальные
# столбцы — по одному на бумагу, значения — прогнозный относительный рост
# от уровня 11.09.2026 (в процентах или в долях — см. share_input в
# parse_forecast_file()). Заголовки столбцов сопоставляются с тикерами по
# FORECAST_TICKER_ALIASES; при необходимости дополните алиасы под
# формулировки в вашем файле.

FORECAST_TICKER_ALIASES <- list(
  GS   = c("gs", "goldman", "goldman sachs"),
  GE   = c("ge", "general electric"),
  AMD  = c("amd"),
  GOOG = c("goog", "googl", "google", "alphabet"),
  NVDA = c("nvda", "nvidia")
)

# Сопоставляет заголовок столбца с известным тикером (без учёта регистра,
# по вхождению алиаса в заголовок). NA, если сопоставить не удалось.
match_ticker_column <- function(header) {
  h <- tolower(trimws(as.character(header)))
  for (tk in names(FORECAST_TICKER_ALIASES)) {
    aliases <- FORECAST_TICKER_ALIASES[[tk]]
    if (h %in% aliases || any(vapply(aliases, function(a) grepl(a, h, fixed = TRUE), logical(1)))) {
      return(tk)
    }
  }
  NA_character_
}

# Список листов в xlsx-файле (для UI-селектора).
forecast_sheet_names <- function(path) {
  openxlsx::getSheetNames(path)
}

# Разбирает файл прогноза в "длинный" data.table: date, ticker,
# forecast_growth_pct. share_input = TRUE, если значения в файле — доли
# (0.05 = 5%), а не проценты (5 = 5%); тогда они умножаются на 100.
parse_forecast_file <- function(path, sheet = 1, share_input = FALSE) {
  raw <- openxlsx::read.xlsx(path, sheet = sheet, detectDates = TRUE)
  if (ncol(raw) < 2) {
    stop("В файле должно быть минимум два столбца: дата и хотя бы одна бумага")
  }

  date_col <- names(raw)[1]
  dates <- raw[[date_col]]
  if (!inherits(dates, "Date")) {
    dates <- suppressWarnings(as.Date(as.numeric(dates), origin = "1899-12-30"))
  }

  ticker_cols <- names(raw)[-1]
  matched <- stats::setNames(vapply(ticker_cols, match_ticker_column, character(1)), ticker_cols)
  matched <- matched[!is.na(matched)]
  if (length(matched) == 0) {
    stop(sprintf(
      "Не удалось сопоставить ни одного столбца с известными тикерами (%s). Заголовки в файле: %s",
      paste(names(FORECAST_TICKER_ALIASES), collapse = ", "),
      paste(ticker_cols, collapse = ", ")
    ))
  }

  rows <- lapply(names(matched), function(col) {
    values <- suppressWarnings(as.numeric(raw[[col]]))
    if (share_input) values <- values * 100
    data.table::data.table(date = dates, ticker = matched[[col]], forecast_growth_pct = values)
  })

  out <- data.table::rbindlist(rows)
  out <- out[!is.na(date) & !is.na(forecast_growth_pct)]
  data.table::setorder(out, ticker, date)
  out[]
}

# Прогнозный рост тикера на дату: значение на саму дату, либо на ближайший
# предыдущий доступный (торговый) день в файле. NA, если данных ещё нет.
forecast_for_date <- function(forecast, tkr, date) {
  sub <- forecast[ticker == tkr & date <= as.Date(date)]
  if (nrow(sub) == 0) return(NA_real_)
  data.table::setorder(sub, -date)
  sub[1, forecast_growth_pct]
}

# Добавляет к таблице метрик портфеля прогноз и отклонение (факт − прогноз)
# от общей базы (по умолчанию 11.09.2026): использует growth_from_base_pct,
# посчитанный build_portfolio_metrics() от той же даты, что и прогноз в
# файле, — иначе сравнение было бы некорректным (прогноз считается от
# уровня 11.09, а не от цены покупки 14.09).
add_forecast_to_metrics <- function(dt, forecast, as_of = Sys.Date()) {
  dt <- data.table::copy(dt)
  if (is.null(forecast) || nrow(forecast) == 0) {
    dt[, forecast_pct := NA_real_]
    dt[, dev_pct := NA_real_]
    return(dt[])
  }
  dt[, forecast_pct := vapply(ticker, forecast_for_date, numeric(1), forecast = forecast, date = as_of)]
  dt[, dev_pct := growth_from_base_pct - forecast_pct]
  dt[]
}
