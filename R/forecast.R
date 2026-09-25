# R/forecast.R
#
# Разбор Excel-файла модельного прогноза (лист "Q_mean_var", блок "mean")
# и сравнение прогноза с фактом. Файл читается ЛОКАЛЬНО, там, где реально
# запущено Shiny-приложение.
#
# ФОРМАТ ФАЙЛА (проверено на quotes 2026-09-13, 22.09.2026). Лист
# "Q_mean_var" содержит блок с шапкой в столбце 1 = "mean":
#   строка-шапка:  mean | ix | .qM0 | .qM1 | .qM2 | ...
#   строки данных: <инструмент> | <индекс> | v0 | v1 | v2 | ...
#   ...до строки, где столбец 1 пуст или равен "var" (начало блока дисперсий).
# Инструменты записаны человекочитаемыми именами ("Goldman Sachs", "Nvidia",
# "Google", "AMD", "General Electric", ...) — сопоставляются с тикерами по
# реестру R/watchlist.R (поле name_model), точным совпадением.
#
# СМЫСЛ ЗНАЧЕНИЙ. .qMk — это УЖЕ НАКОПЛЕННЫЙ относительный прогноз роста
# бумаги на горизонт k (в долях: 0.012 = +1.2%), а НЕ доходность за один шаг.
# Поэтому значения берутся напрямую, без перемножения по шагам.
#
# ШАГ -> ДАТА. Дат в файле нет: шаг k раскладывается на календарь как k-й
# торговый день (пн–пт) от FORECAST_BASELINE_DATE (по умолчанию 11.09.2026):
# .qM0 = базовая дата, .qM1 = следующий торговый день и т.д. Праздники не
# учитываются (приблизительная сетка); при необходимости уточните
# forecast_step_dates().

# Список известных инструментов больше не дублируется здесь — он один и тот же
# для market-data, прогноза и таблицы портфеля и живёт в R/watchlist.R.

# Сопоставляет имя инструмента с известным тикером (без учёта регистра,
# по вхождению алиаса). NA, если сопоставить не удалось.
match_ticker_column <- function(header) {
  # Сопоставление идёт ТОЧНЫМ совпадением с реестром R/watchlist.R
  # (поле name_model — подпись строки ровно как в Q_mean_var), а не поиском
  # подстроки/алиасов. Алиасы уже давали ложные срабатывания: короткий "ge"
  # ловил "General Motors", и строка модели уезжала не к тому тикеру.
  # Реестр покрывает все 26 инструментов модели, старый список алиасов — 5.
  tk <- ticker_by_model_name(header)
  if (length(tk) == 0) return(NA_character_)
  tk[1]
}

# Список листов в xlsx-файле (для UI-селектора).
forecast_sheet_names <- function(path) {
  openxlsx::getSheetNames(path)
}

# Даты для шагов 0..(n-1): торговые дни (пн–пт) начиная с base_date.
forecast_step_dates <- function(n, base_date = FORECAST_BASELINE_DATE) {
  if (n <= 0) return(as.Date(character(0)))
  out <- as.Date(rep(NA_real_, n), origin = "1970-01-01")
  d <- as.Date(base_date)
  # шаг 0 = сама base_date, если это торговый день; иначе — ближайший вперёд
  while (as.POSIXlt(d)$wday %in% c(0, 6)) d <- d + 1
  out[1] <- d
  i <- 2
  while (i <= n) {
    d <- d + 1
    if (!(as.POSIXlt(d)$wday %in% c(0, 6))) { out[i] <- d; i <- i + 1 }
  }
  out
}

# Выбор листа с прогнозом: "Q_mean_var", если он есть, иначе первый лист.
forecast_default_sheet <- function(path) {
  sheets <- tryCatch(openxlsx::getSheetNames(path), error = function(e) character(0))
  if ("Q_mean_var" %in% sheets) return("Q_mean_var")
  if (length(sheets) >= 1) return(sheets[1])
  1
}

# Разбирает файл прогноза в "длинный" data.table: date, ticker,
# forecast_growth_pct (в процентах). as_fraction = TRUE, если значения в
# файле — доли (0.012 = 1.2%): тогда они домножаются на 100 (для формата
# Q_mean_var это норма). base_date — дата шага 0 (см. forecast_step_dates()).
parse_forecast_file <- function(path, sheet = NULL, as_fraction = TRUE,
                                base_date = FORECAST_BASELINE_DATE) {
  if (is.null(sheet)) sheet <- forecast_default_sheet(path)
  raw <- openxlsx::read.xlsx(path, sheet = sheet, colNames = FALSE, detectDates = FALSE)
  if (ncol(raw) < 3) {
    stop("Лист не похож на модельный прогноз: ожидались столбцы mean/ix/.qM0…")
  }

  labels <- trimws(as.character(raw[[1]]))
  header_row <- which(tolower(labels) == "mean")[1]
  if (is.na(header_row)) {
    stop(sprintf(
      "Не найдена строка-шапка блока прогноза (столбец 1 = 'mean') на листе '%s'. Проверьте, что это лист Q_mean_var.",
      sheet
    ))
  }

  # Инструменты идут со следующей строки до пустого столбца 1 или до "var".
  data_rows <- integer(0)
  r <- header_row + 1
  while (r <= nrow(raw)) {
    lab <- labels[r]
    if (is.na(lab) || lab == "" || tolower(lab) == "var") break
    data_rows <- c(data_rows, r)
    r <- r + 1
  }
  if (length(data_rows) == 0) stop("Блок 'mean' не содержит строк с инструментами.")

  # Число шагов: непрерывная серия непустых значений первой строки данных с 3-го столбца.
  first_vals <- suppressWarnings(as.numeric(unlist(raw[data_rows[1], 3:ncol(raw)], use.names = FALSE)))
  n_steps <- if (all(is.na(first_vals))) 0 else max(which(!is.na(first_vals)))
  if (n_steps == 0) stop("В блоке 'mean' не найдено числовых значений прогноза.")

  step_dates <- forecast_step_dates(n_steps, base_date)
  mult <- if (isTRUE(as_fraction)) 100 else 1

  rows <- lapply(data_rows, function(i) {
    tkr <- match_ticker_column(labels[i])
    if (is.na(tkr)) return(NULL)
    vals <- suppressWarnings(as.numeric(unlist(raw[i, 3:(2 + n_steps)], use.names = FALSE)))
    data.table::data.table(
      date = step_dates,
      ticker = tkr,
      forecast_growth_pct = vals * mult
    )
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) {
    stop(sprintf(
      "Ни один инструмент блока 'mean' не сопоставлен с реестром watchlist (%s).",
      paste(WATCHLIST$ticker, collapse = ", ")
    ))
  }

  out <- data.table::rbindlist(rows)
  out <- out[!is.na(date) & !is.na(forecast_growth_pct)]
  data.table::setorder(out, ticker, date)
  out[]
}

# Прогнозный рост тикера на дату: значение на саму дату, либо на ближайший
# предыдущий доступный (торговый) день в файле. NA, если данных ещё нет.
forecast_for_date <- function(forecast, tkr, date) {
  target <- as.Date(date)   # отдельное имя: аргумент не должен затенять столбец date
  sub <- forecast[ticker == tkr & date <= target]
  if (nrow(sub) == 0) return(NA_real_)
  data.table::setorder(sub, -date)
  sub[1, forecast_growth_pct]
}

# Добавляет к таблице метрик портфеля прогноз и отклонение (факт − прогноз)
# от общей базы (по умолчанию 11.09.2026): использует growth_from_base_pct,
# посчитанный build_portfolio_metrics() от той же даты, что и шаг 0 прогноза.
add_forecast_to_metrics <- function(dt, forecast, as_of = Sys.Date()) {
  dt <- data.table::copy(dt)
  if (is.null(forecast) || nrow(forecast) == 0) {
    dt[, forecast_pct := NA_real_]
    dt[, dev_pct := NA_real_]
    dt[, forecast_since_entry_pct := NA_real_]
    dt[, dev_since_entry_pp := NA_real_]
    return(dt[])
  }
  dt[, forecast_pct := vapply(ticker, forecast_for_date, numeric(1), forecast = forecast, date = as_of)]
  dt[, dev_pct := growth_from_base_pct - forecast_pct]

  # Сравнение ЗА ПЕРИОД ВЛАДЕНИЯ — основное на экране. «От базы прогноза»
  # остаётся как характеристика бумаги, но портфель по нему судить нельзя:
  # позиция, купленная позже базы, присвоила бы себе чужой рост.
  if ("purchase_date" %in% names(dt)) {
    dt[, forecast_since_entry_pct := mapply(
      function(tk, d) forecast_between(forecast, tk,
                                       if (is.na(d)) FORECAST_BASELINE_DATE else d,
                                       as_of),
      ticker, purchase_date)]
    dt[, dev_since_entry_pp := growth_pct - forecast_since_entry_pct]
  } else {
    dt[, forecast_since_entry_pct := NA_real_]
    dt[, dev_since_entry_pp := NA_real_]
  }
  dt[]
}

# Прогнозный рост ЗА ПЕРИОД владения: от даты входа до даты наблюдения.
#
# Зачем это нужно. Прогноз хранится как накопленный процент от базовой даты
# файла (11.09.2026). Сравнивать с ним факт можно только если бумага была в
# портфеле с той же базы. На деле шесть позиций из семи открыты 24.09, и
# сравнение «от 11.09» приписывало портфелю движение цены за время, когда
# бумаг ещё не было: витрина показывала «обгоняем модель на 7.71 пп» при
# фактическом результате +0.79%.
#
# Приведение к дате входа: если модель обещала к моменту T0 рост f0, а к
# моменту T рост f1 (оба от общей базы), то ожидаемый рост за период владения
#   (1 + f1/100) / (1 + f0/100) - 1
# Вход раньше базы прогноза — период считается от базы: раньше неё модели
# просто нет, и делать вид, что есть, нельзя.
forecast_between <- function(forecast, tkr, from, to) {
  if (is.null(forecast) || nrow(forecast) == 0) return(NA_real_)
  start <- max(as.Date(from), FORECAST_BASELINE_DATE, na.rm = TRUE)
  f1 <- forecast_for_date(forecast, tkr, to)
  if (is.na(f1)) return(NA_real_)
  f0 <- if (start <= FORECAST_BASELINE_DATE) 0 else forecast_for_date(forecast, tkr, start)
  if (is.na(f0)) f0 <- 0
  ((1 + f1 / 100) / (1 + f0 / 100) - 1) * 100
}
