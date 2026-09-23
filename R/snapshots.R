# R/snapshots.R
#
# Накопление невязки прогноза во времени: раз в день пишем снимок факта и
# прогноза по каждой бумаге в CSV-лог, чтобы на вкладке «Портфель Exante»
# строить динамику отклонения (факт − прогноз) по мере набора истории.
#
# Путь лога переопределяется переменной окружения BLNR_SNAPSHOT_LOG.
# По умолчанию — data/portfolio_snapshots.csv (в .gitignore: это машинно-
# локальная накопительная история, не артефакт репозитория).

SNAPSHOT_LOG_PATH <- Sys.getenv(
  "BLNR_SNAPSHOT_LOG",
  unset = file.path("data", "portfolio_snapshots.csv")
)

# Пустой шаблон лога — единый источник схемы столбцов.
empty_snapshot_dt <- function() {
  data.table::data.table(
    date                 = as.Date(character()),
    ticker               = character(),
    growth_from_base_pct = numeric(),
    forecast_pct         = numeric(),
    dev_pct              = numeric()
  )
}

# Читает лог снимков в data.table (пустой шаблон, если файла нет/битый).
read_snapshots <- function(path = SNAPSHOT_LOG_PATH) {
  if (!file.exists(path)) return(empty_snapshot_dt())
  dt <- tryCatch(data.table::fread(path), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0) return(empty_snapshot_dt())
  dt[, date := as.Date(date)]
  dt[]
}

# Записывает снимок метрик за as_of (одна строка на бумагу). Идемпотентно по
# дате: существующие строки за as_of заменяются, поэтому повторный прогон в
# тот же день не плодит дубликаты. Прогноз обязателен — без него (forecast_pct
# все NA) снимок не пишется, иначе история заполнится пустыми невязками.
# Никогда не бросает исключение — возвращает TRUE/FALSE.
record_snapshot <- function(metrics, as_of = Sys.Date(), path = SNAPSHOT_LOG_PATH) {
  tryCatch({
    if (is.null(metrics) || nrow(metrics) == 0) return(invisible(FALSE))
    if (!all(c("ticker", "growth_from_base_pct", "forecast_pct", "dev_pct") %in% names(metrics))) {
      return(invisible(FALSE))
    }
    if (all(is.na(metrics$forecast_pct))) return(invisible(FALSE))

    snap <- data.table::data.table(
      date                 = as.Date(as_of),
      ticker               = as.character(metrics$ticker),
      growth_from_base_pct = as.numeric(metrics$growth_from_base_pct),
      forecast_pct         = as.numeric(metrics$forecast_pct),
      dev_pct              = as.numeric(metrics$dev_pct)
    )
    hist <- read_snapshots(path)
    hist <- hist[date != as.Date(as_of)]
    out  <- data.table::rbindlist(list(hist, snap), use.names = TRUE, fill = TRUE)
    data.table::setorder(out, date, ticker)

    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    data.table::fwrite(out, path)
    invisible(TRUE)
  }, error = function(e) invisible(FALSE))
}
