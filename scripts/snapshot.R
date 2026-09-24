#!/usr/bin/env Rscript
# scripts/snapshot.R
#
# Headless-снимок факта/прогноза портфеля для НАКОПЛЕНИЯ ИСТОРИИ на сервере.
# Запускается по расписанию (Jenkins/cron/systemd-timer) РАЗ В ДЕНЬ, а не
# «при открытии вкладки» — иначе история копится только когда кто-то смотрит
# (конституция: актуализация данных заданием, а не вручную).
#
# Пишет одну строку на бумагу в BLNR_SNAPSHOT_LOG (тот же файл, что читает
# дашборд). Идемпотентно по дате. Требует загруженного прогноза (FORECAST_XLSX_PATH)
# и сети (Yahoo/Exante). Код возврата != 0, если снимок не записан — чтобы
# планировщик показал сбой, а не «зелёную сборку с пустотой».
#
# Запуск:
#   cd <каталог приложения> && Rscript scripts/snapshot.R
# Переменные окружения — из .Renviron рядом с global.R (readRenviron ниже).

suppressWarnings(suppressMessages({
  library(data.table)
  library(httr)
  library(jsonlite)
  library(openxlsx)
}))

# Каталог приложения = родитель scripts/. Работает и из cron с абсолютным путём.
args0 <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", args0[grep("^--file=", args0)])
app_dir <- if (length(file_arg)) normalizePath(file.path(dirname(file_arg), "..")) else getwd()
setwd(app_dir)
if (file.exists(".Renviron")) readRenviron(".Renviron")

# Те же константы, что в global.R (без запуска Shiny/UI).
FORECAST_BASELINE_DATE <- as.Date(Sys.getenv("FORECAST_BASELINE_DATE", unset = "2026-09-11"))
FORECAST_XLSX_PATH <- Sys.getenv("FORECAST_XLSX_PATH",
                                 unset = path.expand("~/Downloads/quotes 2026-09-13 2.xlsx"))
portfolio_holdings <- data.table::data.table(
  ticker        = c("GS", "GE", "AMD", "GOOG", "NVDA"),
  quantity      = c(5, 16, 8, 3, 12),
  entry_price   = c(995.72, 318.28, 491.77, 342.649, 212.15),
  purchase_date = as.Date("2026-09-14")
)

# Порядок как в global.R: snapshots.R задаёт путь лога (от него каталог кэша),
# watchlist.R — реестр инструментов, marketdata.R — источник котировок.
source("R/snapshots.R")
source("R/watchlist.R")
source("R/marketdata.R")
source("R/exante_api.R")
source("R/portfolio.R")
source("R/forecast.R")

log_line <- function(...) cat(sprintf("[snapshot %s] %s\n", format(Sys.time()), paste0(...)))

forecast <- tryCatch(parse_forecast_file(FORECAST_XLSX_PATH), error = function(e) {
  log_line("ОШИБКА чтения прогноза: ", conditionMessage(e)); NULL
})
if (is.null(forecast) || nrow(forecast) == 0) {
  log_line("Прогноз не загружен — снимок не пишется."); quit(status = 1)
}

metrics <- tryCatch(
  add_forecast_to_metrics(build_portfolio_metrics(), forecast, as_of = Sys.Date()),
  error = function(e) { log_line("ОШИБКА расчёта метрик: ", conditionMessage(e)); NULL }
)
if (is.null(metrics) || nrow(metrics) == 0 || all(is.na(metrics$forecast_pct))) {
  log_line("Метрики/прогноз пусты — снимок не пишется."); quit(status = 1)
}

ok <- record_snapshot(metrics, as_of = Sys.Date())
if (!isTRUE(ok)) { log_line("record_snapshot вернул FALSE."); quit(status = 1) }

hist <- read_snapshots()
log_line(sprintf("Снимок за %s записан. Всего дней в истории: %d. Лог: %s",
                 format(Sys.Date()), length(unique(hist$date)), SNAPSHOT_LOG_PATH))
quit(status = 0)
