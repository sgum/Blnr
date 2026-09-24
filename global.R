# global.R

library(shiny)
library(quantmod)
library(rhandsontable)
library(data.table)
library(plotly)
library(openxlsx)
library(bs4Dash)
# Пакеты, используемые при СБОРКЕ UI (ui.R сорсит global.R первым): без них
# ui.R падает на setSliderColor/useShinyjs/use_theme, т.к. server.R с их
# library() выполняется позже.
library(shinyWidgets)
library(shinyjs)
library(fresh)

tickers <- c("AAPL", "NVDA", "MSFT", "TSLA", "GOOG", "AMZN", "AMD",
             "META", "NFLX", "INTC", "IBM", "GM", "GE", "BP",
             "SHEL", "CVX", "GS", "MS", "JPM", "SAP", "F")

# Портфель пользователя (покупка 14.09.2026), используется как резервный
# источник, если доступ к Exante API не настроен. Количество и цена входа —
# фактические данные из ленты ордеров Exante (Average price по исполненным
# заявкам); цена GOOG указана с точностью со скриншота ордеров.
portfolio_holdings <- data.table::data.table(
  ticker        = c("GS", "GE", "AMD", "GOOG", "NVDA"),
  quantity      = c(5, 16, 8, 3, 12),
  entry_price   = c(995.72, 318.28, 491.77, 342.649, 212.15),
  purchase_date = as.Date("2026-09-14")
)

# Прогноз относительного роста (лист Q_mean_var, блок mean) считается от
# уровня, достигнутого бумагами к этой дате — шаг .qM0 соответствует ей.
FORECAST_BASELINE_DATE <- as.Date("2026-09-11")

# Путь к xlsx с прогнозом по умолчанию: если файл существует локально при
# старте сессии, он подхватывается автоматически (см. server.R). Формат —
# модельный воркбук с листом Q_mean_var (см. R/forecast.R, docs/FORECAST_XLSX.md);
# для каждого нового портфеля делается похожий файл. Имя/путь конкретного файла
# задаётся переменной окружения FORECAST_XLSX_PATH, либо файл грузится вручную
# во вкладке "Загрузка прогнозов".
FORECAST_XLSX_PATH <- Sys.getenv(
  "FORECAST_XLSX_PATH",
  unset = path.expand("~/Downloads/quotes 2026-09-13 2.xlsx")
)

# Порядок важен: snapshots.R задаёт SNAPSHOT_LOG_PATH (от него считается каталог
# кэша в marketdata.R), watchlist.R — реестр инструментов, на который опираются
# и marketdata.R (какие тикеры и через какой эндпоинт), и forecast.R (как связать
# строку модели с тикером).
source("R/snapshots.R")
source("R/watchlist.R")
source("R/marketdata.R")
source("R/exante_api.R")
source("R/portfolio.R")
source("R/forecast.R")
source("R/auth_ad.R")

# Модули интерфейса — в глобальной области, т.к. dashboardUI()/loginUI() строятся
# из server.R (output$gate), а не только из ui.R.
source("ui/sidebar_ui.R")
source("ui/download_history_ui.R")
source("ui/upload_forecasts_ui.R")
source("ui/portfolio_monitoring_ui.R")
source("ui/login_ui.R")
source("ui/dashboard_ui.R")

# shiny::runApp()