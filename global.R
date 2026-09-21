# global.R

library(shiny)
library(quantmod)
library(rhandsontable)
library(data.table)
library(plotly)
library(openxlsx)
library(bs4Dash)

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

# Прогноз относительного роста (в файле пользователя) считается от уровня,
# достигнутого бумагами к этой дате.
FORECAST_BASELINE_DATE <- as.Date("2026-09-11")

# Путь к xlsx с прогнозом по умолчанию: если файл существует локально при
# старте сессии, он подхватывается автоматически (см. server.R); при
# необходимости переопределите переменной окружения FORECAST_XLSX_PATH,
# либо загрузите другой файл прямо во вкладке "Загрузка прогнозов".
FORECAST_XLSX_PATH <- Sys.getenv(
  "FORECAST_XLSX_PATH",
  unset = path.expand("~/Downloads/Portfolio (1).xlsx")
)

# Клиент Exante API и бизнес-логика вкладок "Портфель Exante" / "Загрузка прогнозов"
source("R/exante_api.R")
source("R/portfolio.R")
source("R/forecast.R")

# shiny::runApp()