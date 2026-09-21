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

# Клиент Exante API и бизнес-логика вкладки "Портфель Exante"
source("R/exante_api.R")
source("R/portfolio.R")

# shiny::runApp()