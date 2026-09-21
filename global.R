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

# Портфель пользователя (покупка от 14 сентября 2026), используется как
# резервный источник, если доступ к Exante API не настроен.
portfolio_holdings <- data.table::data.table(
  ticker        = c("GS", "GE", "AMD", "GOOG", "NVDA"),
  quantity      = c(5, 16, 8, 3, 17),
  purchase_date = as.Date("2026-09-14")
)

# Клиент Exante API и бизнес-логика вкладки "Портфель Exante"
source("R/exante_api.R")
source("R/portfolio.R")

# shiny::runApp()