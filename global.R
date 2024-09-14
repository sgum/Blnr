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


# shiny::runApp()