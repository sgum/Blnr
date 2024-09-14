# helpers.R

# Функция для получения данных по тикерам
get_stock_data <- function(ticker) {
  tryCatch({
    stock_data <- getSymbols(ticker, src = "yahoo", auto.assign = FALSE)
    dt <- as.data.table(data.frame(Date = index(stock_data), coredata(stock_data)))
    dt <- dt[, .(Date, Open, High, Low, Close, Volume)]  # Оставляем только нужные колонки
    dt[, Symbol := ticker]  # Добавляем колонку с тикером
    return(dt)
  }, error = function(e) {
    NULL
  })
}

# Функция для сохранения данных в формате Excel
save_to_excel <- function(data, filename) {
  write.xlsx(data, filename)
}
