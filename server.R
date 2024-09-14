# server.R

library(quantmod)
library(rhandsontable)
library(data.table)
library(plotly)
library(openxlsx)
library(shinyWidgets)
library(shinyjs)
library(fresh)

shinyServer(function(input, output, session) {
  
  # Реактивная загрузка данных акций с учетом выбранных дат
  stock_data <- reactive({
    req(input$ticker)
    
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    # Отладочное сообщение
    cat("Загружаем данные для тикеров:", input$ticker, "с дат", start_date, "по", end_date, "\n")
    
    all_data <- rbindlist(lapply(input$ticker, function(ticker) {
      cat("Загружаем данные для тикера:", ticker, "\n")
      stock_data <- tryCatch({
        getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
      }, error = function(e) {
        cat("Ошибка при загрузке данных для тикера", ticker, ": ", e$message, "\n")
        return(NULL)
      })
      
      if (is.null(stock_data)) {
        return(NULL)
      }
      
      dt <- as.data.table(data.frame(Date = index(stock_data), coredata(stock_data)))
      
      # Проверяем структуру данных
      cat("Структура данных для тикера", ticker, ":", colnames(dt), "\n")
      
      # Удаляем префиксы из имен столбцов
      setnames(dt, old = colnames(dt), new = gsub(paste0(ticker, "\\."), "", colnames(dt)))
      
      dt[, ticker := ticker]  # Добавляем колонку с тикером
      return(dt[, .(Date, Open, High, Low, Close, Volume, ticker)])
    }), fill = TRUE)
    
    if (nrow(all_data) == 0) {
      cat("Нет данных для отображения.\n")
    }
    
    return(all_data)
  })
  
  # Рендеринг графика свечей
  output$plot_candlestick <- renderPlotly({
    req(stock_data())
    
    plot_data <- stock_data()
    if (nrow(plot_data) == 0) {
      cat("Нет данных для рендеринга графика.\n")
      return(NULL)
    }
    
    plot_ly(data = plot_data, x = ~Date, type = "candlestick",
            open = ~Open, high = ~High, low = ~Low, close = ~Close) %>%
      layout(title = "График котировок",
             xaxis = list(title = "Дата"),
             yaxis = list(title = "Цена"),
             font = list(family = "Panton"))
  })
  
  # Рендеринг редактируемой таблицы (без колонки Volume)
  output$stock_table <- renderRHandsontable({
    data_for_table <- stock_data()
    if (nrow(data_for_table) == 0) {
      cat("Нет данных для отображения в таблице.\n")
      return(NULL)
    }
    
    rhandsontable(data_for_table[order(-Date), .(Date, Open, High, Low, Close, ticker)]
                    , readOnly = FALSE) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })
  
  # Скачивание данных с сохранением колонки Volume
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("stock_data_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write.xlsx(stock_data(), file)
    }
  )
})
