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

  # Мониторинг портфеля ####

  forecast_data <- reactive({
    req(input$forecast_file)
    tryCatch(
      read_forecast_xlsx(input$forecast_file$datapath, value_type = input$forecast_value_type),
      error = function(e) {
        validate(need(FALSE, paste("Ошибка чтения файла прогноза:", e$message)))
      }
    )
  })

  actual_growth_data <- reactive({
    dt <- fetch_actual_growth(portfolio_holdings$ticker)
    validate(need(nrow(dt) > 0, "Не удалось загрузить фактические котировки с Yahoo Finance"))
    dt
  })

  portfolio_dashboard <- reactive({
    tryCatch(
      compute_dashboard(forecast_data(), actual_growth_data(), portfolio_holdings),
      error = function(e) {
        validate(need(FALSE, paste("Ошибка расчёта дашборда:", e$message)))
      }
    )
  })

  output$stat_actual_growth <- renderUI({
    v <- portfolio_dashboard()$portfolio[date == max(date), actual_growth]
    tags$h2(sprintf("%+.2f%%", v * 100))
  })

  output$stat_forecast_growth <- renderUI({
    v <- portfolio_dashboard()$portfolio[date == max(date), forecast_growth]
    tags$h2(sprintf("%+.2f%%", v * 100))
  })

  output$stat_deviation <- renderUI({
    d <- portfolio_dashboard()$portfolio[date == max(date)]
    tags$h2(sprintf("%+.2f", (d$actual_growth - d$forecast_growth) * 100))
  })

  output$stat_cum_error <- renderUI({
    v <- portfolio_dashboard()$portfolio[date == max(date), cum_error]
    tags$h2(sprintf("%.2f", v * 100))
  })

  output$portfolio_table <- renderRHandsontable({
    dash <- portfolio_dashboard()

    latest_ticker <- dash$by_ticker[date == max(date)]
    tbl <- latest_ticker[, .(
      `Компания` = company,
      `Тикер` = ticker,
      `Кол-во, шт` = quantity,
      `Цена покупки, $` = round(purchase_price, 2),
      `Текущая цена, $` = round(price, 2),
      `Рост с 11.09, %` = round(actual_growth * 100, 2),
      `Прогноз, %` = round(forecast_growth * 100, 2),
      `Отклонение, п.п.` = round((actual_growth - forecast_growth) * 100, 2),
      `Накоп. ошибка, п.п.` = round(cum_error * 100, 2)
    )]

    latest_portfolio <- dash$portfolio[date == max(date)]
    tbl_total <- data.table(
      `Компания` = "ПОРТФЕЛЬ", `Тикер` = "TOTAL",
      `Кол-во, шт` = NA_real_, `Цена покупки, $` = NA_real_, `Текущая цена, $` = NA_real_,
      `Рост с 11.09, %` = round(latest_portfolio$actual_growth * 100, 2),
      `Прогноз, %` = round(latest_portfolio$forecast_growth * 100, 2),
      `Отклонение, п.п.` = round((latest_portfolio$actual_growth - latest_portfolio$forecast_growth) * 100, 2),
      `Накоп. ошибка, п.п.` = round(latest_portfolio$cum_error * 100, 2)
    )

    rhandsontable(rbind(tbl, tbl_total), readOnly = TRUE) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })

  output$portfolio_growth_chart <- renderPlotly({
    dash <- portfolio_dashboard()
    bt <- dash$by_ticker
    pf <- copy(dash$portfolio)

    plot_ly() %>%
      add_lines(data = bt, x = ~date, y = ~(actual_growth * 100), color = ~company,
                legendgroup = ~company, name = ~paste(company, "(факт)")) %>%
      add_lines(data = bt, x = ~date, y = ~(forecast_growth * 100), color = ~company,
                line = list(dash = "dot"), legendgroup = ~company, showlegend = FALSE) %>%
      add_lines(data = pf, x = ~date, y = ~(actual_growth * 100), name = "Портфель (факт)",
                line = list(color = "black", width = 3)) %>%
      add_lines(data = pf, x = ~date, y = ~(forecast_growth * 100), name = "Портфель (прогноз)",
                line = list(color = "black", width = 3, dash = "dot")) %>%
      layout(title = "Темп роста: факт (сплошная) vs прогноз (пунктир)",
             xaxis = list(title = "Дата"), yaxis = list(title = "Темп роста, %"),
             font = list(family = "Panton"))
  })

  output$portfolio_error_chart <- renderPlotly({
    dash <- portfolio_dashboard()
    bt <- dash$by_ticker
    pf <- copy(dash$portfolio)

    plot_ly() %>%
      add_lines(data = bt, x = ~date, y = ~(cum_error * 100), color = ~company) %>%
      add_lines(data = pf, x = ~date, y = ~(cum_error * 100), name = "Портфель",
                line = list(color = "black", width = 3)) %>%
      layout(title = "Накопленная ошибка прогноза",
             xaxis = list(title = "Дата"), yaxis = list(title = "Накопленная ошибка, п.п."),
             font = list(family = "Panton"))
  })
})
