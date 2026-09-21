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

  # Портфель Exante ####

  # Пересчитывается при старте сессии и по нажатию "Обновить котировки"
  portfolio_metrics <- eventReactive(input$portfolio_refresh, {
    build_portfolio_metrics()
  }, ignoreNULL = FALSE)

  portfolio_summary <- reactive({
    summarize_portfolio(portfolio_metrics())
  })

  output$portfolio_source_status <- renderUI({
    if (exante_has_credentials()) {
      tags$span(icon("plug"), " Источник данных: Exante API (боевой счёт)",
                 style = "color: #2e7d32; font-weight: bold;")
    } else {
      tags$span(icon("triangle-exclamation"),
                 " Exante API не настроен (нет EXANTE_CLIENT_ID / EXANTE_APP_ID / EXANTE_SHARED_KEY) — ",
                 "используется портфель, заданный вручную, с котировками Yahoo Finance. См. docs/EXANTE_API.md.",
                 style = "color: #b26a00; font-weight: bold;")
    }
  })

  # Простая KPI-плитка без привязки к конкретной версии API bs4Dash.
  kpi_tile <- function(value, subtitle, bg) {
    div(style = paste0(
          "background:", bg, "; color:white; border-radius:6px;",
          "padding:16px; margin-bottom:15px;"
        ),
        div(style = "font-size:22px; font-weight:bold;", value),
        div(style = "font-size:11px; opacity:0.9;", subtitle))
  }

  output$portfolio_value_box <- renderUI({
    s <- portfolio_summary()
    kpi_tile(paste0("$", format(round(s$current_value, 0), big.mark = " ")),
              "Текущая стоимость портфеля", "#37474f")
  })

  output$portfolio_pnl_box <- renderUI({
    s <- portfolio_summary()
    kpi_tile(paste0(ifelse(s$pnl >= 0, "+", ""), "$", format(round(s$pnl, 0), big.mark = " ")),
              "Прибыль/убыток от покупки",
              ifelse(s$pnl >= 0, "#2e7d32", "#c62828"))
  })

  output$portfolio_growth_box <- renderUI({
    s <- portfolio_summary()
    kpi_tile(paste0(ifelse(s$growth_pct >= 0, "+", ""), round(s$growth_pct, 2), "%"),
              "Рост портфеля с 14.09.2026",
              ifelse(s$growth_pct >= 0, "#2e7d32", "#c62828"))
  })

  output$portfolio_table <- renderRHandsontable({
    dt <- portfolio_metrics()
    display <- dt[, .(
      Тикер          = ticker,
      Количество     = quantity,
      `Цена входа`   = round(entry_price, 2),
      `Тек. цена`    = round(current_price, 2),
      `Стоимость`    = round(current_value, 2),
      `Рост, %`      = round(growth_pct, 2),
      `Вес, %`       = round(weight_pct, 2)
    )]
    rhandsontable(display, readOnly = TRUE) %>%
      hot_table(highlightCol = TRUE, highlightRow = TRUE)
  })

  output$portfolio_pie <- renderPlotly({
    dt <- portfolio_metrics()
    plot_ly(dt, labels = ~ticker, values = ~current_value, type = "pie",
            textinfo = "label+percent") %>%
      layout(font = list(family = "Panton"))
  })

  output$portfolio_growth_plot <- renderPlotly({
    dt <- portfolio_metrics()
    s <- portfolio_summary()
    plot_ly(dt, x = ~ticker, y = ~growth_pct, type = "bar",
            marker = list(color = ~ifelse(growth_pct >= 0, "#2e7d32", "#c62828"))) %>%
      layout(
        title = "Рост по бумагам, %",
        xaxis = list(title = "Тикер"),
        yaxis = list(title = "Рост, %"),
        shapes = list(list(
          type = "line", x0 = 0, x1 = 1, xref = "paper",
          y0 = s$growth_pct, y1 = s$growth_pct,
          line = list(color = "#455a64", dash = "dot")
        )),
        font = list(family = "Panton")
      )
  })
})
