# server.R

library(rhandsontable)
library(data.table)
library(plotly)
library(openxlsx)
library(shinyWidgets)
library(shinyjs)
library(fresh)

shinyServer(function(input, output, session) {

  # Авторизация (шлюз) ####
  # Финансовый стенд: до входа рисуется форма, дашборд создаётся только после
  # успешной проверки (AD + белый список), поэтому данные не уходят в браузер
  # раньше входа. См. R/auth_ad.R и ui/{login,dashboard}_ui.R.
  USER <- reactiveValues(login = NULL)

  output$gate <- renderUI({
    if (is.null(USER$login)) loginUI() else dashboardUI()
  })

  observeEvent(input$auth_submit, {
    res <- tryCatch(auth_check(input$auth_login, input$auth_password),
                    error = function(e) list(ok = FALSE))
    if (isTRUE(res$ok)) {
      cat(sprintf("[AUTH] OK login=%s %s\n", res$login, format(Sys.time())))
      USER$login <- res$login
    } else {
      # Единая ошибка: не различаем «нет пользователя» и «неверный пароль».
      output$login_error <- renderUI(
        tags$div(class = "dt-auth-err", "Неверный логин или пароль, либо нет доступа к стенду.")
      )
      cat(sprintf("[AUTH] FAIL login=%s %s\n",
                  normalize_login(input$auth_login %||% ""), format(Sys.time())))
    }
  })

  output$logout_ui <- renderUI({
    req(USER$login)
    tags$a(href = "#", onclick = "Shiny.setInputValue('auth_logout', Math.random());",
           title = paste0("Выйти (", USER$login, ")"), "Выйти")
  })

  observeEvent(input$auth_logout, {
    cat(sprintf("[AUTH] LOGOUT login=%s %s\n", USER$login %||% "", format(Sys.time())))
    USER$login <- NULL
    session$reload()
  })

  # Данные портфеля ####

  # Единственное место, где стенд ходит в интернет за ценами: старт сессии и
  # кнопка «Обновить». Файл прогноза меняется чаще, чем цены, и пересчёт
  # прогноза поверх уже полученных цен похода в сеть не требует.
  portfolio_prices <- eventReactive(input$portfolio_refresh, {
    out <- build_portfolio_metrics()
    attr(out, "as_of") <- Sys.time()
    out
  }, ignoreNULL = FALSE)

  portfolio_metrics <- reactive({
    add_forecast_to_metrics(portfolio_prices(), forecast_data())
  })

  portfolio_summary <- reactive({
    summarize_portfolio(portfolio_metrics())
  })

  # Портфель целиком против прогноза, на общей базе 11.09: стоимость портфеля
  # по модели против фактической. Именно это число отвечает на вопрос «мы
  # обгоняем модель или отстаём», а не среднее отклонений по бумагам — оно
  # игнорировало бы вес позиции.
  portfolio_vs_forecast <- reactive({
    m <- portfolio_metrics()
    if (is.null(m) || nrow(m) == 0 || !"forecast_pct" %in% names(m)) return(NULL)
    ok <- is.finite(m$base_price) & is.finite(m$forecast_pct) &
          is.finite(m$current_value) & is.finite(m$quantity)
    if (!any(ok)) return(NULL)
    base_value <- sum(m$quantity[ok] * m$base_price[ok])
    fcst_value <- sum(m$quantity[ok] * m$base_price[ok] * (1 + m$forecast_pct[ok] / 100))
    fact_value <- sum(m$current_value[ok])
    if (!is.finite(base_value) || base_value == 0) return(NULL)
    fact_pct <- (fact_value / base_value - 1) * 100
    fcst_pct <- (fcst_value / base_value - 1) * 100
    list(fact_pct = fact_pct, fcst_pct = fcst_pct, dev_pp = fact_pct - fcst_pct)
  })

  # История снимков (факт/прогноз по дням) — для панели накопления невязки.
  snapshots_rv <- reactiveVal(read_snapshots())

  snapshot_done <- reactiveVal(NULL)
  observe({
    m <- portfolio_metrics()
    today <- Sys.Date()
    if (!is.null(m) && nrow(m) > 0 && !all(is.na(m$forecast_pct)) &&
        !identical(snapshot_done(), today)) {
      if (isTRUE(record_snapshot(m, as_of = today))) {
        snapshot_done(today)
        snapshots_rv(read_snapshots())
      }
    }
  })

  # Прогноз роста (из xlsx) ####

  forecast_data   <- reactiveVal(NULL)
  forecast_source <- reactiveVal(NULL)

  # Автозагрузка файла по умолчанию (FORECAST_XLSX_PATH из global.R) при
  # старте сессии — если файл лежит на месте. Ошибка парсинга не критична:
  # файл всегда можно подать вручную через кнопку «Прогноз…».
  if (file.exists(FORECAST_XLSX_PATH)) {
    auto_forecast <- tryCatch(parse_forecast_file(FORECAST_XLSX_PATH), error = function(e) NULL)
    if (!is.null(auto_forecast)) {
      forecast_data(auto_forecast)
      forecast_source(sprintf("автозагрузка: %s", basename(FORECAST_XLSX_PATH)))
    }
  }

  observeEvent(input$open_forecast, {
    showModal(modalDialog(
      title = "Файл модельного прогноза",
      size = "l", easyClose = TRUE, footer = modalButton("Закрыть"),
      tags$p(style = "font-size:12px;color:#5C5C5C",
             "Лист «Q_mean_var», блок «mean»: строки — инструменты реестра, ",
             "столбцы — шаги .qM0, .qM1, … Значения — уже накопленный ",
             "относительный прогноз роста; шаг k раскладывается на торговые ",
             sprintf("дни от %s.", format(FORECAST_BASELINE_DATE, "%d.%m.%Y"))),
      fileInput("forecast_file", "Файл (.xlsx)", accept = ".xlsx", width = "100%"),
      uiOutput("forecast_sheet_ui"),
      checkboxInput("forecast_is_share",
                    "Значения в файле — доли (0.012 = 1.2%), умножить на 100",
                    value = TRUE),
      uiOutput("forecast_modal_status"),
      # 26 инструментов в ширину не влезают в модалку — без своего скролла
      # таблица вылезает за её края поверх страницы.
      tags$div(style = "max-height:300px;overflow:auto;font-size:11px",
               tableOutput("forecast_preview"))
    ))
  })

  output$forecast_sheet_ui <- renderUI({
    req(input$forecast_file)
    sheets <- tryCatch(forecast_sheet_names(input$forecast_file$datapath),
                       error = function(e) character(0))
    if (length(sheets) <= 1) return(NULL)
    sel <- if ("Q_mean_var" %in% sheets) "Q_mean_var" else sheets[1]
    selectInput("forecast_sheet", "Лист", choices = sheets, selected = sel)
  })

  observe({
    req(input$forecast_file)
    sheet <- input$forecast_sheet %||% forecast_default_sheet(input$forecast_file$datapath)
    parsed <- tryCatch(
      parse_forecast_file(input$forecast_file$datapath, sheet = sheet,
                          as_fraction = isTRUE(input$forecast_is_share)),
      error = function(e) {
        showNotification(paste("Ошибка чтения файла прогноза:", conditionMessage(e)),
                          type = "error", duration = 10)
        NULL
      }
    )
    if (!is.null(parsed)) {
      forecast_data(parsed)
      forecast_source(sprintf("%s, лист %s", input$forecast_file$name, sheet))
    }
  })

  output$forecast_modal_status <- renderUI({
    fd <- forecast_data()
    if (is.null(fd) || nrow(fd) == 0) {
      return(tags$p(style = "color:#8a5d00;font-size:12px",
                     "Прогноз не загружен — сравнение факта с моделью недоступно."))
    }
    tags$p(style = "color:#4d7a33;font-size:12px",
           sprintf("Загружено (%s). Бумаг: %d. Горизонт: %s — %s.",
                   forecast_source(), length(unique(fd$ticker)),
                   format(min(fd$date), "%d.%m.%Y"), format(max(fd$date), "%d.%m.%Y")))
  })

  output$forecast_preview <- renderTable({
    fd <- forecast_data()
    req(fd)
    wide <- data.table::dcast(fd, date ~ ticker, value.var = "forecast_growth_pct")
    data.table::setorder(wide, date)
    wide <- utils::head(wide, 15L)
    # date — это Date; renderTable иначе печатает его числом-серийником.
    wide[, date := format(date, "%Y-%m-%d")]
    wide
  }, striped = TRUE, digits = 2)

  # Шапка ####

  output$hd_source <- renderUI({
    if (exante_has_credentials()) {
      tags$span(class = "bdg", "Источник: ", tags$b("Exante API"))
    } else {
      tags$span(class = "bdg warn",
                title = paste("Нет EXANTE_API_ID / EXANTE_SHARED_KEY —",
                              "позиции взяты из портфеля, заданного вручную.",
                              "Цены при этом живые (marketdata.app)."),
                "Позиции: ", tags$b("вручную"))
    }
  })

  output$hd_forecast <- renderUI({
    fd <- forecast_data()
    if (is.null(fd) || nrow(fd) == 0) {
      return(tags$span(class = "bdg warn", "Прогноз ", tags$b("не загружен")))
    }
    tags$span(class = "bdg",
              title = sprintf("%s. База отсчёта %s, горизонт до %s.",
                              forecast_source(),
                              format(FORECAST_BASELINE_DATE, "%d.%m.%Y"),
                              format(max(fd$date), "%d.%m.%Y")),
              "Прогноз: ", tags$b(sprintf("%d бумаг", length(unique(fd$ticker)))))
  })

  # Котировки и честное состояние источника. Если marketdata.app отдал ошибку
  # (исчерпан лимит кредитов, нет токена), об этом говорится прямо в шапке:
  # молчащий источник + нули в плитках читаются как «портфель обнулился».
  output$hd_updated <- renderUI({
    m   <- portfolio_prices()
    err <- md_status_text()
    if (!is.null(err)) {
      return(tags$span(class = "bdg warn",
                       title = "Цены не получены, поэтому стоимость и рост показаны прочерком, а не нулём.",
                       "Нет котировок: ", tags$b(err)))
    }
    d <- suppressWarnings(max(vapply(m$ticker, function(tk)
      as.numeric(md_last_price_date(tk)), numeric(1)), na.rm = TRUE))
    tags$span(class = "bdg", "Котировки на ",
              tags$b(if (is.finite(d)) format(as.Date(d, origin = "1970-01-01"), "%d.%m")
                     else "—"),
              " (закрытие)")
  })

  # KPI ####

  output$kpi_strip <- renderUI({
    s  <- portfolio_summary()
    vf <- portfolio_vs_forecast()
    tiles <- list(
      kpi(fmt_money(s$current_value), "Стоимость портфеля"),
      kpi(fmt_signed_money(s$pnl), "Прибыль/убыток от покупки", tone_of(s$pnl)),
      kpi(fmt_pct(s$growth_pct), "Рост от даты покупки", tone_of(s$growth_pct))
    )
    if (!is.null(vf)) {
      tiles <- c(tiles, list(
        kpi(fmt_pp(vf$dev_pp), "Портфель против модели", tone_of(vf$dev_pp),
            # плитка крайняя справа — подсказку прижимаем к правому краю
            tip_align = "r",
            tip = sprintf(paste("Стоимость портфеля от базы %s: факт %s, модель %s.",
                                "Положительное значение — портфель идёт быстрее модели.",
                                "Считается по стоимости, то есть с учётом веса позиций."),
                          format(FORECAST_BASELINE_DATE, "%d.%m.%Y"),
                          fmt_pct(vf$fact_pct), fmt_pct(vf$fcst_pct)))
      ))
    }
    do.call(tagList, tiles)
  })

  # Таблица позиций ####

  # Выбор инструмента: клик по строке таблицы и выпадающий список над
  # графиком — один и тот же выбор, поэтому клик обновляет список, а график
  # слушает только список.
  observeEvent(input$pick_ticker, {
    updateSelectInput(session, "sel_ticker", selected = input$pick_ticker)
  })

  # Левая колонка: таблица позиций (по высоте содержимого) и под ней сравнение
  # факта с моделью, которое добирает оставшуюся высоту. Панель сравнения
  # появляется только когда прогноз загружен — иначе колонка остаётся из одной
  # таблицы, а не из таблицы и пустой коробки.
  output$left_col <- renderUI({
    fd <- forecast_data()
    has_fc <- !is.null(fd) && nrow(fd) > 0
    base_lab <- format(FORECAST_BASELINE_DATE, "%d.%m.%Y")

    positions <- panel(
      "Позиции",
      tip = paste0(
        "Цены — marketdata.app, тот же источник, что кормит внешнюю модель. ",
        "«От ", format(FORECAST_BASELINE_DATE, "%d.%m"), "» — рост от базы ",
        "прогноза: модель построена от неё же, поэтому факт и прогноз ",
        "сравнимы напрямую. Δ = факт − модель в процентных пунктах, ",
        "плюс означает, что бумага идёт быстрее модели. ",
        "Клик по строке открывает её график справа."),
      body_class = "bd--flush",
      uiOutput("pos_table")
    )
    if (!has_fc) {
      return(tags$div(class = "blnr-col blnr-col--solo", positions))
    }
    tags$div(
      class = "blnr-col",
      positions,
      panel(
        "Факт против модели",
        tip = paste0("По каждой бумаге: фактический рост от ", base_lab,
                     " рядом с прогнозом модели на сегодня. Расхождение ",
                     "столбиков и есть повод для решения по позиции."),
        body_class = "bd--plot",
        plotlyOutput("chart_vs_forecast", height = "100%")
      )
    )
  })

  output$pos_table <- renderUI({
    m <- portfolio_metrics()
    req(nrow(m) > 0)
    sel <- input$sel_ticker %||% ""
    has_fc <- any(is.finite(m$forecast_pct))

    num <- function(x, f) {
      tags$td(class = if (!is.finite(x)) "mut" else if (x >= 0) "pos" else "neg", f(x))
    }
    plain <- function(x, digits = 2) {
      tags$td(class = if (is.finite(x)) NULL else "mut",
              if (is.finite(x)) formatC(x, format = "f", digits = digits, big.mark = " ") else "—")
    }

    rows <- lapply(seq_len(nrow(m)), function(i) {
      r <- m[i]
      tags$tr(
        class = if (identical(r$ticker, sel)) "sel" else NULL,
        onclick = sprintf("Shiny.setInputValue('pick_ticker','%s',{priority:'event'})", r$ticker),
        tags$td(class = "nm", r$ticker),
        tags$td(formatC(r$quantity, format = "d")),
        plain(r$entry_price), plain(r$current_price),
        plain(r$current_value, 0),
        # Вес — числом и заливкой ячейки: отдельная карточка «Структура
        # портфеля» ради тех же пяти чисел заняла бы полосу экрана.
        tags$td(
          style = sprintf(
            "background:linear-gradient(to left,#ffeccc %1$.1f%%,transparent %1$.1f%%)",
            max(0, min(100, r$weight_pct))),
          formatC(r$weight_pct, format = "f", digits = 1), "%"),
        num(r$growth_pct, fmt_pct),
        num(r$growth_from_base_pct, fmt_pct),
        if (has_fc) num(r$forecast_pct, fmt_pct),
        if (has_fc) num(r$dev_pct, fmt_pp)
      )
    })

    s <- portfolio_summary()
    vf <- portfolio_vs_forecast()
    base_lab <- format(FORECAST_BASELINE_DATE, "%d.%m")

    tags$div(class = "rk", tags$table(
      tags$thead(tags$tr(
        tags$th("Тикер"), tags$th("Кол-во"), tags$th("Вход"), tags$th("Тек."),
        tags$th("Стоимость"), tags$th("Вес"), tags$th("От покупки"),
        tags$th(paste0("От ", base_lab)),
        if (has_fc) tags$th("Модель"),
        if (has_fc) tags$th("Δ")
      )),
      tags$tbody(rows),
      tags$tfoot(tags$tr(
        tags$td("Итого"), tags$td(), tags$td(), tags$td(),
        tags$td(fmt_money(s$current_value)), tags$td("100%"),
        tags$td(class = if (s$growth_pct >= 0) "pos" else "neg", fmt_pct(s$growth_pct)),
        tags$td(class = if (!is.null(vf) && vf$fact_pct >= 0) "pos" else "neg",
                if (is.null(vf)) "—" else fmt_pct(vf$fact_pct)),
        if (has_fc) tags$td(class = if (!is.null(vf) && vf$fcst_pct >= 0) "pos" else "neg",
                            if (is.null(vf)) "—" else fmt_pct(vf$fcst_pct)),
        if (has_fc) tags$td(class = if (!is.null(vf) && vf$dev_pp >= 0) "pos" else "neg",
                            if (is.null(vf)) "—" else fmt_pp(vf$dev_pp))
      ))
    ))
  })

  # График инструмента ####

  output$chart_instrument <- renderPlotly({
    tk <- req(input$sel_ticker)
    cnd <- md_candles(tk)
    # shiny::validate явно: jsonlite (грузится через R/marketdata.R) перекрывает
    # validate своей функцией проверки JSON, и голый вызов уходит не туда.
    shiny::validate(shiny::need(nrow(cnd) > 0, sprintf(
      "Нет котировок по %s: marketdata.app не отдал свечи (проверьте MARKETDATA_TOKEN).", tk)))

    p <- plot_ly(
      cnd, x = ~date, type = "candlestick",
      open = ~open, high = ~high, low = ~low, close = ~close, name = tk,
      increasing = list(line = list(color = BLNR_COLORS$ok, width = 1),
                        fillcolor = BLNR_COLORS$ok),
      decreasing = list(line = list(color = BLNR_COLORS$bad, width = 1),
                        fillcolor = BLNR_COLORS$bad),
      hoverinfo = "x+y"
    )

    # Траектория цены по модели: прогноз хранится как накопленный процент от
    # базы, поэтому цена = цена базы * (1 + прогноз/100). Горизонт обрезаем
    # месяцем вперёд — иначе прогноз до 2028 сожмёт свечи в полоску.
    fd <- forecast_data()
    if (!is.null(fd) && nrow(fd) > 0 && tk %in% fd$ticker) {
      base_px <- md_close_on_date(tk, FORECAST_BASELINE_DATE)
      if (is.finite(base_px)) {
        horizon <- max(cnd$date) + 30
        f <- fd[ticker == tk][date <= horizon]
        if (nrow(f) > 0) {
          p <- add_trace(p, data = f, x = ~date,
                         y = base_px * (1 + f$forecast_growth_pct / 100),
                         type = "scatter", mode = "lines", inherit = FALSE,
                         name = "модель",
                         line = list(color = BLNR_COLORS$series, width = 1.6,
                                     dash = "dash"))
        }
      }
    }

    # Цена входа — только если бумага действительно в портфеле.
    m <- portfolio_metrics()
    shapes <- list()
    if (tk %in% m$ticker) {
      ep <- m[ticker == tk][1, entry_price]
      if (is.finite(ep)) {
        shapes <- list(list(type = "line", xref = "paper", x0 = 0, x1 = 1,
                            y0 = ep, y1 = ep,
                            line = list(color = BLNR_COLORS$plan, width = 1,
                                        dash = "dot")))
      }
    }

    blnr_plot_layout(
      p,
      showlegend = FALSE,
      shapes = shapes,
      xaxis = list(title = "", rangeslider = list(visible = FALSE),
                   gridcolor = "#eceff3"),
      yaxis = list(title = "", gridcolor = "#eceff3", tickprefix = "$")
    )
  })

  # Нижний ряд ####
  # Карточка рисуется только когда в ней есть результат: панель сравнения —
  # когда загружен прогноз, панель накопления невязки — когда снимков хотя бы
  # за два дня. Пустых коробок с объяснением, почему они пусты, на экране нет.
  output$bot_panels <- renderUI({
    n_days <- length(unique(snapshots_rv()$date))
    # Меньше двух дней — рисовать нечего, и нижний ряд не занимает экран вовсе.
    if (n_days < 2) return(NULL)
    panel(
      "Невязка во времени",
      tip = paste0("Факт − модель, процентных пунктов, по дням. Снимок ",
                   "пишется раз в день автоматически; лог: ", SNAPSHOT_LOG_PATH, "."),
      body_class = "bd--plot",
      plotlyOutput("chart_deviation", height = "100%")
    )
  })

  output$chart_vs_forecast <- renderPlotly({
    m <- portfolio_metrics()
    req(nrow(m) > 0)
    blnr_plot_layout(
      add_trace(
        plot_ly(m, x = ~ticker, y = ~growth_from_base_pct, type = "bar",
                name = "факт", marker = list(color = BLNR_COLORS$ok)),
        y = ~forecast_pct, name = "модель",
        marker = list(color = BLNR_COLORS$plan)),
      barmode = "group",
      legend = list(orientation = "h", x = 0, y = 1.14, font = list(size = 10)),
      margin = list(l = 40, r = 10, t = 20, b = 26),
      xaxis = list(title = ""),
      yaxis = list(title = "", ticksuffix = "%", gridcolor = "#eceff3")
    )
  })

  output$chart_deviation <- renderPlotly({
    snaps <- snapshots_rv()
    req(nrow(snaps) > 0)
    data.table::setorder(snaps, date)
    days <- format(sort(unique(snaps$date)), "%d.%m")
    p <- plot_ly()
    for (tk in sort(unique(snaps$ticker))) {
      sub <- snaps[ticker == tk]
      p <- add_trace(p, x = format(sub$date, "%d.%m"), y = sub$dev_pct, name = tk,
                     type = "scatter", mode = "lines+markers",
                     line = list(width = 1.4), marker = list(size = 4))
    }
    blnr_plot_layout(
      p,
      legend = list(orientation = "h", x = 0, y = 1.14, font = list(size = 10)),
      margin = list(l = 40, r = 10, t = 20, b = 26),
      # категориальная ось: при малом числе дней plotly иначе растягивает
      # время до долей секунды.
      xaxis = list(title = "", type = "category",
                   categoryorder = "array", categoryarray = days),
      yaxis = list(title = "", ticksuffix = " пп", gridcolor = "#eceff3")
    )
  })
})
