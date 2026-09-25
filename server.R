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

  # Стенд читает ряды из локального хранилища и в интернет не ходит (лимит
  # источника — 100 запросов в сутки, см. R/store.R). Кнопка «Обновить»
  # перечитывает хранилище: ночная загрузка могла отработать за время сессии.
  store_touch <- reactiveVal(Sys.time())
  observeEvent(input$portfolio_refresh, store_touch(Sys.time()))

  # Выбранная дата — торговая сессия с полосы времени. Пока ползунок не
  # построен (пустое хранилище), берём последнюю известную сессию.
  sel_date <- reactive({
    lab <- input$as_of
    if (is.null(lab) || !nzchar(lab)) {
      s <- store_sessions_window(1L)
      return(if (length(s)) s else Sys.Date())
    }
    as.Date(lab, format = "%d.%m.%Y")
  })

  # Фактическая дата — правый край шкалы, последняя сессия в хранилище.
  fact_date <- reactive({
    store_touch()
    s <- store_sessions_window(1L)
    if (length(s)) s else as.Date(NA)
  })

  portfolio_prices <- reactive({
    store_touch()
    build_portfolio_metrics(as_of = sel_date())
  })

  # Прогноз сравнивается на ТУ ЖЕ дату, что и факт: иначе ползунок двигал бы
  # факт, а модель оставалась бы на сегодня, и невязка была бы выдумкой.
  portfolio_metrics <- reactive({
    add_forecast_to_metrics(portfolio_prices(), forecast_data(), as_of = sel_date())
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
    # Только позиции, открытые на выбранную дату: непокупленная бумага не
    # может ни обгонять модель, ни отставать от неё.
    ok <- is.finite(m$base_price) & is.finite(m$forecast_pct) &
          is.finite(m$current_value) & m$quantity_at > 0
    if (!any(ok)) return(NULL)
    base_value <- sum(m$quantity_at[ok] * m$base_price[ok])
    fcst_value <- sum(m$quantity_at[ok] * m$base_price[ok] * (1 + m$forecast_pct[ok] / 100))
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
    # Снимок истории пишется только за ФАКТИЧЕСКУЮ дату: иначе прогулка
    # ползунком по прошлому переписала бы историю задним числом.
    req(identical(sel_date(), fact_date()))
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
    if (!is.finite(d)) {
      return(tags$span(class = "bdg warn", "Котировки: ", tags$b("нет данных")))
    }
    last <- as.Date(d, origin = "1970-01-01")
    stale <- as.integer(Sys.Date() - last)
    st <- store_status()
    tip <- paste0(
      "Цены — закрытие последней дневной сессии из локального хранилища рядов. ",
      "Обновляет его ночное задание Jenkins; стенд в marketdata.app не ходит, ",
      "потому что у аккаунта лимит 100 запросов в сутки. ",
      if (!is.null(st$updated_at))
        paste0("Последняя загрузка: ", format(st$updated_at, "%d.%m %H:%M"), ". ") else "",
      "Инструментов в хранилище: ", st$instruments, ".")
    # Замороженный ряд неотличим от живого: файлы на месте, числа
    # правдоподобные, просто даты кончились. Поэтому возраст выводится явно.
    if (stale > 5L) {
      return(tags$span(class = "bdg warn", title = tip,
                       "Ряды устарели: ", tags$b(format(last, "%d.%m")),
                       sprintf(" (%d дн. назад)", stale)))
    }
    tags$span(class = "bdg", title = tip, "Котировки на ",
              tags$b(format(last, "%d.%m")), " (закрытие)")
  })

  # Правая часть полосы времени: выбранная сессия и отметка фактической даты.
  output$tl_marker <- renderUI({
    sel <- sel_date(); fct <- fact_date()
    at_fact <- isTRUE(identical(sel, fct))
    tagList(
      tags$span(class = "val", format(sel, "%d.%m.%Y")),
      if (at_fact)
        tags$span(class = "bdg ok", "\u25cf факт")
      else
        tags$span(class = "bdg", "факт ", tags$b(format(fct, "%d.%m")))
    )
  })

  # KPI ####

  output$kpi_strip <- renderUI({
    s  <- portfolio_summary()
    vf <- portfolio_vs_forecast()
    sel <- sel_date()
    sel_lab <- format(sel, "%d.%m.%Y")

    # До первой покупки портфеля просто не было. Показать «$0» как результат
    # было бы неправдой того же сорта, что нули при отказе источника, поэтому
    # состояние названо словами.
    if (s$positions == 0) {
      return(kpi("—", "Стоимость портфеля", tip_align = "l",
                 sub = paste0("на ", sel_lab, " бумаг ещё нет"),
                 tip = paste0("Позиции куплены позже выбранной даты. ",
                              "Сдвиньте ползунок вправо, к дате покупки.")))
    }

    tiles <- list(
      kpi(fmt_money(s$current_value), "В бумагах на дату",
          sub = paste0(sel_lab, " \u00b7 позиций: ", s$positions),
          tip = paste0("Стоимость бумаг по ценам закрытия ", sel_lab,
                       ". Денежная часть счёта сюда не входит: остаток кэша ",
                       "отдаёт только Exante API, он на стенде ещё не ",
                       "подключён.")),
      kpi(fmt_signed_money(s$day_pnl), "За сессию", tone_of(s$day_pnl),
          sub = fmt_pct(s$day_pct),
          tip = "Изменение стоимости бумаг за одну торговую сессию — «на момент»."),
      kpi(fmt_signed_money(s$pnl), "Накопленным итогом", tone_of(s$pnl),
          sub = fmt_pct(s$growth_pct),
          tip = paste0("Результат от цены покупки до ", sel_lab,
                       " — по открытым на эту дату позициям."))
    )
    if (!is.null(vf)) {
      tiles <- c(tiles, list(
        kpi(fmt_pp(vf$dev_pp), "Портфель против модели", tone_of(vf$dev_pp),
            # плитка крайняя справа — подсказку прижимаем к правому краю
            tip_align = "r",
            sub = paste0("факт ", fmt_pct(vf$fact_pct), " \u00b7 модель ",
                         fmt_pct(vf$fcst_pct)),
            tip = sprintf(paste("Стоимость портфеля от базы %s на дату %s:",
                                "факт %s, модель %s. Плюс — портфель идёт",
                                "быстрее модели. Считается по стоимости, то",
                                "есть с учётом веса позиций."),
                          format(FORECAST_BASELINE_DATE, "%d.%m.%Y"), sel_lab,
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
    open_n <- nrow(portfolio_metrics()[quantity_at > 0])
    base_lab <- format(FORECAST_BASELINE_DATE, "%d.%m.%Y")

    positions <- panel(
      "Позиции",
      sub = format(sel_date(), "%d.%m.%Y"),
      tip = paste0(
        "Цены — закрытие выбранной торговой сессии из локального хранилища. ",
        "«От ", format(FORECAST_BASELINE_DATE, "%d.%m"), "» — рост от базы ",
        "прогноза: модель построена от неё же, поэтому факт и прогноз ",
        "сравнимы напрямую. Δ = факт − модель в процентных пунктах, ",
        "плюс означает, что бумага идёт быстрее модели. ",
        "Клик по строке открывает её график справа."),
      body_class = "bd--flush",
      uiOutput("pos_table")
    )

    # На дату до первой покупки позиций нет — и сравнивать с моделью нечего.
    # Рисуем одну карточку с коротким пустым состоянием вместо двух белых
    # коробок: карточка без результата на экране не нужна.
    if (open_n == 0) {
      first <- suppressWarnings(min(portfolio_holdings$purchase_date))
      return(tags$div(
        class = "blnr-col blnr-col--compact",
        panel("Позиции", sub = format(sel_date(), "%d.%m.%Y"),
              tip = paste0("Портфель показан на выбранную сессию. Позиции, ",
                           "купленные позже неё, в расчёт не попадают."),
              body_class = "bd--flush",
              empty_state("На эту дату позиций нет. Первая покупка — ",
                          tags$b(format(first, "%d.%m.%Y")), "."))
      ))
    }

    if (!has_fc) {
      return(tags$div(class = "blnr-col blnr-col--solo", positions))
    }
    tags$div(
      class = "blnr-col",
      positions,
      panel(
        "Факт против модели",
        sub = format(sel_date(), "%d.%m.%Y"),
        tip = paste0("По каждой бумаге: фактический рост от ", base_lab,
                     " рядом с прогнозом модели на выбранную дату. ",
                     "Расхождение столбиков и есть повод для решения ",
                     "по позиции."),
        body_class = "bd--plot",
        plotlyOutput("chart_vs_forecast", height = "100%")
      )
    )
  })

  output$pos_table <- renderUI({
    m <- portfolio_metrics()[quantity_at > 0]
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
        tags$td(formatC(r$quantity_at, format = "d")),
        plain(r$entry_price), plain(r$current_price),
        num(r$day_change_pct, fmt_pct),
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
        tags$th("Тикер"), tags$th("Кол-во"), tags$th("Вход"),
        tags$th("Цена"), tags$th("За сессию"),
        tags$th("Стоимость"), tags$th("Вес"), tags$th("От покупки"),
        tags$th(paste0("От ", base_lab)),
        if (has_fc) tags$th("Модель"),
        if (has_fc) tags$th("Δ")
      )),
      tags$tbody(rows),
      tags$tfoot(tags$tr(
        tags$td("Итого"), tags$td(), tags$td(), tags$td(),
        tags$td(class = if (isTRUE(s$day_pct >= 0)) "pos" else "neg",
                fmt_pct(s$day_pct)),
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
    sel <- sel_date()
    store_touch()
    # Показываем ту же глубину, что и шкала времени: экран и ползунок должны
    # говорить об одном отрезке.
    cnd <- md_candles(tk, days = BLNR_TIMELINE_DAYS)
    # shiny::validate явно: jsonlite (грузится через R/marketdata.R) перекрывает
    # validate своей функцией проверки JSON, и голый вызов уходит не туда.
    shiny::validate(shiny::need(nrow(cnd) > 0, sprintf(
      "Нет рядов по %s: ночная загрузка их ещё не положила в хранилище.", tk)))

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
    # базы, поэтому цена = цена базы * (1 + прогноз/100). Обрезаем выбранной
    # датой: на момент времени T мы не могли видеть будущее за T.
    fd <- forecast_data()
    if (!is.null(fd) && nrow(fd) > 0 && tk %in% fd$ticker) {
      base_px <- md_close_on_date(tk, FORECAST_BASELINE_DATE)
      if (is.finite(base_px)) {
        f <- fd[ticker == tk][date <= sel]
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

    shapes <- list(
      # Отметка выбранной сессии — она же положение ползунка.
      list(type = "line", xref = "x", yref = "paper",
           x0 = sel, x1 = sel, y0 = 0, y1 = 1,
           line = list(color = BLNR_COLORS$orange, width = 1.5))
    )
    # Цена входа — только если бумага действительно в портфеле на эту дату.
    m <- portfolio_metrics()[quantity_at > 0]
    if (tk %in% m$ticker) {
      ep <- m[ticker == tk][1, entry_price]
      if (is.finite(ep)) {
        shapes <- c(shapes, list(list(
          type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = ep, y1 = ep,
          line = list(color = BLNR_COLORS$plan, width = 1, dash = "dot"))))
      }
    }

    blnr_plot_layout(
      p,
      showlegend = FALSE,
      shapes = shapes,
      xaxis = list(title = "", rangeslider = list(visible = FALSE),
                   gridcolor = BLNR_COLORS$grid),
      yaxis = list(title = "", gridcolor = BLNR_COLORS$grid, tickprefix = "$")
    )
  })

  # Нижний ряд ####
  # Карточка рисуется только когда в ней есть результат: панель сравнения —
  # когда загружен прогноз, панель накопления невязки — когда снимков хотя бы
  # за два дня. Пустых коробок с объяснением, почему они пусты, на экране нет.
  # История невязки — только до выбранной даты: на момент T будущих снимков
  # ещё не существовало, показывать их значит рисовать то, чего не было.
  snapshots_upto <- reactive({
    snaps <- snapshots_rv()
    if (nrow(snaps) == 0) return(snaps)
    snaps[date <= sel_date()]
  })

  output$bot_panels <- renderUI({
    n_days <- length(unique(snapshots_upto()$date))
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
    m <- portfolio_metrics()[quantity_at > 0]
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
    snaps <- snapshots_upto()
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
