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

  # Реестр операций счёта: состав портфеля, средние цены и денежный остаток
  # на любую дату. Читается из хранилища; кнопка «Обновить» перетягивает его
  # из Exante, если креды настроены.
  ledger_rv <- reactiveVal(NULL)
  ledger_now <- reactive({
    store_touch()
    led <- portfolio_ledger(refresh = isTRUE(ledger_rv()))
    led
  })
  observeEvent(input$portfolio_refresh, {
    ledger_rv(TRUE)
    store_touch(Sys.time())
  })

  # Выбрана дата ПОСЛЕ последней сессии — значит смотрим в будущее: факта там
  # нет и быть не может. Портфель считаем на фактическую дату (состав и цены
  # последние известные), а прогноз — на выбранную: так видно, куда модель
  # ведёт уже открытые позиции.
  is_future <- reactive({
    f <- fact_date()
    !is.na(f) && sel_date() > f
  })

  portfolio_prices <- reactive({
    store_touch()
    d <- if (is_future()) fact_date() else sel_date()
    build_portfolio_metrics(as_of = d, ledger = ledger_now())
  })

  # Прогноз сравнивается на ТУ ЖЕ дату, что и факт: иначе ползунок двигал бы
  # факт, а модель оставалась бы на сегодня, и невязка была бы выдумкой.
  portfolio_metrics <- reactive({
    add_forecast_to_metrics(portfolio_prices(), forecast_data(), as_of = sel_date())
  })

  # Сравнение с моделью считается ЗА ПЕРИОД ВЛАДЕНИЯ, а не от базы прогноза:
  # позиция, купленная позже базы, иначе присваивает себе движение цены за
  # время, когда её не было (на боевом счёте это давало «+7.71 пп против
  # модели» при фактическом результате +0.79%).
  portfolio_vs_model <- reactive({
    m <- portfolio_metrics()
    if (is.null(m) || nrow(m) == 0 || !"forecast_since_entry_pct" %in% names(m)) return(NULL)
    ok <- m$quantity_at > 0 & is.finite(m$entry_value) &
          is.finite(m$current_value) & is.finite(m$forecast_since_entry_pct)
    if (!any(ok)) return(NULL)
    entry <- sum(m$entry_value[ok])
    fact  <- sum(m$current_value[ok])
    model <- sum(m$entry_value[ok] * (1 + m$forecast_since_entry_pct[ok] / 100))
    if (!is.finite(entry) || entry == 0) return(NULL)
    fact_pct <- (fact / entry - 1) * 100
    mdl_pct  <- (model / entry - 1) * 100
    list(fact_pct = fact_pct, fcst_pct = mdl_pct, dev_pp = fact_pct - mdl_pct)
  })

  portfolio_summary <- reactive({
    summarize_portfolio(portfolio_metrics())
  })

  # Ежедневный снимок факта и прогноза. На экран он больше не выводится —
  # невязка считается из реестра операций, которому известна вся история. Но
  # снимок остаётся ЕДИНСТВЕННОЙ записью того, каким прогноз был в тот день:
  # файл модели перезаписывается, и задним числом «прогноз на ту дату» иначе
  # не восстановить. Пишется только за фактическую дату — прогулка ползунком
  # по прошлому не должна переписывать историю.
  snapshot_done <- reactiveVal(NULL)
  observe({
    req(identical(sel_date(), fact_date()))
    m <- portfolio_metrics()
    today <- Sys.Date()
    if (!is.null(m) && nrow(m) > 0 && !all(is.na(m$forecast_pct)) &&
        !identical(snapshot_done(), today)) {
      if (isTRUE(record_snapshot(m, as_of = today))) snapshot_done(today)
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

  # Выгрузка ретроспективных рядов в типовом формате мониторинга владельца
  # (см. R/export_xlsx.R). Глубина — та же, что на экране: скачивается ровно
  # то окно, на которое человек смотрит.
  output$export_xlsx <- downloadHandler(
    filename = function() {
      sprintf("Мониторинг %s.xlsx", format(sel_date(), "%d.%m.%Y"))
    },
    content = function(file) {
      wb <- build_monitoring_workbook(
        tickers = watchlist_active()$ticker,
        days = BLNR_TIMELINE_DAYS,
        as_of = sel_date()
      )
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )

  # Шапка ####

  output$hd_source <- renderUI({
    m <- portfolio_prices()
    src <- if (nrow(m) > 0) m$source[1] else
             if (nrow(ledger_now()) > 0) "exante" else "manual"
    if (identical(src, "exante")) {
      upd <- store_ledger_updated()
      return(tags$span(
        class = "bdg ok",
        title = paste0(
          "Состав портфеля, средние цены и денежный остаток восстановлены из ",
          "истории операций счёта Exante — поэтому их видно на любую дату, а ",
          "не только на сегодня. Сверено со сводкой брокера: количества, ",
          "средние цены и кэш совпадают. Реестр обновлён ",
          if (is.null(upd)) "—" else format(upd, "%d.%m %H:%M"), "."),
        "Счёт: ", tags$b("Exante")))
    }
    tags$span(class = "bdg warn",
              title = paste("Нет EXANTE_API_ID / EXANTE_SHARED_KEY —",
                            "состав портфеля взят из списка, зашитого в коде.",
                            "Он расходится с реальным счётом тем сильнее, чем",
                            "дольше не обновлялся; кэш при этом неизвестен."),
              "Позиции: ", tags$b("вручную"))
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
    m <- portfolio_prices()
    st <- store_status()
    # Бумаги, по которым цены на выбранную дату нет. Раньше здесь висел
    # глобальный флаг ошибки источника, и одна не загруженная бумага делала
    # вид, будто котировок нет вообще.
    miss <- if (nrow(m) > 0) unique(m[quantity_at > 0 & !is.finite(price_at), ticker])
            else character()
    if (!isTRUE(st$ok)) {
      return(tags$span(class = "bdg warn", title = paste(
        "Ночная загрузка рядов ещё не отработала, поэтому цен нет и стоимость",
        "показана прочерком, а не нулём."),
        "Хранилище рядов ", tags$b("пусто")))
    }
    if (length(miss) > 0) {
      return(tags$span(class = "bdg warn", title = paste0(
        "По этим бумагам нет рядов в хранилище, поэтому стоимость и итоги ",
        "показаны прочерком: сумма с пропуском врёт увереннее, чем прочерк. ",
        "Добавьте их в реестр наблюдения и дождитесь ночной загрузки."),
        "Нет цен: ", tags$b(paste(miss, collapse = ", "))))
    }
    d <- suppressWarnings(max(m[quantity_at > 0, as.numeric(session)], na.rm = TRUE))
    if (!is.finite(d)) {
      return(tags$span(class = "bdg", "Котировки: ", tags$b("\u2014")))
    }
    last <- as.Date(d, origin = "1970-01-01")
    stale <- as.integer(Sys.Date() - last)
    tip <- paste0(
      "Цены — закрытие торговой сессии из локального хранилища рядов. ",
      "Обновляет его ночное задание Jenkins; стенд в marketdata.app не ходит, ",
      "потому что у аккаунта лимит 100 запросов в сутки. ",
      if (!is.null(st$updated_at))
        paste0("Последняя загрузка: ", format(st$updated_at, "%d.%m %H:%M"), ". ") else "",
      "Инструментов в хранилище: ", st$instruments, ".")
    if (stale > 5L && identical(sel_date(), fact_date())) {
      return(tags$span(class = "bdg warn", title = tip,
                       "Ряды устарели: ", tags$b(format(last, "%d.%m")),
                       sprintf(" (%d дн. назад)", stale)))
    }
    tags$span(class = "bdg", title = tip, "Котировки на ",
              tags$b(format(last, "%d.%m")), " (закрытие)")
  })

  # Полоса времени строится на сервере: её правая часть зависит от того, до
  # какой даты есть прогноз, а он подгружается уже после сборки страницы.
  output$timeline <- renderUI({
    fd <- forecast_data()
    timelineUI(forecast_dates = if (!is.null(fd) && nrow(fd) > 0) fd$date else NULL)
  })

  observeEvent(input$as_of_today, {
    f <- fact_date()
    if (!is.na(f)) {
      shinyWidgets::updateSliderTextInput(session, "as_of",
                                          selected = format(f, "%d.%m.%Y"))
    }
  })

  # Сессии шкалы — та же ось, что у ползунка.
  timeline_sessions <- reactive({
    store_touch()
    store_sessions_window(BLNR_TIMELINE_DAYS)
  })

  # Точки сделок на дорожке ползунка. Положение считается по индексу сессии,
  # поэтому точка всегда стоит ровно над своим делением шкалы.
  output$tl_events <- renderUI({
    sess <- timeline_sessions()
    ev <- ledger_events(ledger_now())
    req(length(sess) > 1, nrow(ev) > 0)
    ev <- ev[value_date >= min(sess) & value_date <= max(sess)]
    if (nrow(ev) == 0) return(NULL)
    # Несколько сделок одного дня — одна точка: иначе они лягут друг на друга.
    day <- ev[, .(qty = sum(qty),
                  what = paste(sprintf("%s %+g", exante_symbol_to_ticker(symbol), qty),
                               collapse = ", ")),
              by = value_date]
    dots <- lapply(seq_len(nrow(day)), function(i) {
      idx <- findInterval(day$value_date[i], sess)
      left <- (idx - 1) / (length(sess) - 1) * 100
      tags$i(
        class = if (day$qty[i] >= 0) "buy" else "sell",
        style = sprintf("left:%.4f%%", max(0, min(100, left))),
        title = paste0(format(day$value_date[i], "%d.%m.%Y"), ": ", day$what[i])
      )
    })
    do.call(tagList, dots)
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
    vf <- portfolio_vs_model()
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
      kpi(fmt_money(s$current_value), "В бумагах",
          sub = paste0(sel_lab, " \u00b7 позиций: ", s$positions),
          tip = paste0("Стоимость бумаг по ценам закрытия ", sel_lab, ".")),
      kpi(fmt_money(s$cash), "Кэш",
          sub = if (is.finite(s$cash)) "денежный остаток счёта" else "нет данных",
          tip = paste0(
            "Денежный остаток на ", sel_lab,
            " — сумма всех движений по счёту по эту дату включительно ",
            "(пополнения, сделки, комиссии, дивиденды, налоги). ",
            "Считается из истории операций Exante, а не берётся «на сегодня»: ",
            "сегодняшний кэш рядом с прошлой стоимостью бумаг дал бы итог, ",
            "которого никогда не существовало.")),
      kpi(fmt_money(s$total_value), "Итого по счёту",
          sub = "бумаги + кэш",
          tip = paste0("Бумаги плюс денежный остаток на ", sel_lab,
                       ". Прочерк, если неизвестно хотя бы одно слагаемое: ",
                       "сумма с пропуском врёт увереннее, чем прочерк.")),
      kpi(fmt_signed_money(s$day_pnl), "За сессию", tone_of(s$day_pnl),
          sub = fmt_pct(s$day_pct),
          tip = "Изменение стоимости бумаг за одну торговую сессию — «на момент»."),
      kpi(fmt_signed_money(s$pnl), "Накопленным итогом", tone_of(s$pnl),
          sub = fmt_pct(s$growth_pct),
          tip = paste0("Результат от цены покупки до ", sel_lab,
                       " — по открытым на эту дату позициям."))
    )
    if (!is.null(vf) && is_future()) {
      # В будущем факта нет и быть не может: сравнивать его с прогнозом на ту
      # дату значит смешивать сегодняшний результат с декабрьским ожиданием.
      # Поэтому показываем, ЧТО МОДЕЛЬ ОБЕЩАЕТ к выбранной дате, а факт — как
      # отсчётную точку в подписи.
      tiles <- c(tiles, list(
        kpi(fmt_pct(vf$fcst_pct), paste("Модель к", sel_lab),
            tone_of(vf$fcst_pct), tip_align = "r",
            sub = paste0("факт на ", format(fact_date(), "%d.%m"), " ",
                         fmt_pct(vf$fact_pct)),
            tip = paste0(
              "Прогноз по уже открытым позициям к ", sel_lab,
              ", считая от цен входа. Факта на эту дату не существует — ",
              "он показан на последнюю торговую сессию как точка отсчёта. ",
              "Состав портфеля берётся текущий: что будет куплено или продано ",
              "позже, модель не знает."))
      ))
    } else if (!is.null(vf)) {
      tiles <- c(tiles, list(
        kpi(fmt_pp(vf$dev_pp), "Портфель против модели", tone_of(vf$dev_pp),
            # плитка крайняя справа — подсказку прижимаем к правому краю
            tip_align = "r",
            sub = paste0("факт ", fmt_pct(vf$fact_pct), " \u00b7 модель ",
                         fmt_pct(vf$fcst_pct)),
            tip = paste0(
              "Считается ЗА ПЕРИОД ВЛАДЕНИЯ: факт — от цен входа, модель — ",
              "прогноз, приведённый к дате покупки каждой позиции. ",
              "Сравнение «от базы прогноза» здесь не годится: позиция, ",
              "купленная позже базы, присвоила бы себе движение цены за ",
              "время, когда её ещё не было. Плюс — портфель идёт быстрее ",
              "модели. Взвешено по стоимости входа."))
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
      sub = if (is_future())
              paste0("прогноз на ", format(sel_date(), "%d.%m.%Y"))
            else format(sel_date(), "%d.%m.%Y"),
      tip = paste0(
        "Цены — закрытие выбранной торговой сессии из локального хранилища. ",
        "«От покупки» — результат позиции от цены входа. «Модель» — прогноз ",
        "за ТОТ ЖЕ период владения: накопленный прогноз приведён к дате ",
        "покупки, иначе бумага, купленная позже базы прогноза, присвоила бы ",
        "себе движение цены за время, когда её не было. Δ = факт − модель в ",
        "процентных пунктах. Последняя колонка — движение самой бумаги от ",
        "базы прогноза, безотносительно того, когда мы её купили. ",
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
        if (is_future()) "Ожидание по модели" else "Факт против модели",
        sub = if (is_future())
                paste0("к ", format(sel_date(), "%d.%m.%Y"))
              else format(sel_date(), "%d.%m.%Y"),
        tip = paste0("По каждой бумаге за период владения: фактический рост ",
                     "от цены входа рядом с прогнозом модели за тот же ",
                     "период. Расхождение столбиков и есть повод для решения ",
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
        if (has_fc) num(r$forecast_since_entry_pct, fmt_pct),
        if (has_fc) num(r$dev_since_entry_pp, fmt_pp),
        num(r$growth_from_base_pct, fmt_pct)
      )
    })

    s <- portfolio_summary()
    vf <- portfolio_vs_model()
    base_lab <- format(FORECAST_BASELINE_DATE, "%d.%m")

    tags$div(class = "rk", tags$table(
      tags$thead(tags$tr(
        tags$th("Тикер"), tags$th("Кол-во"), tags$th("Вход"),
        tags$th("Цена"), tags$th("За сессию"),
        tags$th("Стоимость"), tags$th("Вес"), tags$th("От покупки"),
        if (has_fc) tags$th("Модель"),
        if (has_fc) tags$th("Δ"),
        tags$th(paste0("Бумага от ", base_lab))
      )),
      tags$tbody(rows),
      tags$tfoot(tags$tr(
        tags$td("Итого"), tags$td(), tags$td(), tags$td(),
        tags$td(class = tone_of(s$day_pct), fmt_pct(s$day_pct)),
        tags$td(fmt_money(s$current_value)), tags$td("100%"),
        tags$td(class = tone_of(s$growth_pct), fmt_pct(s$growth_pct)),
        if (has_fc) tags$td(class = if (is.null(vf)) "mut" else tone_of(vf$fcst_pct),
                            if (is.null(vf)) "\u2014" else fmt_pct(vf$fcst_pct)),
        if (has_fc) tags$td(class = if (is.null(vf)) "mut" else tone_of(vf$dev_pp),
                            if (is.null(vf)) "\u2014" else fmt_pp(vf$dev_pp)),
        tags$td()
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
    # базы, поэтому цена = цена базы * (1 + прогноз/100). Показываем её ЦЕЛИКОМ
    # на горизонт вперёд, а не обрезаем выбранной датой: это не подсматривание
    # будущего, а прогноз, выданный на базовую дату, — его и надо видеть рядом
    # с фактом, чтобы понимать, куда модель вела.
    fd <- forecast_data()
    if (!is.null(fd) && nrow(fd) > 0 && tk %in% fd$ticker) {
      base_px <- md_close_on_date(tk, FORECAST_BASELINE_DATE)
      if (is.finite(base_px)) {
        horizon <- max(cnd$date) + BLNR_FORECAST_HORIZON_DAYS
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

    # Сделки по этой бумаге: где вошли и где вышли. Без них график — просто
    # картинка цены, по которой не видно, что владелец с ней делал.
    ev <- ledger_events(ledger_now())
    ev <- ev[exante_symbol_to_ticker(symbol) == tk &
             value_date >= min(cnd$date) & value_date <= max(cnd$date)]
    if (nrow(ev) > 0) {
      ev[, side := data.table::fifelse(qty >= 0, "покупка", "продажа")]
      p <- add_trace(
        p, data = ev, x = ~value_date, y = ~price, inherit = FALSE,
        type = "scatter", mode = "markers", name = "сделки",
        marker = list(
          size = 11, symbol = "diamond",
          color = ifelse(ev$qty >= 0, BLNR_COLORS$ok, BLNR_COLORS$bad),
          line = list(color = "#fff", width = 1.5)),
        hovertext = sprintf("%s %s %g \u00b7 $%s",
                            format(ev$value_date, "%d.%m.%Y"), ev$side,
                            abs(ev$qty), formatC(ev$price, format = "f", digits = 2)),
        hoverinfo = "text")
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
  # Динамика счёта по сессиям — из реестра операций, поэтому доступна сразу на
  # всю историю, а не «когда накопятся снимки».
  # Окно динамики: шкала ползунка или вся глубина хранилища. Окна в 150
  # сессий мало — счёт может простоять в деньгах весь этот отрезок, и график
  # выглядит пустым, хотя история у счёта есть. По умолчанию показываем всё.
  value_scope <- reactiveVal("all")
  observeEvent(input$vs_window, value_scope("window"))
  observeEvent(input$vs_all,    value_scope("all"))

  value_sessions <- reactive({
    store_touch()
    if (identical(value_scope(), "window")) timeline_sessions()
    else store_sessions_window(1e6L)
  })

  value_series <- reactive({
    portfolio_value_series(ledger_now(), value_sessions())
  })

  # Невязка портфеля по сессиям — из реестра и прогноза, а не из ежедневных
  # снимков стенда: снимки давали две точки и подпись «динамика появится со
  # второго снимка», то есть виджет, которому нечего показать.
  deviation_series <- reactive({
    portfolio_deviation_series(ledger_now(), forecast_data(), timeline_sessions())
  })

  output$bot_panels <- renderUI({
    items <- list()
    if (nrow(value_series()) > 0) {
      v <- value_series()
      unp <- attr(v, "unpriced") %||% character()
      items <- c(items, list(panel(
        "Динамика счёта",
        sub = sprintf("%s — %s", format(min(v$date), "%d.%m.%Y"),
                      format(max(v$date), "%d.%m.%Y")),
        right = tags$div(
          class = "seg sm",
          actionButton("vs_all", "вся история",
                       class = if (identical(value_scope(), "all")) "on" else NULL),
          actionButton("vs_window", "окно шкалы",
                       class = if (identical(value_scope(), "window")) "on" else NULL)
        ),
        tip = paste0(
          "Стоимость бумаг, денежный остаток и итог по счёту на каждую ",
          "торговую сессию окна. Считается из истории операций Exante, а не ",
          "из ежедневных снимков стенда: снимки начинаются со дня первого ",
          "запуска, а история счёта известна целиком. Глубина «всей истории» ",
          "ограничена хранилищем рядов цен. Разрыв значит, что на этих ",
          "сессиях в портфеле была бумага, которую нечем оценить — ",
          "занижать стоимость молча нельзя",
          if (length(unp))
            paste0("; сейчас это ", length(unp), ": ",
                   paste(utils::head(unp, 4), collapse = ", "),
                   if (length(unp) > 4) " и другие" else "")
          else "",
          ". Вертикальная линия — выбранная дата."),
        body_class = "bd--plot",
        plotlyOutput("chart_value", height = "100%")
      )))
    }
    if (nrow(deviation_series()) > 1) {
      items <- c(items, list(panel(
        "Портфель против модели",
        sub = "за период владения",
        tip = paste0(
          "По каждой сессии: фактический результат портфеля от цен входа и ",
          "то, что обещала модель за тот же период владения. Нижняя линия — ",
          "разница в процентных пунктах: выше нуля портфель идёт быстрее ",
          "модели. Прогноз приводится к дате покупки каждой позиции, иначе ",
          "бумага, купленная позже базы прогноза, присвоила бы себе движение ",
          "цены за время, когда её не было."),
        body_class = "bd--plot",
        plotlyOutput("chart_deviation", height = "100%")
      )))
    }
    if (length(items) == 0) return(NULL)
    do.call(tagList, items)
  })

  output$chart_value <- renderPlotly({
    v <- value_series()
    req(nrow(v) > 0)
    sel <- sel_date()
    # Линии, а НЕ области с накоплением. Стопка была бы читабельнее, но кэш на
    # этом счёте уходит в минус (маржинальные заимствования, на истории до
    # −$19 925), а стопка с отрицательной составляющей рисует неправду:
    # верхняя граница перестаёт быть итогом. Поэтому итог — отдельной жирной
    # линией, плюс нулевая отметка, чтобы минус по кэшу был очевиден.
    p <- plot_ly(v, x = ~date, y = ~total, type = "scatter", mode = "lines",
                 name = "итого",
                 line = list(color = BLNR_COLORS$text, width = 2.2))
    p <- add_trace(p, y = ~securities, name = "бумаги",
                   line = list(color = BLNR_COLORS$ok, width = 1.4))
    p <- add_trace(p, y = ~cash, name = "кэш",
                   line = list(color = BLNR_COLORS$plan, width = 1.4,
                               dash = "dot"))
    blnr_plot_layout(
      p,
      legend = list(orientation = "h", x = 0, y = 1.16, font = list(size = 10)),
      margin = list(l = 56, r = 10, t = 20, b = 26),
      shapes = list(
        list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
             line = list(color = BLNR_COLORS$border, width = 1)),
        list(type = "line", xref = "x", yref = "paper",
             x0 = sel, x1 = sel, y0 = 0, y1 = 1,
             line = list(color = BLNR_COLORS$orange, width = 1.5))),
      xaxis = list(title = "", gridcolor = BLNR_COLORS$grid),
      yaxis = list(title = "", gridcolor = BLNR_COLORS$grid, tickprefix = "$")
    )
  })

  output$chart_vs_forecast <- renderPlotly({
    m <- portfolio_metrics()[quantity_at > 0]
    req(nrow(m) > 0)
    blnr_plot_layout(
      add_trace(
        plot_ly(m, x = ~ticker, y = ~growth_pct, type = "bar",
                name = "факт", marker = list(color = BLNR_COLORS$ok)),
        y = ~forecast_since_entry_pct, name = "модель",
        marker = list(color = BLNR_COLORS$plan)),
      barmode = "group",
      legend = list(orientation = "h", x = 0, y = 1.14, font = list(size = 10)),
      margin = list(l = 40, r = 10, t = 20, b = 26),
      xaxis = list(title = ""),
      yaxis = list(title = "", ticksuffix = "%", gridcolor = "#eceff3")
    )
  })

  output$chart_deviation <- renderPlotly({
    d <- deviation_series()
    req(nrow(d) > 1)
    sel <- sel_date()
    p <- plot_ly(d, x = ~date, y = ~fact_pct, type = "scatter", mode = "lines",
                 name = "факт", line = list(color = BLNR_COLORS$ok, width = 2))
    p <- add_trace(p, y = ~model_pct, name = "модель",
                   line = list(color = BLNR_COLORS$plan, width = 1.6,
                               dash = "dash"))
    p <- add_trace(p, y = ~dev_pp, name = "разница, пп",
                   line = list(color = BLNR_COLORS$series, width = 1.2))
    blnr_plot_layout(
      p,
      legend = list(orientation = "h", x = 0, y = 1.16, font = list(size = 10)),
      margin = list(l = 46, r = 10, t = 20, b = 26),
      shapes = list(
        list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
             line = list(color = BLNR_COLORS$border, width = 1)),
        list(type = "line", xref = "x", yref = "paper",
             x0 = sel, x1 = sel, y0 = 0, y1 = 1,
             line = list(color = BLNR_COLORS$orange, width = 1.5))),
      xaxis = list(title = "", gridcolor = BLNR_COLORS$grid),
      yaxis = list(title = "", ticksuffix = "%", gridcolor = BLNR_COLORS$grid)
    )
  })
})
