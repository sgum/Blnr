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
    open_pos <- m[quantity_at > 0]
    if (nrow(open_pos) == 0) return(NULL)
    ok <- is.finite(open_pos$entry_value) & is.finite(open_pos$current_value) &
          is.finite(open_pos$forecast_since_entry_pct)
    if (!any(ok)) {
      # Считать не по чему: у всех позиций нулевой срок владения. Это ОТВЕТ,
      # и он должен дойти до экрана, а не превратиться в пустую плитку.
      return(list(covered = 0L, total = nrow(open_pos)))
    }
    entry <- sum(open_pos$entry_value[ok])
    fact  <- sum(open_pos$current_value[ok]) - entry
    model <- sum(open_pos$entry_value[ok] * open_pos$forecast_since_entry_pct[ok] / 100)
    list(
      # В ДЕНЬГАХ: в процентах это доходность вложенного, и одна позиция с
      # ненулевым сроком выдавала свой результат за результат всего портфеля.
      fact_money = fact, model_money = model, dev_money = fact - model,
      invested = entry, covered = sum(ok), total = nrow(open_pos)
    )
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
      title = "Прогноз и предлагаемый портфель",
      size = "l", easyClose = TRUE, footer = modalButton("Закрыть"),
      tags$p(style = "font-size:12px;color:#5C5C5C",
             "Шаг 2 после выгрузки рядов: сюда возвращается результат ",
             "внешнего расчёта. Лист «Q_mean_var», блок «mean»: строки — ",
             "инструменты реестра, столбцы — шаги .qM0, .qM1, … Значения — ",
             "уже накопленный относительный прогноз роста; шаг k ",
             "раскладывается на торговые дни от базы, заданной файлом."),
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
  # Ось шкалы времени — ОДНА на ползунок и на точки сделок. Пока точки
  # считали своё положение по длине прошлой части, а дорожка была длиннее на
  # будущее, последняя сделка уезжала в правый край, то есть в декабрь.
  timeline_axis <- reactive({
    store_touch()
    past <- store_sessions_window(BLNR_TIMELINE_DAYS)
    fd <- forecast_data()
    future <- if (!is.null(fd) && nrow(fd) > 0) {
      d <- sort(unique(as.Date(fd$date)))
      utils::head(d[d > max(past)], BLNR_FUTURE_DAYS)
    } else as.Date(character())
    list(past = past, future = future, all = c(past, future))
  })

  output$timeline <- renderUI({
    ax <- timeline_axis()
    timelineUI(past = ax$past, future = ax$future)
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
    ax <- timeline_axis()
    all_dates <- ax$all
    ev <- ledger_events(ledger_now())
    req(length(all_dates) > 1, nrow(ev) > 0)
    ev <- ev[value_date >= min(all_dates) & value_date <= max(all_dates)]
    if (nrow(ev) == 0) return(NULL)
    # Несколько сделок одного дня — одна точка: иначе они лягут друг на друга.
    day <- ev[, .(qty = sum(qty),
                  what = paste(sprintf("%s %+g", exante_symbol_to_ticker(symbol), qty),
                               collapse = ", ")),
              by = value_date]
    dots <- lapply(seq_len(nrow(day)), function(i) {
      # Положение считается по ПОЛНОЙ оси ползунка, включая будущую часть:
      # иначе точка нормируется на другую длину и съезжает вправо.
      idx <- findInterval(day$value_date[i], all_dates)
      left <- (idx - 1) / (length(all_dates) - 1) * 100
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
    if (!is.null(vf) && identical(vf$covered, 0L)) {
      reasons <- portfolio_metrics()[quantity_at > 0, model_gap]
      reasons <- reasons[!is.na(reasons)]
      tiles <- c(tiles, list(
        kpi("\u2014", "Лучше модели", tip_align = "r",
            sub = if (length(reasons))
                    paste(names(sort(table(reasons), decreasing = TRUE))[1],
                          sprintf("(%d из %d)", max(table(reasons)), vf$total))
                  else sprintf("нет сравнимых позиций из %d", vf$total),
            tip = paste0(
              "Сравнивать не с чем. Бумага, купленная ДО появления прогноза, ",
              "в сравнение не входит: её факт считался бы за весь срок ",
              "владения, а модель — только с даты прогноза, и разность таких ",
              "отрезков не значит ничего. Купленная сегодня не входит тоже — ",
              "за нулевой срок модель ничего не обещала. Число появится со ",
              "следующей торговой сессии по бумагам, купленным под прогноз."))
      ))
    } else if (!is.null(vf) && is_future()) {
      # В будущем факта нет и быть не может: сравнивать его с прогнозом на ту
      # дату значит смешивать сегодняшний результат с декабрьским ожиданием.
      # Поэтому показываем, ЧТО МОДЕЛЬ ОБЕЩАЕТ к выбранной дате, а факт — как
      # отсчётную точку в подписи.
      tiles <- c(tiles, list(
        kpi(fmt_signed_money(vf$model_money), paste("Модель к", sel_lab),
            tone_of(vf$model_money), tip_align = "r",
            sub = paste0("факт на ", format(fact_date(), "%d.%m"), " ",
                         fmt_signed_money(vf$fact_money)),
            tip = paste0(
              "Прогноз по уже открытым позициям к ", sel_lab,
              ", считая от цен входа. Факта на эту дату не существует — ",
              "он показан на последнюю торговую сессию как точка отсчёта. ",
              "Состав портфеля берётся текущий: что будет куплено или продано ",
              "позже, модель не знает."))
      ))
    } else if (!is.null(vf)) {
      tiles <- c(tiles, list(
        kpi(fmt_signed_money(vf$dev_money), "Лучше модели", tone_of(vf$dev_money),
            # плитка крайняя справа — подсказку прижимаем к правому краю
            tip_align = "r",
            sub = sprintf("факт %s \u00b7 модель %s \u00b7 по %d из %d позиций",
                          fmt_signed_money(vf$fact_money),
                          fmt_signed_money(vf$model_money),
                          vf$covered, vf$total),
            tip = paste0(
              "Считается ЗА СРОК ВЛАДЕНИЯ: факт — от цен входа, модель — ",
              "прогноз, приведённый к дате последней покупки каждой позиции. ",
              "Сравнение от фиксированной даты здесь не годится: бумага, ",
              "купленная позже, присвоила бы себе движение цены за время, ",
              "когда её ещё не было. В расчёт входят только позиции с ",
              "НЕНУЛЕВЫМ сроком владения — купленным сегодня модель ничего ",
              "не обещала. Плюс — портфель идёт быстрее модели. Взвешено по ",
              "стоимости входа."))
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
        "«Вложено» — сколько стоила позиция на входе, «Стоимость» — сколько ",
        "стоит сейчас. Наличные показаны отдельной строкой, а вес считается ",
        "от всего счёта: доля бумаги «28% портфеля» при половине счёта в ",
        "деньгах была бы неправдой. Два итога — по бумагам и по счёту ",
        "целиком. ",
        "«От покупки» — результат позиции от цены входа. «Модель» — прогноз ",
        "за ТОТ ЖЕ период владения: накопленный прогноз приведён к дате ",
        "покупки, иначе бумага, купленная позже базы прогноза, присвоила бы ",
        "себе движение цены за время, когда её не было. Δ = факт − модель в ",
        "процентных пунктах. «Куплено» — дата последней покупки и срок ",
        "владения НА ВЫБРАННУЮ ДАТУ (сдвиньте ползунок — срок вырастет): ",
        "именно от этой даты отсчитываются и результат, и прогноз, ",
        "потому что докупка меняет позицию. При нулевом сроке владения в ",
        "«Ожидалось» стоит прочерк: за ноль дней модель не обещала ничего, и ",
        "показать там 0% значило бы приписать ей обещание топтаться на месте. ",
        "Фиксированной даты отсчёта на экране больше нет — она была бы чужой ",
        "для бумаги, купленной позже. Клик по строке открывает её график."),
      right = if (user_can_trade(USER$login) && exante_has_credentials() &&
                  !is_future() && identical(sel_date(), fact_date()))
                tags$div(style = "display:flex;gap:6px",
                  actionButton("buy_open", "+ Купить", class = "btn-buy"),
                  actionButton("sell_all_open", "Продать всё", class = "btn-sellall")),
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
    # Торговать можно только сейчас: на прошлой или будущей дате поручение
    # бессмысленно, и кнопку там показывать нельзя.
    can_trade <- user_can_trade(USER$login) && exante_has_credentials() &&
                 !is_future() && identical(sel_date(), fact_date())

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
        plain(r$entry_value, 0),
        plain(r$current_value, 0),
        # Вес — числом и заливкой ячейки: отдельная карточка «Структура
        # портфеля» ради тех же пяти чисел заняла бы полосу экрана.
        tags$td(
          style = sprintf(
            "background:linear-gradient(to left,#ffeccc %1$.1f%%,transparent %1$.1f%%)",
            max(0, min(100, r$weight_pct))),
          formatC(r$weight_pct, format = "f", digits = 1), "%"),
        num(r$growth_pct, fmt_pct),
        if (has_fc) {
          if (is.finite(r$forecast_since_entry_pct)) num(r$forecast_since_entry_pct, fmt_pct)
          else tags$td(class = "mut", style = "font-size:10.5px",
                       r$model_gap %||% "\u2014")
        },
        if (has_fc) num(r$dev_since_entry_pp, fmt_pp),
        tags$td(class = "mut",
                if (is.na(r$last_buy_date)) "\u2014"
                else {
                  # Срок считается НА ВЫБРАННУЮ ДАТУ. Ноль означает «куплено в
                  # ту самую сессию, которую смотрим», а не ошибку — поэтому
                  # словами, а не цифрой: «0 дн» рядом с датой двухдневной
                  # давности читается как сбой.
                  n <- as.integer(sel_date() - r$last_buy_date)
                  lbl <- if (is.na(n)) "\u2014"
                         else if (n == 0) "в этот день"
                         else sprintf("%d дн", n)
                  sprintf("%s \u00b7 %s", format(r$last_buy_date, "%d.%m.%y"), lbl)
                }),
        # Продажа только на фактической дате: торговать «на прошлую сессию»
        # нельзя, а кнопка на ней читалась бы как рабочая.
        tags$td(class = "r",
          if (can_trade)
            tags$button(class = "tr-btn sell", title = paste("Продать", r$ticker),
              onclick = sprintf(
                "event.stopPropagation();Shiny.setInputValue('trade_open','sell|%s',{priority:'event'})",
                r$ticker),
              "\u2212")
          else tags$span(class = "mut", "\u2014"))
      )
    })

    s <- portfolio_summary()
    vf <- portfolio_vs_model()

    tags$div(class = "rk", tags$table(
      tags$thead(tags$tr(
        tags$th("Тикер"), tags$th("Кол-во"), tags$th("Вход"),
        tags$th("Цена"), tags$th("За сессию"),
        tags$th("Вложено"), tags$th("Стоимость"), tags$th("Вес"),
        tags$th("Результат"),
        if (has_fc) tags$th("Ожидалось"),
        if (has_fc) tags$th("Лучше модели"),
        tags$th("Куплено"),
        tags$th("")
      )),
      tags$tbody(
        rows,
        # Наличные — такая же часть счёта, как бумаги. Без этой строки рост
        # портфеля читался только по бумагам, а половина счёта лежала в
        # деньгах и в картине не участвовала вовсе.
        if (is.finite(s$cash)) tags$tr(
          class = "cash",
          tags$td(class = "nm", "Наличные"),
          tags$td(), tags$td(), tags$td(), tags$td(), tags$td(),
          tags$td(fmt_money(s$cash)),
          tags$td(sprintf("%.1f%%", s$cash / s$total_value * 100)),
          tags$td(), if (has_fc) tags$td(), if (has_fc) tags$td(),
          tags$td(), tags$td()
        )
      ),
      # Два итога намеренно: по бумагам виден результат вложений, по счёту —
      # сколько портфель стоит на самом деле.
      tags$tfoot(
        tags$tr(
          tags$td("Бумаги"), tags$td(), tags$td(), tags$td(),
          tags$td(class = tone_of(s$day_pct), fmt_pct(s$day_pct)),
          tags$td(fmt_money(s$entry_value)),
          tags$td(fmt_money(s$current_value)),
          tags$td(sprintf("%.1f%%", s$current_value / s$total_value * 100)),
          tags$td(class = tone_of(s$growth_pct), fmt_pct(s$growth_pct)),
          if (has_fc) tags$td(class = if (is.null(vf$model_money)) "mut" else tone_of(vf$model_money),
                              if (is.null(vf$model_money)) "\u2014" else fmt_signed_money(vf$model_money)),
          if (has_fc) tags$td(class = if (is.null(vf$dev_money)) "mut" else tone_of(vf$dev_money),
                              if (is.null(vf$dev_money)) "\u2014" else fmt_signed_money(vf$dev_money)),
          tags$td(), tags$td()
        ),
        tags$tr(
          class = "acct",
          tags$td("Итого по счёту"), tags$td(), tags$td(), tags$td(), tags$td(),
          tags$td(),
          tags$td(fmt_money(s$total_value)),
          tags$td("100%"),
          tags$td(class = tone_of(s$pnl), fmt_signed_money(s$pnl)),
          if (has_fc) tags$td(), if (has_fc) tags$td(),
          tags$td(), tags$td()
        )
      )
    ))
  })

  # --- Сделки ---------------------------------------------------------------
  #
  # ГРАНИЦА. Поручение уходит на боевой счёт ТОЛЬКО из обработчика
  # input$trade_confirm, то есть по явному нажатию владельца в окне
  # подтверждения, где показано точное тело запроса. Ни один автоматический
  # путь — реактив, таймер, стартовый observe — не вызывает
  # exante_place_order() с apply = TRUE. Это правило, а не удобство: стенд
  # распоряжается реальными деньгами.
  trade_req <- reactiveVal(NULL)

  # Кнопки в шапке карточки позиций. Ничего не отправляют — только открывают
  # окно подтверждения, где показано, что именно уйдёт.
  observeEvent(input$buy_open, {
    req(user_can_trade(USER$login))
    shinyjs::runjs(sprintf(
      "Shiny.setInputValue('trade_open','buy|%s',{priority:'event'})",
      input$sel_ticker %||% ""))
  })

  observeEvent(input$sell_all_open, {
    req(user_can_trade(USER$login))
    shinyjs::runjs("Shiny.setInputValue('trade_open','sell_all',{priority:'event'})")
  })

  # Открыть окно подтверждения. Здесь НИЧЕГО не отправляется.
  observeEvent(input$trade_open, {
    req(user_can_trade(USER$login))
    req(input$trade_open)
    parts <- strsplit(input$trade_open, "|", fixed = TRUE)[[1]]
    req(length(parts) >= 1)
    side <- parts[1]
    tk <- if (length(parts) >= 2) parts[2] else NA_character_
    m <- portfolio_metrics()

    if (identical(side, "sell_all")) {
      held <- m[quantity_at > 0]
      if (nrow(held) == 0) {
        showNotification("Портфель пуст — продавать нечего.", type = "warning")
        return(invisible(NULL))
      }
      trade_req(list(side = "sell_all"))
      est <- sum(held$current_value, na.rm = TRUE)
      showModal(modalDialog(
        title = "Продать весь портфель",
        size = "m", easyClose = TRUE,
        footer = tagList(
          modalButton("Отмена"),
          actionButton("trade_confirm",
                       sprintf("Продать %d позиций по рынку", nrow(held)),
                       class = "btn-trade")
        ),
        tags$p(style = "font-size:12px;color:#646b78",
               "На каждую позицию уйдёт отдельное рыночное поручение. ",
               "Действие необратимо: отменить исполненную сделку нельзя, ",
               "обратная покупка пройдёт уже по другой цене."),
        uiOutput("trade_all_list"),
        tags$div(style = "margin-top:10px",
                 checkboxInput("trade_all_ack",
                               sprintf("Да, продать все %d позиций примерно на %s",
                                       nrow(held), fmt_money(est)),
                               value = FALSE)),
        uiOutput("trade_preview")
      ))
      return(invisible(NULL))
    }

    row <- if (!is.na(tk)) m[ticker == tk] else m[0]
    max_qty <- if (nrow(row)) row$quantity_at[1] else 0
    trade_req(list(side = side, ticker = tk, max_qty = max_qty))
    wl <- watchlist_active()
    showModal(modalDialog(
      title = if (identical(side, "buy")) "Покупка" else paste("Продажа", tk),
      size = "m", easyClose = TRUE,
      footer = tagList(
        modalButton("Отмена"),
        actionButton("trade_confirm",
                     if (identical(side, "buy")) "Купить по рынку" else "Продать по рынку",
                     class = "btn-trade")
      ),
      tags$p(style = "font-size:12px;color:#646b78",
             "Поручение рыночное, внутридневное. Цена исполнения будет ",
             "биржевой на момент приёма — показанная ниже это последняя ",
             "известная цена закрытия, а не гарантия."),
      # Бумага выбирается ИЗ РЕЕСТРА НАБЛЮДЕНИЯ: покупать вслепую по тикеру,
      # набранному руками, нельзя — на такую бумагу нет ни ряда цен, ни
      # прогноза, и в портфеле она станет слепым пятном.
      if (identical(side, "buy"))
        selectInput("trade_ticker", "Бумага", width = "100%",
                    choices = stats::setNames(as.list(wl$ticker),
                                              paste0(wl$ticker, " \u00b7 ", wl$name_ru)),
                    selected = if (!is.na(tk) && tk %in% wl$ticker) tk else wl$ticker[1]),
      numericInput("trade_qty", "Количество",
                   value = if (identical(side, "sell") && max_qty > 0) max_qty else 1,
                   min = 1, step = 1,
                   max = if (identical(side, "sell")) max(1, max_qty) else NA),
      if (identical(side, "sell"))
        tags$p(style = "font-size:11.5px;color:#646b78",
               sprintf("В портфеле: %g шт.", max_qty)),
      uiOutput("trade_preview")
    ))
  })

  # Бумага поручения: при покупке её выбирают в окне, при продаже она задана
  # строкой, по которой нажали.
  trade_ticker <- reactive({
    r <- trade_req()
    if (is.null(r)) return(NA_character_)
    if (identical(r$side, "buy")) (input$trade_ticker %||% r$ticker) else r$ticker
  })

  # Список того, что уйдёт при продаже всего портфеля.
  output$trade_all_list <- renderUI({
    r <- trade_req(); req(identical(r$side, "sell_all"))
    held <- portfolio_metrics()[quantity_at > 0]
    led <- ledger_now()
    rows <- lapply(seq_len(nrow(held)), function(i) {
      tk <- held$ticker[i]
      sym <- exante_symbol_for_ticker(tk, ledger = led)
      tags$tr(
        tags$td(tags$b(tk)),
        tags$td(class = "r", formatC(held$quantity_at[i], format = "d")),
        tags$td(class = "r", fmt_money(held$current_value[i])),
        tags$td(class = "r",
                if (is.na(sym)) tags$span(class = "neg", "нет кода")
                else tags$span(class = "mut", sym))
      )
    })
    tags$div(class = "wl-list", style = "max-height:210px",
      tags$table(
        tags$thead(tags$tr(tags$th("Тикер"), tags$th(class = "r", "Кол-во"),
                           tags$th(class = "r", "Ориентировочно"),
                           tags$th(class = "r", "Код"))),
        tags$tbody(rows)))
  })

  output$trade_preview <- renderUI({
    r <- trade_req(); req(r)
    if (identical(r$side, "sell_all")) {
      held <- portfolio_metrics()[quantity_at > 0]
      led <- ledger_now()
      bad <- held$ticker[is.na(vapply(held$ticker, exante_symbol_for_ticker,
                                      character(1), ledger = led))]
      if (length(bad) > 0) {
        return(tags$div(class = "wl-msg bad",
          paste0("Не знаю биржевой код для: ", paste(bad, collapse = ", "),
                 ". Эти позиции придётся продать по отдельности.")))
      }
      if (!isTRUE(input$trade_all_ack)) {
        return(tags$div(class = "wl-msg bad",
          "Отметьте согласие выше — продажа всего портфеля необратима."))
      }
      return(NULL)
    }

    tk <- trade_ticker()
    qty <- suppressWarnings(as.numeric(input$trade_qty))
    if (is.na(tk) || !nzchar(tk)) {
      return(tags$div(class = "wl-msg bad", "Бумага не выбрана."))
    }
    sym <- exante_symbol_for_ticker(tk, ledger = ledger_now())
    if (is.na(sym)) {
      return(tags$div(class = "wl-msg bad",
        paste0("Не знаю биржевой код для ", tk,
               ". Он появится после первой сделки по этой бумаге на счёте — ",
               "гадать суффикс нельзя, GS.NYSE и GS.NASDAQ это разные ",
               "инструменты.")))
    }
    if (!is.finite(qty) || qty <= 0) {
      return(tags$div(class = "wl-msg bad", "Количество должно быть положительным."))
    }
    if (identical(r$side, "sell") && qty > r$max_qty) {
      return(tags$div(class = "wl-msg bad",
        sprintf("В портфеле только %g шт. Продать больше нельзя.", r$max_qty)))
    }
    px <- md_last_price(tk)
    est <- if (is.finite(px)) qty * px else NA_real_
    tags$div(
      class = "wl-msg ok",
      tags$div(tags$b(sym), " \u00b7 ",
               if (identical(r$side, "buy")) "покупка" else "продажа",
               " ", qty, " шт."),
      tags$div(style = "margin-top:4px",
               "Ориентировочно ", tags$b(fmt_money(est)),
               " по последней цене ", fmt_money(px, 2), ".")
    )
  })

  # ЕДИНСТВЕННОЕ место, отправляющее поручение.
  observeEvent(input$trade_confirm, {
    # ПРАВО ТОРГОВАТЬ проверяется здесь, а не только скрытием кнопок: разметку
    # подделывают из консоли браузера за секунду, а это боевой счёт. Смотреть
    # портфель может каждый из белого списка, распоряжаться им — только
    # владелец (BLNR_TRADERS, по умолчанию s.gumerov).
    if (!user_can_trade(USER$login)) {
      cat(sprintf("[TRADE] ОТКАЗ В ПРАВЕ login=%s %s\n",
                  USER$login %||% "?", format(Sys.time())))
      store_append_order(USER$login %||% "?", "?", "?", 0, "нет права",
                         "пользователь не в списке BLNR_TRADERS")
      removeModal()
      showNotification("Распоряжаться счётом может только его владелец.",
                        type = "error", duration = 10)
      return(invisible(NULL))
    }
    r <- trade_req(); req(r)
    qty <- suppressWarnings(as.numeric(input$trade_qty))
    sym <- exante_symbol_for_ticker(r$ticker, ledger = ledger_now())
    bad <- if (is.na(sym)) "неизвестен биржевой код"
           else if (!is.finite(qty) || qty <= 0) "некорректное количество"
           else if (identical(r$side, "sell") && qty > r$max_qty) "больше, чем есть в портфеле"
           else NULL
    if (!is.null(bad)) {
      showNotification(paste("Поручение не отправлено:", bad), type = "error", duration = 10)
      return(invisible(NULL))
    }
    acct <- exante_primary_account()
    if (is.null(acct)) {
      showNotification("Поручение не отправлено: счёт Exante недоступен.",
                        type = "error", duration = 10)
      return(invisible(NULL))
    }
    res <- exante_place_order(acct, sym, r$side, qty, apply = TRUE)
    ok <- isTRUE(res$ok)
    store_append_order(USER$login %||% "?", r$side, sym, qty,
                       if (ok) "отправлено" else "отказ",
                       if (ok) "" else paste(res$error, res$message))
    cat(sprintf("[TRADE] %s %s %s x%g -> %s\n", USER$login %||% "?", r$side,
                sym, qty, if (ok) "OK" else paste(res$error, res$status %||% "")))
    removeModal()
    if (ok) {
      showNotification(sprintf("Поручение отправлено: %s %s %g шт.",
                               if (r$side == "buy") "покупка" else "продажа",
                               sym, qty), type = "message", duration = 10)
      # Реестр перечитываем из Exante: состав счёта изменится, и держать на
      # экране вчерашнюю картину после собственной сделки нельзя.
      ledger_rv(TRUE); store_touch(Sys.time())
    } else {
      showNotification(paste("Брокер отклонил поручение:",
                             substr(res$message %||% res$error, 1, 200)),
                        type = "error", duration = 20)
    }
  })

  # Правая колонка: график инструмента или справочник наблюдения.
  # Справочник — вкладка того же виджета, а не отдельная карточка: он про те же
  # инструменты, что и график, и занимать ими два места на экране незачем.
  right_tab <- reactiveVal("chart")
  observeEvent(input$tab_chart, right_tab("chart"))
  observeEvent(input$tab_registry, right_tab("registry"))

  output$right_col <- renderUI({
    tabs <- tags$div(
      class = "seg sm",
      actionButton("tab_chart", "График",
                   class = if (identical(right_tab(), "chart")) "on" else NULL),
      actionButton("tab_registry", "Справочник",
                   class = if (identical(right_tab(), "registry")) "on" else NULL)
    )
    if (identical(right_tab(), "registry")) {
      wl <- watchlist_active()
      return(panel(
        "Справочник наблюдения",
        sub = sprintf("%d инструментов", nrow(wl)),
        tip = paste0(
          "Список инструментов, по которым ночное задание тянет ряды цен и ",
          "который предлагается в выборе графика. Живёт в хранилище, а не в ",
          "коде, и переживает выкатку. Перед добавлением тикер проверяется у ",
          "источника одним запросом: реестр с несуществующим инструментом ",
          "ронял бы ночную загрузку каждую ночь — она «всё или ничего». ",
          "Удаление убирает инструмент из наблюдения, но ряд цен сохраняется: ",
          "он нужен истории портфеля, если бумага когда-то покупалась. ",
          "Зелёная точка — бумага сейчас в портфеле."),
        right = tabs,
        body_class = "bd--flush",
        tags$div(class = "wl",
                 uiOutput("wl_add"),
                 tags$div(class = "wl-list", uiOutput("wl_table")),
                 uiOutput("wl_msg"))
      ))
    }
    panel(
      "График инструмента",
      tip = paste(
        "Дневные свечи за", BLNR_TIMELINE_DAYS, "торговых сессий —",
        "та же глубина, что у шкалы времени. Пунктир — траектория цены по",
        "модели от базы прогноза; горизонталь — цена входа, если бумага в",
        "портфеле; ромбы — сделки по ней; вертикаль — выбранная дата."
      ),
      right = tags$div(
        style = "display:flex;gap:8px;align-items:center",
        tags$div(style = "width:230px", uiOutput("sel_ticker_ui")),
        tabs
      ),
      body_class = "bd--plot",
      plotlyOutput("chart_instrument", height = "100%")
    )
  })

  # Выбор инструмента строится из ДЕЙСТВУЮЩЕГО реестра: он правится с экрана,
  # и список в разметке устарел бы сразу после первого добавления.
  output$sel_ticker_ui <- renderUI({
    wl <- watchlist_active()
    held <- portfolio_prices()$ticker
    sel <- isolate(input$sel_ticker)
    if (is.null(sel) || !(sel %in% wl$ticker)) {
      sel <- if (length(held) && held[1] %in% wl$ticker) held[1] else wl$ticker[1]
    }
    selectInput("sel_ticker", NULL, width = "100%",
                choices = stats::setNames(as.list(wl$ticker),
                                          paste0(wl$ticker, " \u00b7 ", wl$name_ru)),
                selected = sel)
  })

  # --- справочник ----------------------------------------------------------
  wl_bump <- reactiveVal(0L)
  wl_status <- reactiveVal(NULL)

  output$wl_add <- renderUI({
    tags$div(
      class = "wl-add",
      tags$div(style = "width:110px",
               textInput("wl_ticker", "Тикер", placeholder = "напр. QQQ")),
      tags$div(style = "flex:1 1 auto",
               textInput("wl_name", "Название", placeholder = "необязательно")),
      actionButton("wl_do_add", "Добавить", class = "btn-today")
    )
  })

  observeEvent(input$wl_do_add, {
    res <- tryCatch(watchlist_add(input$wl_ticker, input$wl_name),
                    error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    wl_status(res)
    if (isTRUE(res$ok)) {
      updateTextInput(session, "wl_ticker", value = "")
      updateTextInput(session, "wl_name", value = "")
      wl_bump(wl_bump() + 1L)
    }
  })

  observeEvent(input$wl_do_remove, {
    res <- tryCatch(watchlist_remove(input$wl_do_remove),
                    error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    wl_status(res)
    if (isTRUE(res$ok)) wl_bump(wl_bump() + 1L)
  })

  output$wl_msg <- renderUI({
    st <- wl_status()
    if (is.null(st)) return(NULL)
    tags$div(class = paste("wl-msg", if (isTRUE(st$ok)) "ok" else "bad"), st$message)
  })

  output$wl_table <- renderUI({
    wl_bump()
    wl <- watchlist_all()
    held <- unique(portfolio_prices()[quantity_at > 0, ticker])
    have <- store_tickers()
    rows <- lapply(seq_len(nrow(wl)), function(i) {
      tk <- wl$ticker[i]
      tags$tr(
        tags$td(if (tk %in% held) tags$span(class = "wl-held", title = "в портфеле"),
                tags$b(tk)),
        tags$td(wl$name_ru[i]),
        tags$td(class = "r",
                if (tk %in% have) tags$span(class = "mut", "ряд есть")
                else tags$span(class = "neg", "нет ряда")),
        tags$td(class = "r",
                if (tk %in% held) tags$span(class = "mut", title =
                     "Бумага в портфеле — из наблюдения не убрать", "\u2014")
                else tags$button(
                  class = "wl-del", title = paste("Убрать", tk, "из наблюдения"),
                  onclick = sprintf(
                    "Shiny.setInputValue('wl_do_remove','%s',{priority:'event'})", tk),
                  "\u00d7"))
      )
    })
    tags$table(
      tags$thead(tags$tr(tags$th("Тикер"), tags$th("Название"),
                         tags$th(class = "r", "Ряд цен"), tags$th(""))),
      tags$tbody(rows)
    )
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
        "Результат против модели",
        sub = "прибыль и убыток, $",
        tip = paste0(
          "По каждой сессии: сколько ФАКТИЧЕСКИ заработали или потеряли на ",
          "открытых позициях и сколько обещала модель за тот же срок ",
          "владения. Третья линия — разница. ",
          "Показано в деньгах сознательно: в процентах это была доходность ",
          "вложенного в бумаги, и одна акция IBM за $285 своим падением на ",
          "20% рисовала «портфель −20%», хотя на счёте лежало ещё $45 тысяч ",
          "наличными. Денежная часть счёта в этот расчёт не входит — она ни ",
          "растёт, ни падает. Прогноз приводится к дате последней покупки ",
          "каждой бумаги."),
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
    # В ДЕНЬГАХ, а не в процентах. Проценты здесь считались от вложенного в
    # бумаги, и при одной акции IBM за $285 её −20% рисовались как «портфель
    # −20%», хотя на счёте лежало ещё $45 тысяч наличными. В деньгах подменить
    # смысл нечем: −$58 остаются −$58.
    p <- plot_ly(d, x = ~date, y = ~fact_pnl, type = "scatter", mode = "lines",
                 name = "факт", line = list(color = BLNR_COLORS$ok, width = 2))
    p <- add_trace(p, y = ~model_pnl, name = "модель",
                   line = list(color = BLNR_COLORS$plan, width = 1.6,
                               dash = "dash"))
    p <- add_trace(p, y = ~dev_money, name = "разница",
                   line = list(color = BLNR_COLORS$series, width = 1.2))
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
      yaxis = list(title = "", tickprefix = "$", gridcolor = BLNR_COLORS$grid)
    )
  })
})
