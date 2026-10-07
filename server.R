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

  # Восстановление запомненного входа. Происходит ДО первой отрисовки, поэтому
  # человек не видит форму и не гадает, помнит его стенд или нет.
  # Права проверяются ЗАНОВО: кука удостоверяет личность, но не членство в
  # белом списке — человека могли убрать, пока она жила.
  local({
    tok <- session_cookie_value(session)
    who <- session_token_login(tok)
    if (!is.na(who) && nzchar(who)) {
      if (user_allowed(who)) {
        USER$login <- who
        cat(sprintf("[AUTH] RESTORE login=%s %s\n", who, format(Sys.time())))
        # Скользящий срок: активный пользователь пароль не набирает, а забытая
        # вкладка протухает сама.
        shinyjs::runjs(session_cookie_set_js(session_token_make(who)))
      } else {
        cat(sprintf("[AUTH] RESTORE ОТКАЗ login=%s вне белого списка %s\n",
                    who, format(Sys.time())))
        shinyjs::runjs(session_cookie_clear_js())
      }
    }
  })

  output$gate <- renderUI({
    if (is.null(USER$login)) loginUI() else dashboardUI()
  })

  observeEvent(input$auth_submit, {
    res <- tryCatch(auth_check(input$auth_login, input$auth_password),
                    error = function(e) list(ok = FALSE))
    if (isTRUE(res$ok)) {
      cat(sprintf("[AUTH] OK login=%s %s\n", res$login, format(Sys.time())))
      USER$login <- res$login
      if (blnr_session_enabled()) {
        shinyjs::runjs(session_cookie_set_js(session_token_make(res$login)))
      }
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
    # Куку гасим ДО перезагрузки: иначе восстановление на старте новой сессии
    # тут же вернёт человека внутрь, и «Выйти» перестанет работать.
    shinyjs::runjs(paste0(session_cookie_clear_js(),
                          "setTimeout(function(){location.reload();}, 60);"))
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
  # СЧЁТЧИК, а не флаг. reactiveVal, которому присвоили ТО ЖЕ значение, не
  # оповещает зависимых: после первой сделки флаг становился TRUE, а вторая
  # сделка писала TRUE поверх TRUE — и ни реестр, ни журнал не перечитывались.
  # 07.10.2026 так и вышло: из двух поручений подряд на экране появилось
  # только первое (Google), второе (Intel) было отправлено, записано в журнал
  # и принято брокером, но экран об этом молчал. Счётчик растёт всегда,
  # поэтому оповещение приходит на каждое событие.
  ledger_rv <- reactiveVal(0L)
  ledger_bump <- function() ledger_rv(isolate(ledger_rv()) + 1L)
  ledger_now <- reactive({
    store_touch()
    led <- portfolio_ledger(refresh = ledger_rv() > 0L)
    led
  })
  observeEvent(input$portfolio_refresh, {
    ledger_bump()
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

  forecast_id <- reactiveVal(NULL)   # какой файл хранилища сейчас в работе
  forecast_bump <- reactiveVal(0L)

  # Прогноз берётся из ХРАНИЛИЩА (BLNR_STORE_DIR/forecasts), а не из одного
  # файла по фиксированному пути: тот затирался каждым новым расчётом, жил вне
  # хранилища данных и не помнил ни базы, ни автора. Подробности — в
  # R/forecast_store.R. Первый запуск переносит прежний одиночный файл в
  # хранилище, чтобы стенд не остался без прогноза и старый путь перестал быть
  # источником правды сам собой.
  tryCatch(forecast_store_seed(), error = function(e) NULL)
  local({
    latest <- tryCatch(forecast_store_latest(), error = function(e) NULL)
    if (is.null(latest)) return(invisible(NULL))
    parsed <- forecast_store_read(latest$file)
    if (is.null(parsed) || nrow(parsed) == 0) return(invisible(NULL))
    forecast_data(parsed)
    forecast_id(latest$file)
    forecast_source(sprintf("база %s \u00b7 %s",
                            format(latest$base_date, "%d.%m.%Y"),
                            latest$orig_name))
  })

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
      tags$p(style = "font-size:12px;color:#5C5C5C",
             "Загруженный файл остаётся в хранилище стенда рядом с рядами ",
             "котировок и переживает выкатку. Прежние версии не затираются: ",
             "любую можно вернуть в работу кнопкой «Взять» или скачать."),
      uiOutput("forecast_versions"),
      fileInput("forecast_file", "Загрузить новый файл (.xlsx)", accept = ".xlsx",
                width = "100%"),
      # База — день, НА КОТОРЫЙ посчитана модель: от него шаги .qM0, .qM1, …
      # раскладываются по торговым дням. Раньше она была константой в коде, и
      # любой файл ложился на 11.09.2026 независимо от того, когда его
      # посчитали. Подставляется из имени файла, человеку остаётся проверить.
      uiOutput("forecast_base_ui"),
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

  output$forecast_base_ui <- renderUI({
    req(input$forecast_file)
    guess <- tryCatch(forecast_guess_base(input$forecast_file$datapath,
                                          input$forecast_file$name),
                      error = function(e) Sys.Date())
    dateInput("forecast_base", "База прогноза — день, на который посчитана модель",
              value = guess, format = "dd.mm.yyyy", language = "ru",
              width = "260px")
  })

  output$forecast_sheet_ui <- renderUI({
    req(input$forecast_file)
    sheets <- tryCatch(forecast_sheet_names(input$forecast_file$datapath),
                       error = function(e) character(0))
    if (length(sheets) <= 1) return(NULL)
    sel <- if ("Q_mean_var" %in% sheets) "Q_mean_var" else sheets[1]
    selectInput("forecast_sheet", "Лист", choices = sheets, selected = sel)
  })

  # Поданный файл сразу кладётся В ХРАНИЛИЩЕ, а не просто разбирается в
  # память: временный файл fileInput живёт до конца сессии Shiny, и прогноз
  # пропадал при перезапуске воркера — то есть при каждой выкатке, и сразу у
  # всех. Прежние версии при этом не затираются.
  observeEvent(input$forecast_file, {
    sheet <- input$forecast_sheet %||% forecast_default_sheet(input$forecast_file$datapath)
    res <- tryCatch(
      forecast_store_save(input$forecast_file$datapath,
                          orig_name = input$forecast_file$name,
                          sheet = sheet,
                          as_fraction = isTRUE(input$forecast_is_share),
                          base_date = input$forecast_base,
                          user = USER$login %||% ""),
      error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    if (!isTRUE(res$ok)) {
      showNotification(paste("Прогноз не сохранён:", res$message),
                        type = "error", duration = 12)
      return(invisible(NULL))
    }
    parsed <- forecast_store_read(res$id)
    if (is.null(parsed)) {
      showNotification("Файл сохранён, но не читается обратно.",
                        type = "error", duration = 12)
      return(invisible(NULL))
    }
    forecast_data(parsed)
    forecast_id(res$id)
    r <- forecast_registry()[file == res$id]
    forecast_source(sprintf("база %s \u00b7 %s",
                            format(r$base_date[1], "%d.%m.%Y"), r$orig_name[1]))
    forecast_bump(forecast_bump() + 1L)
    showNotification(res$message, type = "message", duration = 8)
  }, ignoreInit = TRUE)

  # Переключение на любую сохранённую версию: «что обещала позапрошлая»
  # перестаёт быть археологией.
  observeEvent(input$forecast_use, {
    id <- as.character(input$forecast_use)
    parsed <- forecast_store_read(id)
    if (is.null(parsed)) {
      showNotification("Эта версия прогноза не читается.", type = "error",
                        duration = 10)
      return(invisible(NULL))
    }
    r <- forecast_registry()[file == id]
    forecast_data(parsed)
    forecast_id(id)
    forecast_source(sprintf("база %s \u00b7 %s",
                            format(r$base_date[1], "%d.%m.%Y"), r$orig_name[1]))
    forecast_bump(forecast_bump() + 1L)
  })

  # Реестр версий читается по ИЗМЕНЕНИЮ ФАЙЛА, а не один раз за сессию:
  # прогноз кладёт хранилище, общее для всех сессий стенда, и версия,
  # загруженная соседом (или ночным заданием), иначе не появилась бы в списке
  # до перезахода. Проверка — один stat раз в три секунды.
  forecast_reg <- reactivePoll(
    3000, session,
    checkFunc = function() {
      f <- forecast_registry_path()
      if (file.exists(f)) as.numeric(file.mtime(f)) else 0
    },
    valueFunc = function() {
      tryCatch(forecast_registry(), error = function(e) forecast_registry_empty())
    })

  output$forecast_versions <- renderUI({
    forecast_bump()
    reg <- forecast_reg()
    if (nrow(reg) == 0) {
      return(tags$p(style = "font-size:12px;color:#8a5d00",
                    "В хранилище ещё нет ни одного прогноза. Первый же ",
                    "загруженный файл останется здесь и переживёт выкатку."))
    }
    cur <- forecast_id()
    tags$div(
      class = "fc-list",
      tags$div(class = "fc-head", sprintf("В хранилище %d %s", nrow(reg),
        if (nrow(reg) %% 10 == 1 && nrow(reg) %% 100 != 11) "версия" else "версии")),
      lapply(seq_len(nrow(reg)), function(i) {
        r <- reg[i]
        is_cur <- identical(r$file, cur)
        tags$div(
          class = paste("fc-row", if (is_cur) "on"),
          tags$b(format(r$base_date, "%d.%m.%Y")),
          tags$span(class = "nm", title = r$orig_name, r$orig_name),
          tags$span(class = "mut", sprintf("%d бум. \u00b7 до %s", r$tickers,
                                           format(r$horizon, "%m.%Y"))),
          tags$span(class = "mut", sprintf("%s%s",
                    format(r$saved_at, "%d.%m.%Y"),
                    if (nzchar(r$user %||% "")) paste0(" \u00b7 ", r$user) else "")),
          if (is_cur) tags$span(class = "fc-cur", "в работе")
          else tags$button(class = "fc-use", onclick = sprintf(
            "Shiny.setInputValue('forecast_use','%s',{priority:'event'})", r$file),
            "Взять"))
      })
    )
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
    # На экране ТРИ разных факта, и подменять их друг другом нельзя:
    #   * дата сессии   — за какой торговый день цены;
    #   * срез          — когда эти цены легли в хранилище;
    #   * попытка       — когда последний раз ходили к источнику.
    # Без последнего «ряды на 29.09» неотличимо от «загрузка сломалась»: ровно
    # этот вопрос и возник 01.10.2026, когда источник ещё не опубликовал
    # сессию 30.09. Пояснения — под курсором, на экране только числа.
    written <- st$updated_at
    tried   <- st$checked_at %||% st$updated_at
    same <- !is.null(written) && !is.null(tried) &&
            abs(as.numeric(difftime(tried, written, units = "secs"))) < 60
    stamp <- function(label, v) {
      if (is.null(v)) return(NULL)
      tags$span(class = "q", "\u00b7 ", label, " ", format(v, "%d.%m %H:%M"))
    }
    marks <- if (same) list(stamp("срез и попытка", written))
             else list(stamp("срез", written), stamp("попытка", tried))

    tip <- paste0(
      "Три разных числа, и путать их нельзя. ДАТА — за какой торговый день ",
      "цены: это закрытие сессии. СРЕЗ — когда эти цены легли в хранилище ",
      "стенда. ПОПЫТКА — когда стенд последний раз ходил к источнику; если ",
      "она свежее среза, значит источник спрашивали, а нового у него не было. ",
      "Ряды тянет задание Jenkins «311.blnr - marketdata» в 07:00, 09:00 и ",
      "11:00 МСК; сам стенд в marketdata.app не ходит — у аккаунта лимит 100 ",
      "запросов в сутки, и ползунок времени выжег бы его с первого ",
      "пользователя. ПОЧЕМУ УТРОМ ВИСИТ ПОЗАВЧЕРАШНЯЯ СЕССИЯ: дневную свечу ",
      "источник публикует не в момент закрытия биржи, а примерно через 8–9 ",
      "часов — сессия, закрывшаяся в 23:00 МСК, появляется у него только под ",
      "утро. Кнопка «Обновить» перетягивает состав счёта у брокера, но НЕ ",
      "котировки: их из браузера не вытянуть. ",
      if (!is.null(st$source_last_date))
        paste0("На последней попытке у источника была сессия ",
               format(st$source_last_date, "%d.%m.%Y"), ". ") else "",
      "Инструментов в хранилище: ", st$instruments, ".")

    if (stale > 5L && identical(sel_date(), fact_date())) {
      return(tags$span(class = "bdg warn", title = tip,
                       "Ряды устарели: ", tags$b(format(last, "%d.%m")),
                       sprintf(" (%d дн. назад)", stale), marks))
    }
    tags$span(class = "bdg", title = tip, "Котировки ",
              tags$b(format(last, "%d.%m")), marks)
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
      tip = tip_block(
        "Каждая покупка — отдельной строкой, по своему решению.",
        list("Результат", "рост позиции от цены входа до цены выбранной сессии"),
        list("Ожидалось", "НАКОПЛЕННЫЙ прогноз модели за тот же срок — от даты ЭТОЙ покупки до выбранной даты, не мгновенный и не от базы прогноза"),
        list("Лучше модели", "Результат минус Ожидалось, в процентных пунктах"),
        list("Вес", "доля во всём счёте, включая наличные; наличные — отдельной строкой"),
        list("Докупка не усредняет", "две покупки AMD — две строки с разными датами, ценой и сроком"),
        list("Куплено", "дата покупки и срок владения на выбранную дату; от неё и считаются результат и прогноз"),
        list("Полоса слева", sprintf("порог фиксации: ±%g%% годовых (зелёная — прибыль, красная — убыток); тот же сигнал в журнале решений", BLNR_SIGNAL_ANNUAL_PCT)),
        note = "Нулевой срок владения → «Ожидалось» прочерк: за ноль дней модель ничего не обещала. Клик по строке открывает её график."),
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
        # Подпись зависит от вида, поэтому она ОТДЕЛЬНЫЙ вывод: столбики
        # меряют от моей цены входа, линии — от базы прогноза, и молчать о
        # такой разнице нельзя (см. vs_time_plot).
        sub = textOutput("vs_sub", inline = TRUE),
        tip = tip_block(
          "Два вида с РАЗНОЙ точкой отсчёта — смотрите подпись карточки.",
          list("На дату", "по каждой бумаге за срок владения: фактический рост от цены входа рядом с прогнозом за тот же срок; расхождение столбиков — повод для решения"),
          list("Динамика", "ошибка модели по сессиям от базы прогноза; к покупкам отношения не имеет, живёт даже если бумага докуплена вчера"),
          list("Одна бумага", "две кривые — факт и модель; закрашенный зазор и есть ошибка"),
          list("Несколько", "по линии разницы на бумагу, в пп — так они сравнимы на общей шкале"),
          list("Чипы", "выключают бумагу с графика; пунктирный чип — бумаги нет в модели"),
          note = "Зачем «Динамика»: бумага, месяц шедшая по модели и обвалившаяся вчера, и бумага, разошедшаяся с первого дня, дают одинаковый столбик — а решения по ним разные."),
        right = uiOutput("vs_tabs"),
        body_class = "bd--flush",
        tags$div(class = "vs",
                 uiOutput("vs_chips"),
                 tags$div(class = "vs-plot",
                          plotlyOutput("chart_vs_forecast", height = "100%")))
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

    # ОДНА СТРОКА — ОДНО РЕШЕНИЕ. Таблица больше не усредняет покупки: AMD,
    # купленный в сентябре, и AMD, купленный вчера, — это два разных решения с
    # разной ценой входа, разным сроком и разным результатом. Пока они стояли
    # одной строкой со средней ценой, не было видно ни одного из них.
    #
    # Бумага показывается один раз на группу, повторы приглушены: это та же
    # бумага, а не новая позиция.
    open_lots <- tryCatch(lots_open(lots_now()), error = function(e) lots_empty())
    fd <- forecast_data()
    total_val <- suppressWarnings(portfolio_summary()$total_value)

    if (nrow(open_lots) == 0) {
      rows <- list()
    } else {
      data.table::setorder(open_lots, ticker, open_date)
      held_days <- lots_held_days(open_lots, as_of = sel_date())
      rows <- lapply(seq_len(nrow(open_lots)), function(i) {
        L <- open_lots[i]
        mrow <- m[ticker == L$ticker]
        px <- if (nrow(mrow)) mrow$current_price[1] else NA_real_
        day_pct <- if (nrow(mrow)) mrow$day_change_pct[1] else NA_real_
        val <- if (is.finite(px)) px * L$qty else NA_real_
        gr <- if (is.finite(px) && is.finite(L$open_price) && L$open_price > 0)
                (px / L$open_price - 1) * 100 else NA_real_
        wt <- if (is.finite(val) && is.finite(total_val) && total_val > 0)
                val / total_val * 100 else NA_real_
        # Модель сравнивается ОТ ДАТЫ ЭТОЙ ПОКУПКИ: у каждого решения свой
        # период владения, и мерить их общим отрезком значит приписывать
        # вчерашней покупке месячный прогноз.
        fpct <- if (!is.null(fd) && nrow(fd) > 0)
                  forecast_between(fd, L$ticker, L$open_date, sel_date()) else NA_real_
        gap <- if (!is.finite(fpct) && !is.null(fd) && nrow(fd) > 0)
                 forecast_gap_reason(fd, L$ticker, L$open_date, sel_date()) else NULL
        first_of_group <- i == 1L || open_lots$ticker[i - 1L] != L$ticker
        # ИМЯ РЕШЕНИЯ, а не бумаги. Две покупки AMD — это две разные строки, и
        # называться одинаково они не могут: иначе по имени не сказать, о
        # какой из них речь («что там с AMD?» перестаёт быть вопросом с
        # ответом). Дата покупки и есть естественное имя решения. У бумаги с
        # единственной покупкой суффикс не нужен — он только шумит.
        multi <- sum(open_lots$ticker == L$ticker) > 1L
        # \u0421\u0438\u0433\u043d\u0430\u043b \u0444\u0438\u043a\u0441\u0430\u0446\u0438\u0438: \u043f\u043e \u0413\u041e\u0414\u041e\u0412\u041e\u0419 \u0434\u043e\u0445\u043e\u0434\u043d\u043e\u0441\u0442\u0438, \u043f\u0440\u0438\u0432\u0435\u0434\u0451\u043d\u043d\u043e\u0439 \u043e\u0442 \u0440\u0435\u0437\u0443\u043b\u044c\u0442\u0430\u0442\u0430 \u0438
        # \u0441\u0440\u043e\u043a\u0430 \u0434\u0435\u0440\u0436\u0430\u043d\u0438\u044f \u044d\u0442\u043e\u0439 \u041a\u041e\u041d\u041a\u0420\u0415\u0422\u041d\u041e\u0419 \u043f\u043e\u043a\u0443\u043f\u043a\u0438 (\u043d\u0435 \u0432\u0441\u0435\u0439 \u043f\u043e\u0437\u0438\u0446\u0438\u0438 \u043f\u043e \u0431\u0443\u043c\u0430\u0433\u0435,
        # \u043a\u043e\u0442\u043e\u0440\u0430\u044f \u0443\u0441\u0440\u0435\u0434\u043d\u0438\u043b\u0430 \u0431\u044b \u0440\u0430\u0437\u043d\u044b\u0435 \u0440\u0435\u0448\u0435\u043d\u0438\u044f \u043e\u0431\u0440\u0430\u0442\u043d\u043e). \u0421\u043c. R/signals.R \u2014
        # \u0442\u0430\u043c \u0436\u0435 \u043e\u0431\u0430 \u043f\u0440\u0435\u0434\u043e\u0445\u0440\u0430\u043d\u0438\u0442\u0435\u043b\u044f (\u043c\u0438\u043d\u0438\u043c\u0430\u043b\u044c\u043d\u044b\u0439 \u0441\u0440\u043e\u043a, \u043c\u0438\u043d\u0438\u043c\u0430\u043b\u044c\u043d\u044b\u0439 \u0430\u0431\u0441\u043e\u043b\u044e\u0442),
        # \u0431\u0435\u0437 \u043a\u043e\u0442\u043e\u0440\u044b\u0445 \u043e\u0434\u0438\u043d \u0434\u0435\u043d\u044c +0.5% \u0434\u0430\u043b \u0431\u044b \u00ab+180% \u0433\u043e\u0434\u043e\u0432\u044b\u0445\u00bb \u0438 \u043b\u043e\u0436\u043d\u0443\u044e \u0442\u0440\u0435\u0432\u043e\u0433\u0443.
        sig <- fixation_signal(gr, held_days[i])
        tags$tr(
          class = paste(if (identical(L$ticker, sel)) "sel",
                        if (!first_of_group) "lot-more",
                        switch(sig, take_profit = "sig-tp", cut_loss = "sig-cl", "")),
          title = if (nzchar(sig)) signal_text(sig, gr, held_days[i]) else NULL,
          onclick = sprintf("Shiny.setInputValue('pick_ticker','%s',{priority:'event'})", L$ticker),
          tags$td(class = "nm", L$ticker,
                  if (multi) tags$span(class = "lot-tag",
                                       format(L$open_date, "\u00b7 %d.%m")),
                  if (nzchar(sig)) tags$span(
                    class = paste("sig-dot", if (sig == "take_profit") "tp" else "cl"),
                    title = signal_text(sig, gr, held_days[i]),
                    if (sig == "take_profit") "\u25b2" else "\u25bc")),
          tags$td(formatC(L$qty, format = "d")),
          plain(L$open_price), plain(px),
          num(day_pct, fmt_pct),
          plain(L$cost, 0), plain(val, 0),
          tags$td(style = sprintf(
            "background:linear-gradient(to left,#ffeccc %1$.1f%%,transparent %1$.1f%%)",
            max(0, min(100, if (is.finite(wt)) wt else 0))),
            if (is.finite(wt)) sprintf("%.1f %%", wt) else "\u2014"),
          num(gr, fmt_pct),
          if (has_fc) {
            if (is.finite(fpct)) num(fpct, fmt_pct)
            else tags$td(class = "mut", style = "font-size:10.5px", gap %||% "\u2014")
          },
          if (has_fc) num(if (is.finite(fpct) && is.finite(gr)) gr - fpct else NA_real_, fmt_pp),
          tags$td(class = "mut", sprintf(
            "%s \u00b7 %s", format(L$open_date, "%d.%m.%y"),
            if (held_days[i] == 0L) "в этот день" else sprintf("%d дн", held_days[i]))),
          # Продать можно только количество, а НЕ конкретный пакет: у брокера
          # лотов нет. Кнопка продаёт столько же штук, сколько в этом решении,
          # и закрывает при этом самые старые покупки (FIFO) — об этом прямо
          # сказано под курсором, чтобы кнопка не обещала больше, чем делает.
          tags$td(class = "r",
            if (can_trade)
              tags$button(class = "tr-btn sell",
                title = sprintf(paste0("Продать %g шт. %s. У брокера пакетов нет: ",
                                       "уйдёт поручение на это количество, а в учёте ",
                                       "закроются самые старые покупки."),
                                L$qty, L$ticker),
                onclick = sprintf(
                  "event.stopPropagation();Shiny.setInputValue('trade_open','sell|%s',{priority:'event'})",
                  L$ticker),
                "\u2212")
            else tags$span(class = "mut", "\u2014"))
        )
      })
    }

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

  # Кнопки в шапке карточки позиций и в журнале поручений — оба места ведут
  # к ОДНОМУ и тому же окну подтверждения, ничего не отправляют сами.
  observeEvent(input$buy_open, {
    req(user_can_trade(USER$login))
    shinyjs::runjs(sprintf(
      "Shiny.setInputValue('trade_open','buy|%s',{priority:'event'})",
      input$sel_ticker %||% ""))
  })

  observeEvent(input$buy_open2, {
    req(user_can_trade(USER$login))
    # Из журнала поручений бумага графиком не выбрана — список открывается
    # на первом инструменте наблюдения, человек выбирает сам.
    shinyjs::runjs(
      "Shiny.setInputValue('trade_open','buy|',{priority:'event'})")
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
      # В списке помечаем, ЧТО УЖЕ В ПОРТФЕЛЕ. Без пометки две похожие бумаги
      # (Alphabet класса A и C) различались только суффиксом тикера, и выбрать
      # не ту было проще, чем ту: 07.10.2026 так ушло поручение на GOOG при
      # позиции в GOOGL.
      if (identical(side, "buy"))
        local({
          held <- unique(m[quantity_at > 0, ticker])
          lbl <- paste0(wl$ticker, " \u00b7 ", wl$name_ru,
                        ifelse(wl$ticker %in% held, "  \u2022 в портфеле", ""))
          selectInput("trade_ticker", "Бумага", width = "100%",
                      choices = stats::setNames(as.list(wl$ticker), lbl),
                      selected = if (!is.na(tk) && tk %in% wl$ticker) tk else wl$ticker[1])
        }),
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

  # ТЕЛО ПОРУЧЕНИЯ — один реактив, который читают И предпросмотр, И отправка.
  #
  # Пока это были два отдельных выражения, они разошлись: окно показывало
  # GOOGL (выбор из списка), а на счёт ушёл AMD (тикер, с которым окно
  # открыли). 06.10.2026 так куплено 3 акции AMD по $649.03 вместо Google.
  # Увидеть дефект по экрану было нельзя — экран показывал верное.
  trade_order <- reactive({
    r <- trade_req()
    if (is.null(r)) return(NULL)
    build_trade_order(
      side          = r$side,
      dialog_ticker = input$trade_ticker %||% NA_character_,
      opened_ticker = r$ticker %||% NA_character_,
      qty           = input$trade_qty,
      max_qty       = r$max_qty %||% 0,
      symbol_of     = function(tk) exante_symbol_for_ticker(tk, ledger = ledger_now())
    )
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

    o <- trade_order()
    req(!is.null(o))
    tk <- o$ticker; qty <- o$qty; sym <- o$symbol
    if (!isTRUE(o$ok)) {
      return(tags$div(class = "wl-msg bad", o$error))
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

  # Отмена поручения. Как и отправка — через окно подтверждения: отмена
  # необратима в том смысле, что заново поручение придётся подавать руками, и
  # цена к тому моменту будет другой.
  observeEvent(input$order_cancel, {
    req(user_can_trade(USER$login))
    oid <- as.character(input$order_cancel)
    o <- tryCatch(orders_now(), error = function(e) orders_empty())
    r <- o[order_id == oid]
    req(nrow(r) > 0)
    if (!orders_cancellable(r[1])) {
      showNotification("Это поручение уже нельзя отменить: оно исполнено или снято.",
                        type = "warning", duration = 10)
      return(invisible(NULL))
    }
    cancel_req(oid)
    showModal(modalDialog(
      title = "Отменить поручение",
      size = "s", easyClose = TRUE,
      footer = tagList(modalButton("Оставить"),
                       actionButton("cancel_confirm", "Отменить поручение",
                                    class = "btn-trade")),
      tags$p(style = "font-size:12px;color:#646b78",
             "Снимается поручение, которое ещё не исполнилось. Подать его ",
             "заново можно будет только руками, и цена к тому моменту будет ",
             "другой."),
      tags$div(class = "wl-msg ok",
               tags$b(r$symbol[1]), " \u00b7 ",
               if (identical(r$side[1], "buy")) "покупка" else "продажа",
               " ", r$quantity[1], " шт. от ", format(r$at[1], "%d.%m %H:%M"))
    ))
  })

  cancel_req <- reactiveVal(NULL)

  observeEvent(input$cancel_confirm, {
    if (!user_can_trade(USER$login)) {
      store_append_order(USER$login %||% "?", "cancel", "?", 0, "нет права",
                         "пользователь не в списке BLNR_TRADERS")
      removeModal(); return(invisible(NULL))
    }
    oid <- cancel_req(); req(!is.null(oid))
    res <- exante_cancel_order(oid, apply = TRUE)
    removeModal()
    if (isTRUE(res$ok)) {
      orders_mark_cancelled(oid, note = paste("отменено", USER$login %||% "?"))
      cat(sprintf("[TRADE] %s cancel %s -> OK\n", USER$login %||% "?", oid))
      showNotification("Поручение отменено.", type = "message", duration = 8)
      ledger_bump()
    } else {
      cat(sprintf("[TRADE] %s cancel %s -> %s\n", USER$login %||% "?", oid,
                  paste(res$error, res$status %||% "")))
      showNotification(paste("Брокер не отменил поручение:",
                             substr(res$message %||% res$error, 1, 200)),
                        type = "error", duration = 20)
    }
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
    # ТО ЖЕ САМОЕ тело, что показано в окне. Пересобирать его здесь заново
    # нельзя: именно так предпросмотр и отправка разошлись 06.10.2026.
    o <- trade_order(); req(!is.null(o))
    qty <- o$qty
    sym <- o$symbol
    if (!isTRUE(o$ok)) {
      showNotification(paste("Поручение не отправлено:", o$error),
                        type = "error", duration = 10)
      return(invisible(NULL))
    }
    acct <- exante_primary_account()
    if (is.null(acct)) {
      showNotification("Поручение не отправлено: счёт Exante недоступен.",
                        type = "error", duration = 10)
      return(invisible(NULL))
    }
    res <- exante_place_order(acct, sym, o$side, qty, apply = TRUE)
    ok <- isTRUE(res$ok)
    oid <- if (ok) orders_extract_id(res$response) else NA_character_
    store_append_order(USER$login %||% "?", o$side, sym, qty,
                       if (ok) "отправлено" else "отказ",
                       if (ok) "" else paste(res$error, res$message),
                       order_id = oid)
    cat(sprintf("[TRADE] %s %s %s x%g -> %s\n", USER$login %||% "?", o$side,
                sym, qty, if (ok) "OK" else paste(res$error, res$status %||% "")))
    removeModal()
    if (ok) {
      showNotification(sprintf("Поручение отправлено: %s %s %g шт.",
                               if (o$side == "buy") "покупка" else "продажа",
                               sym, qty), type = "message", duration = 10)
      # Реестр перечитываем из Exante: состав счёта изменится, и держать на
      # экране вчерашнюю картину после собственной сделки нельзя.
      ledger_bump(); store_touch(Sys.time())
    } else {
      showNotification(paste("Брокер отклонил поручение:",
                             substr(res$message %||% res$error, 1, 200)),
                        type = "error", duration = 20)
    }
  })

  # Правая колонка: график инструмента или справочник наблюдения.
  # Справочник — вкладка того же виджета, а не отдельная карточка: он про те же
  # инструменты, что и график, и занимать ими два места на экране незачем.
  # Разделы правой колонки — на ОДНОМ уровне: «Решения» и «Поручения» это не
  # вкладки внутри «Журнала», а самостоятельные виджеты рядом с графиком и
  # справочником. Решения отвечают «удачной ли была покупка», поручения —
  # «что именно стенд отправил брокеру и что тот ответил»; это разные вопросы,
  # и прятать один внутрь другого незачем.
  right_tab <- reactiveVal("chart")
  observeEvent(input$tab_chart, right_tab("chart"))
  observeEvent(input$tab_registry, right_tab("registry"))
  observeEvent(input$tab_deals, right_tab("deals"))
  observeEvent(input$tab_orders, right_tab("orders"))

  output$right_col <- renderUI({
    tabs <- tags$div(
      class = "seg sm",
      actionButton("tab_chart", "График",
                   class = if (identical(right_tab(), "chart")) "on" else NULL),
      actionButton("tab_registry", "Справочник",
                   class = if (identical(right_tab(), "registry")) "on" else NULL),
      actionButton("tab_deals", "Решения",
                   class = if (identical(right_tab(), "deals")) "on" else NULL),
      actionButton("tab_orders", "Поручения",
                   class = if (identical(right_tab(), "orders")) "on" else NULL)
    )
    if (identical(right_tab(), "deals")) {
      return(panel(
        "Журнал решений",
        sub = textOutput("ord_sub", inline = TRUE),
        tip = tip_block(
          "Каждая покупка — отдельной строкой от входа до выхода.",
          list("Что в строке", "дата и цена покупки, дата и цена продажи, вложено, выручено, результат в деньгах и процентах, срок держания"),
          list("Пока не продано", "клетки продажи пусты, результат считается по текущей цене"),
          list("Докупка не усредняет", "два решения по одной бумаге — две строки, по каждой видно, удачным оно было"),
          list("FIFO", "продажа закрывает самые старые покупки: «продал три» = «закрылись три самые старые»"),
          list("Пометка «стенд»", "решение принято здесь; остальные сделаны в приложении брокера"),
          list("Полоса слева", sprintf("порог фиксации по годовой доходности: ±%g%%", BLNR_SIGNAL_ANNUAL_PCT)),
          note = "Поручения стенда и ответы брокера — в соседней вкладке «Поручения»."),
        right = tabs,
        body_class = "bd--flush",
        tags$div(class = "wl", tags$div(class = "wl-list", uiOutput("ord_table")))
      ))
    }

    if (identical(right_tab(), "orders")) {
      can_trade <- user_can_trade(USER$login) && exante_has_credentials() &&
                   identical(sel_date(), fact_date())
      return(panel(
        "Журнал поручений",
        sub = textOutput("ord_orders_sub", inline = TRUE),
        tip = tip_block(
          "Что стенд отправил брокеру и что тот ответил.",
          list("Зачем отдельно от решений", "отвечает не «удачной ли была покупка», а «то ли купилось, что я нажал»"),
          list("Исполнение", "по каждому поручению видно, сколько и почём реально прошло у брокера"),
          list("Отмена", "крестик у неисполненного поручения снимает его; исполненную сделку отменить нельзя"),
          note = "06.10.2026 окно показывало GOOGL, а на счёт ушёл AMD — заметить это можно только здесь. Сделки из приложения брокера сюда не попадают."),
        right = tabs,
        body_class = "bd--flush",
        tags$div(class = "wl",
          if (can_trade) tags$div(class = "ord-add",
            actionButton("buy_open2", "+ Новое поручение", class = "btn-buy")),
          tags$div(class = "wl-list", uiOutput("ord_orders_table")))
      ))
    }

    if (identical(right_tab(), "registry")) {
      return(panel(
        "Наблюдение и справочник",
        # Счётчик — ОТДЕЛЬНЫЙ реактивный вывод, а не строка в шапке карточки.
        # Шапка рисуется один раз на открытие вкладки, и статичный текст
        # продолжал утверждать «24 из 24» над таблицей, где бумага уже
        # помечена выключенной: виджет говорил одно, числа под ним другое.
        sub = textOutput("wl_count", inline = TRUE),
        tip = paste0(
          "Верхнее поле — поиск по ГЛОБАЛЬНОМУ справочнику: все бумаги, ",
          "доступные счёту (биржевые списки Exante, обновляются отдельным ",
          "заданием). Оттуда бумага добавляется в наблюдение — это способ ",
          "взять инструмент, которого у стенда до сих пор не было. ",
          "Ниже — сам список наблюдения: по нему ночное задание тянет ряды ",
          "цен, из него строится таблица портфеля и выбор графика. ",
          "«Под наблюдением / Выключен» — обратимый переключатель: ",
          "выключенная бумага не грузится и не показывается, но остаётся в ",
          "реестре вместе со своей строкой прогнозной модели и историей ",
          "сделок. Исходный состав из рабочей таблицы поэтому можно только ",
          "выключить; удалить насовсем — только то, что добавлено вручную ",
          "(помечено «вручную»). Ряд цен при любом из действий сохраняется: ",
          "он нужен истории портфеля, если бумага когда-то покупалась. ",
          "Зелёная точка — бумага сейчас в портфеле, её не выключить."),
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
      tip = tip_block(
        sprintf("Дневные свечи за %d торговых сессий.", BLNR_TIMELINE_DAYS),
        list("Треугольник вверх", "покупка этой бумаги"),
        list("Треугольник вниз", "продажа"),
        list("Пунктирная линия", "траектория цены по модели от базы прогноза"),
        list("Горизонталь", "цена входа, если бумага в портфеле"),
        list("Вертикаль", "выбранная дата"),
        note = "Наведите на маркер сделки — количество, цена и сумма."),
      right = tabs,
      body_class = "bd--plot",
      # Селектор инструмента ПОВЕРХ графика, а не в шапке: в шапке он был узкой
      # коробкой «AMD · AMD», по которой не прочитать ни цену, ни результат.
      # Здесь он широкий и информативный — тикер, название, количество, цена,
      # результат — и стоит над самим графиком, к которому относится.
      tags$div(class = "chart-wrap",
        tags$div(class = "chart-sel", uiOutput("sel_ticker_ui")),
        plotlyOutput("chart_instrument", height = "100%"))
    )
  })

  # Выбор инструмента строится из ДЕЙСТВУЮЩЕГО реестра: он правится с экрана,
  # и список в разметке устарел бы сразу после первого добавления.
  output$sel_ticker_ui <- renderUI({
    wl <- watchlist_active()
    m <- portfolio_metrics()
    held <- portfolio_prices()$ticker
    sel <- isolate(input$sel_ticker)
    if (is.null(sel) || !(sel %in% wl$ticker)) {
      sel <- if (length(held) && held[1] %in% wl$ticker) held[1] else wl$ticker[1]
    }
    # Подпись несёт то, ради чего на график и смотрят: цену, количество,
    # результат. Для бумаги вне портфеля — только цена. selectInput рисует
    # подписи простым текстом, поэтому проценты и цена идут текстом же.
    lbl <- vapply(wl$ticker, function(tk) {
      r <- m[ticker == tk]
      px <- if (nrow(r)) r$current_price[1] else md_last_price(tk)
      part <- paste0(tk, " \u00b7 ", wl$name_ru[wl$ticker == tk][1])
      if (is.finite(px)) part <- paste0(part, "  \u00b7  ", fmt_money(px, 2))
      if (nrow(r) && r$quantity_at[1] > 0) {
        part <- paste0(part, "  \u00b7  ", formatC(r$quantity_at[1], format = "d"),
                       " шт  \u00b7  ", fmt_pct(r$growth_pct[1]))
      }
      part
    }, character(1))
    selectInput("sel_ticker", NULL, width = "100%",
                choices = stats::setNames(as.list(wl$ticker), lbl),
                selected = sel)
  })

  # --- журнал поручений -----------------------------------------------------
  # Сверка с брокером идёт ЛЕНИВО, при открытии вкладки: поручений единицы в
  # день, а фоновый таймер в финансовом стенде — это запросы, о которых никто
  # не помнит.
  orders_now <- reactive({
    right_tab()
    ledger_rv()
    tryCatch(orders_reconcile(), error = function(e) {
      tryCatch(orders_read(), error = function(e2) orders_empty())
    })
  })

  # СДЕЛКИ — то, ради чего журнал и нужен: каждая покупка от входа до выхода.
  # Берутся из реестра операций, то есть включают и сделки, сделанные не со
  # стенда (в приложении брокера). Поручения стенда подмешиваются отдельной
  # пометкой: по ним видно, что решение принято здесь.
  lots_now <- reactive({
    led <- ledger_now()
    ledger_lots(ledger_events(led), as_of = sel_date(),
                price_of = function(tk) md_last_price(tk))
  })

  output$ord_sub <- renderText({
    l <- lots_now()
    if (nrow(l) == 0) return("сделок нет")
    sprintf("%d сделок \u00b7 открыто %d", nrow(l), sum(l$open))
  })

  output$ord_orders_sub <- renderText({
    o <- tryCatch(orders_now(), error = function(e) orders_empty())
    if (nrow(o) == 0) return("поручений нет")
    np <- sum(vapply(seq_len(nrow(o)), function(i) orders_cancellable(o[i]), logical(1)))
    sprintf("%d со стенда%s", nrow(o),
            if (np > 0) sprintf(" \u00b7 %d в очереди", np) else "")
  })

  # Журнал решений (сделки).
  output$ord_table <- renderUI({
    l <- lots_now()
    if (nrow(l) == 0) {
      return(empty_state("Сделок по счёту ещё нет."))
    }
    placed <- tryCatch(orders_now(), error = function(e) orders_empty())
    from_stand <- if (nrow(placed) > 0) placed$order_id[!is.na(placed$order_id)] else character()

    held <- lots_held_days(l, as_of = sel_date())
    money <- function(x, d = 0) if (is.finite(x)) fmt_money(x, d) else "\u2014"
    rows <- lapply(seq_len(nrow(l)), function(i) {
      r <- l[i]
      tone <- if (!is.finite(r$pnl)) "mut" else if (r$pnl >= 0) "pos" else "neg"
      # Сигнал — только у ОТКРЫТЫХ сделок: закрытую уже зафиксировали, порог
      # ей предлагать нечего. См. R/signals.R.
      sig <- if (r$open) fixation_signal(r$pnl_pct, held[i]) else ""
      tags$tr(
        class = paste(if (!r$open) "wl-row--off",
                      switch(sig, take_profit = "sig-tp", cut_loss = "sig-cl", "")),
        title = if (nzchar(sig)) signal_text(sig, r$pnl_pct, held[i]) else NULL,
        tags$td(tags$b(r$ticker),
                if (r$order_id %in% from_stand)
                  tags$span(class = "wl-src", title = "решение принято на стенде", "стенд"),
                if (nzchar(sig)) tags$span(
                  class = paste("sig-dot", if (sig == "take_profit") "tp" else "cl"),
                  title = signal_text(sig, r$pnl_pct, held[i]),
                  if (sig == "take_profit") "▲" else "▼")),
        tags$td(class = "r", formatC(r$qty, format = "d")),
        tags$td(class = "mut", format(r$open_date, "%d.%m.%y")),
        tags$td(class = "r", money(r$open_price, 2)),
        tags$td(class = "r", money(r$cost)),
        tags$td(class = "mut",
                if (is.na(r$close_date)) tags$span(class = "faint", "\u2014")
                else format(r$close_date, "%d.%m.%y")),
        tags$td(class = "r", money(r$close_price, 2)),
        tags$td(class = "r", if (r$open) tags$span(class = "faint", "\u2014")
                             else money(r$proceeds)),
        tags$td(class = "r", if (r$open) money(r$proceeds)
                             else tags$span(class = "faint", "\u2014")),
        tags$td(class = paste("r", tone), fmt_signed_money(r$pnl)),
        tags$td(class = paste("r", tone), fmt_pct(r$pnl_pct)),
        tags$td(class = "mut",
                if (r$open) sprintf("держу %d дн", held[i])
                else sprintf("%d дн", held[i]))
      )
    })
    tags$table(
      class = "lots",
      tags$thead(
        tags$tr(
          tags$th(""), tags$th(class = "r", ""),
          tags$th(colspan = "3", class = "grp", "Покупка"),
          tags$th(colspan = "3", class = "grp", "Продажа"),
          tags$th(class = "grp", "Сейчас"),
          tags$th(colspan = "2", class = "grp", "Результат"),
          tags$th("")),
        tags$tr(
          tags$th("Бумага"), tags$th(class = "r", "Кол-во"),
          tags$th("дата"), tags$th(class = "r", "цена"), tags$th(class = "r", "сумма"),
          tags$th("дата"), tags$th(class = "r", "цена"), tags$th(class = "r", "сумма"),
          tags$th(class = "r", "стоит"),
          tags$th(class = "r", "$"), tags$th(class = "r", "%"),
          tags$th("Срок"))),
      tags$tbody(rows)
    )
  })

  # Журнал поручений: что ушло со стенда и что ответил брокер.
  output$ord_orders_table <- renderUI({
    o <- tryCatch(orders_now(), error = function(e) orders_empty())
    if (nrow(o) == 0) {
      return(empty_state(
        "Со стенда поручений ещё не отправляли. Сделки, сделанные в ",
        "приложении брокера, сюда не попадают — они во вкладке «Решения»."))
    }
    money <- function(x, d = 0) if (is.finite(x)) fmt_money(x, d) else "\u2014"
    rows <- lapply(seq_len(nrow(o)), function(i) {
      r <- o[i]
      filled <- is.finite(r$filled_qty) && r$filled_qty > 0
      mismatch <- filled && is.finite(r$quantity) && r$filled_qty != r$quantity
      # ЦЕНА и СУММА размещённого ордера. Поручение рыночное, своей цены у него
      # нет: пока не исполнено — показываем ОЦЕНКУ по последней известной цене
      # (помечена «≈»), после исполнения — фактическую цену и сумму сделки.
      est_px <- md_last_price(exante_symbol_to_ticker(r$symbol))
      if (filled) {
        px <- r$fill_price; amt <- r$fill_price * r$filled_qty; approx <- FALSE
      } else {
        px <- est_px; amt <- if (is.finite(est_px)) est_px * r$quantity else NA_real_
        approx <- TRUE
      }
      tags$tr(
        class = if (identical(r$status, "отказ")) "wl-row--off" else NULL,
        tags$td(class = "mut", format(r$at, "%d.%m %H:%M")),
        tags$td(tags$b(r$symbol),
                tags$span(class = "wl-src",
                          if (identical(r$side, "buy")) "покупка" else "продажа")),
        tags$td(class = "r", formatC(r$quantity, format = "d")),
        tags$td(class = "r", if (is.finite(px))
                  paste0(if (approx) "\u2248" else "", money(px, 2)) else "\u2014"),
        tags$td(class = "r", if (is.finite(amt))
                  paste0(if (approx) "\u2248" else "", money(amt)) else "\u2014"),
        tags$td(class = if (filled) "pos" else "mut",
                orders_outcome_text(r),
                if (mismatch) tags$span(class = "neg",
                  sprintf(" \u00b7 заказано %g", r$quantity))),
        tags$td(class = "mut", r$user),
        tags$td(class = "r",
          if (orders_cancellable(r) && user_can_trade(USER$login))
            tags$button(class = "tr-btn sell",
              title = sprintf(paste0("Отменить неисполненное поручение: %s %g шт. ",
                                     "Отменяется только то, что ещё не исполнилось."),
                              r$symbol, r$quantity),
              onclick = sprintf(
                "Shiny.setInputValue('order_cancel','%s',{priority:'event'})", r$order_id),
              "\u00d7")
          else tags$span(class = "mut", "\u2014")))
    })
    tags$table(
      tags$thead(tags$tr(
        tags$th("Когда"), tags$th("Бумага"),
        tags$th(class = "r", "Кол-во"),
        tags$th(class = "r", "Цена"),
        tags$th(class = "r", "Сумма"),
        tags$th("Исполнено"), tags$th("Кто"),
        tags$th(class = "r", ""))),
      tags$tbody(rows))
  })

  # --- справочник ----------------------------------------------------------
  wl_bump <- reactiveVal(0L)
  wl_status <- reactiveVal(NULL)

  output$wl_add <- renderUI({
    tags$div(
      class = "wl-find",
      textInput("wl_find", paste0("Добавить из глобального справочника \u00b7 ",
                                 catalog_status_text()),
                placeholder = "тикер или название, напр. QQQ или Berkshire",
                width = "100%"),
      uiOutput("wl_hits")
    )
  })

  output$wl_count <- renderText({
    wl_bump()
    wl <- watchlist_all()
    sprintf("под наблюдением %d из %d", sum(is_watched(wl$active)), nrow(wl))
  })

  # Результаты поиска. Справочник лежит в хранилище, поиск идёт по нему —
  # в сеть на каждую набранную букву стенд не ходит.
  output$wl_hits <- renderUI({
    wl_bump()
    q <- trimws(input$wl_find %||% "")
    if (nchar(q) < 1) return(NULL)
    if (nrow(store_read_catalog()) == 0) {
      return(tags$div(class = "wl-hits", tags$div(class = "wl-hit", tags$span(
        class = "nm", "Справочник не загружен: задание «311.blnr - catalog» ещё не проходило."))))
    }
    hits <- catalog_search(q, limit = 20L)
    if (nrow(hits) == 0) {
      return(tags$div(class = "wl-hits", tags$div(class = "wl-hit", tags$span(
        class = "nm", paste0("По «", q, "» в справочнике ничего нет.")))))
    }
    known <- toupper(watchlist_all()$ticker)
    tags$div(class = "wl-hits", lapply(seq_len(nrow(hits)), function(i) {
      tk <- hits$ticker[i]
      have <- toupper(tk) %in% known
      tags$div(
        class = "wl-hit",
        tags$b(tk),
        tags$span(class = "nm", title = hits$name[i], hits$name[i]),
        tags$span(class = "ex", hits$exchange[i]),
        if (have) tags$button(disabled = NA, "уже есть")
        else tags$button(onclick = sprintf(
          "Shiny.setInputValue('wl_do_add','%s',{priority:'event'})", tk),
          "Добавить")
      )
    }))
  })

  # Добавление: тикер приходит из справочника, поэтому опечатка исключена, но
  # проверка у ИСТОЧНИКА ЦЕН остаётся — это другой источник, и бумага из
  # справочника Exante может не иметь рядов на marketdata.app.
  observeEvent(input$wl_do_add, {
    tk <- input$wl_do_add
    hit <- catalog_lookup(tk)
    res <- tryCatch(watchlist_add(tk, if (nrow(hit) > 0) hit$name[1] else NULL),
                    error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    wl_status(res)
    if (isTRUE(res$ok)) {
      updateTextInput(session, "wl_find", value = "")
      wl_bump(wl_bump() + 1L)
    }
  })

  # Переключатель наблюдения. Бумагу в портфеле выключить нельзя — проверка
  # внутри watchlist_set_active, здесь только передаём текущий состав счёта.
  observeEvent(input$wl_do_toggle, {
    spec <- strsplit(as.character(input$wl_do_toggle), "\\|", fixed = FALSE)[[1]]
    res <- tryCatch(
      watchlist_set_active(spec[1], identical(spec[2], "on"),
                           held = unique(portfolio_prices()[quantity_at > 0, ticker])),
      error = function(e) list(ok = FALSE, message = conditionMessage(e)))
    wl_status(res)
    if (isTRUE(res$ok)) wl_bump(wl_bump() + 1L)
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
    on <- is_watched(wl$active)
    # Выключенные — в конец: список нужен прежде всего для того, что под
    # наблюдением сейчас.
    ord <- order(!on, wl$ticker)
    rows <- lapply(ord, function(i) {
      tk <- wl$ticker[i]
      is_on <- on[i]
      in_port <- tk %in% held
      added <- identical(wl$source[i], "added")
      tags$tr(
        class = if (!is_on) "wl-row--off",
        tags$td(if (in_port) tags$span(class = "wl-held", title = "в портфеле"),
                tags$b(tk),
                if (added) tags$span(class = "wl-src", title =
                  "Добавлено вручную из глобального справочника", "вручную")),
        tags$td(wl$name_ru[i]),
        tags$td(class = "r",
                if (!is_on) tags$span(class = "mut", "не грузится")
                else if (tk %in% have) tags$span(class = "mut", "ряд есть")
                else tags$span(class = "neg", "нет ряда")),
        tags$td(class = "r",
                # У бумаги в портфеле переключателя нет: без её ряда нечем
                # считать стоимость и историю позиции. Раньше здесь стоял
                # ПРОЧЕРК с объяснением в title — то есть объяснения не было:
                # прочерк не выглядит тем, на что наводят курсор, и строка
                # читалась как «сломалось» (замечание владельца 27.09.2026).
                # Теперь причина написана словами прямо в колонке.
                if (in_port) tags$span(
                     class = "wl-sw is-held",
                     title = paste0(tk, " сейчас в портфеле: пока бумага на ",
                                    "счёте, её ряд нужен стоимости и истории ",
                                    "позиции. Выключить можно после продажи."),
                     "в портфеле")
                else tags$button(
                  class = if (is_on) "wl-sw" else "wl-sw off",
                  title = if (is_on)
                    paste("Выключить", tk, "из мониторинга; реестр и история сохранятся")
                    else paste("Вернуть", tk, "в мониторинг"),
                  onclick = sprintf(
                    "Shiny.setInputValue('wl_do_toggle','%s|%s',{priority:'event'})",
                    tk, if (is_on) "off" else "on"),
                  if (is_on) "под наблюдением" else "выключен")),
        tags$td(class = "r",
                if (added && !in_port) tags$button(
                  class = "wl-del", title = paste("Удалить", tk, "из реестра насовсем"),
                  onclick = sprintf(
                    "Shiny.setInputValue('wl_do_remove','%s',{priority:'event'})", tk),
                  "\u00d7"))
      )
    })
    tags$table(
      tags$thead(tags$tr(tags$th("Тикер"), tags$th("Название"),
                         tags$th(class = "r", "Ряд цен"),
                         tags$th(class = "r", "Мониторинг"), tags$th(""))),
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

    # Сделки по этой бумаге: где вошли и где вышли. Отмечаются по ДАТЕ СДЕЛКИ
    # (trade_date), а не по дате расчётов: иначе вчерашняя покупка с расчётами
    # «завтра» уезжала за край свечей и маркер пропадал (поймано владельцем
    # 07.10.2026 — вчерашняя покупка AMD не показывалась).
    #
    # ПОКУПКА и ПРОДАЖА — РАЗНЫМИ символами, не только цветом: треугольник
    # вверх (вошёл) и треугольник вниз (вышел) читаются даже без цвета, а два
    # ромба одного вида различались только оттенком.
    ev <- ledger_events(ledger_now())
    ev <- ev[exante_symbol_to_ticker(symbol) == tk &
             trade_date >= min(cnd$date) & trade_date <= max(cnd$date)]
    if (nrow(ev) > 0) {
      ev[, side := data.table::fifelse(qty >= 0, "Покупка", "Продажа")]
      # Тултип — СТРУКТУРА, а не строка в строку: заголовок события, затем
      # количество, цена и сумма по полкам. Разметку plotly понимает в HTML.
      ev[, htxt := sprintf(paste0(
        "<b>%s \u00b7 %s</b><br>",
        "дата сделки: %s<br>",
        "количество: %g шт.<br>",
        "цена: $%s<br>",
        "сумма: $%s"),
        side, tk, format(trade_date, "%d.%m.%Y"), abs(qty),
        formatC(price, format = "f", digits = 2, big.mark = " "),
        formatC(abs(qty) * price, format = "f", digits = 0, big.mark = " "))]
      buys <- ev[qty >= 0]; sells <- ev[qty < 0]
      if (nrow(buys) > 0) p <- add_trace(
        p, data = buys, x = ~trade_date, y = ~price, inherit = FALSE,
        type = "scatter", mode = "markers", name = "Покупка",
        marker = list(size = 13, symbol = "triangle-up",
          color = BLNR_COLORS$ok, line = list(color = "#fff", width = 1.5)),
        hovertext = ~htxt, hoverinfo = "text")
      if (nrow(sells) > 0) p <- add_trace(
        p, data = sells, x = ~trade_date, y = ~price, inherit = FALSE,
        type = "scatter", mode = "markers", name = "Продажа",
        marker = list(size = 13, symbol = "triangle-down",
          color = BLNR_COLORS$bad, line = list(color = "#fff", width = 1.5)),
        hovertext = ~htxt, hoverinfo = "text")
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
        # Переключатель отрезка. Прежние подписи «вся история» и «окно шкалы»
        # не объясняли ничего: «окно шкалы» — внутренний термин, а по какой
        # период смотришь, не было видно вовсе. Теперь в самой кнопке стоят
        # ДАТЫ, а подсказка говорит, откуда отрезок берётся.
        right = local({
          tl <- timeline_sessions()
          wnd <- if (length(tl)) sprintf("%s \u2014 %s",
                                         format(min(tl), "%d.%m.%y"),
                                         format(max(tl), "%d.%m.%y")) else "\u2014"
          all_from <- suppressWarnings(min(v$date))
          tags$div(
            class = "seg sm",
            actionButton("vs_all",
                         if (is.finite(all_from))
                           paste("весь срок \u00b7 с", format(all_from, "%m.%y")) else "весь срок",
                         title = paste0(
                           "Весь срок счёта: с первой операции по сегодня. ",
                           "Глубина ограничена хранилищем рядов цен."),
                         class = if (identical(value_scope(), "all")) "on" else NULL),
            actionButton("vs_window", paste("шкала \u00b7", wnd),
                         title = paste0(
                           "Тот же отрезок, что на шкале времени вверху (",
                           wnd, "). Нужен, чтобы динамика счёта и шкала ",
                           "показывали одно и то же окно."),
                         class = if (identical(value_scope(), "window")) "on" else NULL)
          )
        }),
        tip = tip_block(
          "Стоимость бумаг, кэш и итог по счёту на каждую сессию.",
          list("Итого", "жирная линия — бумаги плюс кэш"),
          list("Отрезок", "«весь срок» — с первой операции; «шкала» — окно шкалы времени вверху"),
          list("Разрыв", "на этих сессиях была бумага, которую нечем оценить — занижать стоимость молча нельзя"),
          list("Вертикаль", "выбранная дата"),
          note = paste0(
            "Считается из истории операций Exante, а не из снимков стенда: снимки начались бы со дня запуска, а история счёта известна целиком.",
            if (length(unp))
              paste0(" Сейчас не оценить: ", paste(utils::head(unp, 4), collapse = ", "),
                     if (length(unp) > 4) " и другие" else "") else "")),
        body_class = "bd--plot",
        plotlyOutput("chart_value", height = "100%")
      )))
    }
    if (nrow(deviation_series()) > 1) {
      items <- c(items, list(panel(
        "Результат против модели",
        sub = "прибыль и убыток, $",
        tip = tip_block(
          "По сессиям: заработали против того, что обещала модель.",
          list("Факт", "сколько реально заработали или потеряли на открытых позициях"),
          list("Модель", "сколько обещала за тот же срок владения"),
          list("Разница", "факт минус модель"),
          note = "В деньгах, а не в процентах: падение одной акции IBM на 20% рисовало бы «портфель −20%» при $45 тыс. наличными. Кэш в расчёт не входит — он не растёт и не падает."),
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

  # --- «Факт против модели»: вид и выключенные тикеры ----------------------
  # Состояние ЭКРАННОЕ, не хранилищное: это фильтр взгляда, а не реестр
  # наблюдения. Выключенная здесь бумага остаётся в портфеле, в таблице и в
  # ночной загрузке — она просто не мешает читать шкалу.
  vs_view <- reactiveVal("bars")
  vs_off <- reactiveVal(character())
  observeEvent(input$vs_tab_bars, vs_view("bars"))
  observeEvent(input$vs_tab_time, vs_view("time"))
  observeEvent(input$vs_toggle, {
    tk <- as.character(input$vs_toggle)
    cur <- vs_off()
    vs_off(if (tk %in% cur) setdiff(cur, tk) else c(cur, tk))
  })
  observeEvent(input$vs_all, vs_off(character()))

  # Тикеры, которые виджет показывает сейчас. Выключены все до единого —
  # показываем всё: пустой график вместо ответа хуже, чем прежний вид.
  vs_tickers <- reactive({
    all_tk <- sort(unique(portfolio_metrics()[quantity_at > 0, ticker]))
    keep <- setdiff(all_tk, vs_off())
    if (length(keep) == 0) all_tk else keep
  })

  output$vs_sub <- renderText({
    if (identical(vs_view(), "time")) {
      fd <- forecast_data()
      b <- if (!is.null(fd) && nrow(fd) > 0) min(fd$date) else NA
      # Подпись обязана называть то, ЧТО на холсте: при одной бумаге это две
      # кривые в процентах, при нескольких — по одной линии разницы в пп. Одна
      # подпись на оба случая заставляла гадать, факт это или модель.
      keep <- vs_tickers()
      one <- length(keep) == 1
      what <- if (one) sprintf("%s \u00b7 факт и модель", keep[1])
              else "расхождение факта и модели"
      return(if (is.na(b)) what
             else sprintf("%s от базы %s", what, format(b, "%d.%m.%Y")))
    }
    if (is_future()) paste0("к ", format(sel_date(), "%d.%m.%Y"))
    else format(sel_date(), "%d.%m.%Y")
  })

  output$vs_tabs <- renderUI({
    tags$div(
      class = "seg sm",
      actionButton("vs_tab_bars", "На дату",
                   class = if (identical(vs_view(), "bars")) "on" else NULL),
      actionButton("vs_tab_time", "Динамика",
                   class = if (identical(vs_view(), "time")) "on" else NULL)
    )
  })

  output$vs_chips <- renderUI({
    m <- portfolio_metrics()[quantity_at > 0]
    req(nrow(m) > 0)
    data.table::setorder(m, ticker)
    all_tk <- m$ticker
    off <- vs_off()
    fd <- forecast_data()
    fc_tickers <- if (is.null(fd) || nrow(fd) == 0) character() else unique(fd$ticker)
    tags$div(
      class = "vs-chips",
      lapply(seq_along(all_tk), function(i) {
        tk <- all_tk[i]
        is_off <- tk %in% off
        # «Не с чем сравнить» означает РАЗНОЕ в двух видах, и один флаг на оба
        # врал бы: IBM куплен до базы прогноза, поэтому в столбиках модели у
        # него нет, а ошибка самой модели по нему считается прекрасно — она от
        # покупок не зависит.
        no_model <- if (identical(vs_view(), "time")) !(tk %in% fc_tickers)
                    else !is.finite(m$forecast_since_entry_pct[i])
        tags$button(
          class = paste("vs-chip", if (is_off) "off", if (no_model) "nomodel"),
          title = paste0(
            if (is_off) paste("Вернуть", tk, "на график")
            else paste("Убрать", tk, "с графика; в портфеле бумага остаётся"),
            if (no_model) {
              if (identical(vs_view(), "time")) " \u00b7 этой бумаги нет в модели"
              else paste0(" \u00b7 сравнения за период владения нет: ",
                          m$model_gap[i] %||% "нет в модели")
            }),
          onclick = sprintf(
            "Shiny.setInputValue('vs_toggle','%s',{priority:'event'})", tk),
          tk)
      }),
      if (length(off) > 0) tags$button(
        class = "vs-chip all",
        title = "Вернуть все бумаги на график",
        onclick = "Shiny.setInputValue('vs_all', Math.random(), {priority:'event'})",
        "все")
    )
  })

  output$chart_vs_forecast <- renderPlotly({
    keep <- vs_tickers()
    if (identical(vs_view(), "time")) return(vs_time_plot(keep))
    m <- portfolio_metrics()[quantity_at > 0][ticker %in% keep]
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

  # Динамика расхождения по сессиям — линия на бумагу, в процентных пунктах.
  # Ноль означает «шла ровно по модели», поэтому нулевая линия рисуется всегда:
  # без неё знак расхождения приходится вычислять по подписям оси.
  vs_time_plot <- function(keep) {
    # ОТ БАЗЫ ПРОГНОЗА, а не от последней покупки. Считая от покупки, график
    # мерит период владения: 25.09.2026 шесть бумаг из семи были докуплены
    # накануне, ряд получился длиной в одну сессию, и plotly растянул ось на
    # миллисекунду вокруг единственной точки — виджет выглядел пустым.
    # Ошибка МОДЕЛИ к покупкам отношения не имеет: она существует с того дня,
    # на который модель посчитана.
    d <- ticker_deviation_series(ledger_now(), forecast_data(),
                                 timeline_sessions(), tickers = keep,
                                 anchor = "forecast_base")
    shiny::validate(shiny::need(
      nrow(d) > 0 && any(is.finite(d$dev_pp)),
      "Сравнивать нечего: по выбранным бумагам нет строк в модели."))
    # Бумага, по которой сравнивать не с чем (нет строки в модели, куплена до
    # прогноза), даёт ряд из одних NA. plotly рисует такой ряд пустой линией
    # БЕЗ ИМЕНИ, и в легенде появляется безымянный пункт, который ничего не
    # обозначает. Отбрасываем её здесь; почему её нет — написано на чипе.
    # Бумага, по которой сравнивать не с чем (нет строки в модели, куплена до
    # прогноза), даёт ряд из одних NA. plotly рисует такой ряд пустой линией
    # БЕЗ ИМЕНИ, и в легенде появляется безымянный пункт, который ничего не
    # обозначает. Отбрасываем её здесь; почему её нет — написано на чипе.
    have <- d[, .(ok = any(is.finite(dev_pp))), by = ticker][ok == TRUE, ticker]
    d <- d[ticker %in% have]
    tks <- sort(unique(d$ticker))
    p <- plot_ly()

    # ОДНА бумага — показываем ФАКТ И МОДЕЛЬ ПО ОТДЕЛЬНОЙ ЛИНИИ, а расхождение
    # остаётся зазором между ними. Одна линия разницы отвечала на вопрос
    # «насколько разошлись», но не отвечала на вопрос «а куда шла каждая»:
    # +20 пп получаются и когда факт вырос на 20 при нулевой модели, и когда
    # факт упал на 10, а модель обещала −30. Решения по этим случаям разные.
    # (Замечание владельца 27.09.2026: «это факт или модель? мне рядом бы их».)
    #
    # НЕСКОЛЬКО бумаг — по одной линии разницы на бумагу: шесть кривых факта и
    # шесть модели на одном холсте не читаются, а разница сравнима между
    # бумагами и на общей шкале.
    if (length(tks) == 1) {
      sub <- d[ticker == tks[1]]
      p <- add_trace(p, data = sub, x = ~date, y = ~model_pct, type = "scatter",
                     mode = "lines", name = "модель",
                     line = list(width = 1.6, color = BLNR_COLORS$plan,
                                 dash = "dash"),
                     hovertemplate = "%{x|%d.%m.%Y}<br>модель: %{y:+.2f}%<extra></extra>")
      # Заливка идёт К ПРЕДЫДУЩЕЙ линии, поэтому факт добавляется вторым:
      # закрашенный зазор и есть ошибка модели, её не нужно вычислять глазом.
      p <- add_trace(p, data = sub, x = ~date, y = ~fact_pct, type = "scatter",
                     mode = "lines", name = "факт", fill = "tonexty",
                     fillcolor = "rgba(255,165,0,0.13)",
                     line = list(width = 2, color = BLNR_COLORS$ok),
                     hovertemplate = "%{x|%d.%m.%Y}<br>факт: %{y:+.2f}%<extra></extra>")
      y_suffix <- "%"
    } else {
      for (i in seq_along(tks)) {
        sub <- d[ticker == tks[i]]
        p <- add_trace(p, data = sub, x = ~date, y = ~dev_pp, type = "scatter",
                       mode = "lines", name = tks[i],
                       line = list(width = 1.8, color = vs_line_color(i)),
                       hovertemplate = paste0("%{x|%d.%m.%Y}<br>", tks[i],
                                              ": %{y:+.2f} пп<extra></extra>"))
      }
      y_suffix <- " пп"
    }
    blnr_plot_layout(
      p,
      legend = list(orientation = "h", x = 0, y = 1.16, font = list(size = 10)),
      margin = list(l = 46, r = 10, t = 20, b = 26),
      shapes = list(
        list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 0, y1 = 0,
             line = list(color = BLNR_COLORS$border, width = 1)),
        list(type = "line", xref = "x", yref = "paper",
             x0 = sel_date(), x1 = sel_date(), y0 = 0, y1 = 1,
             line = list(color = BLNR_COLORS$orange, width = 1.5))),
      xaxis = list(title = "", gridcolor = BLNR_COLORS$grid),
      yaxis = list(title = "", ticksuffix = y_suffix, gridcolor = BLNR_COLORS$grid)
    )
  }

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
