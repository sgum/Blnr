# ui/dashboard_ui.R
#
# Один экран стенда. Строится ТОЛЬКО после входа (server.R -> output$gate),
# поэтому до авторизации ни разметка, ни данные в браузер не уходят.
#
# Оформление — по действующему стенду portfolio.dtwin.ru (витрина проекта 290,
# самый свежий образец мониторинга ЦД): те же токены, те же размеры карточек,
# та же роль оранжевого (тонкая линейка и активная кнопка, а не заливка шапок).
# Палитра и структура блоков — в R/ui_kit.R, здесь только раскладка экрана.
#
# Почему не bs4Dash с вкладками, как было раньше. Три вкладки заставляли
# переключаться между котировками, портфелем и прогнозом — то есть держать
# сравнение в голове, хотя решение принимается именно из сравнения.
#
# Раскладка (от 1440x900 — без вертикальной прокрутки):
#   шапка 52px
#   полоса KPI
#   слева: [позиции] + [факт против модели]   справа: [график инструмента]
#   низ:   [невязка во времени] — появляется, только когда снимков хотя бы
#          за два дня; иначе нижнего ряда на экране нет вовсе.
# Уже 1180px раскладка складывается в одну колонку и начинает прокручиваться.
#
# fluidPage() нужен ради Bootstrap: без него selectInput/fileInput рисуются
# «голыми» — ровно те грабли, на которых уже поймали форму входа, см. docs/dev.md.

# Палитра подставляется отдельной короткой строкой, а сами правила идут
# литералом. Одним sprintf() на весь CSS нельзя: в R длина СТРОКИ ФОРМАТА
# ограничена 8192 байтами, и при её превышении вызов падает
# («fmt length exceeds maximal format length») — то есть экран не собирается
# вообще. Побочная выгода: в литерале проценты пишутся как есть, без удвоения.
dashboardCSS <- function() {
  cl <- BLNR_COLORS
  vars <- sprintf(":root{
  --bg:%s; --surface:%s; --muted:%s; --orange:%s; --orange-dk:%s; --on-orange:%s;
  --text:%s; --dim:%s; --faint:%s; --border:%s; --grid:%s;
  --ok:%s; --bad:%s; --warn:%s; --r:8px;
  --font:'Panton','IBM Plex Sans','Segoe UI',system-ui,-apple-system,sans-serif;
}",
    cl$bg, cl$surface, cl$muted, cl$orange, cl$orange_d, cl$on_orange,
    cl$text, cl$dim, cl$faint, cl$border, cl$grid, cl$ok, cl$bad, cl$warn)

  rules <- "
html,body{height:100%;margin:0}
body{background:var(--bg);color:var(--text);font-family:var(--font);
  font-size:12.5px;line-height:1.35;font-variant-numeric:tabular-nums}
.container-fluid{padding:0!important;height:100%}

.blnr{display:flex;flex-direction:column;height:100vh;overflow:hidden}

/* --- шапка страницы ---------------------------------------------------- */
.hdr{flex:0 0 auto;display:flex;align-items:center;gap:10px;height:52px;
  padding:0 14px;background:var(--surface);border-bottom:3px solid var(--orange);
  box-shadow:0 2px 8px rgba(31,36,48,.07);flex-wrap:nowrap;overflow-x:auto}
.ttl{flex:none;font-weight:800;color:var(--orange-dk);font-size:15px;
  white-space:nowrap}
.ttl small{display:block;font-weight:600;color:var(--dim);font-size:10.5px;
  margin-top:-2px}
.hdr .sp{flex:1 1 auto}
.bdg{display:inline-flex;align-items:center;gap:5px;font-size:11px;
  font-weight:700;border-radius:20px;padding:3px 9px;border:1px solid var(--border);
  background:var(--surface);white-space:nowrap;color:var(--dim)}
.bdg b{color:var(--text)}
.bdg.ok{color:var(--ok);border-color:#b8dcbb;background:#f1f8f1}
.bdg.warn{color:#8a5a00;border-color:#f2d59a;background:#fff8e6}
.seg{display:flex;border:1px solid var(--border);border-radius:6px;
  overflow:hidden;flex:none}
.seg button{border:0;background:var(--surface);padding:5px 9px;font-size:12px;
  font-weight:700;color:var(--dim);cursor:pointer;white-space:nowrap}
.seg button+button{border-left:1px solid var(--border)}
.seg button:hover{background:var(--orange);color:var(--on-orange)}
.seg button.on{background:var(--orange);color:var(--on-orange)}
.seg.sm button{padding:3px 7px;font-size:11px}
.hdr a{color:var(--dim);text-decoration:none;font-size:12px;font-weight:700;
  white-space:nowrap}
.hdr a:hover{color:var(--orange-dk)}

/* --- полоса времени ---------------------------------------------------- */
.tl{flex:0 0 auto;display:flex;align-items:center;gap:12px;padding:4px 14px 0;
  background:var(--surface);border-bottom:1px solid var(--border)}
.tl .lab{display:flex;align-items:center;font-size:11px;color:var(--dim);
  font-weight:700;white-space:nowrap;flex:none}
.tl .sld{flex:1 1 auto;min-width:0;position:relative}
/* Точки сделок поверх дорожки ползунка. Слой не перехватывает мышь целиком —
   только сами точки, иначе по нему нельзя было бы двигать ползунок. */
.tl-ev{position:absolute;left:0;right:0;top:20px;height:0;z-index:4;
  pointer-events:none}
.tl-ev i{position:absolute;top:0;width:8px;height:8px;border-radius:50%;
  transform:translateX(-50%);border:1.5px solid #fff;pointer-events:auto;
  cursor:help;box-shadow:0 0 0 1px rgba(31,36,48,.18)}
.tl-ev i.buy{background:var(--ok)}
.tl-ev i.sell{background:var(--bad)}
.tl .val{font-weight:800;font-size:14px;white-space:nowrap;flex:none}
.btn-today{flex:none;height:26px;padding:0 10px;border:1px solid var(--border);
  background:var(--surface);border-radius:6px;font-size:11.5px;font-weight:700;
  color:var(--dim);cursor:pointer;white-space:nowrap}
.btn-today:hover{background:var(--orange);color:var(--on-orange);
  border-color:var(--orange)}
/* Будущая часть шкалы — та, что правее фактической даты: заливка дорожки там
   гасится, чтобы прогноз не читался как уже случившийся факт. */
.blnr .tl .irs-bar{background:var(--orange)}
.tl-future{position:absolute;top:16px;height:6px;border-radius:0 3px 3px 0;
  background:repeating-linear-gradient(90deg,var(--faint) 0 3px,transparent 3px 6px);
  opacity:.5;z-index:3;pointer-events:none}
/* ionRangeSlider: прижимаем по высоте и красим в фирменный оранжевый.
   Селекторы БЕЗ имени скина (.irs--flat / .irs--shiny): скин задаётся
   настройкой shinyWidgets и меняется, а классы .irs-bar / .irs-handle
   одинаковы во всех. Привязка к скину давала синюю полосу, которой нет в
   палитре. */
.blnr .tl .irs{height:36px;font-family:var(--font)}
.blnr .tl .irs-line{top:16px;height:6px;background:var(--grid);border:0;
  border-radius:3px}
.blnr .tl .irs-bar{top:16px;height:6px;background:var(--orange);border:0;
  border-radius:3px}
.blnr .tl .irs-handle{top:9px;width:20px;height:20px}
.blnr .tl .irs-handle>i:first-child,.blnr .tl .irs-handle.single{background:var(--orange-dk)}
.blnr .tl .irs-single{background:var(--orange-dk);color:#fff;font-size:10.5px;
  font-weight:700;top:0;border-radius:4px}
.blnr .tl .irs-single:before{border-top-color:var(--orange-dk)}
.blnr .tl .irs-min,.blnr .tl .irs-max{background:transparent;color:var(--faint);
  font-size:10px;top:2px}
.blnr .tl .irs-grid{display:none}
.tl .form-group{margin:0}

/* --- KPI --------------------------------------------------------------- */
.kpis{flex:0 0 auto;display:grid;gap:8px;padding:8px 12px;
  grid-template-columns:repeat(auto-fit,minmax(160px,1fr))}
.k{background:var(--surface);border:1px solid var(--border);
  border-radius:var(--r);padding:6px 11px;min-width:0}
.k .h{display:flex;align-items:center;font-size:11px;color:var(--dim);
  font-weight:600;white-space:nowrap}
.k .v{font-size:20px;font-weight:800;line-height:1.15;white-space:nowrap}
.k .s{font-size:11px;color:var(--dim);white-space:nowrap;overflow:hidden;
  text-overflow:ellipsis}
.pos{color:var(--ok)} .neg{color:var(--bad)} .mut{color:var(--faint)}

/* --- сетка экрана ------------------------------------------------------ */
.blnr-main{flex:1 1 auto;min-height:0;display:grid;gap:8px;padding:0 12px 12px;
  grid-template-rows:minmax(0,1fr) auto}
/* Селектор ПОТОМКА, а не прямого ребёнка: display:contents у обёртки uiOutput
   убирает её из раскладки, но НЕ из дерева для сопоставления селекторов —
   карточка остаётся ребёнком div.shiny-html-output. С `>` правило молча не
   применялось, нижний ряд разворачивался на пол-экрана и сплющивал верхний. */
.blnr-row--bot .card{height:212px}
.blnr-row{display:grid;gap:8px;min-height:0}
.blnr-row--top{grid-template-columns:minmax(0,1.12fr) minmax(360px,1fr)}
/* Левая колонка: таблица позиций по высоте содержимого, под ней график
   добирает остаток. Иначе таблица из пяти строк растягивалась на всю высоту
   и полэкрана уходило в белое поле. */
.blnr-col{display:grid;gap:8px;min-height:0;grid-template-rows:auto minmax(0,1fr)}
.blnr-col--solo{grid-template-rows:minmax(0,1fr)}
/* Карточка по высоте содержимого: когда показывать нечего, пусть под ней
   будет фон страницы, а не белое поле на пол-экрана. */
.blnr-col--compact{grid-template-rows:auto;align-content:start}
/* uiOutput оборачивает содержимое в свой div: без display:contents он стал бы
   единственной ячейкой грида и ломал раскладку ряда. */
.blnr-row--top>.shiny-html-output,.blnr-row--bot>.shiny-html-output{display:contents}
.blnr-col>.shiny-html-output{display:contents}
.blnr-row--bot{grid-template-columns:repeat(auto-fit,minmax(0,1fr))}

/* --- карточка ---------------------------------------------------------- */
/* overflow у карточки НЕ скрываем: всплывающая подсказка под «i» живёт внутри
   шапки и при overflow:hidden обрезается карточкой — подсказка просто не
   появляется, хотя разметка и стиль верны. Скролл вешаем на тело карточки. */
.card{background:var(--surface);border:1px solid var(--border);
  border-radius:var(--r);display:flex;flex-direction:column;min-height:0;
  min-width:0}
.ch{flex:0 0 auto;display:flex;align-items:center;gap:8px;padding:6px 10px;
  border-bottom:2px solid var(--orange);font-weight:800;font-size:13px;
  min-height:36px}
.ch .t{white-space:nowrap}
.ch .sub{font-weight:600;color:var(--dim);font-size:11.5px;white-space:nowrap;
  overflow:hidden;text-overflow:ellipsis}
.ch .sp{flex:1 1 auto}
.bd{flex:1 1 auto;min-height:0;overflow:auto;padding:6px 8px}
.bd--plot{overflow:hidden;padding:2px}
.bd--plot>div,.bd--plot .plotly,.bd--plot .html-widget{height:100%!important}
.bd--flush{padding:0;display:flex;flex-direction:column}
.empty{padding:14px 12px;font-size:12px;color:var(--dim)}
/* --- справочник наблюдения --- */
.wl{display:flex;flex-direction:column;min-height:0;height:100%}
.wl-add{flex:none;display:flex;gap:6px;align-items:flex-end;padding:8px 10px;
  border-bottom:1px solid var(--border);background:var(--muted)}
.wl-add .shiny-input-container{margin:0}
.wl-add input.form-control{height:26px;font-size:11.5px;padding:3px 8px;
  border-radius:6px;border-color:var(--border)}
.wl-add label{font-size:10.5px;color:var(--dim);font-weight:700;margin:0 0 2px}
.wl-list{flex:1 1 auto;min-height:0;overflow:auto}
.wl-list table{width:100%;border-collapse:collapse}
.wl-list th{position:sticky;top:0;background:var(--muted);z-index:2;
  font-size:10.5px;color:var(--dim);font-weight:700;text-align:left;
  padding:4px 8px;border-bottom:1px solid var(--border)}
.wl-list td{padding:3px 8px;border-bottom:1px solid #f0f2f5;height:24px;
  font-size:11.5px}
.wl-list td.r{text-align:right}
.wl-list tr:hover{background:#fff7e6}
.wl-del{border:0;background:transparent;color:var(--faint);cursor:pointer;
  font-size:13px;line-height:1;padding:0 4px}
.wl-del:hover{color:var(--bad)}
/* Переключатель наблюдения. Кнопка, а не крестик: выключение обратимо, и
   выглядеть оно должно обратимым — крестик читается как удаление. */
.wl-sw{border:1px solid var(--border);background:#fff;color:var(--dim);
  cursor:pointer;font-size:10.5px;font-weight:700;line-height:1;
  padding:3px 7px;border-radius:5px;white-space:nowrap}
.wl-sw:hover{border-color:var(--orange);color:var(--text)}
.wl-sw.off{background:var(--orange);border-color:var(--orange);
  color:var(--on-orange)}
.wl-row--off td{color:var(--faint)}
.wl-row--off td b{font-weight:600}
.wl-src{font-size:10px;color:var(--faint);margin-left:5px;font-weight:600}
/* Поиск по глобальному справочнику. Результаты — список под полем, а не
   выпадающий selectize на десять тысяч строк: тот грузит весь справочник в
   браузер на каждое открытие вкладки. */
.wl-find{flex:none;padding:8px 10px;border-bottom:1px solid var(--border);
  background:var(--muted)}
.wl-find .shiny-input-container{margin:0;width:100%}
.wl-find input.form-control{height:26px;font-size:11.5px;padding:3px 8px;
  border-radius:6px;border-color:var(--border)}
.wl-find label{font-size:10.5px;color:var(--dim);font-weight:700;margin:0 0 2px}
.wl-hits{max-height:190px;overflow:auto;margin-top:6px;background:#fff;
  border:1px solid var(--border);border-radius:6px}
.wl-hits:empty{display:none}
.wl-hit{display:flex;gap:8px;align-items:center;padding:4px 8px;
  border-bottom:1px solid #f0f2f5;font-size:11.5px}
.wl-hit:last-child{border-bottom:0}
.wl-hit b{min-width:56px}
.wl-hit .nm{flex:1 1 auto;overflow:hidden;text-overflow:ellipsis;
  white-space:nowrap;color:var(--dim)}
.wl-hit .ex{font-size:10px;color:var(--faint);font-weight:700}
.wl-hit button{border:0;background:var(--orange);color:var(--on-orange);
  font-weight:700;font-size:10.5px;padding:3px 8px;border-radius:5px;
  cursor:pointer}
.wl-hit button[disabled]{background:var(--border);color:var(--faint);
  cursor:default}
.wl-sub{font-size:10.5px;color:var(--dim);font-weight:700;padding:6px 10px 2px}
.wl-msg{flex:none;padding:6px 10px;font-size:11.5px;border-top:1px solid var(--border)}
.wl-msg.ok{color:var(--ok);background:#f1f8f1}
.wl-msg.bad{color:var(--bad);background:#fdf1f1}
.wl-held{display:inline-block;width:7px;height:7px;border-radius:50%;
  background:var(--ok);margin-right:6px;vertical-align:middle}
/* --- сделки --- */
.tr-btn{border:1px solid var(--border);background:var(--surface);border-radius:4px;
  width:20px;height:18px;line-height:1;font-size:13px;font-weight:700;
  color:var(--dim);cursor:pointer;padding:0}
.tr-btn.sell:hover{background:var(--bad);border-color:var(--bad);color:#fff}
.btn-buy{height:22px;padding:0 9px;border:1px solid var(--border);
  background:var(--surface);border-radius:6px;font-size:11px;font-weight:700;
  color:var(--dim);cursor:pointer;white-space:nowrap}
.btn-buy:hover{background:var(--ok);border-color:var(--ok);color:#fff}
.btn-sellall{height:22px;padding:0 9px;border:1px solid var(--border);
  background:var(--surface);border-radius:6px;font-size:11px;font-weight:700;
  color:var(--dim);cursor:pointer;white-space:nowrap}
.btn-sellall:hover{background:var(--bad);border-color:var(--bad);color:#fff}
.btn-sellall{height:22px;padding:0 9px;border:1px solid var(--border);
  background:var(--surface);border-radius:6px;font-size:11px;font-weight:700;
  color:var(--dim);cursor:pointer;white-space:nowrap}
.btn-sellall:hover{background:var(--bad);border-color:var(--bad);color:#fff}
.btn-trade{height:30px;padding:0 14px;border:1px solid var(--orange-dk);
  background:var(--orange);color:var(--on-orange);border-radius:6px;
  font-size:12.5px;font-weight:800;cursor:pointer}
.btn-trade:hover{background:var(--orange-dk);color:#fff}
.empty b{color:var(--text)}

/* --- подсказка под i --------------------------------------------------- */
.ii{position:relative;display:inline-flex;align-items:center;
  justify-content:center;width:15px;height:15px;border-radius:50%;
  border:1.3px solid var(--faint);color:var(--dim);
  font:italic 700 10px Georgia,serif;cursor:help;flex:none;margin-left:5px;
  vertical-align:middle}
.ii .tip{display:none;position:absolute;z-index:90;top:20px;left:50%;
  transform:translateX(-50%);width:360px;max-width:80vw;background:#1f2430;
  color:#eef0f4;font:400 12px/1.45 var(--font);font-style:normal;
  padding:10px 12px;border-radius:7px;box-shadow:0 8px 24px rgba(0,0,0,.25);
  text-align:left;cursor:default;white-space:normal}
.ii.l .tip{left:-8px;transform:none}
.ii.r .tip{left:auto;right:-8px;transform:none}
.ii:hover .tip,.ii:focus .tip{display:block}

/* --- таблица позиций --------------------------------------------------- */
.rk{overflow:auto;flex:1;min-height:0}
.rk table{width:100%;border-collapse:collapse}
.rk th{position:sticky;top:0;background:var(--muted);z-index:2;font-size:10.5px;
  color:var(--dim);font-weight:700;text-align:right;padding:4px 6px;
  border-bottom:1px solid var(--border);white-space:nowrap}
.rk th:first-child,.rk td:first-child{text-align:left}
.rk td{padding:2px 6px;border-bottom:1px solid #f0f2f5;white-space:nowrap;
  height:23px;text-align:right}
.rk tbody tr{cursor:pointer}
.rk tbody tr:hover{background:#fff7e6}
.rk tbody tr.sel{background:#ffefcc}
.rk tfoot td{padding:5px 6px;font-weight:800;border-top:1px solid var(--border);
  background:var(--muted)}
.rk tbody tr.cash td{background:#f8f9fb;color:var(--dim);font-weight:700}
.rk tbody tr.cash:hover td{background:#f4f5f7}
.rk tfoot tr.acct td{border-top:2px solid var(--orange)}
.rk .nm{font-weight:800}

/* --- компактные контролы Shiny ---------------------------------------- */
.blnr .form-group{margin:0}
.blnr .selectize-input{min-height:26px;height:26px;padding:3px 22px 3px 8px;
  font-size:11.5px;line-height:18px;border-radius:6px;border-color:var(--border)}
.blnr .selectize-dropdown{font-size:11.5px}
.blnr .shiny-input-container{width:auto!important}

/* Уже 1180px колонки складываются в одну, экран начинает прокручиваться, и
   высоты приходится задавать явно: в потоке без фиксированной высоты строка
   minmax(0,1fr) разворачивается на всё содержимое и один график занимает
   полтора экрана. */
@media(max-width:1180px){
  .blnr{height:auto;overflow:visible}
  .blnr-main{grid-template-rows:none}
  .blnr-row--top{grid-template-columns:1fr}
  .blnr-row--top>.card{height:400px}
  .blnr-col{grid-template-rows:none}
  .blnr-col>.card:last-child{height:300px}
  .blnr-row--bot .card{height:240px}
}
"
  tags$style(HTML(paste0(vars, rules)))
}

# Полоса времени. Правый край шкалы — фактическая дата (последняя сессия в
# хранилище), слева от неё ретроспектива на BLNR_TIMELINE_DAYS сессий.
timelineUI <- function(past, future = as.Date(character())) {
  if (length(past) == 0) {
    # Хранилище пусто — ползунку не из чего строить ось. Рисовать пустой
    # виджет незачем: о причине уже сказано плашкой в шапке.
    return(NULL)
  }
  # Ось строит сервер (timeline_axis): ползунок и точки сделок обязаны мерить
  # ОДНОЙ линейкой, иначе точки нормируются на другую длину и съезжают.
  all_dates <- c(past, future)
  labels <- format(all_dates, "%d.%m.%Y")
  fact <- format(max(past), "%d.%m.%Y")

  tags$div(
    class = "tl",
    tags$span(class = "lab", "Дата",
              info_tip(paste0(
                "Портфель и сравнение с моделью показаны на выбранную дату. ",
                "Слева от отметки «факт» — ", length(past), " торговых сессий ",
                "из локального хранилища; выходных и праздников на шкале нет, ",
                "в эти дни цены не существует. ",
                if (length(future))
                  paste0("Справа от неё — ", length(future), " дат прогноза до ",
                         format(max(future), "%d.%m.%Y"),
                         ": там нет факта, показана только траектория модели. ")
                else "",
                "Позиция, купленная позже выбранной даты, в расчёт не ",
                "попадает — портфеля на тот момент ещё не было. ",
                "Точками отмечены сделки: зелёная — покупка, красная — ",
                "продажа."))),
    tags$div(class = "sld",
             # Слой точек лежит НАД дорожкой: события счёта видно прямо на
             # шкале, без отдельного виджета и без текста на экране.
             tags$div(class = "tl-ev", uiOutput("tl_events", inline = TRUE)),
             # Заштрихованная зона правее фактической даты: там факта нет,
             # только траектория модели.
             if (length(future))
               tags$div(class = "tl-future",
                        style = sprintf("left:%.4f%%;right:0",
                                        (length(past) - 1) / (length(all_dates) - 1) * 100)),
             shinyWidgets::sliderTextInput(
               "as_of", label = NULL, choices = labels, selected = fact,
               grid = FALSE, force_edges = TRUE, width = "100%"
             )),
    actionButton("as_of_today", "Сегодня", class = "btn-today",
                 title = paste("Вернуть ползунок на фактическую дату —", fact)),
    uiOutput("tl_marker", inline = TRUE)
  )
}

dashboardUI <- function() {
  fluidPage(
    tags$head(dashboardCSS()),
    tags$div(
      class = "blnr",

      # --- шапка ---------------------------------------------------------
      tags$header(
        class = "hdr",
        tags$div(class = "ttl", "Blnr", tags$small("портфель · broker.dtwin.ru")),
        uiOutput("hd_source", inline = TRUE),
        uiOutput("hd_forecast", inline = TRUE),
        tags$span(class = "sp"),
        uiOutput("hd_updated", inline = TRUE),
        # Порядок кнопок = порядок работы владельца: сначала выгрузить ряды
        # для внешнего расчёта, потом загрузить обратно его результат. Шаги
        # пронумерованы, чтобы последовательность читалась с экрана, а не
        # держалась в голове.
        # Третьим шагом здесь должен встать импорт предлагаемого к покупке
        # портфеля. Его НЕТ сознательно: присланный образец
        # («Portfolio 2026-09.13.xlsx») оказался не списком бумаг к покупке, а
        # той же матрицей mean/var, что и прогноз, — 26 инструментов на 527
        # шагов. Строить покупку на догадке о смысле файла нельзя, нужен
        # образец того, что считать целевым портфелем.
        tags$div(
          class = "seg",
          downloadButton("export_xlsx", "1 · Ряды в Excel", class = "dl"),
          actionButton("open_forecast", "2 · Прогноз\u2026"),
          actionButton("portfolio_refresh", "Обновить")
        ),
        uiOutput("logout_ui", inline = TRUE)
      ),

      # --- полоса времени -------------------------------------------------
      # Ось строится по РЕАЛЬНЫМ торговым сессиям из хранилища, а не по
      # календарю: на календарной шкале половина делений — выходные, где цены
      # нет и портфель показать не из чего.
      uiOutput("timeline"),

      # --- KPI -----------------------------------------------------------
      uiOutput("kpi_strip", class = "kpis"),

      # --- рабочая область ------------------------------------------------
      tags$div(
        class = "blnr-main",
        tags$div(
          class = "blnr-row blnr-row--top",
          uiOutput("left_col"),
          uiOutput("right_col")
        ),
        tags$div(
          class = "blnr-row blnr-row--bot",
          uiOutput("bot_panels")
        )
      )
    )
  )
}
