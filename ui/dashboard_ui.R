# ui/dashboard_ui.R
#
# Один экран стенда. Строится ТОЛЬКО после входа (server.R -> output$gate),
# поэтому до авторизации ни разметка, ни данные в браузер не уходят.
#
# Почему не bs4Dash с вкладками, как было раньше. Три вкладки заставляли
# переключаться между котировками, портфелем и прогнозом — то есть держать
# сравнение в голове, хотя решение принимается именно из сравнения. Плюс
# цветные «шапки» карточек bs4Dash тратили оранжевый на декорацию, а на
# финансовом экране цвет обязан означать только рост/падение.
#
# Раскладка (от 1440x900 — без вертикальной прокрутки):
#   шапка 44px
#   полоса KPI
#   слева: [позиции] + [факт против модели]   справа: [график инструмента]
#   низ:   [невязка во времени] — появляется, только когда снимков хотя бы
#          за два дня; иначе нижнего ряда на экране нет вовсе.
# Уже 1180px раскладка складывается в одну колонку и начинает прокручиваться.
#
# fluidPage() нужен ради Bootstrap: без него selectInput/fileInput рисуются
# «голыми» — ровно те грабли, на которых уже поймали форму входа, см. docs/dev.md.

dashboardCSS <- function() {
  cl <- BLNR_COLORS
  tags$style(HTML(sprintf("
:root{
  --ink:%s; --ink2:%s; --line:%s; --bg:%s; --card:%s;
  --accent:%s; --warn:%s; --up:%s; --down:%s; --mute:%s;
}
html,body{height:100%%;margin:0}
body{background:var(--bg);color:var(--ink);
     font-family:Panton,'Segoe UI',Arial,sans-serif;font-size:12px}
.container-fluid{padding:0!important;height:100%%}

.blnr{display:flex;flex-direction:column;height:100vh;overflow:hidden}

/* --- шапка ------------------------------------------------------------ */
.blnr-head{flex:0 0 auto;display:flex;align-items:center;gap:14px;height:44px;
  padding:0 12px;background:var(--card);border-bottom:1px solid var(--line)}
.blnr-brand{font-weight:700;font-size:15px;letter-spacing:.5px}
.blnr-head .sp{flex:1 1 auto}
.chip{display:inline-flex;align-items:center;gap:6px;height:24px;padding:0 9px;
  border:1px solid var(--line);border-radius:12px;font-size:11px;color:var(--ink2);
  white-space:nowrap}
.chip b{color:var(--ink)}
.chip--warn{border-color:var(--warn);color:#8a5d00;background:#fdf6e7}
.blnr-head a{color:var(--ink2);text-decoration:none;font-size:12px}
.blnr-head a:hover{color:var(--ink)}
.btn-ghost{height:26px;padding:0 10px;border:1px solid var(--line);
  background:var(--card);border-radius:4px;color:var(--ink);font-size:11px;
  cursor:pointer}
.btn-ghost:hover{border-color:var(--accent);color:#1d6d84}

/* --- KPI --------------------------------------------------------------- */
.blnr-kpi{flex:0 0 auto;display:grid;gap:8px;padding:8px 12px;
  grid-template-columns:repeat(auto-fit,minmax(150px,1fr))}
.kpi{background:var(--card);border:1px solid var(--line);border-left:3px solid var(--mute);
  border-radius:4px;padding:6px 10px}
.kpi--up{border-left-color:var(--up)} .kpi--down{border-left-color:var(--down)}
.kpi-val{font-size:20px;font-weight:700;line-height:1.15;
  font-variant-numeric:tabular-nums}
.kpi--up .kpi-val{color:var(--up)} .kpi--down .kpi-val{color:var(--down)}
.kpi-lab{font-size:11px;color:var(--ink2);margin-top:1px}

/* --- сетка экрана ------------------------------------------------------ */
.blnr-main{flex:1 1 auto;min-height:0;display:grid;gap:8px;padding:0 12px 12px;
  grid-template-rows:minmax(0,1fr) auto}
/* Селектор ПОТОМКА, а не прямого ребёнка: display:contents у обёртки uiOutput
   убирает её из раскладки, но НЕ из дерева для сопоставления селекторов —
   панель остаётся ребёнком div.shiny-html-output. С `>` правило молча не
   применялось, нижний ряд разворачивался на пол-экрана и сплющивал верхний. */
.blnr-row--bot .panel{height:212px}
.blnr-row{display:grid;gap:8px;min-height:0}
.blnr-row--top{grid-template-columns:minmax(0,1.12fr) minmax(0,1fr)}
/* Левая колонка: таблица позиций по высоте содержимого, под ней график
   добирает остаток. Иначе таблица из пяти строк растягивалась на всю высоту
   и полэкрана уходило в белое поле. */
.blnr-col{display:grid;gap:8px;min-height:0;grid-template-rows:auto minmax(0,1fr)}
.blnr-col--solo{grid-template-rows:minmax(0,1fr)}
.blnr-row--top>.shiny-html-output{display:contents}
.blnr-row--bot{grid-template-columns:repeat(auto-fit,minmax(0,1fr))}
/* uiOutput оборачивает содержимое в свой div: без display:contents он стал бы
   единственной ячейкой грида, и панели перестали бы делить строку поровну. */
.blnr-row--bot>.shiny-html-output{display:contents}

/* overflow у панели НЕ скрываем: всплывающая подсказка под «i» живёт внутри
   шапки и при overflow:hidden обрезается панелью — подсказка просто не
   появляется, хотя разметка и стиль верны. Скролл вешаем на тело панели. */
.panel{background:var(--card);border:1px solid var(--line);border-radius:4px;
  display:flex;flex-direction:column;min-height:0}
.panel-head{flex:0 0 auto;display:flex;align-items:center;gap:7px;height:29px;
  padding:0 10px;border-bottom:1px solid var(--line);font-weight:700;font-size:12px}
.panel-head-sp{flex:1 1 auto}
.panel-body{flex:1 1 auto;min-height:0;overflow:auto;padding:6px 8px}
.panel-body--plot{overflow:hidden;padding:2px}
.panel-body--plot>div,.panel-body--plot .plotly,.panel-body--plot .html-widget{height:100%%!important}
.panel-body--flush{padding:0}

/* --- подсказка под i --------------------------------------------------- */
.itip{display:inline-flex;align-items:center;justify-content:center;width:14px;
  height:14px;border:1px solid var(--mute);border-radius:50%%;color:var(--mute);
  font-size:9px;font-weight:700;font-style:italic;cursor:help;position:relative;
  flex:0 0 auto}
.itip:hover{border-color:var(--accent);color:var(--accent)}
.itip:hover::after{content:attr(data-tip);position:absolute;top:18px;left:-6px;
  z-index:2000;width:300px;padding:8px 10px;background:#2f2f2f;color:#fff;
  border-radius:4px;font-size:11px;font-weight:400;font-style:normal;
  line-height:1.45;white-space:normal;box-shadow:0 4px 14px rgba(0,0,0,.28)}

/* --- таблица позиций --------------------------------------------------- */
table.pos{width:100%%;border-collapse:collapse;font-variant-numeric:tabular-nums}
table.pos th{position:sticky;top:0;z-index:1;background:var(--card);
  border-bottom:1px solid var(--line);padding:5px 6px;text-align:right;
  font-size:10.5px;font-weight:700;color:var(--ink2);white-space:nowrap}
table.pos th:first-child,table.pos td:first-child{text-align:left}
table.pos td{padding:5px 6px;text-align:right;border-bottom:1px solid #F0EFED;
  white-space:nowrap}
table.pos tbody tr{cursor:pointer}
table.pos tbody tr:hover{background:#F7FAFB}
table.pos tbody tr.is-sel{background:#EAF4F8;box-shadow:inset 3px 0 0 var(--accent)}
table.pos tfoot td{padding:6px;font-weight:700;border-top:1px solid var(--line);
  text-align:right}
table.pos tfoot td:first-child{text-align:left}
.tk{font-weight:700}
.up{color:var(--up)} .down{color:var(--down)} .na{color:var(--mute)}

/* --- компактные контролы Shiny ---------------------------------------- */
.blnr .form-group{margin:0}
.blnr .selectize-input{min-height:24px;height:24px;padding:2px 22px 2px 8px;
  font-size:11px;line-height:18px;border-radius:4px;border-color:var(--line)}
.blnr .selectize-input.items{padding-top:2px}
.blnr .selectize-dropdown{font-size:11px}
.blnr .shiny-input-container{width:auto!important}

/* Уже 1180px колонки складываются в одну, экран начинает прокручиваться, и
   высоты приходится задавать явно: в потоке без фиксированной высоты строка
   minmax(0,1fr) разворачивается на всё содержимое и один график занимает
   полтора экрана. */
@media(max-width:1180px){
  .blnr{height:auto;overflow:visible}
  .blnr-main{grid-template-rows:none}
  .blnr-row--top{grid-template-columns:1fr}
  .blnr-row--top>.panel{height:400px}
  .blnr-col{grid-template-rows:none}
  .blnr-col>.panel:last-child{height:300px}
  .blnr-row--bot .panel{height:240px}
}
", cl$ink, cl$ink2, cl$line, cl$bg, cl$card, cl$accent, cl$warn, cl$up,
   cl$down, cl$mute)))
}

dashboardUI <- function() {
  fluidPage(
    tags$head(dashboardCSS()),
    tags$div(
      class = "blnr",

      # --- шапка ---------------------------------------------------------
      tags$header(
        class = "blnr-head",
        tags$span(class = "blnr-brand", "Blnr"),
        uiOutput("hd_source", inline = TRUE),
        uiOutput("hd_forecast", inline = TRUE),
        tags$span(class = "sp"),
        uiOutput("hd_updated", inline = TRUE),
        actionButton("portfolio_refresh", "Обновить", class = "btn-ghost"),
        actionButton("open_forecast", "Прогноз…", class = "btn-ghost"),
        uiOutput("logout_ui", inline = TRUE)
      ),

      # --- KPI -----------------------------------------------------------
      uiOutput("kpi_strip", class = "blnr-kpi"),

      # --- рабочая область ------------------------------------------------
      tags$div(
        class = "blnr-main",
        tags$div(
          class = "blnr-row blnr-row--top",
          uiOutput("left_col"),
          panel(
            "График инструмента",
            tip = paste(
              "Дневные свечи за", WATCHLIST_RETRO_DAYS, "торговых дней —",
              "та же глубина ретроспективы, что и во внешнем прогнозировании.",
              "Пунктир — траектория цены по модели от базы прогноза;",
              "горизонталь — цена входа, если бумага в портфеле."
            ),
            # Выбор задаётся прямо в разметке, а не updateSelectInput из
            # сервера: обновление, отправленное до того, как виджет появился
            # на клиенте, теряется — график остаётся пустым.
            right = tags$div(
              style = "width:240px",
              selectInput(
                "sel_ticker", NULL, width = "100%",
                choices = stats::setNames(
                  as.list(watchlist_active()$ticker),
                  paste0(watchlist_active()$ticker, " · ", watchlist_active()$name_ru)),
                selected = portfolio_holdings$ticker[1]
              )
            ),
            body_class = "panel-body--plot",
            plotlyOutput("chart_instrument", height = "100%")
          )
        ),
        tags$div(
          class = "blnr-row blnr-row--bot",
          uiOutput("bot_panels")
        )
      )
    )
  )
}
