# ui/dashboard_ui.R
#
# Полный интерфейс дашборда (bs4DashPage). Вынесен в функцию, чтобы шлюз
# авторизации (ui.R -> output$gate) создавал его ТОЛЬКО после входа: до
# авторизации ни разметка дашборда, ни его данные в браузер не уходят.

dashboardUI <- function() {
  bs4DashPage(

    # Общее ####
    title = "Blnr",
    dark = NULL,
    header = bs4DashNavbar(
      title = "Blnr",
      tags$head(includeCSS("www/CSS.css")),
      # Выход — справа в шапке (см. server.R output$logout_ui).
      # bs4Dash требует у элемента навбара класс 'dropdown'.
      rightUi = tags$li(class = "nav-item dropdown", uiOutput("logout_ui")),
      status = "light"),
    # Боковое меню ####
    sidebar = sidebarUI(),

    # Body ####
    body = bs4DashBody(
      useShinyjs(),
      use_theme(create_theme(
        bs4dash_status(light = "#D0D0D0"),
        bs4dash_status(light = "white", primary = "orange"),
        bs4dash_vars(navbar_light_hover_color = "orange"),

        bs4dash_sidebar_dark(
          bg = "white",
          color = '#15120F',
          hover_color  = 'white',
          submenu_active_bg  = '#30d5c8',
          hover_bg = "#6C757D",
          submenu_bg = "white",
          submenu_color = "#15120F",
          submenu_hover_color = "#6C757D",
          submenu_hover_bg = "white",
          submenu_active_color = "white"
        ),

        bs4dash_layout(sidebar_width = "340px", main_bg = "#ECE9E2")

      )),
      tags$head(
        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Panton:wght@400;700&display=swap"),
        tags$style(HTML("
          body {
            font-family: 'Panton', sans-serif;
            font-size: 10px;
          }
          .table th, .table td {
            font-size: 10px;
          }
          .plotly-title, .plotly-label {
            font-family: 'Panton', sans-serif;
          }
        "))
      ),
      bs4TabItems(
        downloadHistoryUI(),      # Вкладка "1. Котировка"
        portfolioMonitoringUI(),  # Вкладка "Портфель Exante"
        uploadForecastsUI()       # Вкладка "Загрузка прогнозов"
      )
    )
  )
}
