# ui.R

# Подключаем глобальные переменные
source("global.R", local = TRUE)

# Подключаем файлы для вкладок и бокового меню
source("ui/sidebar_ui.R", local = TRUE)
source("ui/download_history_ui.R", local = TRUE)
source("ui/upload_forecasts_ui.R", local = TRUE)

shinyUI(
  bs4DashPage(
    
    # Общее ####
    title = "Blnr", 
    dark = NULL,
    header = bs4DashNavbar(
      title = "Blnr", 
      tags$head(includeCSS("www/CSS.css")),
      tags$style(HTML(
        "<script>
                      $( document ).ready(function() {
                       $(\".tab-content [type='checkbox']\").on('click', function(){
                        var is_checked = this.checked;
                         if(typeof is_checked === 'boolean' && is_checked === true){
                          setTimeout(function() {
                           window.scrollTo(0,document.body.scrollHeight);
                            }, 200)
                               }
                                })
                                   })
                  </script>")), 
      status = "light"), 
    # Бковое меню ####
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
          hover_color  = 'white',#цвет теста пункта меню при наведении
          submenu_active_bg  = '#30d5c8', #цвет выбранного подпункта меню
          hover_bg = "#6C757D",#цвет фона пункта меню при наведении
          submenu_bg = "white", #цвет фона меню
          submenu_color = "#15120F", #цвет текса подпункта меню
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
        downloadHistoryUI(),  # Вызов функции для вкладки "1. Котировка"
        uploadForecastsUI()   # Вызов функции для вкладки "Загрузка прогнозов"
      )
    )
  )
)
