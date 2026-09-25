# ui.R — тонкий шлюз авторизации.
#
# Вся страница строится в output$gate (server.R): до входа — форма
# loginUI(), после входа — dashboardUI(). Так дашборд и его данные не
# уходят в браузер, пока пользователь не прошёл проверку (финансовый стенд).
# Модули интерфейса подключаются в global.R (глобальная область), чтобы их
# видел и server.R.

source("global.R", local = TRUE)

shinyUI(
  tagList(
    useShinyjs(),
    tags$head(
      tags$title("Blnr"),
      # Panton — фирменный шрифт ЦД, его нет в Google Fonts (прежняя ссылка
      # туда просто отдавала 404, и весь стенд рисовался системным шрифтом).
      # Начертание лежит в www/Panton-Regular.otf, @font-face — в www/CSS.css.
      tags$link(rel = "stylesheet", href = "CSS.css"),
      loginCSS()
    ),
    uiOutput("gate")
  )
)
