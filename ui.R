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
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Panton:wght@400;700&display=swap"),
      loginCSS()
    ),
    uiOutput("gate")
  )
)
