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
      # Значок вкладки (favicon) — фирменный символ ЦД, общий для всех стендов.
      # Лежит в www/ самого приложения, а не только на фронтовом nginx: у
      # стендов, закрытых гейтом, запрос /favicon.ico уходит в редирект на
      # форму входа, и браузер получает HTML вместо картинки. Файл в www/
      # отдаётся раньше любой авторизационной логики.
      # Источник: DT.F005.Intranet / 2. ЦД.Символика / 2. ЦД.Логотип/favicon.ico
      tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
      tags$link(rel = "shortcut icon", type = "image/x-icon", href = "favicon.ico"),
      tags$link(rel = "apple-touch-icon", href = "favicon.ico"),
      # Panton — фирменный шрифт ЦД, его нет в Google Fonts (прежняя ссылка
      # туда просто отдавала 404, и весь стенд рисовался системным шрифтом).
      # Начертание лежит в www/Panton-Regular.otf, @font-face — в www/CSS.css.
      tags$link(rel = "stylesheet", href = "CSS.css"),
      loginCSS()
    ),
    uiOutput("gate")
  )
)
