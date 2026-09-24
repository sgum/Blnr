# portfolio_monitoring_ui.R

portfolioMonitoringUI <- function() {
  bs4TabItem(
    tabName = "portfolio_monitoring",
    fluidRow(
      column(12,
        bs4Card(
          title = "Файл прогноза", width = 12, collapsible = FALSE,
          p("Загрузите файл с расчётными темпами роста акций портфеля (уровень на 11.09.2026, прогноз с 14.09.2026). ",
            "Формат: столбец с датой + по столбцу на каждую акцию (GS, GE, AMD, GOOG, NVDA или название компании)."),
          fluidRow(
            column(8, fileInput("forecast_file", label = NULL, accept = ".xlsx",
                                  buttonLabel = "Обзор...", placeholder = "Файл не выбран")),
            column(4, radioButtons("forecast_value_type", "Значения в файле",
                                     choices = c("Проценты (2 = +2%)" = "percent",
                                                 "Доли (0.02 = +2%)" = "fraction",
                                                 "Уровни цены" = "price"),
                                     selected = "percent"))
          )
        )
      )
    ),
    fluidRow(
      column(3, bs4Card(width = 12, title = "Факт. рост портфеля", status = "primary", htmlOutput("stat_actual_growth"))),
      column(3, bs4Card(width = 12, title = "Прогноз роста портфеля", status = "info", htmlOutput("stat_forecast_growth"))),
      column(3, bs4Card(width = 12, title = "Отклонение, п.п.", status = "warning", htmlOutput("stat_deviation"))),
      column(3, bs4Card(width = 12, title = "Накопленная ошибка, п.п.", status = "danger", htmlOutput("stat_cum_error")))
    ),
    fluidRow(
      column(12,
        bs4Card(
          title = "Портфель на последнюю дату", width = 12,
          rHandsontableOutput("portfolio_table")
        )
      )
    ),
    fluidRow(
      column(6, bs4Card(title = "Факт vs Прогноз, темп роста", width = 12, plotlyOutput("portfolio_growth_chart"))),
      column(6, bs4Card(title = "Накопленная ошибка прогноза", width = 12, plotlyOutput("portfolio_error_chart")))
    )
  )
}
