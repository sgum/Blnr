# upload_forecasts_ui.R

uploadForecastsUI <- function() {
  bs4TabItem(
    tabName = "tab2",
    fluidRow(
      bs4Card(
        title = "Загрузка прогноза роста",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        p("Файл: первый столбец — дата (торговый день), остальные столбцы — по бумаге ",
          "(GS/GE/AMD/GOOG/NVDA или их варианты написания). Значения — прогнозный ",
          "относительный рост от уровня, достигнутого бумагой к 11.09.2026."),
        fileInput("forecast_file", "Файл прогноза (.xlsx)", accept = c(".xlsx")),
        uiOutput("forecast_sheet_ui"),
        checkboxInput("forecast_is_share", "Значения в файле — доли (0.05), а не проценты (5) — умножить на 100", value = FALSE),
        uiOutput("forecast_status"),
        tableOutput("forecast_preview")
      )
    )
  )
}
