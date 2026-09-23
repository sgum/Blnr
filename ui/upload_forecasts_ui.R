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
        p("Файл модельного прогноза: лист «Q_mean_var», блок «mean» ",
          "(строки — инструменты Goldman Sachs/Nvidia/Google/AMD/General Electric…, ",
          "столбцы — шаги .qM0, .qM1, …). Значения — уже накопленный относительный ",
          "прогноз роста бумаги; шаг k раскладывается на торговые дни от 11.09.2026."),
        fileInput("forecast_file", "Файл прогноза (.xlsx)", accept = c(".xlsx")),
        uiOutput("forecast_sheet_ui"),
        checkboxInput("forecast_is_share", "Значения в файле — доли (0.012 = 1.2%) — умножить на 100", value = TRUE),
        uiOutput("forecast_status"),
        tableOutput("forecast_preview")
      )
    )
  )
}
