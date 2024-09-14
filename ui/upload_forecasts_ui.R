# upload_forecasts_ui.R

uploadForecastsUI <- function() {
  return(
    bs4TabItem(
      tabName = "tab2",
      bs4Card(
        title = "Загрузка прогнозов",
        h3("Функционал для загрузки прогнозов")
      )
    )
  )
}
