# portfolio_monitoring_ui.R

portfolioMonitoringUI <- function() {
  cat("Rendering portfolioMonitoringUI...\n")
  result <- bs4TabItem(
    tabName = "portfolio_monitoring",
    h3("Мониторинг портфеля")
  )
  cat("Finished rendering portfolioMonitoringUI...\n")
  return(result)
}
