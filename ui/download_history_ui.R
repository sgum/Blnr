# download_history_ui.R

downloadHistoryUI <- function() {
  return(
    bs4TabItem(
      tabName = "tab1",
      fluidRow(
          bs4Card(
            title = "Графики котировок",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            plotlyOutput("plot_candlestick")
          ),
          bs4Card(
            title = "Таблица котировок",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            rHandsontableOutput("stock_table")
        )
      )
    )
  )
}
