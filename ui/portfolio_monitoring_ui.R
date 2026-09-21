# portfolio_monitoring_ui.R

portfolioMonitoringUI <- function() {
  bs4TabItem(
    tabName = "portfolio_monitoring",
    fluidRow(
      column(
        12,
        uiOutput("portfolio_source_status"),
        actionButton("portfolio_refresh", "Обновить котировки", icon = icon("rotate"))
      )
    ),
    br(),
    fluidRow(
      column(4, uiOutput("portfolio_value_box")),
      column(4, uiOutput("portfolio_pnl_box")),
      column(4, uiOutput("portfolio_growth_box"))
    ),
    fluidRow(
      bs4Card(
        title = "Позиции портфеля",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 7,
        rHandsontableOutput("portfolio_table")
      ),
      bs4Card(
        title = "Структура портфеля",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 5,
        plotlyOutput("portfolio_pie")
      )
    ),
    fluidRow(
      bs4Card(
        title = "Рост относительно даты покупки (14.09.2026)",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        plotlyOutput("portfolio_growth_plot")
      )
    )
  )
}
