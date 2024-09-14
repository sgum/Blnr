# ui/sidebar_ui.R

sidebarUI <- function() {
  return(
    bs4DashSidebar(
      minified = TRUE,
      width = "350px",
      imageOutput("logo", height = "70px"),
      skin = "light",
      status = "primary",
      elevation = 4,
      bs4SidebarMenu(
        id = "sidebarMenu",
        tags$style(HTML('.sidebar{font-family: Panton;
                                 color: black}
                        a:hover{ background: #D0D0D0 }
                        }
                      ')), # .active a{ background: #817CB1
        setSliderColor(c(rep("orange",100)), c(1:100)),
        chooseSliderSkin("Flat"),
        
        bs4SidebarMenuItem(
          "1. Котировка", tabName = "tab1", icon = icon("download"),
          fluidRow(
            column(12,  selectInput("ticker", label = "", selected = tickers[1], choices = tickers, multiple = TRUE)),
            column(12, sliderInput("date_range", "Диапазон дат",
                                   min = Sys.Date() - 365, max = Sys.Date(),
                                   value = c(Sys.Date() - 120, Sys.Date()),
                                   timeFormat = "%Y-%m-%d")),
            column(12, actionButton("download_data", "", icon = icon("download"), title = "Скачать данные"))
          )
        ),
        bs4SidebarMenuItem("Загрузка прогнозов", tabName = "tab2", icon = icon("upload"))
      )
    )
  )
}
