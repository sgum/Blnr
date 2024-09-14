# Конфигурация для пакета targets

library(targets)
source("R/functions.R")

tar_option_set(
  packages = c("dplyr", "ggplot2", "shiny", "httr") # необходимые пакеты
)

list(
  tar_target(
    raw_data,
    download_data(), # функция для скачивания данных
    format = "file"
  ),
  tar_target(
    processed_data,
    preprocess_data(raw_data), # функция для обработки данных
    format = "file"
  )
  # Добавьте другие таргеты по мере необходимости
)
