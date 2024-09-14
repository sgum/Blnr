# setup_project.R

# Функция для создания директории, если она не существует
create_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE)
    message("Создана директория: ", path)
  } else {
    message("Директория уже существует: ", path)
  }
}

# Функция для создания файла, если он не существует
create_file <- function(path, content = "") {
  if (!file.exists(path)) {
    writeLines(content, con = path)
    message("Создан файл: ", path)
  } else {
    message("Файл уже существует: ", path)
  }
}

# Список директорий для создания
dirs_to_create <- c(
  "R",
  "R/modules",
  "data",
  "data/raw",
  "data/processed",
  "www",
  "scripts",
  "tests",
  "docs"
)

# Создание директорий
for (dir in dirs_to_create) {
  create_dir(dir)
}

# Список файлов для создания с базовым содержанием
files_to_create <- list(
  "R/functions.R" = "# Здесь будут ваши пользовательские функции\n",
  "scripts/download_data.R" = "# Скрипт для загрузки данных\n",
  "scripts/preprocess_data.R" = "# Скрипт для обработки данных\n",
  "tests/test_functions.R" = "# Тесты для ваших функций\n",
  "docs/README.md" = "# Документация проекта\n",
  "app.R" = "# Основной файл Shiny-приложения\n",
  "_targets.R" = "# Конфигурация для пакета targets\n",
  ".gitignore" = "# Файлы и папки, игнорируемые Git\n",
  "DESCRIPTION" = paste(
    "Package: Blnr",
    "Title: Blnr Project",
    "Version: 0.1.0",
    "Authors@R: person(\"Ваше Имя\", email = \"your.email@example.com\", role = c(\"aut\", \"cre\"))",
    "Description: Описание вашего проекта.",
    "License: MIT",
    "Encoding: UTF-8",
    "LazyData: true",
    sep = "\n"
  )
)

# Создание файлов с содержимым
for (file_path in names(files_to_create)) {
  create_file(file_path, files_to_create[[file_path]])
}

# Инициализация renv (опционально)
if (!file.exists("renv.lock")) {
  message("Инициализация renv...")
  install.packages("renv") # Установка пакета, если не установлен
  renv::init(bare = TRUE)
}

# Инициализация git-репозитория (опционально)
if (!file.exists(".git")) {
  message("Инициализация git-репозитория...")
  system("git init")
}
