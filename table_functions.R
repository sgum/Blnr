# Функция для создания адаптивной HTML-таблицы с поддержкой кириллицы
create_responsive_table <- function(data, caption = "", col_names = NULL, align = NULL) {
  # Проверяем наличие данных
  if (is.null(data) || nrow(data) == 0) {
    return("<p>Нет данных для отображения</p>")
  }
  
  # Если имена столбцов не указаны, используем имена из данных
  if (is.null(col_names)) {
    col_names <- names(data)
  }
  
  # Если выравнивание не указано, используем центрирование для всех столбцов
  if (is.null(align)) {
    align <- rep("center", ncol(data))
  } else if (length(align) < ncol(data)) {
    # Если выравнивание указано не для всех столбцов, дополняем центрированием
    align <- c(align, rep("center", ncol(data) - length(align)))
  }
  
  # Начинаем формировать HTML-код
  html <- c()
  
  # Добавляем обертку для адаптивности
  html <- c(html, "
<div class="table-responsive">
")
  
  # Добавляем заголовок таблицы, если он указан
  if (caption != "") {
    html <- c(html, paste0("<h3 style="text-align: center; font-weight: bold; margin-bottom: 20px;">", caption, "</h3>"))
  }
  
  # Начинаем таблицу
  html <- c(html, "<table class="table table-striped table-hover" style="width: 100%; margin-bottom: 30px; font-family: 'Panton', sans-serif;">")
  
  # Добавляем заголовок таблицы
  html <- c(html, "<thead style="background-color: #337ab7; color: white;">")
  html <- c(html, "<tr>")
  
  # Добавляем заголовки столбцов
  for (i in seq_along(col_names)) {
    html <- c(html, paste0("<th style="text-align: ", align[i], ";">", col_names[i], "</th>"))
  }
  
  html <- c(html, "</tr>")
  html <- c(html, "</thead>")
  html <- c(html, "<tbody>")
  
  # Добавляем строки данных
  for (i in seq_len(nrow(data))) {
    html <- c(html, "<tr>")
    
    # Добавляем ячейки
    for (j in seq_len(ncol(data))) {
      html <- c(html, paste0("<td style="text-align: ", align[j], ";">", data[i, j], "</td>"))
    }
    
    html <- c(html, "</tr>")
  }
  
  # Закрываем таблицу
  html <- c(html, "</tbody>")
  html <- c(html, "</table>")
  html <- c(html, "</div>")
  
  # Объединяем все части HTML-кода
  return(paste(html, collapse = "
"))
}

# Функция для загрузки данных из CSV-файла или использования резервных данных
load_data_or_fallback <- function(csv_path, fallback_data) {
  if (file.exists(csv_path)) {
    # Загружаем данные из CSV с указанием кодировки UTF-8
    data <- read.csv(csv_path, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
    return(data)
  } else {
    # Возвращаем резервные данные
    return(fallback_data)
  }
}