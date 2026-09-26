#!/usr/bin/env Rscript
# scripts/fetch_catalog.R
#
# Обновление ГЛОБАЛЬНОГО СПРАВОЧНИКА инструментов из биржевых списков Exante.
# Запускается заданием Jenkins «311.blnr - catalog (справочник)».
#
# Отдельно от ночной загрузки цен сознательно: состав листингов меняется раз в
# недели, а цены — каждый торговый день. Требование конституции — расписание из
# ритма источника, а не «за компанию с соседней джобой».
#
# «Всё или ничего»: если хоть одна биржа не ответила, прежний файл остаётся на
# месте. Половина справочника выглядит исправной и молча скрывает половину
# бумаг — это хуже, чем справочник вчерашней свежести.

suppressWarnings(suppressMessages({
  library(data.table); library(httr); library(jsonlite)
}))

for (f in c("R/store.R", "R/exante_api.R", "R/catalog.R")) source(f)

cat("Справочник инструментов\n")
cat("Хранилище:", BLNR_STORE_DIR, "\n")
cat("Биржи:", paste(BLNR_CATALOG_EXCHANGES, collapse = ", "), "\n\n")

if (!exante_has_credentials()) {
  cat("ОШИБКА: нет кред Exante (EXANTE_API_ID / EXANTE_SHARED_KEY в .Renviron).\n")
  quit(status = 1)
}

was <- nrow(store_read_catalog())
res <- catalog_fetch()
cat(res$message, "\n")

if (!isTRUE(res$ok)) {
  cat("ОШИБКА: справочник не собран целиком, прежний файл не тронут.\n")
  quit(status = 1)
}

n <- nrow(res$data)
# Проверка результата, а не только «запрос прошёл». Пять тысяч строк — это
# заведомо меньше одной NASDAQ (4775 на 26.09.2026); такой ответ означает, что
# часть площадок отдала обрезанный список, а не что бумаги кончились.
if (n < 5000L) {
  cat(sprintf("ОШИБКА: в справочнике %d инструментов — подозрительно мало, не пишем.\n", n))
  quit(status = 1)
}

store_write_catalog(res$data)
cat(sprintf("\nЗаписано: %d инструментов (было %d).\n", n, was))
cat("По биржам:\n")
print(res$data[, .N, by = exchange][order(-N)])
