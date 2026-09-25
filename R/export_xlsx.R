# R/export_xlsx.R
#
# Выгрузка ретроспективных рядов в ТИПОВОМ формате мониторинга владельца
# (эталон — «OptionActual <дата>.xlsx»). Формат разобран по эталону
# 25.09.2026 и воспроизводится как есть, а не «по мотивам»: файл открывается
# теми же формулами и сценариями, что и рабочая таблица.
#
# Структура эталона:
#   * лист «Реестр» — указатель: номер листа, тикер, имя компании, тип блока,
#     ссылка, идентификатор страницы, глубина истории, частотность, статус;
#   * по листу на инструмент, имя листа — его номер начиная с 2. Строка 1 —
#     шапка указателя, строка 2 — значения для этого инструмента, строка 3 —
#     заголовки блока данных, дальше сами данные;
#   * лист «СборкаАкции» — все инструменты в одной таблице
#     (Date, Open, High, Low, Close, Volume, Symbol).
#
# Опционные листы («Сборка») здесь не формируются: цепочки с marketdata.app на
# текущем тарифе не тянутся, и рисовать пустой лист с заголовками значит выдать
# отсутствие данных за данные.

library(openxlsx)

# Заголовки листа «Реестр» — дословно из эталона, включая переносы строк.
MONITORING_REGISTRY_HEADER <- c(
  "Имя листа", "Тикер", "Имя Компании", "OPTONCHAIN", "Ссылки на страницу",
  "ID страницы", "Дней назад \nДля исторических данных",
  "Частотность\nminutely\nhourly\ndaily", "Статус обновления"
)

MONITORING_DATA_HEADER <- c("Date", "Open", "High", "Low", "Close", "Volume", "Simbol")

# Собирает книгу мониторинга по инструментам реестра наблюдения.
# `series_fn` отдаёт ряд по тикеру — подменяется в тестах, чтобы проверка не
# зависела ни от сети, ни от содержимого хранилища.
build_monitoring_workbook <- function(tickers = watchlist_active()$ticker,
                                      days = WATCHLIST_RETRO_DAYS,
                                      as_of = Sys.Date(),
                                      series_fn = function(tk) md_candles(tk, days = days)) {
  wb <- openxlsx::createWorkbook()
  reg <- data.table::data.table()
  all_rows <- list()
  sheet_no <- 1L

  for (tk in tickers) {
    cnd <- series_fn(tk)
    if (!is.data.frame(cnd) || nrow(cnd) == 0) {
      # Инструмент без ряда попадает в «Реестр» со статусом «нет данных», но
      # своего листа не получает: пустой лист с заголовками читается как
      # данные, которых нет.
      reg <- rbind(reg, data.table::data.table(
        sheet = NA_character_, ticker = tk,
        name = watchlist_name(tk), kind = "STOCKDATA",
        link = NA_character_, page_id = NA_integer_,
        days = days, freq = "daily", status = "нет данных"), fill = TRUE)
      next
    }
    cnd <- cnd[date <= as.Date(as_of)]
    if (nrow(cnd) == 0) next
    sheet_no <- sheet_no + 1L
    sheet <- as.character(sheet_no)

    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, t(MONITORING_REGISTRY_HEADER),
                        startRow = 1, colNames = FALSE)
    openxlsx::writeData(wb, sheet, t(c(
      sheet_no, tk, watchlist_name(tk), "STOCKDATA",
      paste("Перейти на", sheet_no, "лист"), NA, days, "daily", "есть данные")),
      startRow = 2, colNames = FALSE)
    openxlsx::writeData(wb, sheet, t(MONITORING_DATA_HEADER),
                        startRow = 3, colNames = FALSE)
    body <- data.table::data.table(
      Date = cnd$date, Open = cnd$open, High = cnd$high, Low = cnd$low,
      Close = cnd$close, Volume = cnd$volume, Simbol = tk)
    openxlsx::writeData(wb, sheet, body, startRow = 4, colNames = FALSE)
    openxlsx::setColWidths(wb, sheet, 1:7, c(11, 10, 10, 10, 10, 14, 9))

    all_rows[[tk]] <- data.table::data.table(
      Date = cnd$date, Open = cnd$open, High = cnd$high, Low = cnd$low,
      Close = cnd$close, Volume = cnd$volume, Symbol = tk)
    reg <- rbind(reg, data.table::data.table(
      sheet = sheet, ticker = tk, name = watchlist_name(tk), kind = "STOCKDATA",
      link = paste("Перейти на", sheet_no, "лист"), page_id = NA_integer_,
      days = days, freq = "daily", status = "есть данные"), fill = TRUE)
  }

  # «Реестр» первым листом — как в эталоне.
  openxlsx::addWorksheet(wb, "Реестр")
  openxlsx::worksheetOrder(wb) <- c(length(names(wb)), seq_len(length(names(wb)) - 1L))
  openxlsx::writeData(wb, "Реестр", t(MONITORING_REGISTRY_HEADER),
                      startRow = 1, colNames = FALSE)
  if (nrow(reg) > 0) {
    openxlsx::writeData(wb, "Реестр", reg, startRow = 2, colNames = FALSE)
  }
  openxlsx::setColWidths(wb, "Реестр", 1:9, c(10, 9, 22, 13, 22, 12, 14, 14, 18))

  openxlsx::addWorksheet(wb, "СборкаАкции")
  if (length(all_rows) > 0) {
    comb <- data.table::rbindlist(all_rows)
    data.table::setorder(comb, Symbol, Date)
    openxlsx::writeData(wb, "СборкаАкции", comb, startRow = 1, colNames = TRUE)
    openxlsx::setColWidths(wb, "СборкаАкции", 1:7, c(11, 10, 10, 10, 10, 14, 9))
  }
  wb
}

# Имя компании по тикеру для листа «Реестр».
watchlist_name <- function(ticker) {
  i <- match(toupper(ticker), toupper(WATCHLIST$ticker))
  if (is.na(i)) return(as.character(ticker))
  WATCHLIST$name_ru[i]
}
