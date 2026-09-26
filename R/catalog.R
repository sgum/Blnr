# R/catalog.R
#
# Глобальный справочник инструментов: что ВООБЩЕ можно взять на счёт.
#
# Зачем он появился. Добавить бумагу в наблюдение раньше можно было только
# набрав тикер руками — и попав в него с первого раза. Есть ли она в аккаунте
# и как называется, выяснялось уже после добавления либо не выяснялось вовсе:
# опечатка садилась в реестр и ронять ночную загрузку начинала следующей
# ночью. Справочник отвечает на «что вообще есть» ДО добавления.
#
# Источник — биржевые списки Exante (/md/3.0/exchanges/<биржа>): это те бумаги,
# которые действительно доступны счёту, а не абстрактный список тикеров мира.
# Тянутся отдельным заданием (scripts/fetch_catalog.R): состав листингов меняется
# раз в недели, и привязывать его к ночной загрузке цен незачем — у источника
# другой ритм.
#
# ВАЖНО: справочник Exante и источник цен marketdata.app — РАЗНЫЕ источники.
# Наличие бумаги в справочнике не значит, что по ней будут ряды, поэтому
# watchlist_add() всё равно проверяет тикер у источника цен одним запросом.

# Биржи, с которых берём состав. Американские площадки, на которых стоит счёт;
# добавить ещё — дописать сюда, выгрузка по каждой идёт отдельным запросом.
BLNR_CATALOG_EXCHANGES <- strsplit(
  Sys.getenv("BLNR_CATALOG_EXCHANGES", unset = "NASDAQ,NYSE,ARCA,AMEX,BATS"),
  "[,;]")[[1]]

# Выгрузка одной биржи. Путь именно /md/3.0/exchanges/<id> — вариант
# .../symbols отвечает 404 (проверено 26.09.2026 на живом контуре).
catalog_fetch_exchange <- function(exchange) {
  res <- exante_get(sprintf("/md/3.0/exchanges/%s", exchange))
  if (!is.null(res$error)) {
    return(list(ok = FALSE, message = paste0(exchange, ": ", res$error),
                data = empty_catalog()))
  }
  if (!is.list(res) || length(res) == 0) {
    return(list(ok = FALSE, message = paste0(exchange, ": пустой ответ"),
                data = empty_catalog()))
  }
  pick <- function(x, field) {
    v <- x[[field]]
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)[1]
  }
  dt <- data.table::rbindlist(lapply(res, function(x) list(
    ticker    = pick(x, "ticker"),
    symbol_id = pick(x, "symbolId"),
    name      = pick(x, "description") ,
    exchange  = pick(x, "exchange"),
    currency  = pick(x, "currency"),
    country   = pick(x, "country"),
    type      = pick(x, "symbolType")
  )), use.names = TRUE, fill = TRUE)
  # Держим только то, что имеет смысл добавлять в наблюдение: акции и фонды.
  # Опционы, фьючерсы и CFD стенд не считает — их появление в справочнике
  # выглядело бы предложением купить то, чего портфель не умеет учитывать.
  dt <- dt[type %in% c("STOCK", "FUND") & !is.na(ticker) & nzchar(ticker)]
  if (nrow(dt) == 0) {
    return(list(ok = FALSE, message = paste0(exchange, ": ни одной акции в ответе"),
                data = empty_catalog()))
  }
  dt[, type := NULL]
  list(ok = TRUE, message = sprintf("%s: %d", exchange, nrow(dt)), data = dt[])
}

# Полная пересборка справочника. Возвращает list(ok, message, data).
# «Всё или ничего» по каждой бирже: если площадка не ответила, прежний файл
# НЕ перезаписывается — половина справочника неотличима от целого.
catalog_fetch <- function(exchanges = BLNR_CATALOG_EXCHANGES) {
  parts <- list(); notes <- character()
  for (x in trimws(exchanges)) {
    r <- catalog_fetch_exchange(x)
    notes <- c(notes, r$message)
    if (!isTRUE(r$ok)) return(list(ok = FALSE, message = paste(notes, collapse = "; "),
                                   data = empty_catalog()))
    parts[[x]] <- r$data
  }
  all <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  all <- unique(all, by = c("ticker", "exchange"))
  list(ok = TRUE, message = paste(notes, collapse = "; "), data = all[])
}

# Точное совпадение по тикеру — нужно, чтобы подставить название бумаги при
# добавлении и показать, на какой она бирже.
catalog_lookup <- function(ticker) {
  # Аргумент кладём в отдельную переменную: внутри data.table имя `ticker`
  # означает СТОЛБЕЦ, и сравнение столбца с самим собой находило бы всё.
  want <- toupper(trimws(as.character(ticker)[1]))
  cat_dt <- store_read_catalog()
  if (nrow(cat_dt) == 0) return(empty_catalog())
  utils::head(cat_dt[toupper(ticker) == want], 5)
}

# Поиск по справочнику для экрана. Совпадение по началу тикера идёт первым:
# человек, набравший "QQQ", хочет сам QQQ, а не двадцать фондов со словом
# "Nasdaq" в названии. Пустой запрос ничего не возвращает — выводить десять
# тысяч строк незачем.
catalog_search <- function(query, limit = 25L) {
  q <- toupper(trimws(as.character(query %||% "")))
  if (nchar(q) < 1) return(empty_catalog())
  cat_dt <- store_read_catalog()
  if (nrow(cat_dt) == 0) return(empty_catalog())
  tk <- toupper(cat_dt$ticker); nm <- toupper(cat_dt$name)
  rank <- ifelse(tk == q, 0L,
          ifelse(startsWith(tk, q), 1L,
          ifelse(grepl(q, tk, fixed = TRUE), 2L,
          ifelse(grepl(q, nm, fixed = TRUE), 3L, NA_integer_))))
  hit <- which(!is.na(rank))
  if (length(hit) == 0) return(empty_catalog())
  out <- cat_dt[hit][order(rank[hit], nchar(ticker), ticker)]
  utils::head(out, limit)
}

# Строка о состоянии справочника для шапки виджета: без неё непонятно, пуст он
# потому, что ничего не нашлось, или потому, что задание ни разу не прошло.
catalog_status_text <- function() {
  n <- nrow(store_read_catalog())
  if (n == 0) return("справочник не загружен")
  up <- store_catalog_updated()
  sprintf("%s инстр., обновлён %s", format(n, big.mark = " "),
          if (is.null(up)) "—" else format(as.Date(up), "%d.%m.%Y"))
}
