# R/exante_candles.R
#
# Дневные свечи из market-data брокера Exante — ИСТОЧНИК КОТИРОВОК СТЕНДА
# с 01.10.2026.
#
# ПОЧЕМУ УШЛИ С marketdata.app. Там мы были на бесплатном тарифе, у которого
# прямо заявлено «24h Delayed Stock Data»: свеча за вчерашнюю сессию
# появлялась только в середине следующего дня, и никакое расписание заходов
# это не лечило (разбор — docs/dev.md). Exante отдаёт свечу сразу после
# закрытия биржи, стоит $0 сверх брокерского счёта и считает те же цены, по
# которым брокер оценивает портфель, — то есть сверка становится точнее.
#
# ЧЕГО ЗДЕСЬ НЕТ. Объёма торгов Exante по дневным свечам не отдаёт вовсе
# (поля ответа: open, high, low, close, timestamp — проверено 01.10.2026).
# Колонка volume заполняется NA; в выгрузке мониторинга она станет пустой.
# Это сознательная плата за свежесть, а не недосмотр.
#
# ОГРАНИЧЕНИЕ ЧАСТОТЫ. Эндпоинт режет пачки запросов жёстко и непредсказуемо:
# 25 подряд дали HTTP 429, причём даже пауза в 20 секунд не спасала — два
# запроса из трёх всё равно получали отказ, а снималось ограничение примерно
# за минуту. Поэтому здесь не «пауза между запросами», а ПОВТОР С НАРАСТАЮЩЕЙ
# ЗАДЕРЖКОЙ: пауза на угад не работает, а повтор работает.

# Длительность свечи в секундах. 86400 — дневная.
EXANTE_DAY <- 86400L

# Сколько раз повторять запрос, упёршийся в ограничение частоты, и с какой
# задержкой. Шаги подобраны по замеру: ограничение снималось за ~минуту.
EXANTE_RETRIES <- as.integer(Sys.getenv("BLNR_EXANTE_RETRIES", unset = "6"))
EXANTE_BACKOFF <- c(5, 10, 20, 30, 45, 60)

exante_empty_candles <- function() {
  data.table::data.table(
    date = as.Date(character()), open = numeric(), high = numeric(),
    low = numeric(), close = numeric(), volume = numeric()
  )
}

# Запрос с повтором на 429. Возвращает то же, что exante_get, либо список с
# полем error после исчерпания попыток.
exante_get_retry <- function(path, query = list(), retries = EXANTE_RETRIES,
                             sleep_fn = Sys.sleep) {
  for (i in seq_len(max(1L, retries))) {
    res <- exante_get(path, query = query)
    throttled <- is.list(res) && identical(res$error, "exante_http_error") &&
                 isTRUE(res$status == 429)
    if (!throttled) return(res)
    if (i < retries) sleep_fn(EXANTE_BACKOFF[min(i, length(EXANTE_BACKOFF))])
  }
  list(error = "exante_rate_limited",
       message = sprintf("источник ограничил частоту, %d попыток подряд", retries))
}

# Завершены ли сутки UTC, к которым относится свеча.
#
# ЗАЧЕМ ИМЕННО UTC, а не биржевое время. Метку времени дневной свечи Exante
# ставит ПОЛНОЧЬЮ UTC, и сама свеча идёт до 24:00 UTC (замерено 01.10.2026:
# свеча `2026-09-30T00:00Z` по AMD — это торговый день 30.09, close 613.7).
# Американская сессия 09:30–16:00 ET укладывается в те же сутки UTC
# (13:30–20:00Z), поэтому дата UTC и есть дата торгового дня. Перевод метки в
# America/New_York дал бы 29.09 20:00 — то есть сдвинул бы ВСЮ историю на день
# назад, и стенд считал бы прибыль по ценам предыдущей сессии.
#
# ВАЖНО ПРО «ЗАКРЫТИЕ». Поскольку свеча идёт до 24:00 UTC, её close — это
# последняя сделка, включая постмаркет до 20:00 ET, а не официальное закрытие
# 16:00 ET. Для оценки счёта это даже ближе к брокерской, но с официальным
# закрытием marketdata.app числа расходятся примерно на треть процента
# (AMD 30.09: 613.7 против 611.76).
#
# Отсюда правило: свеча принимается, только когда её сутки UTC ЗАКОНЧИЛИСЬ.
# Раньше она ещё растёт — запись такой свечи выдала бы промежуточную цену за
# итог дня, и стенд соврал бы, не показав ни одной ошибки.
exante_day_complete <- function(d, now = Sys.time()) {
  today_utc <- as.Date(format(as.POSIXct(now), tz = "UTC", "%Y-%m-%d"))
  as.Date(d) < today_utc
}

# Дневные свечи по symbolId. Возвращает data.table, отсортированный по дате,
# БЕЗ незакрытой сессии. Пустая таблица означает отказ источника — вызывающий
# код обязан отличать её от «цена равна нулю».
exante_candles <- function(symbol_id, days = 400L, now = Sys.time(),
                           sleep_fn = Sys.sleep) {
  sid <- as.character(symbol_id)[1]
  if (!nzchar(sid) || is.na(sid)) return(exante_empty_candles())
  res <- exante_get_retry(sprintf("/md/3.0/ohlc/%s/%d", sid, EXANTE_DAY),
                          query = list(size = as.integer(days)),
                          sleep_fn = sleep_fn)
  if (!is.null(res$error) || !is.list(res) || length(res) == 0) {
    return(exante_empty_candles())
  }
  num <- function(x) { v <- suppressWarnings(as.numeric(x)); if (length(v) == 0) NA_real_ else v[1] }
  dt <- data.table::rbindlist(lapply(res, function(r) list(
    # timestamp — миллисекунды начала свечи, ПОЛНОЧЬ UTC. Дата берётся в UTC:
    # перевод в биржевую зону сдвинул бы всю историю на день назад (см.
    # exante_day_complete).
    date  = as.Date(format(as.POSIXct(num(r$timestamp) / 1000,
                                      origin = "1970-01-01", tz = "UTC"),
                           tz = "UTC", "%Y-%m-%d")),
    open  = num(r$open), high = num(r$high),
    low   = num(r$low),  close = num(r$close),
    # Объёма дневных свечей Exante не отдаёт — колонка остаётся, но пустая.
    volume = NA_real_
  )), use.names = TRUE, fill = TRUE)
  dt <- dt[!is.na(date) & is.finite(close)]
  if (nrow(dt) == 0) return(exante_empty_candles())
  dt <- dt[vapply(date, exante_day_complete, logical(1), now = now)]
  if (nrow(dt) == 0) return(exante_empty_candles())
  data.table::setorder(dt, date)
  unique(dt, by = "date")[]
}

# symbolId для тикера НА ОСНОВЕ СПРАВОЧНИКА, когда бумаги нет ни в портфеле,
# ни в истории операций (exante_symbol_for_ticker покрывает только эти два
# случая). Биржу не угадываем суффиксом: "GS.NYSE" и "GS.NASDAQ" — разные
# инструменты. Берём из справочника и, если бумага торгуется на нескольких
# площадках, предпочитаем основную листинговую.
EXANTE_EXCHANGE_RANK <- c("NASDAQ", "NYSE", "ARCA", "AMEX", "BATS")

exante_symbol_from_catalog <- function(ticker) {
  tk <- toupper(trimws(as.character(ticker)[1]))
  cat_dt <- tryCatch(store_read_catalog(), error = function(e) NULL)
  if (is.null(cat_dt) || nrow(cat_dt) == 0) return(NA_character_)
  hit <- cat_dt[toupper(ticker) == tk & !is.na(symbol_id) & nzchar(symbol_id)]
  if (nrow(hit) == 0) return(NA_character_)
  rank <- match(toupper(hit$exchange), EXANTE_EXCHANGE_RANK)
  rank[is.na(rank)] <- length(EXANTE_EXCHANGE_RANK) + 1L
  hit$symbol_id[order(rank)][1]
}

# Полное разрешение тикера в symbolId: сначала то, что уже есть на счёте и в
# истории (там биржа известна точно), потом справочник.
exante_resolve_symbol <- function(ticker, ledger = NULL, positions = NULL) {
  sid <- exante_symbol_for_ticker(ticker, ledger = ledger, positions = positions)
  if (!is.na(sid) && nzchar(sid)) return(sid)
  exante_symbol_from_catalog(ticker)
}
