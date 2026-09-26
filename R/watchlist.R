# R/watchlist.R
#
# Реестр наблюдаемых инструментов — единый источник правды для всего стенда:
# market-data (какие тикеры тянуть), прогноз (как связать строку модели с
# тикером) и таблица портфеля.
#
# Состав и тикеры взяты из рабочей таблицы пользователя (реестр листов
# market-data: OPTIONCHAIN/STOCKDATA). Золото и нефть наблюдаются через ETF
# (GLD/USO) — так же, как в таблице.
#
# ИНДЕКСОВ ЗДЕСЬ НЕТ СОЗНАТЕЛЬНО. В блоке `mean` модельного воркбука 26 строк,
# три из них — S&P 500, Dow 30 и Nasdaq. marketdata.app на нашем тарифе не
# отдаёт индексные данные вовсе: эндпоинт /v1/indices/ отвечает 404
# {"s":"no_data"} по всем тикерам, включая VIX (проверено 25.09.2026). Держать
# в реестре то, чего источник не даёт, — значит каждую ночь ронять загрузку на
# заведомо недостижимом, и красный прогон перестаёт что-либо значить.
# Строки прогноза по индексам просто не сопоставляются с тикером и
# отбрасываются при разборе файла — на расчёт портфеля они не влияют.
# Если индексные данные появятся, вернуть их сюда вместе с выбором эндпоинта
# в R/marketdata.R либо завести через ETF (SPY / DIA / QQQ).
#
# GOOGL добавлен отдельной строкой с пустым name_model: брокерский счёт держит
# именно класс A (GOOGL.NASDAQ), а в модельном воркбуке строка «Google» одна и
# сопоставлена с GOOG. Это РАЗНЫЕ бумаги с разной ценой, поэтому подменять
# одну другой нельзя — прогноз по GOOGL останется пустым, пока в модели не
# появится своя строка.
#
# `name_model` — подпись строки ровно как в Q_mean_var. Сопоставление
# прогноза с тикером идёт ТОЧНЫМ совпадением по этому полю, а не поиском
# подстроки: подстрочные алиасы уже давали ложные срабатывания («ge» ловил
# «General Motors»), см. docs/dev.md.

WATCHLIST <- data.table::data.table(
  ticker = c("AAPL", "NVDA", "MSFT", "TSLA", "GOOG", "AMZN", "AMD", "META",
             "NFLX", "INTC", "IBM", "GM", "GE", "BP", "SHEL", "CVX", "GS",
             "MS", "JPM", "SAP", "F", "GLD", "USO", "GOOGL"),
  name_model = c("Apple", "Nvidia", "Microsoft", "Tesla", "Google", "Amazon",
                 "AMD", "Meta", "Netflix", "Intel", "IBM", "General Motors",
                 "General Electric", "BP", "Shell", "Chevron", "Goldman Sachs",
                 "Morgan Stanley Bank", "JPMorgan Chase & Co", "SAP", "Ford",
                 "Gold", "Oil", NA_character_),
  name_ru = c("Apple", "NVIDIA", "Microsoft", "Tesla", "Alphabet", "Amazon",
              "AMD", "Meta", "Netflix", "Intel", "IBM", "General Motors",
              "General Electric", "BP", "Shell", "Chevron", "Goldman Sachs",
              "Morgan Stanley", "JPMorgan Chase", "SAP", "Ford",
              "Золото (ETF GLD)", "Нефть WTI (ETF USO)",
              "Alphabet класс A")
)

# Глубина ретроспективы по умолчанию — 120 торговых дней: столько же берёт
# твоя таблица market-data и внешняя модель, чтобы дашборд и прогноз считались
# на одном окне.
WATCHLIST_RETRO_DAYS <- as.integer(Sys.getenv("BLNR_RETRO_DAYS", unset = "120"))

# Длина шкалы времени на экране — сколько торговых сессий ретроспективы лежит
# слева от фактической даты.
BLNR_TIMELINE_DAYS <- as.integer(Sys.getenv("BLNR_TIMELINE_DAYS", unset = "150"))

# Насколько далеко вперёд от последней свечи рисуется прогнозная кривая.
# Горизонт модели — до 2028 года; целиком он сжал бы свечи в полоску.
BLNR_FORECAST_HORIZON_DAYS <- as.integer(
  Sys.getenv("BLNR_FORECAST_HORIZON_DAYS", unset = "45"))

# Сколько дат прогноза показывать ПРАВЕЕ фактической даты на шкале времени.
# Горизонт модели — до 2028 года; вся она на шкале сделала бы прошлое
# нечитаемым, а ради него шкала и нужна.
BLNR_FUTURE_DAYS <- as.integer(Sys.getenv("BLNR_FUTURE_DAYS", unset = "60"))

# Реестр наблюдения = ИСХОДНЫЙ набор выше + то, что добавлено с экрана из
# глобального справочника. Хранилище даёт только состояние: включён инструмент
# в мониторинг или выключен.
#
# ДВА РАЗНЫХ ДЕЙСТВИЯ, которые раньше были одним. Бумага из рабочей таблицы
# (source = "seed") — часть исходного состава: по ней есть строка прогнозной
# модели и, возможно, история сделок, поэтому её можно только ВЫКЛЮЧИТЬ из
# мониторинга, но не выкинуть из реестра. Бумага, добавленная вручную
# (source = "added"), — ошибочно взятая или уже ненужная, её можно удалить
# насовсем. Прежняя версия удаляла и то и другое, и выброшенная seed-бумага
# исчезала из стенда, оставляя строку модели без тикера.
watchlist_all <- function() {
  seed <- data.table::copy(WATCHLIST)
  seed[, `:=`(source = "seed", active = TRUE, added_at = as.Date(NA))]
  st <- tryCatch(store_read_watchlist(), error = function(e) NULL)
  if (is.null(st) || nrow(st) == 0) return(seed[])

  # Состояние из хранилища накладывается на исходный набор ПО ТИКЕРУ. Так
  # растущий seed не теряется (хранилище, записанное до его пополнения, не
  # знает о новых бумагах), а подписи и name_model всегда берутся из кода —
  # они там правятся вместе с разбором модели.
  idx <- match(toupper(seed$ticker), toupper(st$ticker))
  seed[!is.na(idx), active := st$active[idx[!is.na(idx)]]]
  extra <- st[!(toupper(ticker) %in% toupper(seed$ticker))]
  if (nrow(extra) == 0) return(seed[])
  extra[, source := "added"]
  data.table::rbindlist(list(seed, extra), use.names = TRUE, fill = TRUE)[]
}

# Что реально идёт в мониторинг: ночная загрузка, таблица портфеля, выбор
# графика. Выключенное сюда не попадает — ряды по нему не тянутся.
# BLNR_WATCHLIST дополнительно сужает набор ("GS,GE,AMD") для отладки.
watchlist_active <- function() {
  base <- watchlist_all()[is_watched(active)]
  raw <- Sys.getenv("BLNR_WATCHLIST", unset = "")
  if (!nzchar(raw)) return(base)
  want <- toupper(trimws(strsplit(raw, "[,;]")[[1]]))
  base[toupper(ticker) %in% want]
}

# Флаг наблюдения по столбцу. NA — это «состояние неизвестно», а не
# «выключено»: инструмент, попавший в реестр с пустым флагом, должен остаться
# под наблюдением, иначе один битый CSV молча снимает бумагу с мониторинга.
is_watched <- function(x) is.na(x) | as.logical(x)

# Включить или выключить инструмент в мониторинге. Выключить бумагу, которая
# СЕЙЧАС в портфеле, нельзя: без её ряда история и стоимость портфеля считались
# бы по дыре, и падение цены выглядело бы как «нет данных».
watchlist_set_active <- function(ticker, on, held = character()) {
  tk <- ticker_norm(ticker)
  cur <- watchlist_all()
  i <- match(tk, toupper(cur$ticker))
  if (is.na(i)) return(list(ok = FALSE, message = paste0(tk, " в реестре не значится.")))
  on <- isTRUE(on)
  if (!on && tk %in% toupper(held)) {
    return(list(ok = FALSE, message = paste0(
      tk, " сейчас в портфеле: пока бумага на счёте, её ряд нужен истории.")))
  }
  if (!on && sum(is_watched(cur$active)) <= 1L) {
    return(list(ok = FALSE, message = "Нельзя выключить последний инструмент мониторинга."))
  }
  cur[i, active := on]
  store_write_watchlist(cur)
  list(ok = TRUE, message = paste0(
    tk, if (on) " включён в мониторинг." else
      " выключен из мониторинга. Ряд цен и история сохранены."))
}

# Добавить инструмент из глобального справочника. Перед добавлением он
# ПРОВЕРЯЕТСЯ у источника цен: справочник Exante говорит, что бумагу можно
# купить, но ряды тянет marketdata.app — это разные источники, и реестр с
# тикером, которого нет у второго, ронял бы ночную загрузку каждую ночь
# (она принципиально «всё или ничего»). Возвращает list(ok, message).
watchlist_add <- function(ticker, name_ru = NULL, probe = TRUE) {
  tk <- ticker_norm(ticker)
  if (!nzchar(tk) || !grepl("^[A-Z0-9.-]{1,12}$", tk)) {
    return(list(ok = FALSE, message = "Тикер должен быть из латиницы, цифр, точки или дефиса."))
  }
  cur <- watchlist_all()
  i <- match(tk, toupper(cur$ticker))
  if (!is.na(i)) {
    # Бумага уже известна стенду. Это не ошибка: выключенную надо просто
    # включить обратно, а не заводить второй строкой.
    if (!is_watched(cur$active[i])) return(watchlist_set_active(tk, TRUE))
    return(list(ok = FALSE, message = paste0(tk, " уже под наблюдением.")))
  }
  if (isTRUE(probe)) {
    pr <- md_probe_ticker(tk)
    if (!isTRUE(pr$ok)) {
      return(list(ok = FALSE, message = paste0("Источник цен не знает ", tk, ": ", pr$message)))
    }
  }
  if (is.null(name_ru) || !nzchar(trimws(name_ru))) {
    hit <- catalog_lookup(tk)
    name_ru <- if (nrow(hit) > 0) hit$name[1] else tk
  }
  add <- data.table::data.table(
    ticker = tk, name_model = NA_character_, name_ru = trimws(name_ru),
    source = "added", active = TRUE, added_at = Sys.Date()
  )
  store_write_watchlist(data.table::rbindlist(list(cur, add), use.names = TRUE,
                                              fill = TRUE))
  list(ok = TRUE, message = paste0(tk, " добавлен в наблюдение. Ряд цен появится после ночной загрузки."))
}

# Приведение тикера к виду, в котором он годится и источнику цен, и ИМЕНИ
# ФАЙЛА. Классы акций Exante пишет через слэш (BRK/A, BF/B — 28 бумаг из
# 11758), а слэш в тикере — это разделитель пути: ряд такой бумаги ушёл бы в
# candles/BRK/A.csv, то есть в несуществующий подкаталог, и запись упала бы
# уже ночью. marketdata.app считает BRK.B, BRK/B и BRK-B ОДНОЙ бумагой
# (проверено 26.09.2026 сравнением цен: все три дают 505.18, а BRK.A — 759200,
# то есть источник действительно различает классы, а разделитель — нет).
# Поэтому приводим к точке: она безопасна в имени файла.
ticker_norm <- function(ticker) {
  gsub("[/\\\\]", ".", toupper(trimws(as.character(ticker)[1])))
}

# Удалить инструмент из реестра НАСОВСЕМ. Только для добавленных вручную:
# исходный состав из рабочей таблицы удалять нельзя, его выключают.
# Ряд цен в хранилище не трогаем: он нужен истории портфеля, если бумага
# когда-то покупалась.
watchlist_remove <- function(ticker) {
  tk <- ticker_norm(ticker)
  cur <- watchlist_all()
  i <- match(tk, toupper(cur$ticker))
  if (is.na(i)) {
    return(list(ok = FALSE, message = paste0(tk, " в реестре не значится.")))
  }
  if (!identical(cur$source[i], "added")) {
    return(list(ok = FALSE, message = paste0(
      tk, " — из исходного состава наблюдения: его можно выключить из ",
      "мониторинга, но не удалить.")))
  }
  store_write_watchlist(cur[-i])
  list(ok = TRUE, message = paste0(tk, " удалён из реестра. Ряд цен сохранён."))
}

# Тикер по подписи строки модели (точное совпадение, без учёта регистра и
# краевых пробелов). NA, если такой строки в реестре нет.
ticker_by_model_name <- function(name) {
  wl <- watchlist_all()
  key <- tolower(trimws(as.character(name)))
  idx <- match(key, tolower(trimws(wl$name_model)))
  wl$ticker[idx]
}

