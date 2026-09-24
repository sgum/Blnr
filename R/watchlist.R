# R/watchlist.R
#
# Реестр наблюдаемых инструментов — единый источник правды для всего стенда:
# market-data (какие тикеры тянуть), прогноз (как связать строку модели с
# тикером) и таблица портфеля.
#
# Состав и тикеры взяты из рабочей таблицы пользователя (реестр листов
# market-data: OPTIONCHAIN/STOCKDATA/INDEXDATA) и совпадают один в один с 26
# строками блока `mean` листа Q_mean_var внешней модели. Золото и нефть
# наблюдаются через ETF (GLD/USO) — так же, как в таблице; индексы идут
# отдельным эндпоинтом marketdata.app (/v1/indices/...), у них нет объёма.
#
# `name_model` — подпись строки ровно как в Q_mean_var. Сопоставление
# прогноза с тикером идёт ТОЧНЫМ совпадением по этому полю, а не поиском
# подстроки: подстрочные алиасы уже давали ложные срабатывания («ge» ловил
# «General Motors»), см. docs/dev.md.

WATCHLIST <- data.table::data.table(
  ticker = c("AAPL", "NVDA", "MSFT", "TSLA", "GOOG", "AMZN", "AMD", "META",
             "NFLX", "INTC", "IBM", "GM", "GE", "BP", "SHEL", "CVX", "GS",
             "MS", "JPM", "SAP", "F", "GLD", "USO", "SPX", "DJI", "IXIC"),
  name_model = c("Apple", "Nvidia", "Microsoft", "Tesla", "Google", "Amazon",
                 "AMD", "Meta", "Netflix", "Intel", "IBM", "General Motors",
                 "General Electric", "BP", "Shell", "Chevron", "Goldman Sachs",
                 "Morgan Stanley Bank", "JPMorgan Chase & Co", "SAP", "Ford",
                 "Gold", "Oil", "S&P 500", "Dow 30", "Nasdaq"),
  name_ru = c("Apple", "NVIDIA", "Microsoft", "Tesla", "Alphabet", "Amazon",
              "AMD", "Meta", "Netflix", "Intel", "IBM", "General Motors",
              "General Electric", "BP", "Shell", "Chevron", "Goldman Sachs",
              "Morgan Stanley", "JPMorgan Chase", "SAP", "Ford",
              "Золото (ETF GLD)", "Нефть WTI (ETF USO)", "S&P 500",
              "Dow Jones 30", "Nasdaq Composite"),
  # stock — /v1/stocks/... (сюда же ETF GLD/USO); index — /v1/indices/...
  type = c(rep("stock", 23), rep("index", 3))
)

# Глубина ретроспективы по умолчанию — 120 торговых дней: столько же берёт
# твоя таблица market-data и внешняя модель, чтобы дашборд и прогноз считались
# на одном окне.
WATCHLIST_RETRO_DAYS <- as.integer(Sys.getenv("BLNR_RETRO_DAYS", unset = "120"))

# Какие тикеры показывать на экране. По умолчанию — весь реестр; переменной
# BLNR_WATCHLIST можно сузить ("GS,GE,AMD,GOOG,NVDA").
watchlist_active <- function() {
  raw <- Sys.getenv("BLNR_WATCHLIST", unset = "")
  if (!nzchar(raw)) return(data.table::copy(WATCHLIST))
  want <- toupper(trimws(strsplit(raw, "[,;]")[[1]]))
  WATCHLIST[toupper(ticker) %in% want]
}

# Тикер по подписи строки модели (точное совпадение, без учёта регистра и
# краевых пробелов). NA, если такой строки в реестре нет.
ticker_by_model_name <- function(name) {
  key <- tolower(trimws(as.character(name)))
  idx <- match(key, tolower(trimws(WATCHLIST$name_model)))
  WATCHLIST$ticker[idx]
}

# Тип инструмента (stock|index) по тикеру — нужен для выбора эндпоинта.
instrument_type <- function(ticker) {
  idx <- match(toupper(trimws(as.character(ticker))), toupper(WATCHLIST$ticker))
  out <- WATCHLIST$type[idx]
  out[is.na(out)] <- "stock"
  out
}
