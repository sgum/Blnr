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
# `name_model` — подпись строки ровно как в Q_mean_var. Сопоставление
# прогноза с тикером идёт ТОЧНЫМ совпадением по этому полю, а не поиском
# подстроки: подстрочные алиасы уже давали ложные срабатывания («ge» ловил
# «General Motors»), см. docs/dev.md.

WATCHLIST <- data.table::data.table(
  ticker = c("AAPL", "NVDA", "MSFT", "TSLA", "GOOG", "AMZN", "AMD", "META",
             "NFLX", "INTC", "IBM", "GM", "GE", "BP", "SHEL", "CVX", "GS",
             "MS", "JPM", "SAP", "F", "GLD", "USO"),
  name_model = c("Apple", "Nvidia", "Microsoft", "Tesla", "Google", "Amazon",
                 "AMD", "Meta", "Netflix", "Intel", "IBM", "General Motors",
                 "General Electric", "BP", "Shell", "Chevron", "Goldman Sachs",
                 "Morgan Stanley Bank", "JPMorgan Chase & Co", "SAP", "Ford",
                 "Gold", "Oil"),
  name_ru = c("Apple", "NVIDIA", "Microsoft", "Tesla", "Alphabet", "Amazon",
              "AMD", "Meta", "Netflix", "Intel", "IBM", "General Motors",
              "General Electric", "BP", "Shell", "Chevron", "Goldman Sachs",
              "Morgan Stanley", "JPMorgan Chase", "SAP", "Ford",
              "Золото (ETF GLD)", "Нефть WTI (ETF USO)")
)

# Глубина ретроспективы по умолчанию — 120 торговых дней: столько же берёт
# твоя таблица market-data и внешняя модель, чтобы дашборд и прогноз считались
# на одном окне.
WATCHLIST_RETRO_DAYS <- as.integer(Sys.getenv("BLNR_RETRO_DAYS", unset = "120"))

# Какие тикеры показывать и грузить. По умолчанию весь реестр; переменной
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

