# R/portfolio.R
#
# Бизнес-логика вкладки "Портфель Exante": получение текущих позиций
# (через Exante API, если настроены переменные окружения; иначе — по
# вручную заданному портфелю portfolio_holdings из global.R) и расчёт
# роста от даты покупки.

# Позиции портфеля: ticker, quantity, entry_price, current_price, source
# ("exante" | "manual"). Цена входа и текущая цена берутся напрямую из
# Exante API, если он настроен (это фактическая средняя цена исполнения и
# последняя цена по счёту); иначе используются данные из
# portfolio_holdings (реальные цены исполнения ордеров) и текущая цена
# заполняется позже котировкой с Yahoo (см. build_portfolio_metrics()).
# Реестр операций счёта. Берётся из локального хранилища; если его нет, а
# креды Exante настроены — тянется из API и сохраняется. Так стенд работает и
# на сервере без кред (на последнем сохранённом реестре, с его датой на
# экране), и на машине с кредами.
portfolio_ledger <- function(refresh = FALSE) {
  if (!refresh && store_has_ledger()) return(store_read_ledger())
  if (!exante_has_credentials()) return(empty_ledger())
  acct <- exante_primary_account()
  if (is.null(acct)) return(empty_ledger())
  led <- exante_transactions_dt(acct)
  if (!is.data.frame(led) || nrow(led) == 0) return(empty_ledger())
  store_write_ledger(led)
  led
}

# Позиции на дату. Основной путь — реестр операций: он даёт состав портфеля,
# количество и среднюю цену на ЛЮБУЮ дату, а сводка Exante знает только
# сегодняшний день, чего для ползунка времени недостаточно.
# Сверено с боевым счётом 25.09.2026: количества и средние цены совпали со
# сводкой брокера по всем позициям, расхождений ноль.
#
# Резервный путь — portfolio_holdings из global.R, когда реестра нет вовсе.
# Он помечается source = "manual", и на экране об этом сказано: зашитый список
# расходится с реальным счётом тем сильнее, чем дольше он не обновлялся.
get_portfolio_positions <- function(as_of = Sys.Date(), ledger = portfolio_ledger()) {
  if (nrow(ledger) > 0) {
    pos <- ledger_positions_at(ledger, as_of)
    pos <- pos[quantity > 0]
    if (nrow(pos) > 0) {
      return(data.table::data.table(
        ticker        = pos$ticker,
        quantity      = pos$quantity,
        entry_price   = pos$avg_price,
        current_price = NA_real_,
        purchase_date = pos$first_date,
        source        = "exante"
      ))
    }
    # Реестр есть, позиций на эту дату нет — это ответ, а не отсутствие данных.
    return(data.table::data.table(
      ticker = character(), quantity = numeric(), entry_price = numeric(),
      current_price = numeric(), purchase_date = as.Date(character()),
      source = character()
    ))
  }

  dt <- data.table::copy(portfolio_holdings)
  dt[, current_price := NA_real_]
  dt[, source := "manual"]
  dt[, .(ticker, quantity, entry_price, current_price, purchase_date, source)]
}

# Источник котировок — marketdata.app (R/marketdata.R), НЕ Yahoo.
# Yahoo с сервера приложений (Petr) отдаёт 401 на каждый запрос: «фактическая»
# половина дашборда молча оказывалась пустой ($0 / −100%), при этом стенд
# выглядел исправным. См. docs/dev.md.

# Цена закрытия тикера на дату (или ближайший предыдущий торговый день) —
# точка входа для расчёта роста.
get_close_on_date <- function(ticker, date) {
  md_close_on_date(ticker, date)
}

# Последняя доступная цена тикера.
get_last_close <- function(ticker) {
  md_last_price(ticker)
}

# Полная таблица метрик портфеля: количество, цена входа, текущая цена,
# стоимость, абсолютный рост (%), вес в портфеле, P&L. Цена входа/текущая
# цена берутся из positions (Exante API или portfolio_holdings), когда
# известны; недостающие значения досчитываются через marketdata.app.
#
# Дополнительно считается growth_from_base_pct — рост от base_date
# (по умолчанию FORECAST_BASELINE_DATE, 11.09.2026), а не от цены покупки:
# именно от этой даты считается прогноз в файле пользователя (см.
# R/forecast.R), так что сравнивать факт с прогнозом нужно на одной базе.
build_portfolio_metrics <- function(as_of = Sys.Date(),
                                     ledger = portfolio_ledger(),
                                     positions = get_portfolio_positions(as_of, ledger),
                                     entry_date = min(portfolio_holdings$purchase_date),
                                     base_date = FORECAST_BASELINE_DATE) {
  as_of <- as.Date(as_of)
  dt <- data.table::copy(positions)
  if (nrow(dt) == 0) {
    # Позиций на эту дату нет. Возвращаем пустую таблицу нужной схемы, а не
    # NULL: вызывающий код обязан отличать «портфеля не было» от «данные не
    # пришли», и то и другое здесь выражается явно.
    dt <- data.table::data.table(
      ticker = character(), quantity = numeric(), entry_price = numeric(),
      current_price = numeric(), purchase_date = as.Date(character()),
      source = character(), price_at = numeric(), price_prev = numeric(),
      session = as.Date(character()), held = logical(), quantity_at = numeric(),
      base_price = numeric(), entry_value = numeric(), current_value = numeric(),
      prev_value = numeric(), growth_pct = numeric(),
      growth_from_base_pct = numeric(), day_change_pct = numeric(),
      pnl = numeric(), weight_pct = numeric()
    )
    data.table::setattr(dt, "as_of", as_of)
    data.table::setattr(dt, "cash", ledger_cash_at(ledger, as_of))
    return(dt[])
  }

  # Цена и предыдущая сессия на ВЫБРАННУЮ дату: «текущая» цена — это цена на
  # момент, который держит ползунок, а не обязательно последняя известная.
  px <- lapply(dt$ticker, md_close_with_prev, on_date = as_of)
  dt[, price_at   := vapply(px, function(x) x$close, numeric(1))]
  dt[, price_prev := vapply(px, function(x) x$prev,  numeric(1))]
  dt[, session    := as.Date(vapply(px, function(x) as.numeric(x$session), numeric(1)),
                             origin = "1970-01-01")]

  # Позиция существует только с даты покупки. Без этого портфель «был» и за
  # полгода до того, как его купили, — числа выглядели бы осмысленно и были бы
  # выдумкой.
  # Дата покупки известна не всегда (позиции Exante её не несут). Неизвестна —
  # считаем позицию открытой: выдумывать дату входа нельзя.
  dt[, held := if ("purchase_date" %in% names(dt))
                 is.na(purchase_date) | as.Date(purchase_date) <= as_of
               else TRUE]
  dt[, quantity_at := data.table::fifelse(held, quantity, 0)]

  dt[is.na(entry_price), entry_price := sapply(ticker, get_close_on_date, date = entry_date)]
  dt[, current_price := price_at]
  dt[, base_price    := sapply(ticker, get_close_on_date, date = base_date)]

  dt[, entry_value   := quantity_at * entry_price]
  dt[, current_value := quantity_at * price_at]
  dt[, prev_value    := quantity_at * price_prev]
  dt[, growth_pct            := (price_at / entry_price - 1) * 100]
  dt[, growth_from_base_pct  := (price_at / base_price - 1) * 100]
  dt[, day_change_pct        := (price_at / price_prev - 1) * 100]
  dt[, pnl                   := current_value - entry_value]

  total_current <- sum(dt$current_value, na.rm = TRUE)
  dt[, weight_pct := current_value / total_current * 100]

  data.table::setattr(dt, "as_of", as_of)
  # Денежная часть счёта на ту же дату: итог портфеля — это бумаги ПЛЮС кэш,
  # и брать кэш «на сегодня» рядом с прошлой стоимостью бумаг значит показать
  # состояние, которого никогда не существовало.
  data.table::setattr(dt, "cash", ledger_cash_at(ledger, as_of))
  dt[]
}

# Сводные метрики по всему портфелю.
#
# na.rm = TRUE здесь НЕЛЬЗЯ: если источник котировок отдал ошибку, все
# current_price приходят NA, сумма с na.rm даёт 0, и витрина показывает
# «стоимость $0, рост −100%» — уверенную неправду вместо «нет данных».
# Именно так стенд выглядел 25.09.2026, когда marketdata.app упёрся в лимит
# кредитов. Поэтому: нет цены хотя бы по одной позиции — итог NA, а интерфейс
# обязан показать прочерк и причину.
summarize_portfolio <- function(metrics) {
  cash <- attr(metrics, "cash") %||% NA_real_
  held <- if ("quantity_at" %in% names(metrics)) metrics[quantity_at > 0] else metrics
  entry_value   <- sum(held$entry_value)
  current_value <- sum(held$current_value)
  prev_value    <- if ("prev_value" %in% names(held)) sum(held$prev_value) else NA_real_
  list(
    entry_value   = entry_value,
    current_value = current_value,
    prev_value    = prev_value,
    # «Накопленным итогом» — от покупки до выбранной даты.
    pnl           = current_value - entry_value,
    growth_pct    = (current_value / entry_value - 1) * 100,
    # «На момент» — изменение за одну торговую сессию.
    day_pnl       = current_value - prev_value,
    day_pct       = (current_value / prev_value - 1) * 100,
    cash          = cash,
    # Итого по счёту: бумаги плюс денежный остаток. NA, если неизвестно хотя бы
    # одно слагаемое — сумма с пропуском врёт увереннее, чем прочерк.
    total_value   = current_value + cash,
    positions     = nrow(held),
    priced        = sum(is.finite(held$current_price)),
    total         = nrow(held)
  )
}

# Динамика портфеля по сессиям: стоимость бумаг, денежный остаток и итог по
# счёту на каждую торговую сессию окна.
#
# Считается из реестра операций, а не из снимков: снимки начинаются со дня,
# когда стенд впервые запустили, а реестр знает всю историю счёта. Поэтому
# кривая доступна сразу, а не «когда накопится».
#
# Способ. Количество бумаги на дату — накопительная сумма её событий по эту
# дату; findInterval даёт индекс последнего события до сессии за один проход,
# без цикла по датам. Цена — закрытие сессии из локального хранилища.
portfolio_value_series <- function(ledger, sessions, currency = "USD") {
  empty <- data.table::data.table(
    date = as.Date(character()), securities = numeric(),
    cash = numeric(), total = numeric()
  )
  if (nrow(ledger) == 0 || length(sessions) == 0) return(empty)
  sessions <- sort(as.Date(sessions))

  # Денежный остаток на каждую сессию.
  cash_tx <- ledger[asset == currency, .(delta = sum(amount, na.rm = TRUE)),
                    by = value_date]
  data.table::setorder(cash_tx, value_date)
  cash_tx[, cum := cumsum(delta)]
  idx <- findInterval(sessions, cash_tx$value_date)
  cash <- ifelse(idx == 0, 0, cash_tx$cum[pmax(idx, 1)])

  # Стоимость бумаг: по каждой бумаге количество на сессию * цена закрытия.
  ev <- ledger_events(ledger, currency)
  securities <- rep(0, length(sessions))
  for (sym in unique(ev$symbol)) {
    e <- ev[symbol == sym]
    data.table::setorder(e, value_date)
    e <- e[, .(qty = sum(qty)), by = value_date]
    e[, cum := cumsum(qty)]
    j <- findInterval(sessions, e$value_date)
    qty <- ifelse(j == 0, 0, e$cum[pmax(j, 1)])
    if (all(abs(qty) < 1e-9)) next

    cnd <- md_candles(exante_symbol_to_ticker(sym), days = length(sessions) + 400)
    if (nrow(cnd) == 0) next
    k <- findInterval(sessions, cnd$date)
    px <- ifelse(k == 0, NA_real_, cnd$close[pmax(k, 1)])
    contrib <- qty * px
    # Нет цены — нет и слагаемого: подставлять ноль значит занижать портфель
    # молча. Такие сессии станут NA в итоге, и на графике будет разрыв.
    securities <- securities + ifelse(abs(qty) < 1e-9, 0, contrib)
  }

  data.table::data.table(
    date = sessions, securities = securities, cash = cash,
    total = securities + cash
  )
}
