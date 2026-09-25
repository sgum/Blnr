# R/ledger.R
#
# Реестр операций по счёту: из истории транзакций Exante восстанавливаются
# позиции, денежный остаток и средняя цена входа НА ЛЮБУЮ ДАТУ.
#
# Зачем это нужно. Сводка счёта (/md/2.0/summary) отдаёт только СЕГОДНЯШНЕЕ
# состояние. С ползунком времени этого мало: на прошлую дату иначе получится
# сегодняшний кэш рядом с прошлой стоимостью бумаг — итог, которого никогда
# не существовало. История транзакций даёт и то и другое на одну дату.
#
# Проверено на боевом счёте 25.09.2026 (вход с известным ответом):
#   * сумма всех денежных ног = остаток в сводке, расхождение 0.00;
#   * количества, восстановленные по ногам инструментов, совпали со сводкой
#     по всем семи позициям, расхождений 0.
# Эта сверка не разовая: ledger_reconcile() гоняет её при каждом чтении, и
# расхождение выводится на экран. Реестр, тихо разошедшийся с брокером, —
# худший из возможных исходов: числа остаются правдоподобными.
#
# Устройство ответа Exante (проверено):
#   * у сделки ДВЕ ноги с общим orderId: нога инструмента (asset = symbolId,
#     sum = количество, transactionPrice = цена исполнения) и денежная
#     (asset = "USD", sum = сумма со знаком);
#   * комиссия — отдельная строка с тем же orderId и asset = "USD";
#   * valueDate — дата зачисления; поле `when` — метка времени в миллисекундах.

library(data.table)

# Пустой реестр — единый источник схемы.
empty_ledger <- function() {
  data.table::data.table(
    id = integer(), value_date = as.Date(character()), type = character(),
    symbol = character(), asset = character(), amount = numeric(),
    price = numeric(), order_id = character()
  )
}

# Денежный остаток на дату: сумма всех денежных движений по эту дату
# включительно. Не «сегодняшний кэш минус последующее», а прямой подсчёт —
# так результат не зависит от того, свежа ли сводка.
ledger_cash_at <- function(ledger, as_of = Sys.Date(), currency = "USD") {
  if (nrow(ledger) == 0) return(NA_real_)
  target <- as.Date(as_of)
  sum(ledger[asset == currency & value_date <= target, amount], na.rm = TRUE)
}

# Операции по бумагам, собранные в события: одно событие = один ордер.
# Количество берётся с ноги инструмента, деньги — с денежных ног того же
# ордера (сделка плюс комиссия), поэтому стоимость входа учитывает комиссию,
# которую владелец реально заплатил.
ledger_events <- function(ledger, currency = "USD") {
  if (nrow(ledger) == 0) {
    return(data.table::data.table(
      value_date = as.Date(character()), symbol = character(),
      qty = numeric(), cash = numeric(), price = numeric()
    ))
  }
  # Нога инструмента, а не валюты. Валютные конвертации приходят теми же
  # транзакциями (asset = "EUR", "EUR/USD"), и без фильтра они становятся
  # «позициями» на тысячи штук. Признак бумаги — суффикс биржи через точку
  # ("AMD.NASDAQ", опцион "AMD.CBOE.20G2026.C220"); у валют его нет.
  inst <- ledger[asset != currency & nzchar(asset) &
                 grepl(".", asset, fixed = TRUE) &
                 !grepl("/", asset, fixed = TRUE)]
  if (nrow(inst) == 0) {
    return(data.table::data.table(
      value_date = as.Date(character()), symbol = character(),
      qty = numeric(), cash = numeric(), price = numeric()
    ))
  }
  # Ордера без orderId (например зачисления) группируем по дате и бумаге.
  inst[, grp := data.table::fifelse(nzchar(order_id), order_id,
                                    paste0(symbol, "@", format(value_date)))]
  # Комиссия в базу стоимости НЕ входит: Exante считает averagePrice без неё,
  # и расхождение со сводкой брокера читалось бы как ошибка реестра. Комиссии
  # видны отдельно и в денежном остатке учтены полностью.
  cash <- ledger[asset == currency & nzchar(symbol) & type != "COMMISSION"]
  cash[, grp := data.table::fifelse(nzchar(order_id), order_id,
                                    paste0(symbol, "@", format(value_date)))]

  ev <- inst[, .(value_date = min(value_date),
                 qty = sum(amount, na.rm = TRUE),
                 price = suppressWarnings(stats::weighted.mean(
                   price, abs(amount), na.rm = TRUE))),
             by = .(symbol, grp)]
  cs <- cash[, .(cash = sum(amount, na.rm = TRUE)), by = .(grp)]
  ev <- merge(ev, cs, by = "grp", all.x = TRUE)
  ev[is.na(cash), cash := 0]
  data.table::setorder(ev, value_date, symbol)
  ev[, .(value_date, symbol, qty, cash, price)]
}

# Позиции на дату: количество, стоимость входа и средняя цена по методу
# скользящего среднего (при продаже стоимость уменьшается пропорционально
# проданной доле — иначе средняя цена «запоминала» бы уже закрытые лоты).
ledger_positions_at <- function(ledger, as_of = Sys.Date(), currency = "USD") {
  empty <- data.table::data.table(
    symbol = character(), ticker = character(), quantity = numeric(),
    cost = numeric(), avg_price = numeric(), opened_date = as.Date(character()),
    last_buy_date = as.Date(character()),
    first_date = as.Date(character()), last_date = as.Date(character())
  )
  ev <- ledger_events(ledger, currency)
  if (nrow(ev) == 0) return(empty)
  target <- as.Date(as_of)
  ev <- ev[value_date <= target]
  if (nrow(ev) == 0) return(empty)

  out <- lapply(split(ev, ev$symbol), function(e) {
    data.table::setorder(e, value_date)
    qty <- 0; cost <- 0; opened <- as.Date(NA); last_buy <- as.Date(NA)
    for (i in seq_len(nrow(e))) {
      dq <- e$qty[i]; dc <- e$cash[i]
      # Дата открытия ТЕКУЩЕГО лота, а не первой сделки по бумаге. AMD куплена
      # в 2024-м, полностью распродана в 2025-м и куплена заново 24.09.2026 —
      # и до этой правки позиция показывала возраст 737 дней вместо нуля, то
      # есть приписывала себе движение цены за время, когда бумаги не было.
      if (abs(qty) < 1e-9 && dq > 0) opened <- e$value_date[i]
      if (dq > 0) {
        qty <- qty + dq
        cost <- cost + (-dc)          # покупка: деньги ушли, стоимость выросла
        # Дата ПОСЛЕДНЕЙ покупки: от неё отсчитывается сравнение с моделью.
        # Докупка сдвигает точку отсчёта — иначе прогноз мерился бы от входа,
        # которого в текущем виде позиции уже нет.
        last_buy <- e$value_date[i]
      } else if (dq < 0 && qty > 0) {
        frac <- min(1, (-dq) / qty)   # продажа: доля закрытого лота
        cost <- cost * (1 - frac)
        qty <- qty + dq
      } else {
        qty <- qty + dq
      }
      if (abs(qty) < 1e-9) {                        # лот закрыт полностью
        opened <- as.Date(NA); last_buy <- as.Date(NA)
      }
    }
    data.table::data.table(
      symbol = e$symbol[1],
      ticker = exante_symbol_to_ticker(e$symbol[1]),
      quantity = qty, cost = cost,
      avg_price = if (qty > 0) cost / qty else NA_real_,
      opened_date = opened, last_buy_date = last_buy,
      first_date = min(e$value_date), last_date = max(e$value_date)
    )
  })
  res <- data.table::rbindlist(out)
  res <- res[abs(quantity) > 1e-9]
  data.table::setorder(res, ticker)
  res[]
}

# Сверка реестра со сводкой брокера. Возвращает вектор расхождений; пустой —
# значит восстановленное состояние совпало с тем, что показывает Exante.
# Вызывается при каждом чтении: реестр, тихо разошедшийся с брокером, даёт
# правдоподобные и неверные числа, а это худший исход из возможных.
ledger_reconcile <- function(ledger, api_positions, api_cash, tol = 0.01) {
  issues <- character()
  rec_cash <- ledger_cash_at(ledger, Sys.Date())
  if (is.finite(api_cash) && is.finite(rec_cash) && abs(rec_cash - api_cash) > tol) {
    issues <- c(issues, sprintf("кэш: реестр %.2f, брокер %.2f", rec_cash, api_cash))
  }
  rec <- ledger_positions_at(ledger, Sys.Date())
  api <- data.table::as.data.table(api_positions)
  if (nrow(api) > 0) {
    m <- merge(rec[, .(symbol, quantity)], api[, .(symbol, qty_api = quantity)],
               by = "symbol", all = TRUE)
    m[is.na(quantity), quantity := 0][is.na(qty_api), qty_api := 0]
    bad <- m[abs(quantity - qty_api) > 1e-6]
    for (i in seq_len(nrow(bad))) {
      issues <- c(issues, sprintf("%s: реестр %g, брокер %g",
                                  bad$symbol[i], bad$quantity[i], bad$qty_api[i]))
    }
  }
  issues
}
