# R/lots.R
#
# ЖУРНАЛ РЕШЕНИЙ: каждая покупка — отдельная сделка со своей судьбой.
#
# Зачем это отдельно от позиций. Таблица позиций отвечает на вопрос «что у меня
# есть и почём в среднем». Она НЕ отвечает на вопрос «удачным ли было вот то
# решение»: докупка усредняет цену входа, и две покупки одной бумаги —
# сентябрьская удачная и октябрьская неудачная — сливаются в одну строку со
# средней ценой, по которой не видно ни одной из них.
#
# Здесь каждая покупка живёт отдельной строкой от входа до выхода: дата и цена
# покупки, дата и цена продажи, вложено, выручено, результат. Пока бумага не
# продана, результат считается по текущей цене и помечается как незакрытый.
#
# СОПОСТАВЛЕНИЕ ПРОДАЖ С ПОКУПКАМИ — FIFO: продаётся то, что куплено раньше.
# Это не единственный способ (бывает LIFO, бывает по средней), но единственный,
# который можно объяснить словами и проверить руками: «продал три штуки —
# закрылись три самые старые». Средняя цена позиции при этом считается
# по-прежнему скользящим средним (ledger_positions_at) и со сводкой брокера
# сходится — это два разных взгляда на одни сделки, а не замена одного другим.

lots_empty <- function() {
  data.table::data.table(
    symbol = character(), ticker = character(),
    open_date = as.Date(character()), open_price = numeric(),
    qty = numeric(), cost = numeric(),
    close_date = as.Date(character()), close_price = numeric(),
    proceeds = numeric(), pnl = numeric(), pnl_pct = numeric(),
    open = logical(), order_id = character()
  )
}

# Разбор событий реестра в сделки. `events` — результат ledger_events().
# `price_of(ticker)` даёт текущую цену для незакрытых сделок; если цены нет,
# результат по такой сделке остаётся NA, а не нулём.
ledger_lots <- function(events, as_of = Sys.Date(),
                        price_of = function(tk) NA_real_) {
  if (is.null(events) || nrow(events) == 0) return(lots_empty())
  ev <- data.table::copy(events)[value_date <= as.Date(as_of)]
  if (nrow(ev) == 0) return(lots_empty())
  data.table::setorder(ev, value_date)

  out <- list()
  for (sym in unique(ev$symbol)) {
    e <- ev[symbol == sym]
    open_lots <- list()   # очередь открытых покупок, FIFO
    for (i in seq_len(nrow(e))) {
      qty <- e$qty[i]
      # Цена события: из поля price, иначе из денежной ноги (деньги / штуки).
      px <- e$price[i]
      if (!is.finite(px) && is.finite(e$cash[i]) && abs(qty) > 0) {
        px <- abs(e$cash[i]) / abs(qty)
      }
      if (qty > 0) {
        open_lots[[length(open_lots) + 1L]] <- list(
          date = e$value_date[i], price = px, qty = qty,
          order_id = e$order_id[i])
        next
      }
      if (qty >= 0) next
      # ПРОДАЖА закрывает самые старые покупки.
      left <- -qty
      while (left > 1e-9 && length(open_lots) > 0) {
        lot <- open_lots[[1]]
        take <- min(lot$qty, left)
        out[[length(out) + 1L]] <- data.table::data.table(
          symbol = sym, open_date = lot$date, open_price = lot$price,
          qty = take, close_date = e$value_date[i], close_price = px,
          open = FALSE, order_id = lot$order_id)
        lot$qty <- lot$qty - take
        left <- left - take
        if (lot$qty <= 1e-9) open_lots[[1]] <- NULL else open_lots[[1]] <- lot
      }
      # Продажа без покупки в истории (перенос со счёта, корпоративное
      # действие) просто игнорируется: выдумывать ей цену входа нельзя.
    }
    for (lot in open_lots) {
      if (lot$qty <= 1e-9) next
      out[[length(out) + 1L]] <- data.table::data.table(
        symbol = sym, open_date = lot$date, open_price = lot$price,
        qty = lot$qty, close_date = as.Date(NA), close_price = NA_real_,
        open = TRUE, order_id = lot$order_id)
    }
  }
  if (length(out) == 0) return(lots_empty())

  dt <- data.table::rbindlist(out)
  dt[, ticker := exante_symbol_to_ticker(symbol)]
  dt[, cost := open_price * qty]
  # Для незакрытых сделок «выручка» — это текущая стоимость: сколько дадут
  # сейчас. Нет цены — NA, а не ноль: ноль здесь читался бы как «всё потеряно».
  cur <- vapply(dt$ticker, function(tk) price_of(tk), numeric(1))
  dt[, proceeds := data.table::fifelse(open, cur * qty, close_price * qty)]
  dt[, pnl := proceeds - cost]
  dt[, pnl_pct := data.table::fifelse(is.finite(cost) & cost != 0,
                                      pnl / cost * 100, NA_real_)]
  data.table::setorder(dt, -open_date, ticker)
  dt[, .(symbol, ticker, open_date, open_price, qty, cost, close_date,
         close_price, proceeds, pnl, pnl_pct, open, order_id)]
}

# Открытые сделки на дату — то, что показывается отдельными строками в
# «Позициях»: одна строка = одно решение, а не усреднённая позиция.
lots_open <- function(lots) {
  if (nrow(lots) == 0) return(lots)
  lots[open == TRUE]
}

# Сколько дней держится (или держалась) сделка. Ноль означает «куплено в тот
# же день», а не ошибку, поэтому вызывающий код пишет это словами.
lots_held_days <- function(lots, as_of = Sys.Date()) {
  end <- data.table::fifelse(is.na(lots$close_date), as.Date(as_of), lots$close_date)
  as.integer(end - lots$open_date)
}
