# R/orders.R
#
# Журнал поручений стенда: что отправили и ЧТО ИЗ ЭТОГО ВЫШЛО.
#
# ЗАЧЕМ ВТОРАЯ ПОЛОВИНА. До 06.10.2026 журнал записывал только факт отправки:
# «s.gumerov, buy, AMD.NASDAQ, 3, отправлено». В тот день владелец покупал
# Google, окно подтверждения показывало GOOGL, а на счёт ушёл AMD — и журнал
# об этом честно написал. Но его никто не видел: он лежал файлом в хранилище и
# на экран не выводился вовсе, а строка «отправлено» не говорит, по какой цене
# и сколько реально куплено.
#
# Отсюда два требования, из которых всё здесь следует:
#   1) журнал обязан показывать ИСПОЛНЕНИЕ, а не только отправку — тогда «не та
#      бумага» видна сразу, в тот же день, а не всплывает неделей позже;
#   2) журнал обязан быть НА ЭКРАНЕ. Запись, которую не читают, не работает.

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

ORDERS_COLS <- c("at", "user", "side", "symbol", "quantity", "status", "detail",
                 "order_id", "filled_qty", "fill_price", "filled_at",
                 "broker_status")

orders_empty <- function() {
  data.table::data.table(
    at = as.POSIXct(character()), user = character(), side = character(),
    symbol = character(), quantity = numeric(), status = character(),
    detail = character(), order_id = character(), filled_qty = numeric(),
    fill_price = numeric(), filled_at = character(), broker_status = character()
  )
}

# Чтение журнала с приведением старых строк к новой схеме: до 06.10.2026
# колонок об исполнении не было вовсе, и терять из-за этого прежние записи
# нельзя — по ним восстанавливается история распоряжений.
orders_read <- function() {
  f <- store_orders_path()
  if (!file.exists(f)) return(orders_empty())
  dt <- tryCatch(data.table::fread(f), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0) return(orders_empty())
  for (col in ORDERS_COLS) {
    if (!col %in% names(dt)) {
      data.table::set(dt, j = col,
                      value = if (col %in% c("quantity", "filled_qty", "fill_price"))
                        NA_real_ else NA_character_)
    }
  }
  dt[, at := as.POSIXct(at)]
  for (col in c("user", "side", "symbol", "status", "detail", "order_id",
                "filled_at", "broker_status")) {
    data.table::set(dt, j = col, value = as.character(dt[[col]]))
  }
  for (col in c("quantity", "filled_qty", "fill_price")) {
    data.table::set(dt, j = col, value = suppressWarnings(as.numeric(dt[[col]])))
  }
  data.table::setorder(dt, -at)
  dt[, ..ORDERS_COLS]
}

orders_write <- function(dt) {
  dir.create(BLNR_STORE_DIR, showWarnings = FALSE, recursive = TRUE)
  data.table::setorder(dt, at)
  data.table::fwrite(dt[, ..ORDERS_COLS], store_orders_path())
  invisible(TRUE)
}

# Достать orderId из ответа брокера. Ответ на POST /trade/3.0/orders — массив
# поручений (у рыночного оно одно), поэтому разбор бережный: без orderId
# сверить исполнение будет нечем, но и падать из-за этого нельзя — поручение
# уже ушло.
orders_extract_id <- function(response) {
  if (is.null(response)) return(NA_character_)
  pick <- function(x) {
    if (!is.list(x)) return(NA_character_)
    v <- x$orderId %||% x$id %||% NULL
    if (is.null(v) || length(v) == 0) NA_character_ else as.character(v)[1]
  }
  if (!is.null(response$orderId) || !is.null(response$id)) return(pick(response))
  if (length(response) >= 1 && is.list(response[[1]])) return(pick(response[[1]]))
  NA_character_
}

# Разбор состояния поручения у брокера: статус и средневзвешенная цена
# исполнения. Частичное исполнение тоже считается — поэтому не «цена первой
# сделки», а средняя по объёму.
orders_parse_state <- function(order) {
  out <- list(status = NA_character_, qty = NA_real_, price = NA_real_,
              at = NA_character_)
  if (!is.list(order)) return(out)
  st <- order$orderState %||% list()
  out$status <- as.character(st$status %||% NA_character_)[1]
  fills <- st$fills %||% list()
  if (length(fills) == 0) return(out)
  q <- suppressWarnings(as.numeric(vapply(fills, function(f) as.character(f$quantity %||% NA), character(1))))
  p <- suppressWarnings(as.numeric(vapply(fills, function(f) as.character(f$price %||% NA), character(1))))
  tm <- vapply(fills, function(f) as.character(f$timestamp %||% f$time %||% ""), character(1))
  good <- is.finite(q) & is.finite(p) & q > 0
  if (!any(good)) return(out)
  out$qty <- sum(q[good])
  out$price <- sum(q[good] * p[good]) / out$qty
  out$at <- utils::tail(tm[good][nzchar(tm[good])], 1)
  if (length(out$at) == 0) out$at <- NA_character_
  out
}

# Сверка журнала с брокером: по строкам, где исполнение ещё неизвестно,
# спрашиваем состояние поручения и дописываем, что вышло.
#
# Делается ЛЕНИВО, при открытии журнала, а не таймером: поручений единицы в
# день, а таймер в финансовом стенде — это фоновые запросы, о которых никто не
# помнит. Глубина ограничена: смысла опрашивать поручения месячной давности нет.
orders_reconcile <- function(max_age_days = 14, get_order = NULL, now = Sys.time()) {
  dt <- orders_read()
  if (nrow(dt) == 0) return(dt)
  if (is.null(get_order)) {
    get_order <- function(id) exante_get(sprintf("/trade/3.0/orders/%s", id))
  }
  need <- which(
    !is.na(dt$order_id) & nzchar(dt$order_id) &
    (is.na(dt$filled_qty) | !is.finite(dt$filled_qty)) &
    as.numeric(difftime(now, dt$at, units = "days")) <= max_age_days
  )
  if (length(need) == 0) return(dt)
  changed <- FALSE
  for (i in need) {
    res <- tryCatch(get_order(dt$order_id[i]), error = function(e) NULL)
    if (is.null(res) || !is.null(res$error)) next
    st <- orders_parse_state(if (is.list(res) && is.null(res$orderState) &&
                                 length(res) >= 1 && is.list(res[[1]])) res[[1]] else res)
    if (is.na(st$status) && !is.finite(st$qty)) next
    data.table::set(dt, i = i, j = "broker_status", value = st$status)
    if (is.finite(st$qty)) {
      data.table::set(dt, i = i, j = "filled_qty", value = st$qty)
      data.table::set(dt, i = i, j = "fill_price", value = st$price)
      data.table::set(dt, i = i, j = "filled_at", value = st$at)
    }
    changed <- TRUE
  }
  if (changed) orders_write(dt)
  orders_read()
}

# Можно ли ещё отменить поручение. Отменяется только НЕИСПОЛНЕННОЕ: у
# исполненной сделки отменять нечего, а кнопка над ней обещала бы невозможное.
# Пустой статус брокера трактуем как «похоже, ещё живо»: поручение, которое
# стенд отправил минуту назад и не успел сверить, отменить как раз нужно.
orders_cancellable <- function(row) {
  if (is.na(row$order_id) || !nzchar(row$order_id)) return(FALSE)
  if (!identical(row$status, "отправлено")) return(FALSE)
  if (is.finite(row$filled_qty) && row$filled_qty > 0) return(FALSE)
  bs <- tolower(as.character(row$broker_status %||% ""))
  if (is.na(bs) || !nzchar(bs)) return(TRUE)
  !(bs %in% c("filled", "cancelled", "canceled", "rejected"))
}

# Отметить поручение отменённым в журнале.
orders_mark_cancelled <- function(order_id, note = "") {
  dt <- orders_read()
  i <- which(dt$order_id == as.character(order_id)[1])
  if (length(i) == 0) return(invisible(FALSE))
  data.table::set(dt, i = i, j = "status", value = "отменено")
  data.table::set(dt, i = i, j = "broker_status", value = "cancelled")
  if (nzchar(note)) data.table::set(dt, i = i, j = "detail", value = note)
  orders_write(dt)
  invisible(TRUE)
}

# Короткая строка о том, что вышло из поручения, — для экрана.
orders_outcome_text <- function(row) {
  if (!is.na(row$filled_qty) && is.finite(row$filled_qty) && row$filled_qty > 0) {
    return(sprintf("%s %g шт. по %s",
                   if (identical(row$side, "buy")) "куплено" else "продано",
                   row$filled_qty, fmt_money(row$fill_price, 2)))
  }
  bs <- row$broker_status
  if (!is.na(bs) && nzchar(bs)) return(sprintf("у брокера: %s", bs))
  if (identical(row$status, "отправлено")) return("исполнение не подтверждено")
  row$status
}
