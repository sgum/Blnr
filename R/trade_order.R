# R/trade_order.R
#
# Сборка тела поручения — ЕДИНСТВЕННОЕ место, отвечающее на вопрос «что уйдёт
# на счёт».
#
# ЗАЧЕМ ОТДЕЛЬНОЙ ФУНКЦИЕЙ. 06.10.2026 владелец купил через стенд 3 акции
# Google — а на счёт ушла покупка 3 акций AMD по $649.03 (исполнено, $1947).
# Причина: окно подтверждения и отправка читали РАЗНЫЕ выражения об одном и
# том же. Предпросмотр брал выбор из выпадающего списка (GOOGL), а отправка —
# тикер, с которым окно ОТКРЫВАЛОСЬ, то есть бумагу, выбранную на графике
# (AMD). По экрану дефект увидеть было нельзя: экран показывал верное.
#
# Отсюда правило: предпросмотр и отправка вызывают ЭТУ функцию и ничего
# больше. Пока «что показать» и «что отправить» считаются двумя выражениями,
# они рано или поздно разойдутся, и цена расхождения — реальные деньги.

# side          — "buy", "sell" или "sell_all"
# dialog_ticker — выбор в выпадающем списке окна (есть только при покупке)
# opened_ticker — тикер, с которым окно открыли (строка таблицы при продаже)
# qty           — количество из поля ввода
# max_qty       — сколько есть в портфеле (ограничение для продажи)
# symbol_of     — функция тикер -> биржевой код (symbolId)
#
# Возвращает list(side, ticker, qty, symbol, ok, error). `ok = FALSE` означает
# «отправлять нельзя», и `error` объясняет почему теми же словами, что увидит
# человек в окне.
build_trade_order <- function(side,
                              dialog_ticker = NA_character_,
                              opened_ticker = NA_character_,
                              qty = NA_real_,
                              max_qty = 0,
                              symbol_of = function(tk) NA_character_) {
  side <- as.character(side)[1]
  if (identical(side, "sell_all")) {
    return(list(side = side, ticker = NA_character_, qty = NA_real_,
                symbol = NA_character_, ok = TRUE, error = NULL))
  }

  # ПРИ ПОКУПКЕ бумагу выбирают в самом окне; тикер, с которым окно открыли, —
  # лишь начальное значение списка. ПРИ ПРОДАЖЕ наоборот: продаём ту строку, по
  # которой нажали, и подменить её списком нельзя — списка там и нет.
  tk <- if (identical(side, "buy")) {
    if (!is.na(dialog_ticker) && nzchar(dialog_ticker)) dialog_ticker else opened_ticker
  } else {
    opened_ticker
  }
  tk <- if (is.na(tk)) NA_character_ else toupper(trimws(as.character(tk)[1]))

  out <- list(side = side, ticker = tk, qty = suppressWarnings(as.numeric(qty)[1]),
              symbol = NA_character_, ok = FALSE, error = NULL)

  if (is.na(tk) || !nzchar(tk)) {
    out$error <- "Бумага не выбрана."
    return(out)
  }
  out$symbol <- symbol_of(tk)
  if (is.null(out$symbol) || length(out$symbol) == 0) out$symbol <- NA_character_
  if (is.na(out$symbol) || !nzchar(out$symbol)) {
    out$error <- paste0(
      "Не знаю биржевой код для ", tk,
      ". Он появится после первой сделки по этой бумаге на счёте — гадать ",
      "суффикс нельзя, GS.NYSE и GS.NASDAQ это разные инструменты.")
    return(out)
  }
  if (!is.finite(out$qty) || out$qty <= 0) {
    out$error <- "Количество должно быть положительным."
    return(out)
  }
  if (identical(side, "sell") && out$qty > max_qty) {
    out$error <- sprintf("В портфеле только %g шт. Продать больше нельзя.", max_qty)
    return(out)
  }
  out$ok <- TRUE
  out
}
