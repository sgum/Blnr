#!/usr/bin/env Rscript
# scripts/fetch_ledger.R
#
# Обновление реестра операций счёта из Exante в локальное хранилище.
# Запускается тем же ночным заданием, что и загрузка котировок: стенд читает
# реестр из хранилища и в Exante на каждое открытие экрана не ходит.
#
# Реестр — источник состава портфеля, средних цен и денежного остатка НА
# ЛЮБУЮ ДАТУ (сводка счёта знает только сегодня). Поэтому он обязан быть
# сверен с брокером при каждом обновлении: реестр, тихо разошедшийся со
# сводкой, даёт правдоподобные и неверные числа.
#
# Код возврата: 0 — успех, 1 — отказ (прежний реестр НЕ перезаписан).

suppressWarnings(suppressMessages({
  library(data.table); library(httr); library(jsonlite)
}))

app_dir <- {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) normalizePath(file.path(dirname(f), "..")) else getwd()
}
setwd(app_dir)

SNAPSHOT_LOG_PATH <- Sys.getenv("BLNR_SNAPSHOT_LOG",
                                unset = file.path("data", "portfolio_snapshots.csv"))
source("R/watchlist.R"); source("R/store.R"); source("R/exante_api.R")
source("R/ledger.R")

say <- function(...) cat(sprintf(...), "\n", sep = "")

if (!exante_has_credentials()) {
  say("ОТКАЗ: не заданы EXANTE_API_ID / EXANTE_SHARED_KEY — реестр не тронут.")
  quit(status = 1L)
}

acct <- exante_primary_account()
if (is.null(acct)) {
  say("ОТКАЗ: не нашли счёт с движением — реестр не тронут.")
  quit(status = 1L)
}
say("Счёт: %s", acct)

led <- exante_transactions_dt(acct)
if (!is.data.frame(led) || nrow(led) == 0) {
  say("ОТКАЗ: история операций пуста или недоступна — реестр не тронут.")
  quit(status = 1L)
}
say("Получено операций: %d (%s — %s)", nrow(led),
    format(min(led$value_date), "%d.%m.%Y"), format(max(led$value_date), "%d.%m.%Y"))

# Сверка ДО записи: расходится с брокером — не пишем.
info <- exante_account_cash(acct)
summary <- exante_get_account_summary(acct)
api_pos <- if (length(summary$positions %||% list()) > 0) {
  data.table::data.table(
    symbol   = vapply(summary$positions, function(p) as.character(p$id), character(1)),
    quantity = vapply(summary$positions, function(p) as.numeric(p$quantity), numeric(1))
  )
} else data.table::data.table(symbol = character(), quantity = numeric())

issues <- ledger_reconcile(led, api_pos, info$cash)
if (length(issues) > 0) {
  say("")
  say("ОТКАЗ: восстановленное состояние разошлось со сводкой брокера:")
  for (x in issues) say("  - %s", x)
  say("Прежний реестр оставлен без изменений.")
  quit(status = 1L)
}

store_write_ledger(led)
pos <- ledger_positions_at(led)
say("")
say("Готово: позиций %d, кэш %.2f %s, сверка с брокером без расхождений.",
    nrow(pos), ledger_cash_at(led), info$currency)
quit(status = 0L)
