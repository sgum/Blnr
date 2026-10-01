#!/usr/bin/env Rscript
# scripts/fetch_marketdata.R
#
# Загрузка дневных рядов по всему реестру наблюдения в локальное хранилище
# (R/store.R). ЕДИНСТВЕННОЕ место в проекте, которое ходит за котировками:
# стенд читает только хранилище и в сеть не выходит вовсе.
#
# Источник выбирается переменной BLNR_QUOTES_SOURCE: "exante" (по умолчанию
# с 01.10.2026) или "marketdata". Причина перехода — у бесплатного тарифа
# marketdata.app заявлена суточная задержка данных, и утром на стенде лежала
# позапрошлая сессия.
#
# Запускается заданием Jenkins (jenkins/Jenkinsfile_marketdata), а не cron и не
# руками: нужна история прогонов и ответ на вопрос «когда это последний раз
# отработало успешно».
#
# Задание обязано делать четыре вещи (конституция ЦД), и они здесь есть:
#   1) прежняя версия сохраняется ДО записи (store_rotate) — откат копированием;
#   2) падаем на недоступном источнике, а не собираем на том, что нашлось;
#   3) результат проверяется: глубина ряда, свежесть последней даты, монотонность;
#   4) история прогонов и логи остаются в Jenkins.
#
# Код возврата: 0 — успех, 1 — отказ (ряды НЕ перезаписаны либо перезаписаны
# частично и об этом сказано явно).

suppressWarnings(suppressMessages({
  library(data.table); library(httr); library(jsonlite)
}))

app_dir <- {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) normalizePath(file.path(dirname(f), "..")) else getwd()
}
setwd(app_dir)

# Загрузчику сеть нужна — в отличие от стенда.
Sys.setenv(BLNR_ALLOW_ONLINE = "TRUE")

SNAPSHOT_LOG_PATH <- Sys.getenv("BLNR_SNAPSHOT_LOG",
                                unset = file.path("data", "portfolio_snapshots.csv"))
source("R/watchlist.R"); source("R/store.R"); source("R/marketdata.R")
source("R/exante_api.R"); source("R/exante_candles.R")

# ИСТОЧНИК КОТИРОВОК. С 01.10.2026 — Exante: на бесплатном тарифе
# marketdata.app заявлена суточная задержка («24h Delayed Stock Data»), и
# свеча за вчерашнюю сессию появлялась только в середине следующего дня.
# Переключатель оставлен, чтобы вернуться одной переменной окружения, не
# выкатывая код: прежний источник исправен, он просто медленный.
QUOTES_SOURCE <- tolower(Sys.getenv("BLNR_QUOTES_SOURCE", unset = "exante"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Сколько торговых дней считаем минимально приемлемой глубиной. Ряд короче —
# это не «мало данных», а признак того, что источник отдал огрызок.
MIN_ROWS <- as.integer(Sys.getenv("BLNR_STORE_MIN_ROWS", unset = "60"))
# Насколько свежей обязана быть последняя дата. Три календарных дня закрывают
# выходные; больше — значит ряд заморожен, даже если файл выглядит целым.
MAX_STALE_DAYS <- as.integer(Sys.getenv("BLNR_STORE_MAX_STALE", unset = "5"))

say <- function(...) cat(sprintf(...), "\n", sep = "")

if (identical(QUOTES_SOURCE, "exante")) {
  if (!exante_has_credentials()) {
    say("ОТКАЗ: нет кред Exante — источник недоступен, ряды не тронуты.")
    quit(status = 1L)
  }
} else if (!md_has_token()) {
  say("ОТКАЗ: не задан MARKETDATA_TOKEN — источник недоступен, ряды не тронуты.")
  quit(status = 1L)
}
say("Источник котировок: %s", QUOTES_SOURCE)

wl <- watchlist_active()

# Разрешение тикеров в symbolId делается ОДИН раз до прохода: биржу угадывать
# нельзя ("GS.NYSE" и "GS.NASDAQ" — разные инструменты), а справочник и
# реестр операций читаются с диска, не из сети.
SYMBOLS <- list()
if (identical(QUOTES_SOURCE, "exante")) {
  led_for_symbols <- tryCatch(store_read_ledger(), error = function(e) NULL)
  for (tk in wl$ticker) {
    SYMBOLS[[tk]] <- exante_resolve_symbol(tk, ledger = led_for_symbols)
  }
  unresolved <- names(SYMBOLS)[vapply(SYMBOLS, function(x) is.na(x) || !nzchar(x), logical(1))]
  if (length(unresolved) > 0) {
    say("ОТКАЗ: не удалось определить symbolId для %d бумаг: %s.",
        length(unresolved), paste(unresolved, collapse = ", "))
    say("Биржу угадывать нельзя. Обновите справочник заданием «311.blnr - catalog».")
    quit(status = 1L)
  }
}

# Единая точка получения ряда: загрузчик дальше не знает, какой источник
# настроен, и логика «всё или ничего» остаётся общей.
fetch_one <- function(tk, days) {
  if (identical(QUOTES_SOURCE, "exante")) {
    return(exante_candles(SYMBOLS[[tk]], days = days))
  }
  md_candles(tk, days = days, online = TRUE)
}
fetch_error <- function() {
  if (identical(QUOTES_SOURCE, "exante")) "источник не отдал ряд" else md_status_text()
}
say("Реестр наблюдения: %d инструментов, глубина %d дней.", nrow(wl), BLNR_STORE_DAYS)
# Что было ДО прогона — чтобы пустая загрузка была видна в логе строкой, а не
# вычислялась сравнением двух сборок. Именно так пряталась пустышка: прогон
# печатал «Готово, 24 инструмента, 9600 строк» по данным из собственного
# хранилища и отчитывался зелёным.
before <- store_status()$last_date
say("Хранилище: %s", normalizePath(BLNR_STORE_DIR, mustWork = FALSE))

# (0) РАЗВЕДКА ОДНИМ ЗАПРОСОМ: появилась ли у источника сессия свежее той, что
# уже лежит в хранилище.
#
# Зачем. Дневную свечу marketdata.app публикует НЕ сразу после закрытия биржи,
# а примерно через 8–9 часов: 01.10.2026 в 05:47 MSK сессия 30.09 (закрылась
# накануне в 23:00 MSK) ещё не отдавалась вовсе, ответ приходил со статусом 203
# и последней свечой 29.09. Поэтому один утренний заход — лотерея: попал
# позже публикации — ряды свежие, попал раньше — стенд весь день показывает
# позапрошлую сессию, и владелец спрашивает, почему на экране не вчерашний
# торговый день (так и случилось 01.10.2026).
#
# Лечится не переносом времени, а НЕСКОЛЬКИМИ заходами. Но полный проход стоит
# 24 запроса из 100 в сутки, трижды в день это уже 72 — поэтому сначала
# спрашиваем ОДИН инструмент, и только если у источника есть что-то новее,
# идём за всеми. Нормальный день: 24 запроса на загрузку плюс по одному на
# каждый лишний заход.
probe_tk <- wl$ticker[1]
probe <- fetch_one(probe_tk, days = 5)
if (nrow(probe) == 0) {
  say("ОТКАЗ: разведка по %s не удалась: %s", probe_tk,
      fetch_error() %||% "нет данных")
  say("Хранилище оставлено в прежнем состоянии — прошлые ряды целы.")
  quit(status = 1L)
}
src_last <- max(probe$date)
say("Разведка (%s): у источника последняя сессия %s.", probe_tk,
    format(src_last, "%d.%m.%Y"))

if (!is.null(before) && !is.na(before) && src_last <= before) {
  # Это НЕ отказ: выходные, праздник или свеча ещё не опубликована. Отметку о
  # проверке пишем всегда — иначе на стенде видно только дату записи, и
  # «ряды на 29.09» невозможно отличить от «ряды на 29.09, потому что 30.09
  # у источника ещё нет».
  store_note_check(source_last_date = src_last)
  say("")
  say("Новой сессии у источника нет: в хранилище %s, у источника %s.",
      format(before, "%d.%m.%Y"), format(src_last, "%d.%m.%Y"))
  say("Ряды не тронуты, отметка о проверке обновлена. Потрачен 1 запрос.")
  quit(status = 0L)
}

# (1) Прежняя версия — ДО записи.
store_rotate()

fetched <- list()
failed  <- character()

for (i in seq_len(nrow(wl))) {
  tk <- wl$ticker[i]
  # online = TRUE явно: нам нужен именно поход в API, а не чтение хранилища,
  # иначе загрузчик читал бы сам себя и ряды никогда бы не обновлялись.
  cnd <- fetch_one(tk, days = BLNR_STORE_DAYS)
  if (nrow(cnd) == 0) {
    failed <- c(failed, tk)
    say("  %-5s ОТКАЗ: %s", tk, fetch_error() %||% "нет данных")
    next
  }
  fetched[[tk]] <- cnd
  say("  %-5s %4d строк, %s — %s", tk, nrow(cnd),
      format(min(cnd$date), "%d.%m.%Y"), format(max(cnd$date), "%d.%m.%Y"))
}

# (2) Падаем на недоступном источнике. Частично собранное хранилище
# неотличимо от исправного, поэтому при любом отказе не пишем НИЧЕГО.
if (length(failed) > 0) {
  say("")
  say("ОТКАЗ: не получены ряды по %d из %d инструментов (%s).",
      length(failed), nrow(wl), paste(failed, collapse = ", "))
  say("Хранилище оставлено в прежнем состоянии — прошлые ряды целы.")
  quit(status = 1L)
}

# (3) Проверка результата ДО записи — общая функция, у неё есть свои тесты
# (tests/test_functions.R, раздел 7): проверка, которую нельзя предъявить
# плохому входу, ничего не гарантирует.
issues <- store_verify_series(fetched, today = Sys.Date(),
                              min_rows = MIN_ROWS, max_stale_days = MAX_STALE_DAYS)
if (length(issues) > 0) {
  say("")
  say("ОТКАЗ по проверке результата:")
  for (x in issues) say("  - %s", x)
  say("Хранилище оставлено в прежнем состоянии.")
  quit(status = 1L)
}

for (tk in names(fetched)) store_write_candles(tk, fetched[[tk]])

last_date <- max(vapply(fetched, function(d) as.numeric(max(d$date)), numeric(1)))
store_write_meta(list(
  updated_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  checked_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
  instruments = length(fetched),
  last_date   = format(as.Date(last_date, origin = "1970-01-01")),
  source_last_date = format(as.Date(last_date, origin = "1970-01-01")),
  depth_days  = BLNR_STORE_DAYS,
  rows        = sum(vapply(fetched, nrow, integer(1)))
))

after <- as.Date(last_date, origin = "1970-01-01")
say("")
say("Готово: %d инструментов, %d строк.", length(fetched),
    sum(vapply(fetched, nrow, integer(1))))
if (is.null(before) || is.na(before)) {
  say("Ряды доведены до %s (хранилище заполнено впервые).", format(after, "%d.%m.%Y"))
} else if (after > before) {
  say("Ряды ПРОДВИНУЛИСЬ: %s -> %s.", format(before, "%d.%m.%Y"), format(after, "%d.%m.%Y"))
} else {
  # Не отказ: в выходные и праздники новой сессии нет. Но сказать об этом
  # нужно прямо, иначе пустой прогон неотличим от рабочего.
  say("Ряды НЕ продвинулись: как были %s, так и остались. Новой сессии у источника нет.",
      format(before, "%d.%m.%Y"))
}
quit(status = 0L)
