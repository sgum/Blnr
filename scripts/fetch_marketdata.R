#!/usr/bin/env Rscript
# scripts/fetch_marketdata.R
#
# Ночная загрузка дневных рядов по всему реестру наблюдения в локальное
# хранилище (R/store.R). ЕДИНСТВЕННОЕ место в проекте, которое обращается к
# marketdata.app: у аккаунта лимит 100 запросов в сутки, и стенд, ходящий в API
# на каждом открытии экрана, выжигает его с первого пользователя.
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

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Сколько торговых дней считаем минимально приемлемой глубиной. Ряд короче —
# это не «мало данных», а признак того, что источник отдал огрызок.
MIN_ROWS <- as.integer(Sys.getenv("BLNR_STORE_MIN_ROWS", unset = "60"))
# Насколько свежей обязана быть последняя дата. Три календарных дня закрывают
# выходные; больше — значит ряд заморожен, даже если файл выглядит целым.
MAX_STALE_DAYS <- as.integer(Sys.getenv("BLNR_STORE_MAX_STALE", unset = "5"))

say <- function(...) cat(sprintf(...), "\n", sep = "")

if (!md_has_token()) {
  say("ОТКАЗ: не задан MARKETDATA_TOKEN — источник недоступен, ряды не тронуты.")
  quit(status = 1L)
}

wl <- watchlist_active()
say("Реестр наблюдения: %d инструментов, глубина %d дней.", nrow(wl), BLNR_STORE_DAYS)
# Что было ДО прогона — чтобы пустая загрузка была видна в логе строкой, а не
# вычислялась сравнением двух сборок. Именно так пряталась пустышка: прогон
# печатал «Готово, 24 инструмента, 9600 строк» по данным из собственного
# хранилища и отчитывался зелёным.
before <- store_status()$last_date
say("Хранилище: %s", normalizePath(BLNR_STORE_DIR, mustWork = FALSE))

# (1) Прежняя версия — ДО записи.
store_rotate()

fetched <- list()
failed  <- character()

for (i in seq_len(nrow(wl))) {
  tk <- wl$ticker[i]
  # online = TRUE явно: нам нужен именно поход в API, а не чтение хранилища,
  # иначе загрузчик читал бы сам себя и ряды никогда бы не обновлялись.
  cnd <- md_candles(tk, days = BLNR_STORE_DAYS, online = TRUE)
  if (nrow(cnd) == 0) {
    failed <- c(failed, tk)
    say("  %-5s ОТКАЗ: %s", tk, md_status_text() %||% "нет данных")
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
  instruments = length(fetched),
  last_date   = format(as.Date(last_date, origin = "1970-01-01")),
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
