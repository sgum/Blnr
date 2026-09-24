#!/usr/bin/env Rscript
# tests/test_functions.R
#
# Прогон: cd <каталог приложения> && Rscript tests/test_functions.R
# Код возврата != 0 при любом провале — годится как гейт в Jenkinsfile.
#
# Главная цель этого файла — НЕ покрытие ради покрытия, а предохранители под
# классы дефектов, которые уже случались на этом стенде (см. docs/dev.md).
# Ключевой из них — ЗАТЕНЕНИЕ ИМЕНИ СТОЛБЦА аргументом функции в `[.data.table`:
#   sub <- dt[date <= as.Date(date)]   # `date` справа — тоже СТОЛБЕЦ, всегда TRUE
# Такой код не падает и не даёт NA — он молча возвращает последнюю строку.
# Поймать его можно только входом с ИЗВЕСТНЫМ РАЗНЫМ ответом на разные даты,
# поэтому ниже проверяется именно различие, а не «не NA».

suppressWarnings(suppressMessages({
  library(data.table)
  library(openxlsx)
  library(httr)
  library(jsonlite)
}))

app_dir <- {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) normalizePath(file.path(dirname(f), "..")) else getwd()
}
setwd(app_dir)

FORECAST_BASELINE_DATE <- as.Date("2026-09-11")
portfolio_holdings <- data.table(
  ticker = c("GS", "GE"), quantity = c(5, 16),
  entry_price = c(995.72, 318.28), purchase_date = as.Date("2026-09-14")
)

source("R/snapshots.R"); source("R/watchlist.R"); source("R/marketdata.R")
source("R/exante_api.R"); source("R/portfolio.R"); source("R/forecast.R")
source("R/auth_ad.R")

FAILED <- 0L
ok <- function(what, cond) {
  if (isTRUE(cond)) {
    cat(sprintf("  ok   %s\n", what))
  } else {
    cat(sprintf("  FAIL %s\n", what)); FAILED <<- FAILED + 1L
  }
}

cat("== 1. Выборка по дате не должна затеняться именем аргумента ==\n")
# Синтетические свечи: у каждой даты СВОЙ close, поэтому правильная выборка
# обязана давать РАЗНЫЕ значения. Сеть не нужна — подменяем md_candles.
fake <- data.table(
  date   = as.Date(c("2026-09-09", "2026-09-10", "2026-09-11", "2026-09-12")),
  open = 0, high = 0, low = 0,
  close  = c(100, 200, 300, 400),
  volume = NA_real_
)
local({
  md_candles <<- function(ticker, days = NULL, type = NULL) fake
  on.exit(rm(md_candles, envir = .GlobalEnv), add = TRUE)
  ok("close на 09.09 = 100",            identical(md_close_on_date("X", as.Date("2026-09-09")), 100))
  ok("close на 11.09 = 300 (не 400!)",  identical(md_close_on_date("X", as.Date("2026-09-11")), 300))
  ok("разные даты -> разные значения",
     md_close_on_date("X", as.Date("2026-09-09")) != md_close_on_date("X", as.Date("2026-09-12")))
  ok("выходной откатывается назад (13.09 -> 400)",
     identical(md_close_on_date("X", as.Date("2026-09-13")), 400))
  ok("дата раньше истории -> NA", is.na(md_close_on_date("X", as.Date("2026-01-01"))))
})

cat("== 2. То же для прогноза (forecast_for_date) ==\n")
fc <- data.table(
  date = as.Date(c("2026-09-11", "2026-09-14", "2026-09-15")),
  ticker = "GS", forecast_growth_pct = c(1, 2, 3)
)
ok("прогноз на 11.09 = 1",           identical(forecast_for_date(fc, "GS", as.Date("2026-09-11")), 1))
ok("прогноз на 15.09 = 3",           identical(forecast_for_date(fc, "GS", as.Date("2026-09-15")), 3))
ok("разные даты -> разные значения",
   forecast_for_date(fc, "GS", as.Date("2026-09-11")) != forecast_for_date(fc, "GS", as.Date("2026-09-15")))
ok("неизвестный тикер -> NA",        is.na(forecast_for_date(fc, "ZZZ", as.Date("2026-09-15"))))

cat("== 3. Реестр инструментов: точное сопоставление, без подстрок ==\n")
ok("Goldman Sachs -> GS",       identical(ticker_by_model_name("Goldman Sachs"), "GS"))
ok("General Electric -> GE",    identical(ticker_by_model_name("General Electric"), "GE"))
# Классический дефект: подстрочный алиас "ge" ловил "General Motors".
ok("General Motors -> GM (не GE!)", identical(ticker_by_model_name("General Motors"), "GM"))
ok("Gold -> GLD",               identical(ticker_by_model_name("Gold"), "GLD"))
ok("Nasdaq -> IXIC",            identical(ticker_by_model_name("Nasdaq"), "IXIC"))
ok("неизвестное имя -> NA",     is.na(ticker_by_model_name("Неизвестная Компания")))
ok("в реестре 26 инструментов", nrow(WATCHLIST) == 26)
ok("тикеры уникальны",          !any(duplicated(WATCHLIST$ticker)))
ok("имена модели уникальны",    !any(duplicated(tolower(trimws(WATCHLIST$name_model)))))
ok("индексы идут отдельным эндпоинтом",
   identical(sort(WATCHLIST[type == "index", ticker]), c("DJI", "IXIC", "SPX")))

cat("== 4. Снимки истории: идемпотентность по дате ==\n")
tmp <- tempfile(fileext = ".csv")
m <- data.table(ticker = c("GS", "GE"), growth_from_base_pct = c(2.1, 3.0),
                forecast_pct = c(1.3, 2.5), dev_pct = c(0.8, 0.5))
record_snapshot(m, as_of = as.Date("2026-09-22"), path = tmp)
record_snapshot(m, as_of = as.Date("2026-09-22"), path = tmp)   # повтор того же дня
record_snapshot(m, as_of = as.Date("2026-09-23"), path = tmp)
h <- read_snapshots(tmp)
ok("повтор за тот же день не дублирует", nrow(h) == 4)
ok("дней в истории = 2",                 length(unique(h$date)) == 2)
ok("пустой прогноз не пишется",
   !isTRUE(record_snapshot(
     data.table(ticker = "X", growth_from_base_pct = 1,
                forecast_pct = NA_real_, dev_pct = NA_real_),
     path = tempfile())))
unlink(tmp)

cat("== 5. Авторизация: белый список и пустой пароль ==\n")
ok("пустой пароль отклоняется до домена", !auth_check("s.gumerov", "")$ok)
ok("пустой логин отклоняется",            !auth_check("", "whatever")$ok)
ok("не из белого списка отклоняется",     !auth_check("i.hacker", "whatever")$ok)
ok("логин нормализуется к короткому",     identical(normalize_login("S.Gumerov@dtwin.ru"), "s.gumerov"))
ok("bind пробует UPN и NETBIOS",
   identical(ad_bind_candidates("s.gumerov"), c("s.gumerov@ad.dtwin.ru", "AD\\s.gumerov")))

cat(sprintf("\nИтог: %s\n", if (FAILED == 0L) "все проверки пройдены" else sprintf("ПРОВАЛОВ: %d", FAILED)))
quit(status = if (FAILED == 0L) 0L else 1L)
