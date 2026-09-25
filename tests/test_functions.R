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

source("R/snapshots.R"); source("R/watchlist.R"); source("R/store.R")
source("R/marketdata.R")
source("R/exante_api.R"); source("R/ledger.R")
source("R/portfolio.R"); source("R/forecast.R")
source("R/auth_ad.R"); source("R/export_xlsx.R"); source("R/ui_kit.R")

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
  # Подменяем источник свечей — но ВОССТАНАВЛИВАЕМ, а не удаляем: rm() сносил
  # настоящую md_candles из глобальной области, и следующие разделы падали на
  # «could not find function». Прибор не должен ломать то, что измеряет.
  real_md_candles <- md_candles
  md_candles <<- function(ticker, days = NULL, online = FALSE) fake
  on.exit(assign("md_candles", real_md_candles, envir = .GlobalEnv), add = TRUE)
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
# Индексы из реестра убраны: источник их на нашем тарифе не отдаёт. Строки
# прогноза по ним просто не сопоставляются и отбрасываются при разборе.
ok("Nasdaq больше не сопоставляется", is.na(ticker_by_model_name("Nasdaq")))
ok("неизвестное имя -> NA",     is.na(ticker_by_model_name("Неизвестная Компания")))
ok("в реестре 24 инструмента", nrow(WATCHLIST) == 24)
# GOOGL и GOOG — разные бумаги с разной ценой. Счёт держит класс A, модель
# знает только GOOG, поэтому подменять одну другой нельзя.
ok("GOOGL и GOOG различаются",
   all(c("GOOG", "GOOGL") %in% WATCHLIST$ticker))
ok("у GOOGL нет строки модели",
   is.na(WATCHLIST[ticker == "GOOGL", name_model]))
ok("тикеры уникальны",          !any(duplicated(WATCHLIST$ticker)))
ok("имена модели уникальны",    !any(duplicated(tolower(trimws(WATCHLIST$name_model)))))
ok("индексов в реестре нет",
   !any(c("SPX", "DJI", "IXIC") %in% WATCHLIST$ticker))

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

cat("== 6. Нет котировок -> прочерк, а НЕ ноль ==\n")
# 25.09.2026 marketdata.app упёрся в лимит кредитов, все цены пришли NA, и
# стенд показал «стоимость $0, рост −100%» — уверенную неправду. Виноват был
# na.rm = TRUE в сумме: он превращает «неизвестно» в ноль. Проверка ловит
# именно это: итоги обязаны быть NA, а форматирование — давать прочерк.
noprice <- data.table(
  ticker = c("GS", "GE"), quantity = c(5, 16),
  entry_price = c(995.72, 318.28), current_price = NA_real_,
  entry_value = c(5, 16) * c(995.72, 318.28), current_value = NA_real_
)
sp <- summarize_portfolio(noprice)
ok("стоимость без цен = NA, не 0",   is.na(sp$current_value))
ok("рост без цен = NA, не -100%",    is.na(sp$growth_pct))
ok("посчитано, по скольким есть цена", identical(sp$priced, 0L) && identical(sp$total, 2L))
# Частичные данные тоже не должны «дорисовываться» нулём.
part <- data.table::copy(noprice)
part[1, `:=`(current_price = 1000, current_value = 5000)]
ok("часть цен известна -> итог всё равно NA",
   is.na(summarize_portfolio(part)$current_value))
ok("форматирование NA даёт прочерк",
   identical(fmt_money(NA_real_), "\u2014") &&
   identical(fmt_pct(NA_real_), "\u2014") &&
   identical(fmt_pp(NA_real_), "\u2014"))
# Источник обязан УМЕТЬ сказать «нет данных»: при ответе про лимит кредитов
# md_status_text() возвращает причину, а не NULL.
local({
  md_note_error(list(error = "marketdata_http_error", status = 429,
                     message = "You've reached your API credit limit."))
  ok("причина сбоя источника поднимается в интерфейс",
     grepl("лимит", md_status_text() %||% ""))
  .MD_STATE$last_error <- NULL
})

cat("== 7. Хранилище рядов ==\n")
# Стенд обязан читать ряды из хранилища и НЕ ходить в интернет: лимит
# marketdata.app — 100 запросов в сутки, а ползунок времени по всему реестру
# это сотни обращений на одно открытие экрана.
local({
  tmpstore <- file.path(tempdir(), paste0("store_", as.integer(runif(1, 1e6, 9e6))))
  old_dir <- BLNR_STORE_DIR
  BLNR_STORE_DIR <<- tmpstore
  on.exit({ BLNR_STORE_DIR <<- old_dir; unlink(tmpstore, recursive = TRUE) }, add = TRUE)

  ok("пустое хранилище -> пустой ряд, а не ошибка", nrow(store_read_candles("GS")) == 0)
  # Без хранилища и без разрешения на сеть стенд обязан СКАЗАТЬ причину,
  # а не молча вернуть NA, из которого потом получится $0.
  .MD_STATE$last_error <- NULL
  c0 <- md_candles("GS", online = FALSE)
  ok("без хранилища и без сети — пусто", nrow(c0) == 0)
  ok("причина названа: хранилище пусто",
     grepl("хранилищ", md_status_text() %||% ""))
  .MD_STATE$last_error <- NULL

  series <- data.table(
    date = seq(as.Date("2026-09-01"), by = "day", length.out = 10),
    open = 1, high = 2, low = 0.5,
    close = as.numeric(seq(100, 109)), volume = NA_real_
  )
  store_write_candles("GS", series)
  ok("ряд читается обратно целиком", nrow(store_read_candles("GS")) == 10)
  ok("тикер виден в списке хранилища", "GS" %in% store_tickers())
  # Ровно тот дефект, что уже ловили дважды: выборка по дате не должна
  # возвращать последнюю строку независимо от аргумента.
  ok("цена на дату берётся из хранилища",
     isTRUE(all.equal(md_close_on_date("GS", as.Date("2026-09-03"), online = FALSE), 102)))
  ok("разные даты -> разные цены",
     md_close_on_date("GS", as.Date("2026-09-03"), online = FALSE) !=
     md_close_on_date("GS", as.Date("2026-09-09"), online = FALSE))
  ok("текущая цена = закрытие последней свечи",
     isTRUE(all.equal(md_last_price("GS"), 109)))
  ok("дата текущей цены — последняя сессия",
     identical(md_last_price_date("GS"), as.Date("2026-09-10")))
  ok("глубина ограничивается аргументом days", nrow(md_candles("GS", days = 3)) == 3)

  # Откат должен быть копированием: повторно скачать нельзя, лимит конечен.
  store_rotate()
  vers <- list.dirs(file.path(BLNR_STORE_DIR, "versions"), recursive = FALSE)
  ok("прежняя версия сохраняется до записи", length(vers) == 1)
  ok("в снимке лежит копия ряда",
     file.exists(file.path(vers[1], "GS.csv")))

  store_write_meta(list(updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
                        instruments = 1, last_date = "2026-09-10"))
  st <- store_status()
  ok("состояние хранилища читается", isTRUE(st$ok) && st$instruments == 1)
  ok("дата последнего ряда видна", identical(st$last_date, as.Date("2026-09-10")))

  # Гейты качества загрузки. Каждый предъявлен входу, на котором обязан
  # ответить «нет», и входу, на котором обязан промолчать.
  good <- data.table(
    date = seq(as.Date("2026-07-01"), by = "day", length.out = 80),
    open = 1, high = 2, low = 0.5,
    close = as.numeric(seq(200, 279)), volume = NA_real_
  )
  ok("исправный ряд претензий не вызывает",
     length(store_verify_series(list(GS = good), today = as.Date("2026-09-19"))) == 0)
  ok("короткий ряд отклоняется",
     any(grepl("строк", store_verify_series(list(GS = good[1:10]),
                                            today = as.Date("2026-07-11")))))
  bad_na <- data.table::copy(good); bad_na[5, close := NA_real_]
  ok("пустая цена закрытия отклоняется",
     any(grepl("пустые цены", store_verify_series(list(GS = bad_na),
                                                  today = as.Date("2026-09-19")))))
  bad_dup <- data.table::copy(good); bad_dup[2, date := bad_dup$date[1]]
  ok("дубли дат отклоняются",
     any(grepl("возрастают", store_verify_series(list(GS = bad_dup),
                                                 today = as.Date("2026-09-19")))))
  # Самый коварный: файл целый, данные правдоподобные, но ряд заморожен —
  # витрина показала бы прошлое как настоящее.
  ok("замороженный ряд отклоняется",
     any(grepl("дней назад", store_verify_series(list(GS = good),
                                                 today = as.Date("2026-12-01")))))
  ok("пустой ряд отклоняется",
     any(grepl("пустой", store_verify_series(list(GS = empty_candles()),
                                             today = as.Date("2026-09-19")))))
})

cat("== 8. Портфель на момент времени ==\n")
# Ползунок времени показывает состояние на выбранную СЕССИЮ. Два дефекта,
# которые здесь легко допустить и невозможно заметить глазами:
#   * цена берётся последняя известная, а не на выбранную дату (портфель
#     «не реагирует» на ползунок, но выглядит правдоподобно);
#   * позиция считается существующей до даты покупки (портфель «был» за
#     полгода до того, как его купили).
local({
  tmpstore <- file.path(tempdir(), paste0("asof_", as.integer(runif(1, 1e6, 9e6))))
  old_dir <- BLNR_STORE_DIR
  BLNR_STORE_DIR <<- tmpstore
  on.exit({ BLNR_STORE_DIR <<- old_dir; unlink(tmpstore, recursive = TRUE) }, add = TRUE)

  ser <- function(from) data.table(
    date = seq(as.Date("2026-09-01"), by = "day", length.out = 20),
    open = 1, high = 2, low = 0.5,
    close = as.numeric(seq(from, from + 19)), volume = NA_real_
  )
  store_write_candles("GS", ser(1000))
  store_write_candles("GE", ser(300))

  holdings <- data.table(
    ticker = c("GS", "GE"), quantity = c(5, 16),
    entry_price = c(1000, 300), current_price = NA_real_,
    purchase_date = as.Date(c("2026-09-05", "2026-09-15")), source = "manual"
  )

  m10 <- build_portfolio_metrics(positions = holdings, ledger = empty_ledger(), as_of = as.Date("2026-09-10"))
  s10 <- summarize_portfolio(m10)
  # 10.09 — пятая свеча после 05.09: close GS = 1009. GE куплена только 15.09.
  ok("цена берётся на выбранную дату, а не последняя",
     isTRUE(all.equal(m10[ticker == "GS", price_at], 1009)))
  ok("бумага, купленная позже, в расчёт не входит", s10$positions == 1)
  ok("стоимость на дату по одной позиции",
     isTRUE(all.equal(s10$current_value, 5 * 1009)))

  m18 <- build_portfolio_metrics(positions = holdings, ledger = empty_ledger(), as_of = as.Date("2026-09-18"))
  s18 <- summarize_portfolio(m18)
  ok("после второй покупки позиций две", s18$positions == 2)
  # Сравнивать итоги двух дат мало: они различаются и из-за второй покупки.
  # Проверяем цену ОДНОЙ И ТОЙ ЖЕ бумаги — так дефект «цена всегда последняя»
  # не спрячется за изменением состава.
  ok("другая дата -> другая цена по той же бумаге",
     m10[ticker == "GS", price_at] != m18[ticker == "GS", price_at])
  # «За сессию» — от предыдущей торговой сессии, а не от даты покупки.
  ok("за сессию считается от предыдущей свечи",
     isTRUE(all.equal(m18[ticker == "GS", price_prev], 1016)))
  ok("итог за сессию суммирует только открытые позиции",
     isTRUE(all.equal(s18$day_pnl, 5 * (1017 - 1016) + 16 * (317 - 316))))

  m01 <- build_portfolio_metrics(positions = holdings, ledger = empty_ledger(), as_of = as.Date("2026-09-01"))
  ok("до первой покупки позиций нет", summarize_portfolio(m01)$positions == 0)

  # Ось времени — только реальные торговые дни, и общие для всех бумаг.
  store_write_candles("SHORT", ser(50)[1:5])
  ok("сессии — пересечение по всем бумагам", length(store_sessions()) == 5)
  ok("окно ретроспективы ограничено",
     length(store_sessions_window(3, tickers = c("GS", "GE"))) == 3)
  ok("окно отсчитывается от указанной даты",
     identical(max(store_sessions_window(3, as_of = as.Date("2026-09-10"),
                                         tickers = c("GS", "GE"))),
               as.Date("2026-09-10")))
})

cat("== 9. Реестр операций счёта ==\n")
# Реестр восстанавливает позиции, среднюю цену и кэш на ЛЮБУЮ дату. Сводка
# брокера знает только сегодня, и с ползунком времени этого мало: сегодняшний
# кэш рядом с прошлой стоимостью бумаг — состояние, которого не существовало.
local({
  # Схема как в ответе Exante: у сделки две ноги с общим orderId (инструмент
  # и деньги), комиссия — отдельной строкой с тем же orderId.
  leg <- function(i, d, type, sym, asset, amount, price = NA_real_, ord = "") {
    data.table(id = i, value_date = as.Date(d), type = type, symbol = sym,
               asset = asset, amount = amount, price = price, order_id = ord)
  }
  led <- rbindlist(list(
    leg(1, "2026-01-10", "FUNDING/WITHDRAWAL", "", "USD", 10000),
    leg(2, "2026-02-01", "TRADE", "AMD.NASDAQ", "AMD.NASDAQ",  10, 100, "o1"),
    leg(3, "2026-02-01", "TRADE", "AMD.NASDAQ", "USD",      -1000, NA,  "o1"),
    leg(4, "2026-02-01", "COMMISSION", "AMD.NASDAQ", "USD",     -5, NA,  "o1"),
    leg(5, "2026-03-01", "TRADE", "AMD.NASDAQ", "AMD.NASDAQ",  10, 200, "o2"),
    leg(6, "2026-03-01", "TRADE", "AMD.NASDAQ", "USD",      -2000, NA,  "o2"),
    leg(7, "2026-04-01", "TRADE", "AMD.NASDAQ", "AMD.NASDAQ", -10, 300, "o3"),
    leg(8, "2026-04-01", "TRADE", "AMD.NASDAQ", "USD",       3000, NA,  "o3"),
    # валютная конвертация: не бумага, в позиции попасть не должна
    leg(9, "2026-04-02", "AUTOCONVERSION", "", "EUR/USD",   -500, NA),
    leg(10, "2026-04-02", "AUTOCONVERSION", "", "USD",        540, NA)
  ))

  ok("кэш на дату считается по эту дату включительно",
     isTRUE(all.equal(ledger_cash_at(led, as.Date("2026-02-01")), 10000 - 1000 - 5)))
  ok("более поздние движения в кэш на дату не попадают",
     isTRUE(all.equal(ledger_cash_at(led, as.Date("2026-01-31")), 10000)))
  ok("кэш на конец учитывает всё",
     isTRUE(all.equal(ledger_cash_at(led), 10000 - 1000 - 5 - 2000 + 3000 + 540)))

  p1 <- ledger_positions_at(led, as.Date("2026-02-15"))
  ok("после первой покупки 10 штук", isTRUE(all.equal(p1[ticker == "AMD", quantity], 10)))
  # Комиссия в среднюю цену НЕ входит: Exante считает averagePrice без неё,
  # и расхождение со сводкой читалось бы как ошибка реестра.
  ok("средняя цена без комиссии", isTRUE(all.equal(p1[ticker == "AMD", avg_price], 100)))

  p2 <- ledger_positions_at(led, as.Date("2026-03-15"))
  ok("докупка усредняет цену", isTRUE(all.equal(p2[ticker == "AMD", avg_price], 150)))

  # Продажа половины лота: количество падает, а средняя цена остаётся —
  # стоимость уменьшается пропорционально, а не «запоминает» закрытый лот.
  p3 <- ledger_positions_at(led, as.Date("2026-04-15"))
  ok("после продажи половины осталось 10", isTRUE(all.equal(p3[ticker == "AMD", quantity], 10)))
  ok("средняя цена после продажи не изменилась",
     isTRUE(all.equal(p3[ticker == "AMD", avg_price], 150)))

  ok("валютная конвертация не стала позицией", !("EUR/USD" %in% p3$symbol))
  # Дата открытия — текущего лота, а не первой сделки по бумаге: распроданная
  # и купленная заново бумага иначе приписывает себе движение цены за время,
  # когда её не было.
  led_re <- rbindlist(list(
    leg(30, "2026-05-01", "TRADE", "IBM.NYSE", "IBM.NYSE",   5, 100, "r1"),
    leg(31, "2026-05-01", "TRADE", "IBM.NYSE", "USD",     -500, NA,  "r1"),
    leg(32, "2026-06-01", "TRADE", "IBM.NYSE", "IBM.NYSE",  -5, 120, "r2"),
    leg(33, "2026-06-01", "TRADE", "IBM.NYSE", "USD",       600, NA,  "r2"),
    leg(34, "2026-08-01", "TRADE", "IBM.NYSE", "IBM.NYSE",   3, 150, "r3"),
    leg(35, "2026-08-01", "TRADE", "IBM.NYSE", "USD",      -450, NA,  "r3")
  ))
  pr <- ledger_positions_at(led_re, as.Date("2026-09-01"))
  ok("дата открытия — текущего лота, а не первой сделки",
     identical(pr[ticker == "IBM", opened_date], as.Date("2026-08-01")))
  ok("первая сделка по бумаге сохранена отдельно",
     identical(pr[ticker == "IBM", first_date], as.Date("2026-05-01")))
  ok("после полной продажи позиции нет",
     nrow(ledger_positions_at(led_re, as.Date("2026-07-01"))) == 0)
  ok("до первой сделки позиций нет",
     nrow(ledger_positions_at(led, as.Date("2026-01-20"))) == 0)

  # Опцион НЕ должен сливаться с акцией: базовый тикер у них общий, и при
  # наивном сокращении опционная позиция оценивалась бы по цене акции.
  ok("опцион распознаётся по числу частей symbolId",
     identical(exante_is_option(c("AMD.NASDAQ", "AMD.CBOE.20G2026.C220")),
               c(FALSE, TRUE)))
  ok("акция сводится к тикеру",
     identical(exante_symbol_to_ticker("AMD.NASDAQ"), "AMD"))
  ok("опцион НЕ сводится к базовой бумаге",
     identical(exante_symbol_to_ticker("AMD.CBOE.20G2026.C220"),
               "AMD.CBOE.20G2026.C220"))
  led_opt <- rbindlist(list(
    led,
    leg(20, "2026-05-01", "TRADE", "AMD.CBOE.20G2026.C220",
        "AMD.CBOE.20G2026.C220", 2, 30, "o9"),
    leg(21, "2026-05-01", "TRADE", "AMD.CBOE.20G2026.C220", "USD", -6000, NA, "o9")
  ))
  po <- ledger_positions_at(led_opt, as.Date("2026-05-15"))
  ok("опцион стал отдельной позицией, а не прибавкой к акции",
     nrow(po[ticker == "AMD"]) == 1 && nrow(po[ticker == "AMD.CBOE.20G2026.C220"]) == 1)
  ok("количество акции не выросло от опциона",
     isTRUE(all.equal(po[ticker == "AMD", quantity], 10)))

  # Сверка со сводкой брокера обязана ЗАМЕЧАТЬ расхождение, иначе она
  # бесполезна: реестр, тихо разошедшийся с брокером, даёт правдоподобные
  # и неверные числа.
  api_ok  <- data.table(symbol = "AMD.NASDAQ", quantity = 10)
  api_bad <- data.table(symbol = "AMD.NASDAQ", quantity = 12)
  cash_now <- ledger_cash_at(led)
  ok("сверка молчит на совпадении",
     length(ledger_reconcile(led, api_ok, cash_now)) == 0)
  ok("сверка ловит расхождение по количеству",
     any(grepl("AMD", ledger_reconcile(led, api_bad, cash_now))))
  ok("сверка ловит расхождение по кэшу",
     any(grepl("кэш", ledger_reconcile(led, api_ok, cash_now + 100))))

  # Портфель целиком на дату: позиции из реестра плюс кэш на ту же дату.
  tmpstore <- file.path(tempdir(), paste0("led_", as.integer(runif(1, 1e6, 9e6))))
  old_dir <- BLNR_STORE_DIR; BLNR_STORE_DIR <<- tmpstore
  on.exit({ BLNR_STORE_DIR <<- old_dir; unlink(tmpstore, recursive = TRUE) }, add = TRUE)
  store_write_candles("AMD", data.table(
    date = seq(as.Date("2026-01-01"), by = "day", length.out = 120),
    open = 1, high = 2, low = 0.5,
    close = as.numeric(seq(100, 219)), volume = NA_real_))
  m <- build_portfolio_metrics(as_of = as.Date("2026-03-15"), ledger = led)
  sm <- summarize_portfolio(m)
  ok("кэш приезжает вместе с метриками",
     isTRUE(all.equal(sm$cash, ledger_cash_at(led, as.Date("2026-03-15")))))
  ok("итого = бумаги + кэш",
     isTRUE(all.equal(sm$total_value, sm$current_value + sm$cash)))
  ok("позиции взяты из реестра, а не из зашитого списка",
     identical(sort(unique(m$ticker)), "AMD"))

  # Реестр есть, но на эту дату позиций нет — это ОТВЕТ, а не отсутствие
  # данных: кэш при этом известен и показывается.
  m0 <- build_portfolio_metrics(as_of = as.Date("2026-01-20"), ledger = led)
  s0 <- summarize_portfolio(m0)
  ok("портфель до первой сделки пуст, но кэш известен",
     s0$positions == 0 && isTRUE(all.equal(s0$cash, 10000)))
})

cat("== 10. Сравнение с моделью за период владения ==\n")
# Прогноз накоплен от базовой даты файла. Позиция, купленная ПОЗЖЕ базы, при
# сравнении «от базы» присваивает себе движение цены за время, когда её не
# было: на боевом счёте это давало «обгоняем модель на 7.71 пп» при
# фактическом результате +0.79%.
local({
  fc <- data.table(
    date = as.Date(c("2026-09-11", "2026-09-18", "2026-09-25")),
    ticker = "GS", forecast_growth_pct = c(0, 10, 21)
  )
  ok("рост от базы берётся как есть",
     isTRUE(all.equal(forecast_between(fc, "GS", as.Date("2026-09-11"), as.Date("2026-09-25")), 21)))
  # Куплено 18.09, когда модель уже обещала +10%. К 25.09 обещано +21% от базы,
  # значит за период владения — (1.21/1.10 - 1) = 10%, а не 21%.
  ok("вход позже базы -> прогноз приводится к дате входа",
     isTRUE(all.equal(forecast_between(fc, "GS", as.Date("2026-09-18"), as.Date("2026-09-25")), 10)))
  ok("вход раньше базы -> считаем от базы",
     isTRUE(all.equal(forecast_between(fc, "GS", as.Date("2026-01-01"), as.Date("2026-09-25")), 21)))
  ok("неизвестный тикер -> NA",
     is.na(forecast_between(fc, "ZZZ", as.Date("2026-09-18"), as.Date("2026-09-25"))))

  m <- data.table(
    ticker = c("GS", "GS"), growth_pct = c(12, 12),
    growth_from_base_pct = c(23, 23),
    purchase_date = as.Date(c("2026-09-18", "2026-09-11"))
  )
  r <- add_forecast_to_metrics(m, fc, as_of = as.Date("2026-09-25"))
  ok("купленная позже сравнивается с приведённым прогнозом",
     isTRUE(all.equal(r[1, forecast_since_entry_pct], 10)) &&
     isTRUE(all.equal(r[1, dev_since_entry_pp], 2)))
  ok("купленная с базы сравнивается с полным прогнозом",
     isTRUE(all.equal(r[2, forecast_since_entry_pct], 21)))
  # Ровно тот артефакт, ради которого всё затевалось.
  ok("сравнение от базы и за период владения дают РАЗНОЕ",
     r[1, dev_pct] != r[1, dev_since_entry_pp])
})

cat("== 11. Динамика счёта не занижается молча ==\n")
# Инструмент, который держался, но цены на него нет (все опционы на текущем
# тарифе), раньше просто пропускался — стоимость бумаг занижалась, а линия
# оставалась гладкой и правдоподобной.
local({
  tmpstore <- file.path(tempdir(), paste0("vs_", as.integer(runif(1, 1e6, 9e6))))
  old_dir <- BLNR_STORE_DIR; BLNR_STORE_DIR <<- tmpstore
  on.exit({ BLNR_STORE_DIR <<- old_dir; unlink(tmpstore, recursive = TRUE) }, add = TRUE)
  sess <- seq(as.Date("2026-03-02"), by = "day", length.out = 10)
  store_write_candles("AMD", data.table(
    date = sess, open = 1, high = 2, low = 0.5,
    close = rep(100, 10), volume = NA_real_))

  leg <- function(i, d, type, sym, asset, amount, price = NA_real_, ord = "") {
    data.table(id = i, value_date = as.Date(d), type = type, symbol = sym,
               asset = asset, amount = amount, price = price, order_id = ord)
  }
  base <- list(
    leg(1, "2026-03-01", "FUNDING/WITHDRAWAL", "", "USD", 10000),
    leg(2, "2026-03-03", "TRADE", "AMD.NASDAQ", "AMD.NASDAQ", 10, 100, "a1"),
    leg(3, "2026-03-03", "TRADE", "AMD.NASDAQ", "USD", -1000, NA, "a1")
  )
  v1 <- portfolio_value_series(rbindlist(base), sess)
  ok("акция с ценой считается", isTRUE(all.equal(v1[date == as.Date("2026-03-05"), securities], 1000)))
  ok("до покупки бумаг ноль", isTRUE(all.equal(v1[date == as.Date("2026-03-02"), securities], 0)))

  # Добавляем ОПЦИОН, которого нет в хранилище цен.
  withopt <- rbindlist(c(base, list(
    leg(4, "2026-03-05", "TRADE", "AMD.CBOE.20G2026.C220", "AMD.CBOE.20G2026.C220", 2, 30, "o1"),
    leg(5, "2026-03-05", "TRADE", "AMD.CBOE.20G2026.C220", "USD", -6000, NA, "o1")
  )))
  v2 <- portfolio_value_series(withopt, sess)
  ok("неоценимый инструмент даёт NA, а не тихий ноль",
     is.na(v2[date == as.Date("2026-03-06"), securities]))
  ok("итог на такой сессии тоже NA",
     is.na(v2[date == as.Date("2026-03-06"), total]))
  ok("до его покупки сессии остаются посчитанными",
     isTRUE(all.equal(v2[date == as.Date("2026-03-04"), securities], 1000)))
  ok("список неоценимых инструментов возвращается",
     identical(attr(v2, "unpriced"), "AMD.CBOE.20G2026.C220"))
})

cat("== 12. Выгрузка в типовом формате мониторинга ==\n")
# Формат разобран по эталону владельца («OptionActual <дата>.xlsx»). Проверка
# держит его строение: если лист «Реестр» переедет или у листа инструмента
# сдвинется блок данных, файл перестанет открываться рабочими формулами —
# а по самому файлу это не видно, он выглядит целым.
local({
  ser <- function(tk) data.table(
    date = seq(as.Date("2026-06-01"), by = "day", length.out = 30),
    open = 1, high = 2, low = 0.5,
    close = as.numeric(seq(100, 129)), volume = 1000
  )
  wb <- build_monitoring_workbook(tickers = c("AAPL", "NVDA"), days = 30,
                                  series_fn = ser)
  f <- tempfile(fileext = ".xlsx")
  openxlsx::saveWorkbook(wb, f, overwrite = TRUE)
  on.exit(unlink(f), add = TRUE)

  # Порядок листов проверяем В ФАЙЛЕ: names(wb) отдаёт порядок СОЗДАНИЯ, а не
  # тот, в котором листы лягут в книгу (его задаёт worksheetOrder). Проверка по
  # names() краснела на исправном файле — мерила не то.
  sheets <- openxlsx::getSheetNames(f)
  ok("первый лист — Реестр", identical(sheets[1], "Реестр"))
  ok("есть сводный лист СборкаАкции", "СборкаАкции" %in% sheets)
  ok("лист на каждый инструмент, нумерация с 2",
     all(c("2", "3") %in% sheets))

  reg <- openxlsx::read.xlsx(f, sheet = "Реестр", colNames = FALSE, rows = 1:3)
  ok("шапка реестра как в эталоне",
     identical(as.character(unlist(reg[1, 1:4])),
               c("Имя листа", "Тикер", "Имя Компании", "OPTONCHAIN")))
  ok("в реестре строка на инструмент",
     identical(as.character(reg[2, 2]), "AAPL"))

  sh <- openxlsx::read.xlsx(f, sheet = "2", colNames = FALSE, rows = 1:5)
  ok("на листе инструмента шапка в строке 1, значения в строке 2",
     identical(as.character(sh[1, 1]), "Имя листа") &&
     identical(as.character(sh[2, 2]), "AAPL"))
  ok("заголовки данных в строке 3",
     identical(as.character(unlist(sh[3, 1:6])),
               c("Date", "Open", "High", "Low", "Close", "Volume")))
  ok("данные начинаются со строки 4", !is.na(sh[4, 5]))

  comb <- openxlsx::read.xlsx(f, sheet = "СборкаАкции", detectDates = TRUE)
  ok("в сводном листе все инструменты", nrow(comb) == 60)
  ok("в сводном листе колонка Symbol",
     "Symbol" %in% names(comb) && identical(sort(unique(comb$Symbol)), c("AAPL", "NVDA")))
  ok("даты записаны датами, а не текстом", inherits(comb$Date, "Date"))

  # Инструмент без ряда попадает в реестр со статусом, но своего листа НЕ
  # получает: пустой лист с заголовками читается как данные, которых нет.
  wb2 <- build_monitoring_workbook(
    tickers = c("AAPL", "NVDA"), days = 30,
    series_fn = function(tk) if (tk == "NVDA") empty_candles() else ser(tk))
  f2 <- tempfile(fileext = ".xlsx"); openxlsx::saveWorkbook(wb2, f2, overwrite = TRUE)
  on.exit(unlink(f2), add = TRUE)
  ok("у инструмента без данных листа нет", !("3" %in% openxlsx::getSheetNames(f2)))
  reg2 <- openxlsx::read.xlsx(f2, sheet = "Реестр", colNames = FALSE, rows = 1:3)
  ok("но в реестре он есть со статусом «нет данных»",
     any(grepl("нет данных", as.character(unlist(reg2)))))
})

cat("== 13. Сборка интерфейса ==\n")
# Гейт против класса дефектов «экран не собрался», который до выкладки ничем
# не виден: перекрытые имена функций (jsonlite::validate поверх shiny::validate,
# httr::config поверх plotly::config), пакет, нужный при СБОРКЕ UI, но
# подключённый только в server.R, потерянный source() после перестановки
# модулей. Всё это даёт белый экран уже на проде, а не ошибку при старте.
ui_render <- tryCatch({
  suppressWarnings(suppressMessages({
    library(shiny); library(plotly); library(shinyjs); library(shinyWidgets)
  }))
  source("R/ui_kit.R"); source("ui/login_ui.R"); source("ui/dashboard_ui.R")
  # as.character() на теге НЕДОСТАТОЧНО: htmltools выносит содержимое
  # tags$head в отдельное поле renderTags()$head, поэтому весь CSS экрана в
  # строку не попадает — проверки по стилям тогда «проходят» на пустоте.
  flat <- function(x) {
    rt <- htmltools::renderTags(x)
    paste(paste(as.character(rt$head), collapse = "\n"),
          paste(as.character(rt$html), collapse = "\n"), sep = "\n")
  }
  list(login = flat(loginUI()), dash = flat(dashboardUI()))
}, error = function(e) e)

ok("UI собирается без ошибки",
   !inherits(ui_render, "condition"))
if (!inherits(ui_render, "condition")) {
  ok("в форме входа есть поля логина и пароля",
     grepl("auth_login", ui_render$login, fixed = TRUE) &&
     grepl("auth_password", ui_render$login, fixed = TRUE))
  ok("на дашборде есть левая колонка и график инструмента",
     grepl("left_col", ui_render$dash, fixed = TRUE) &&
     grepl("chart_instrument", ui_render$dash, fixed = TRUE))
  # Выбор инструмента должен быть проставлен прямо в разметке: сделанный из
  # сервера updateSelectInput доходит до клиента раньше, чем появляется сам
  # виджет, и график остаётся пустым.
  ok("в выпадающем списке предвыбран инструмент портфеля",
     grepl(sprintf("selected>%s", portfolio_holdings$ticker[1]), ui_render$dash) ||
     grepl(sprintf("value=\"%s\" selected", portfolio_holdings$ticker[1]), ui_render$dash))
  ok("в списке весь реестр наблюдения",
     sum(vapply(WATCHLIST$ticker,
                function(tk) grepl(sprintf(">%s · |>%s · ", tk, tk),
                                   ui_render$dash), logical(1))) >= 20)
  # Пояснения живут под «i» (конституция): виджета, содержимого которого —
  # только текст-подсказка, на экране быть не должно. Разметка — как на
  # portfolio.dtwin.ru: вложенный .tip, а не ::after.
  ok("подсказки оформлены вложенным .tip под «i»",
     grepl("class=\"ii ", ui_render$dash, fixed = TRUE) &&
     grepl("class=\"tip\"", ui_render$dash, fixed = TRUE))
  # Подсказка шириной 360px без привязки к краю уезжает за границу экрана и
  # обрезается. Каждый значок обязан нести класс края — «ii» без него значит,
  # что кто-то добавил подсказку и про край не подумал.
  ok("у каждой подсказки задан край привязки",
     !grepl("class=\"ii\"", ui_render$dash, fixed = TRUE) &&
     grepl("\\.ii\\.l \\.tip\\{", ui_render$dash) &&
     grepl("\\.ii\\.r \\.tip\\{", ui_render$dash))
  # Карточка обрезает всплывающую подсказку, если ей вернуть overflow:hidden.
  ok("карточка не обрезает всплывающую подсказку",
     !grepl("\\.card\\{[^}]*overflow:hidden", ui_render$dash))
  # Оформление берётся с portfolio.dtwin.ru: оранжевый — тонкой линейкой под
  # шапкой страницы и заголовком карточки, а не заливкой.
  ok("шапка и заголовки карточек с оранжевой линейкой",
     grepl("\\.hdr\\{[^}]*border-bottom:3px solid var\\(--orange\\)", ui_render$dash) &&
     grepl("\\.ch\\{[^}]*border-bottom:2px solid var\\(--orange\\)", ui_render$dash))
  # display:contents убирает обёртку uiOutput из РАСКЛАДКИ, но не из дерева
  # для селекторов: правило через `>` к панели внутри неё не применяется
  # (высота нижнего ряда молча терялась, и он распирал экран).
  ok("высота нижнего ряда задана селектором потомка",
     grepl("\\.blnr-row--bot \\.card\\{height", ui_render$dash) &&
     !grepl("\\.blnr-row--bot>\\.card\\{height", ui_render$dash))
}

cat(sprintf("\nИтог: %s\n", if (FAILED == 0L) "все проверки пройдены" else sprintf("ПРОВАЛОВ: %d", FAILED)))
quit(status = if (FAILED == 0L) 0L else 1L)
