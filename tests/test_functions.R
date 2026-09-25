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
source("R/exante_api.R"); source("R/portfolio.R"); source("R/forecast.R")
source("R/auth_ad.R"); source("R/ui_kit.R")

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
ok("в реестре 23 инструмента", nrow(WATCHLIST) == 23)
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

cat("== 8. Сборка интерфейса ==\n")
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
