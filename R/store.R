# R/store.R
#
# Локальное хранилище рядов котировок.
#
# Зачем оно вообще. У аккаунта marketdata.app лимит 100 запросов в сутки
# (проверено 25.09.2026: заголовок x-api-ratelimit-limit). Ползунок времени на
# 150 дней по всему реестру наблюдения — это сотни обращений на одно открытие
# экрана, то есть лимит выгорает с первого пользователя. Поэтому ряды тянет
# ОДИН раз в сутки задание Jenkins (scripts/fetch_marketdata.R), кладёт сюда, а
# стенд читает только отсюда и в интернет не ходит вовсе.
#
# Это же требование конституции ЦД: повторяющееся обновление данных —
# заданием Jenkins, с сохранением прежней версии ДО сборки, падением на
# отсутствующем источнике и проверкой результата.
#
# Каталог задаётся BLNR_STORE_DIR и ОБЯЗАН лежать вне рабочей копии: стадия
# деплоя делает `git clean -fdx`, и всё, что внутри репозитория, стирается при
# каждой выкатке (так уже сгорели кэш котировок и лог снимков, 25.09.2026).

BLNR_STORE_DIR <- Sys.getenv("BLNR_STORE_DIR", unset = file.path("data", "store"))

# Глубина ряда, которую держим в хранилище. Больше, чем показывает экран:
# ползунок должен уметь отойти назад и всё равно иметь слева ретроспективу.
BLNR_STORE_DAYS <- as.integer(Sys.getenv("BLNR_STORE_DAYS", unset = "400"))

store_candles_dir <- function() file.path(BLNR_STORE_DIR, "candles")
store_candles_path <- function(ticker) {
  file.path(store_candles_dir(), paste0(toupper(trimws(ticker)), ".csv"))
}
store_meta_path <- function() file.path(BLNR_STORE_DIR, "meta.json")

empty_candles <- function() {
  data.table::data.table(
    date = as.Date(character()), open = numeric(), high = numeric(),
    low = numeric(), close = numeric(), volume = numeric()
  )
}

# Читает ряд из хранилища. Пустая таблица, если ряда нет или файл битый —
# вызывающий код обязан отличать это от «цена равна нулю».
# Память процесса на прочитанные ряды. Нужна из-за ползунка времени: каждый
# его шаг пересчитывает портфель, а это десяток чтений одних и тех же файлов.
# Ключ включает время правки файла, поэтому после ночной загрузки память
# обновляется сама и подсунуть устаревший ряд не может.
.STORE_MEM <- new.env(parent = emptyenv())

store_read_candles <- function(ticker) {
  f <- store_candles_path(ticker)
  if (!file.exists(f)) return(empty_candles())
  key <- paste0(f, "@", as.numeric(file.mtime(f)))
  hit <- .STORE_MEM[[key]]
  if (!is.null(hit)) return(hit)

  dt <- tryCatch(data.table::fread(f), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0 || !all(c("date", "close") %in% names(dt))) {
    return(empty_candles())
  }
  dt[, date := as.Date(date)]
  data.table::setorder(dt, date)
  # Чистим прошлые версии этого же ряда, чтобы память не росла после загрузок.
  old <- grep(paste0("^", f, "@"), ls(.STORE_MEM), value = TRUE, fixed = FALSE)
  if (length(old)) rm(list = old, envir = .STORE_MEM)
  assign(key, dt[], envir = .STORE_MEM)
  dt[]
}

store_write_candles <- function(ticker, dt) {
  rm(list = ls(.STORE_MEM), envir = .STORE_MEM)
  dir.create(store_candles_dir(), showWarnings = FALSE, recursive = TRUE)
  data.table::setorder(dt, date)
  data.table::fwrite(dt, store_candles_path(ticker))
  invisible(TRUE)
}

store_tickers <- function() {
  f <- list.files(store_candles_dir(), pattern = "\\.csv$")
  toupper(sub("\\.csv$", "", f))
}

# --- Метаданные прогона -----------------------------------------------------
# Без них по самому хранилищу не отличить свежий ряд от замороженного полгода
# назад: файлы на месте, данные правдоподобные, а витрина показывает прошлое
# как настоящее. Дата в мете выводится на экран.
store_read_meta <- function() {
  f <- store_meta_path()
  if (!file.exists(f)) return(NULL)
  tryCatch(jsonlite::fromJSON(f), error = function(e) NULL)
}

store_write_meta <- function(meta) {
  dir.create(BLNR_STORE_DIR, showWarnings = FALSE, recursive = TRUE)
  jsonlite::write_json(meta, store_meta_path(), auto_unbox = TRUE, pretty = TRUE)
  invisible(TRUE)
}

# Состояние хранилища для интерфейса: когда обновлялось, сколько инструментов,
# до какой даты доведены ряды. NULL-поля означают «загрузка ещё не отработала».
store_status <- function() {
  meta <- store_read_meta()
  tks <- store_tickers()
  list(
    updated_at  = if (!is.null(meta$updated_at)) as.POSIXct(meta$updated_at, tz = "UTC") else NULL,
    instruments = length(tks),
    last_date   = if (!is.null(meta$last_date)) as.Date(meta$last_date) else NULL,
    ok          = length(tks) > 0
  )
}

# --- Ротация ----------------------------------------------------------------
# Откат должен быть копированием, а не повторным скачиванием: лимит запросов
# конечен, и «просто перезалить» после неудачной загрузки нельзя.
store_rotate <- function(keep = 7L) {
  src <- store_candles_dir()
  if (!dir.exists(src)) return(invisible(FALSE))
  vdir <- file.path(BLNR_STORE_DIR, "versions")
  dst <- file.path(vdir, format(Sys.time(), "%Y-%m-%d_%H%M%S"))
  dir.create(dst, showWarnings = FALSE, recursive = TRUE)
  file.copy(list.files(src, full.names = TRUE), dst, overwrite = TRUE)
  old <- sort(list.dirs(vdir, recursive = FALSE), decreasing = TRUE)
  if (length(old) > keep) unlink(old[(keep + 1L):length(old)], recursive = TRUE)
  invisible(TRUE)
}

# --- Проверка результата загрузки ------------------------------------------
# Прогон без ошибки и с мусором на выходе — обычное дело, поэтому ряды
# проверяются ДО записи в хранилище. Возвращает вектор претензий; пустой
# вектор означает «можно писать».
#
# Что именно ловим и почему:
#   короткий ряд   — источник отдал огрызок, а не «мало истории»;
#   пустые закрытия — дырки молча превращаются в NA и дальше в нули на витрине;
#   неупорядоченные или дублирующиеся даты — выборка по дате станет неверной;
#   устаревший хвост — ряд заморожен, файл при этом выглядит целым, и витрина
#                      показывает прошлое как настоящее (ровно так стенд 200
#                      два года показывал снимок 2024 года).
store_verify_series <- function(series, today = Sys.Date(),
                                min_rows = 60L, max_stale_days = 5L) {
  issues <- character()
  for (tk in names(series)) {
    cnd <- series[[tk]]
    if (is.null(cnd) || nrow(cnd) == 0) {
      issues <- c(issues, sprintf("%s: пустой ряд", tk)); next
    }
    if (nrow(cnd) < min_rows) {
      issues <- c(issues, sprintf("%s: всего %d строк (ожидалось >= %d)",
                                  tk, nrow(cnd), min_rows))
    }
    if (any(is.na(cnd$close))) {
      issues <- c(issues, sprintf("%s: есть пустые цены закрытия", tk))
    }
    if (is.unsorted(cnd$date, strictly = TRUE)) {
      issues <- c(issues, sprintf("%s: даты не строго возрастают (дубли или разнобой)", tk))
    }
    stale <- as.integer(as.Date(today) - max(cnd$date))
    if (stale > max_stale_days) {
      issues <- c(issues, sprintf("%s: ряд кончается %s — %d дней назад",
                                  tk, format(max(cnd$date), "%d.%m.%Y"), stale))
    }
  }
  issues
}

# --- Торговые сессии --------------------------------------------------------
# Ось времени для ползунка строится по РЕАЛЬНЫМ торговым дням из хранилища, а
# не по календарю: календарная шкала даёт выходные и праздники, на которых
# цены нет, и пользователь выбирает дату, для которой нечего показать.
#
# Берём пересечение дат по указанным бумагам (по умолчанию — по всему
# реестру): сессия годится, только если цена известна по всем инструментам,
# иначе портфель на эту дату посчитался бы по неполному набору.
store_sessions <- function(tickers = store_tickers()) {
  tickers <- tickers[nzchar(tickers)]
  if (length(tickers) == 0) return(as.Date(character()))
  dates <- NULL
  for (tk in tickers) {
    d <- store_read_candles(tk)$date
    if (length(d) == 0) return(as.Date(character()))
    dates <- if (is.null(dates)) d else intersect(dates, d)
  }
  sort(as.Date(dates, origin = "1970-01-01"))
}

# Последние `n` сессий по состоянию на дату (включительно). Это и есть
# «ретроспектива слева от фактической даты».
store_sessions_window <- function(n = 150L, as_of = NULL,
                                  tickers = store_tickers()) {
  s <- store_sessions(tickers)
  if (length(s) == 0) return(s)
  if (!is.null(as_of)) s <- s[s <= as.Date(as_of)]
  utils::tail(s, n)
}

# --- Реестр операций счёта --------------------------------------------------
# Хранится рядом с рядами котировок и по тем же причинам: тянуть 450 операций
# из Exante на каждое движение ползунка незачем, а на сервере кред может не
# быть вовсе — тогда стенд работает на последнем сохранённом реестре и честно
# показывает его дату.
store_ledger_path <- function() file.path(BLNR_STORE_DIR, "ledger.csv")

store_read_ledger <- function() {
  f <- store_ledger_path()
  if (!file.exists(f)) return(empty_ledger())
  dt <- tryCatch(data.table::fread(f), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0) return(empty_ledger())
  dt[, value_date := as.Date(value_date)]
  for (col in c("type", "symbol", "asset", "order_id")) {
    if (col %in% names(dt)) dt[[col]] <- as.character(dt[[col]])
  }
  data.table::setorder(dt, value_date, id)
  dt[]
}

store_write_ledger <- function(ledger) {
  dir.create(BLNR_STORE_DIR, showWarnings = FALSE, recursive = TRUE)
  data.table::fwrite(ledger, store_ledger_path())
  invisible(TRUE)
}

store_has_ledger <- function() {
  f <- store_ledger_path()
  file.exists(f) && file.info(f)$size > 0
}

# Когда реестр последний раз обновлялся из Exante.
store_ledger_updated <- function() {
  f <- store_ledger_path()
  if (!file.exists(f)) return(NULL)
  file.mtime(f)
}
