# R/forecast_store.R
#
# Где живут экселевские файлы прогноза, пока модель не встроена в стенд.
#
# КАК БЫЛО И ЧЕМ ПЛОХО. Путь к файлу задавался переменной FORECAST_XLSX_PATH и
# указывал на ОДИН файл (на сервере — /mnt/data-external/Blnr-data/forecast.xlsx,
# в коде по умолчанию — ~/Downloads/...). Новый расчёт клался поверх старого:
#   * прежняя версия исчезала, и «а что мы сравнивали неделю назад» ответа не
#     имело;
#   * по файлу не читалось, на какую дату он рассчитан, кто и когда его положил;
#   * файл, поданный через форму на экране, жил ровно до конца сессии Shiny:
#     воркер перезапускался — и прогноз пропадал у всех;
#   * каталог не совпадал с хранилищем рядов, то есть за данными стенда надо
#     было следить в двух местах.
#
# КАК СТАЛО. Каталог `forecasts/` внутри хранилища (BLNR_STORE_DIR): рядом с
# рядами котировок и реестром операций, ВНЕ рабочей копии — стадия деплоя
# делает `git clean -fdx`. Каждый загруженный файл кладётся отдельным именем и
# не затирает прежние; рядом реестр registry.csv: база прогноза, горизонт,
# число бумаг, лист, кто и когда загрузил, отпечаток содержимого.
#
# Стенд при старте берёт САМЫЙ СВЕЖИЙ по базе прогноза. Прежние остаются
# доступными и скачиваются с экрана — сравнение «что обещала позапрошлая
# версия» перестаёт быть археологией.
#
# Имя файла — `<база>-<8 знаков отпечатка>.xlsx`, например
# `2026-09-11-3f9a1c02.xlsx`. Дата первой: файлы сортируются сами. Отпечаток
# в имени: один и тот же расчёт, поданный дважды, не заводит второй записи, а
# два разных расчёта на одну базу не затирают друг друга.

# БАЗА ПРОГНОЗА — свойство ФАЙЛА, а не константа кода.
#
# parse_forecast_file раскладывает шаги .qM0, .qM1, … по торговым дням от
# base_date, и по умолчанию это глобальная FORECAST_BASELINE_DATE. Пока файл
# был один, разницы не было. Как только версий становится несколько, общая
# константа делает хранилище бессмысленным: расчёт, сделанный в октябре,
# лёг бы на сентябрьскую базу, то есть его кривая уехала бы на месяц назад, а
# «свежая версия» перестала бы что-либо означать.
#
# Поэтому база спрашивается при загрузке и хранится в реестре. Подсказка для
# поля — дата из имени файла («quotes 2026-09-13.xlsx»), иначе дата правки
# самого файла: человеку остаётся подтвердить, а не вспоминать.
forecast_guess_base <- function(path, orig_name = basename(path)) {
  m <- regmatches(orig_name, regexpr("20\\d{2}[-._]\\d{2}[-._]\\d{2}", orig_name))
  if (length(m) == 1) {
    d <- as.Date(gsub("[._]", "-", m))
    if (!is.na(d)) return(d)
  }
  if (file.exists(path)) return(as.Date(file.mtime(path)))
  Sys.Date()
}

forecast_dir <- function() file.path(BLNR_STORE_DIR, "forecasts")
forecast_registry_path <- function() file.path(forecast_dir(), "registry.csv")

forecast_registry_empty <- function() {
  data.table::data.table(
    file = character(), base_date = as.Date(character()),
    horizon = as.Date(character()), tickers = integer(), sheet = character(),
    as_fraction = logical(), orig_name = character(), user = character(),
    saved_at = as.POSIXct(character()), sha256 = character()
  )
}

forecast_registry <- function() {
  f <- forecast_registry_path()
  if (!file.exists(f)) return(forecast_registry_empty())
  dt <- tryCatch(data.table::fread(f), error = function(e) NULL)
  if (is.null(dt) || nrow(dt) == 0 || !"file" %in% names(dt)) {
    return(forecast_registry_empty())
  }
  for (col in names(forecast_registry_empty())) {
    if (!col %in% names(dt)) data.table::set(dt, j = col, value = NA)
  }
  dt[, base_date := as.Date(base_date)]
  dt[, horizon := as.Date(horizon)]
  dt[, saved_at := as.POSIXct(saved_at)]
  # Строка реестра без файла на диске — это не запись, а обещание. Такие
  # прячем: иначе стенд предложит выбрать прогноз, которого нет.
  dt <- dt[file.exists(file.path(forecast_dir(), file))]
  data.table::setorder(dt, -base_date, -saved_at)
  dt[]
}

# Положить файл в хранилище. `path` — путь к временному файлу (fileInput его
# кладёт во временный каталог, живущий до конца сессии). Возвращает
# list(ok, message, id), где id — имя файла в хранилище.
forecast_store_save <- function(path, orig_name = basename(path),
                                sheet = NULL, as_fraction = TRUE,
                                base_date = NULL, user = "") {
  if (!file.exists(path)) return(list(ok = FALSE, message = "файла нет"))
  if (is.null(base_date) || is.na(base_date)) {
    base_date <- forecast_guess_base(path, orig_name)
  }
  base_date <- as.Date(base_date)
  parsed <- tryCatch(parse_forecast_file(path, sheet = sheet,
                                         as_fraction = as_fraction,
                                         base_date = base_date),
                     error = function(e) e)
  if (inherits(parsed, "error")) {
    return(list(ok = FALSE, message = paste("файл не разобран:",
                                            conditionMessage(parsed))))
  }
  if (is.null(parsed) || nrow(parsed) == 0) {
    return(list(ok = FALSE, message = "в файле не нашлось ни одной строки прогноза"))
  }
  sha <- paste(openssl::sha256(file(path, "rb")), collapse = "")
  reg <- forecast_registry()
  hit <- reg[sha256 == sha]
  if (nrow(hit) > 0) {
    return(list(ok = TRUE, id = hit$file[1], duplicate = TRUE,
                message = sprintf("Этот файл уже в хранилище (загружен %s).",
                                  format(hit$saved_at[1], "%d.%m.%Y"))))
  }
  # В реестр пишем ЗАЯВЛЕННУЮ базу, а не min(parsed$date): если база выпала
  # на выходной, шаг 0 сдвинут на ближайший торговый день, и запись «база
  # 14.09» вместо «12.09» рассказывала бы о файле не то.
  base <- base_date
  id <- sprintf("%s-%s.xlsx", format(base, "%Y-%m-%d"), substr(sha, 1, 8))
  dir.create(forecast_dir(), showWarnings = FALSE, recursive = TRUE)
  ok <- file.copy(path, file.path(forecast_dir(), id), overwrite = TRUE)
  if (!isTRUE(ok)) return(list(ok = FALSE, message = "не удалось записать в хранилище"))
  row <- data.table::data.table(
    file = id, base_date = base, horizon = max(parsed$date),
    tickers = length(unique(parsed$ticker)),
    sheet = if (is.null(sheet)) NA_character_ else as.character(sheet),
    as_fraction = isTRUE(as_fraction),
    orig_name = orig_name, user = user, saved_at = Sys.time(), sha256 = sha)
  data.table::fwrite(data.table::rbindlist(list(reg, row), use.names = TRUE,
                                           fill = TRUE),
                     forecast_registry_path())
  list(ok = TRUE, id = id, duplicate = FALSE,
       message = sprintf("Прогноз сохранён: база %s, бумаг %d.",
                         format(base, "%d.%m.%Y"), row$tickers))
}

# Разобрать сохранённый прогноз по его id. Лист и «доли/проценты» берутся из
# реестра: те же значения, что были при загрузке, иначе один и тот же файл
# читался бы по-разному в разные дни.
forecast_store_read <- function(id) {
  reg <- forecast_registry()
  r <- reg[file == id]
  if (nrow(r) == 0) return(NULL)
  f <- file.path(forecast_dir(), id)
  if (!file.exists(f)) return(NULL)
  sheet <- if (is.na(r$sheet[1]) || !nzchar(r$sheet[1])) NULL else r$sheet[1]
  tryCatch(parse_forecast_file(f, sheet = sheet,
                               as_fraction = isTRUE(r$as_fraction[1]),
                               base_date = r$base_date[1]),
           error = function(e) NULL)
}

# Самый свежий прогноз: по базе, при равной базе — по времени загрузки.
# Возвращает строку реестра или NULL.
forecast_store_latest <- function() {
  reg <- forecast_registry()
  if (nrow(reg) == 0) return(NULL)
  reg[1]
}

# Разовый перенос файла из прежнего одиночного пути в хранилище. Нужен, чтобы
# стенд не остался без прогноза в первый запуск после выкатки и чтобы старый
# путь перестал быть источником правды сам собой, без ручного шага.
#
# База переносимого файла — та самая FORECAST_BASELINE_DATE: именно с ней он
# до сих пор и читался, и менять её задним числом значило бы подменить то, с
# чем стенд сравнивал всё это время.
forecast_store_seed <- function(path = FORECAST_XLSX_PATH) {
  if (nrow(forecast_registry()) > 0) return(invisible(NULL))
  p <- path.expand(path)
  if (!file.exists(p)) return(invisible(NULL))
  res <- forecast_store_save(p, orig_name = basename(p),
                             base_date = FORECAST_BASELINE_DATE,
                             user = "перенос")
  invisible(res)
}
