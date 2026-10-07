# R/signals.R
#
# Сигнал фиксации: позиция или сделка, которая по ГОДОВОЙ доходности перешла
# порог вверх (повод подумать о фиксации прибыли) или вниз (повод подумать о
# фиксации убытка).
#
# ПОЧЕМУ ГОДОВАЯ, А НЕ АБСОЛЮТНАЯ. «+8%» значит разное за неделю и за год:
# за неделю это блестяще, за год — скромно. Приводим к году, чтобы сравнивать
# решения разного возраста на одной мерке. Приведение ЛИНЕЙНОЕ
# (pnl_pct * 365 / дни), как думает человек: «4% за месяц ≈ 48% годовых».
# Сложный процент за короткий срок уходит в тысячи процентов и только пугает.
#
# ДВА ПРЕДОХРАНИТЕЛЯ, без которых сигнал врёт:
#   1. СРОК. Позиция моложе min_days не сигналит вообще. За один день +0.5%
#      дают +180% годовых — это шум, а не повод фиксировать.
#   2. АБСОЛЮТ. Движение меньше min_abs_pct не сигналит, как бы внушительно
#      оно ни выглядело в пересчёте на год: фиксировать +1.5% бессмысленно.
# Оба предохранителя И порог настраиваются переменными окружения — пороги
# фиксации у каждого свои.

BLNR_SIGNAL_ANNUAL_PCT <- as.numeric(Sys.getenv("BLNR_SIGNAL_ANNUAL_PCT", unset = "30"))
BLNR_SIGNAL_MIN_DAYS   <- as.integer(Sys.getenv("BLNR_SIGNAL_MIN_DAYS",   unset = "7"))
BLNR_SIGNAL_MIN_ABS_PCT<- as.numeric(Sys.getenv("BLNR_SIGNAL_MIN_ABS_PCT",unset = "4"))

# Годовая доходность по результату за срок владения. NA, если срок неизвестен
# или нулевой — делить на ноль нельзя, а «за ноль дней» годовых не бывает.
annualized_pct <- function(pnl_pct, days) {
  days <- as.numeric(days)
  out <- pnl_pct * 365 / days
  out[!is.finite(pnl_pct) | !is.finite(days) | days < 1] <- NA_real_
  out
}

# Сигнал по одной позиции/сделке. Возвращает "take_profit", "cut_loss" или "" .
fixation_signal <- function(pnl_pct, days,
                            annual = BLNR_SIGNAL_ANNUAL_PCT,
                            min_days = BLNR_SIGNAL_MIN_DAYS,
                            min_abs = BLNR_SIGNAL_MIN_ABS_PCT) {
  if (!is.finite(pnl_pct) || !is.finite(days)) return("")
  if (days < min_days) return("")
  if (abs(pnl_pct) < min_abs) return("")
  ann <- annualized_pct(pnl_pct, days)
  if (!is.finite(ann)) return("")
  if (ann >= annual)  return("take_profit")
  if (ann <= -annual) return("cut_loss")
  ""
}

# Человекочитаемое пояснение сигнала — для подсказки и для будущего
# уведомления. Сознательно НЕ «продай» / «купи»: стенд сигналит порог, а
# решение остаётся за владельцем.
signal_text <- function(signal, pnl_pct, days) {
  ann <- annualized_pct(pnl_pct, days)
  base <- if (is.finite(ann)) sprintf("%+.0f%% годовых (%+.1f%% за %d дн)",
                                      ann, pnl_pct, as.integer(days)) else ""
  switch(signal,
    take_profit = paste0("Прибыль выше порога: ", base, ". Повод подумать о фиксации."),
    cut_loss    = paste0("Убыток ниже порога: ", base, ". Повод подумать о фиксации."),
    "")
}
