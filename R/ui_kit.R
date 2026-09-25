# R/ui_kit.R
#
# Кирпичи интерфейса стенда: карточка, KPI-плитка, подсказка под «i»,
# форматирование чисел.
#
# ОФОРМЛЕНИЕ НЕ ПРИДУМАНО ЗДЕСЬ. Токены, размеры и структура блоков взяты с
# действующего стенда portfolio.dtwin.ru (витрина проекта 290, самый свежий
# образец мониторинга ЦД) — чтобы два стенда компании выглядели как один
# продукт, а не как две разные работы. Оттуда же и роль оранжевого: он не
# заливает шапки карточек, а идёт тонкой линейкой (3px под шапкой страницы,
# 2px под заголовком карточки) и заливкой активной кнопки. Красный и зелёный
# остаются только под рост и падение.
#
# Правило конституции, которое здесь зашито: пояснительный текст не занимает
# места на экране — он живёт под «i» в ШАПКЕ своего виджета (info_tip), а
# карточка рисуется только когда в ней есть результат (за это отвечает
# вызывающий код, возвращая NULL из renderUI).

BLNR_COLORS <- list(
  bg       = "#f3f4f7",
  surface  = "#ffffff",
  muted    = "#f8f9fb",
  orange   = "#FFA500",
  orange_d = "#CC8400",
  on_orange= "#1a1814",
  text     = "#1f2430",
  dim      = "#646b78",
  faint    = "#9aa1ad",
  border   = "#dde1e7",
  grid     = "#eceff3",
  ok       = "#2e7d32",
  bad      = "#c62828",
  warn     = "#ef6c00",
  plan     = "#8a93a3",
  fact     = "#1f2430",
  series   = "#1F77B4"
)

# Подсказка под «i». Разметка как на portfolio.dtwin.ru: текст лежит вложенным
# элементом, а не в ::after — так он доступен и по наведению, и по фокусу с
# клавиатуры, и его видно проверкой по HTML.
#
# `align` обязателен и без умолчания «по центру»: подсказка шириной 360px,
# центрированная по значку, у левого края экрана уезжает в отрицательные
# координаты и обрезается (поймано на карточке «Позиции»: x = −85).
# "l" — прижать к левому краю значка (значок в левой части экрана),
# "r" — к правому (значок в правой трети).
info_tip <- function(text, align = c("l", "r")) {
  align <- match.arg(align)
  tags$span(class = paste("ii", align), tabindex = "0", "i",
            tags$span(class = "tip", text))
}

# Карточка с шапкой. Заголовок — жирная строка на белом с оранжевой линейкой
# снизу; цветной «плашки» у шапки нет.
panel <- function(title, ..., tip = NULL, tip_align = "l", sub = NULL,
                  right = NULL, body_class = NULL) {
  tags$section(
    class = "card",
    tags$header(
      class = "ch",
      tags$span(class = "t", title),
      if (!is.null(tip)) info_tip(tip, tip_align),
      if (!is.null(sub)) tags$span(class = "sub", sub),
      tags$span(class = "sp"),
      right
    ),
    tags$div(class = paste("bd", body_class), ...)
  )
}

# KPI-плитка: подпись сверху, крупное число, необязательная вторая строка.
# tone — "pos"/"neg"/NULL, красит только само число.
kpi <- function(value, label, tone = NULL, tip = NULL, tip_align = "l",
                sub = NULL) {
  tags$div(
    class = "k",
    tags$div(class = "h", label, if (!is.null(tip)) info_tip(tip, tip_align)),
    tags$div(class = paste("v", tone), value),
    if (!is.null(sub)) tags$div(class = "s", sub)
  )
}

# Компактное пустое состояние ВНУТРИ уже существующей карточки. Отдельную
# карточку ради строчки текста заводить нельзя (конституция), но и оставлять
# белую коробку без объяснения — тоже: пользователь решит, что стенд сломан.
empty_state <- function(...) tags$div(class = "empty", ...)

fmt_money <- function(x, digits = 0) {
  if (!is.finite(x)) return("—")
  paste0("$", formatC(round(x, digits), format = "f", digits = digits,
                      big.mark = " "))
}

fmt_signed_money <- function(x, digits = 0) {
  if (!is.finite(x)) return("—")
  paste0(if (x >= 0) "+" else "−", fmt_money(abs(x), digits))
}

fmt_pct <- function(x, digits = 2) {
  if (!is.finite(x)) return("—")
  paste0(if (x >= 0) "+" else "−", formatC(abs(x), format = "f", digits = digits), "%")
}

fmt_pp <- function(x, digits = 2) {
  if (!is.finite(x)) return("—")
  paste0(if (x >= 0) "+" else "−", formatC(abs(x), format = "f", digits = digits), " пп")
}

# Класс цвета роста/падения — имена те же, что на portfolio.dtwin.ru.
tone_of <- function(x) if (!is.finite(x)) NULL else if (x >= 0) "pos" else "neg"

# Единая раскладка plotly для всех графиков стенда: без заголовка внутри
# холста (он уже в шапке карточки — дублировать значит тратить высоту),
# плотные поля, сетка цветом --grid, шрифт не мельче 11px.
blnr_plot_layout <- function(p, ...) {
  out <- plotly::layout(
    p,
    margin = list(l = 44, r = 12, t = 8, b = 30),
    font = list(family = "Panton, 'IBM Plex Sans', Arial, sans-serif", size = 11,
                color = BLNR_COLORS$text),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    hoverlabel = list(font = list(family = "Panton, 'IBM Plex Sans', Arial, sans-serif",
                                  size = 11)),
    ...
  )
  plotly::config(out, displayModeBar = FALSE, locale = "ru")
}
