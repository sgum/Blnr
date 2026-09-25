# R/ui_kit.R
#
# Мелкие кирпичи интерфейса стенда: панель, KPI-плитка, подсказка под «i»,
# форматирование чисел. Вынесены отдельно, чтобы разметка экрана
# (ui/dashboard_ui.R) читалась как композиция, а не как стена из div/style.
#
# Правило конституции, которое здесь зашито:
#   * пояснительный текст не занимает места на экране — он живёт под «i» в
#     ШАПКЕ виджета, к которому относится (info_tip);
#   * карточка рисуется, только когда в ней есть результат — за это отвечает
#     вызывающий код (возврат NULL из renderUI), а не пустая коробка с текстом.

# Фирменная палитра ЦД.ЧГ.0244 + две роли, которых в ней нет: рост/падение.
# Финансовый экран без красно-зелёной пары нечитаем, поэтому зелёный взят
# брендовый (#70AD47), а к нему подобран приглушённый кирпичный, который с
# этой палитрой не спорит.
BLNR_COLORS <- list(
  ink    = "#484848",
  ink2   = "#5C5C5C",
  line   = "#DEDDDB",
  bg     = "#F4F3F1",
  card   = "#FFFFFF",
  accent = "#67B5CC",
  warn   = "#E8A327",
  up     = "#70AD47",
  down   = "#C0504D",
  mute   = "#9A9A9A"
)

# Подсказка под «i». Текст виден по наведению и НЕ занимает места в макете.
info_tip <- function(text) {
  tags$span(class = "itip", `data-tip` = text, "i")
}

# Панель = карточка с шапкой. Без цветной «шапки-плашки»: заголовок — просто
# жирная строка на белом, как в терминалах; цвет на экране остаётся значимым
# (рост/падение), а не декоративным.
panel <- function(title, ..., tip = NULL, right = NULL, body_class = NULL) {
  tags$section(
    class = "panel",
    tags$header(
      class = "panel-head",
      tags$span(title),
      if (!is.null(tip)) info_tip(tip),
      tags$span(class = "panel-head-sp"),
      right
    ),
    tags$div(class = paste("panel-body", body_class), ...)
  )
}

# KPI-плитка: крупное число, мелкая подпись. tone — "up"/"down"/NULL.
kpi <- function(value, label, tone = NULL, tip = NULL) {
  tags$div(
    class = paste0("kpi", if (!is.null(tone)) paste0(" kpi--", tone) else ""),
    tags$div(class = "kpi-val", value),
    tags$div(class = "kpi-lab", label, if (!is.null(tip)) info_tip(tip))
  )
}

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

tone_of <- function(x) if (!is.finite(x)) NULL else if (x >= 0) "up" else "down"

# Единая раскладка plotly для всех графиков стенда: без заголовка внутри
# холста (заголовок уже в шапке панели — дублировать значит тратить высоту),
# плотные поля, шрифт не мельче 11px.
blnr_plot_layout <- function(p, ...) {
  out <- plotly::layout(
    p,
    margin = list(l = 44, r = 12, t = 8, b = 30),
    font = list(family = "Panton, Arial, sans-serif", size = 11,
                color = BLNR_COLORS$ink),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)",
    hoverlabel = list(font = list(family = "Panton, Arial, sans-serif", size = 11)),
    ...
  )
  plotly::config(out, displayModeBar = FALSE, locale = "ru")
}
