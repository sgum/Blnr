# rating_buy_ui.R

ratingBuyUI <- function() {
  cat("Rendering ratingBuyUI...\n")
  result <- bs4TabItem(
    tabName = "rating_buy",
    h3("Рейтингование и покупка")
  )
  cat("Finished rendering ratingBuyUI...\n")
  return(result)
}
