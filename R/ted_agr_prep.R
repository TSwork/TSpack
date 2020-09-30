#' Prepares data for multiple item agreement questions
#'
#' \code{ted_agr_prep} returns a tibble of a given multiple level agreement question,
#'     prepared for tables or graphs.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df An imported dataframe
#' @param .dd A data dictionary
#' @param .x A question number
#' @param .l The three level names
#' @return A tibble
#'
#' @examples
#' ted_agr_prep(df, dd, Q13, c("Agree", "Neither agree nor disagree", "Disagree"))
#'
#' @importFrom magrittr %>%
#' @export
ted_agr_prep <- function (.df, .dd, .x, .l){
  v <- rlang::enquo(.x)
  q <- rlang::quo_name(v)
  labs <- tibble::tibble(title = .dd[.dd$qnames == q, c("title")][[1]], labs = .dd[.dd$qnames == q, c("value")][[1]], qvar = .dd[.dd$qnames == q, c("name")][[1]])
  if (!(missing(.l))) {

.temp <- tagr(.df, .dd, q, .l, labs)
  }
  .temp
}
