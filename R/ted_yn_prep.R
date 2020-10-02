#' Prepares data for multiple two item questions
#'
#' \code{ted_yn_prep} returns a tibble of a given multiple item yes/no like question,
#'     prepared for tables or graphs.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df An imported dataframe
#' @param .dd A data dictionary
#' @param .x A question number
#' @param .l The two level names
#' @return A tibble
#'
#' @examples
#' ted_yn_prep(df, dd, Q17, .l=c("Strength","Weakness"))
#'
#' @importFrom magrittr %>%
#' @export
ted_yn_prep <- function (.df, .dd, .x, .l){
  v <- rlang::enquo(.x)
  q <- rlang::quo_name(v)
  labs <- tibble::tibble(title = .dd[.dd$qnames == q, c("title")][[1]], labs = .dd[.dd$qnames == q, c("value")][[1]], qvar = .dd[.dd$qnames == q, c("name")][[1]])
  if (!(missing(.l))) {
    .temp <- tyn(.df, .dd, q, .l, labs)
  }
  .temp
}
