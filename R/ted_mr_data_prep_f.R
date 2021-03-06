#' Prepares data for multiple response questions
#'
#' \code{ted_mr_data_prep_f} returns a tibble of a given question,
#'     prepared for tables or graphs.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df An imported dataframe
#' @param .dd A data dictionary
#' @param .x A question number
#' @param .fct A factor or character
#' @param .full Respondents are to be aggregated at the full board level - T or F
#' @return A tibble
#'
#' @examples
#' df %>% ted_mr_data_prep_f(dd, Q25, Q9, .full=FALSE)
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_subset
#' @importFrom stringr str_c
#' @export
ted_mr_data_prep_f <- function(.df, .dd, .x, .fct, .full=TRUE) {
  v <- rlang::enquo(.x)
  q <- rlang::quo_name(v)
  f <- rlang::enquo(.fct)
  qf <- rlang::quo_name(f)

labs <- tmrlabs(.dd, q)

  if (.full == TRUE) {
    .temp <- .df %>% tmrf1(.dd, q, qf)
    return(.temp)
  } else {
    .temp2 <- .df %>% tmrf2(.dd, q, qf, f)
    .temp2 <- dplyr::left_join(labs, .temp2, by = c(qvar = "key")) %>%
      select(title, !!f, labs, value, n) %>%
      arrange(!!f)
    .temp2
  }
}

