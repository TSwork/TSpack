#' Prepares data for multiple response questions
#'
#' \code{ted_mr_data_prep} returns a tibble of a given question,
#'     prepared for tables or graphs.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df An imported dataframe
#' @param .dd A data dictionary
#' @param .x A question number
#' @param .full Respondents are to be aggregated at the full board level - T or F
#' @return A tibble
#'
#' @examples
#' df %>% ted_mr_data_prep(dd, Q25, .full = F)
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_subset
#' @importFrom stringr str_c
#' @export
ted_mr_data_prep <- function (.df, .dd, .x, .full=TRUE) {
  v <- rlang::enquo(.x)
  q <- rlang::quo_name(v)
  labs <- tmrlabs(.dd, q)

   if (.full == TRUE) {
    .temp <- .df %>% tmr1(.dd, q)

    .temp <- dplyr::left_join(labs, .temp, by = c(qvar = "key")) %>%
      dplyr::select(-qvar)
    if ( is.ordered(.temp$labs) != TRUE ){
      .temp <- .temp %>% dplyr::mutate(labs=forcats::fct_reorder(labs, value))
    }
    .temp
    return(.temp)
  } else {

    .temp <- .df %>% tmr2(.dd, q)

    .temp2 <- dplyr::left_join(labs, .temp, by = c(qvar = "key"))  %>%
      dplyr::select(-qvar)
    if ( is.ordered(.temp2$labs) != TRUE ){
      .temp2 <- .temp2 %>% dplyr::mutate(labs=forcats::fct_reorder(labs, value))
    }
    .temp2
  }
}
