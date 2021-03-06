#' Makes Graph of Single response questions
#'
#' \code{ted_sr_data_prep} returns a bar graph of a given question.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df An imported dataframe
#' @param .dd A data dictionary
#' @param .x A question number
#' @return A tibble
#'
#' @examples
#' df %>% ted_sr_data_prep(dd, Q8)
#'
#' @importFrom magrittr %>%
#' @export
ted_sr_data_prep <- function(.df, .dd, .x){
  x <-  rlang::enquo(.x)

  .temp <- tsr(.df, .dd, x)

  if(!is.ordered(.temp$labs) ){
    .temp <- .temp %>% dplyr::mutate(labs=forcats::fct_reorder(labs, value))
  }
  .temp
  }
