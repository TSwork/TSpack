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
  qx <- rlang::quo_text(x)

  tmptitle <- .dd %>% dplyr::filter(qnames == qx) %>% dplyr::pull(title)
  tmptitle <- tmptitle[[1]]

  .temp <- .df %>%
    dplyr::filter(!is.na(!!x)) %>%
    dplyr::pull(!!x) %>%
    forcats::fct_count(sort = T, prop = T) %>%
    dplyr::add_tally(wt = n) %>%
    dplyr::mutate(title=tmptitle) %>%
    dplyr::select(title, labs="f",value="p", dplyr::everything())

  if(!is.ordered(.temp$labs) ){
    .temp <- .temp %>% dplyr::mutate(labs=forcats::fct_reorder(labs, value))
  }
  .temp
  }
