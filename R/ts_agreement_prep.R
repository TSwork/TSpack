#' Prepares data for multiple item agreement questions
#'
#' \code{ts_agreement_prep} returns a tibble of a given multiple level agreement question,
#'     prepared for tables or graphs.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df An imported dataframe
#' @param .dd A data dictionary
#' @param .x A question number
#' @param .l The three level names
#' @return A graph
#'
#' @examples
#' df %>% newfun(dd, Q8)
#'
#' @importFrom magrittr %>%
#' @export
ts_agreement_prep <- function (.df, .dd, .x, .l){
  myround <- function(x) trunc(x+0.51)
  v <- rlang::enquo(.x)
  q <- rlang::quo_name(v)
  labs <- tibble::tibble(title = .dd[.dd$qnames == q, c("title")][[1]], labs = .dd[.dd$qnames == q, c("value")][[1]], qvar = .dd[.dd$qnames == q, c("name")][[1]])
  if (!(missing(.l))) {
    .temp <- .df %>%
      dplyr::select(dplyr::contains(q, ignore.case = FALSE)) %>%
      tidyr::gather() %>%
      mutate(value = factor(value, c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))) %>%
      mutate(value = fct_collapse(value, Agree = c("Strongly agree", "Agree"), `Neither agree nor disagree` =  c("Neither agree nor disagree"), Disagree = c("Disagree", "Strongly disagree"))) %>%
      dplyr::add_count(key, value) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::group_by(key) %>%
      dplyr::mutate(tot = sum(n), Percent = n/tot) %>%
      dplyr::select(key, value, tot,  Percent) %>%
      dplyr::mutate_at(dplyr::vars(value),  ~factor(., levels = .l, ordered = TRUE)) %>%
      dplyr::mutate(temp = as.integer(value), temp2 = temp*Percent) %>%
      {dplyr::left_join(labs, ., by = c(qvar = "key"))} %>%
      dplyr::group_by(labs) %>%
      dplyr::mutate(temp3 = mean(temp2)) %>%
      dplyr::ungroup()
  }
  .temp
}
