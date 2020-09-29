#' Prepares data for multiple response questions
#'
#' \code{multiple_response_data_prep} returns a tibble of a given question,
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
#' df %>% multiple_response_data_prep(dd, Q25, .full = F)
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_subset
#' @importFrom stringr str_c
#' @export
multiple_response_data_prep <- function (.df, .dd, .x, .full=TRUE) {
  v <- rlang::enquo(.x)
  q <- rlang::quo_name(v)
  labs <- tibble::tibble(title = .dd[.dd$name %in% str_subset( .dd$name, str_c(q,"_","[0-9]*$")), c("title")][[1]],
                         labs =  .dd[.dd$name %in% str_subset( .dd$name, str_c(q,"_","[0-9]*$")), c("value")][[1]],
                         qvar =  .dd[.dd$name %in% str_subset( .dd$name, str_c(q,"_","[0-9]*$")), c("name")][[1]])
  if (.full == TRUE) {
    .temp <- .df %>%
      dplyr::select(c(stringr::str_subset( .dd$name, str_c(q,"_","[0-9]*$")), "Company2")) %>%
      dplyr::filter_at(dplyr::vars(dplyr::contains(q)), dplyr::any_vars(!is.na(.))) %>%
      dplyr::mutate_at(vars(-c("Company2")), ~as.integer(!is.na(.))) %>%
      dplyr::group_by(Company2)  %>%
      dplyr::summarise_all( ~ifelse(mean(., na.rm = TRUE) >= 0.5, 1, 0) )  %>%
      dplyr::ungroup() %>%
      dplyr::add_count() %>%
      dplyr::summarise_at(vars(-c("Company2")), mean, na.rm = TRUE) %>%
      tidyr::gather(key = "key", value = "value", -n)

    .temp <- dplyr::left_join(labs, .temp, by = c(qvar = "key"))
    return(.temp)
  } else {

    .temp <- .df %>%
      dplyr::select(c(stringr::str_subset( .dd$name, str_c(q,"_","[0-9]*$")))) %>%
      dplyr::filter_at(dplyr::vars(dplyr::contains(q)), dplyr::any_vars(!is.na(.))) %>%
      dplyr::mutate_all(~as.integer(!is.na(.)))  %>%
      add_count() %>%
      dplyr::summarise_all(~mean(., na.rm = TRUE)) %>%
      tidyr::gather(key = "key", value = "value", -n)

    .temp2 <- dplyr::left_join(labs, .temp, by = c(qvar = "key"))
    .temp2
  }
}
