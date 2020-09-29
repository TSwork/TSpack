#' Makes Graph of Single response, ordinal questions
#'
#' \code{single_response_respondent_level_ord} returns a bar graph of a given question.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df An imported dataframe
#' @param .dd A data dictionary
#' @param .x The question id number of an ordinal question
#' @return A bar chart
#'
#' @examples
#' df %>% single_response_respondent_level_ord(dd, Q11)
#'
#' @importFrom magrittr %>%
#' @export
single_response_respondent_level_ord <- function(.df, .dd, .x){
  x <-  rlang::enquo(.x)
  qx <- rlang::quo_text(x)

  tmptitle <- .dd %>% dplyr::filter(qnames == qx) %>% dplyr::pull(title)
  tmptitle <- tmptitle[[1]]

  tempdf <- .df %>%
    dplyr::filter(!is.na(!!x)) %>%
    dplyr::pull(!!x) %>%
    forcats::fct_count(sort = T, prop = T) %>%
    dplyr::add_tally(wt = n)

  ggplot2::ggplot(tempdf, ggplot2::aes(x=f, y=p, ymax = 1)) +
    ggplot2::geom_col(width = 0.6, fill = "#003A63") +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(p, accuracy = 0.1)), fontface = "bold", color = "#000000", size = 3.5, hjust = 0) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = NULL, expand = ggplot2::expand_scale(add = c(0, 0), mult = c(0, 0.06))) +
    ggplot2::scale_x_discrete(labels = function(f) stringr::str_wrap(f, 30), expand = ggplot2::expand_scale(add = c(0.1, 0.1))) +
    ggplot2::labs(x = NULL, y = NULL,
         title = stringr::str_wrap(tmptitle, width = ifelse(stringr::str_length(tmptitle > 60), stringr::str_length(tmptitle)/1.8, 60)),
         caption = paste("n = ",tempdf$n[1])) +
    ggplot2::theme(plot.title    = ggplot2::element_text(hjust = 0.5, size = 10, margin = ggplot2::margin(0.05, 5, 0.5, 0, "cm"), face = "bold"),
          plot.subtitle = ggplot2::element_blank(),
          plot.caption  = ggplot2::element_text(size = ggplot2::rel(0.7)),
          plot.margin   = ggplot2::margin(0.5, 0, 0.25, 1, "cm"),
          legend.title    = ggplot2::element_blank(),
          legend.text     = ggplot2::element_text(size = ggplot2::rel(0.6)),
          legend.key      = ggplot2::element_rect(size = 0.5),
          legend.key.size = ggplot2::unit(0.3, "cm"),
          legend.margin   = ggplot2::margin(0, 0, 0, 0, "cm"),
          legend.position = "top",
          axis.title  = ggplot2::element_blank(),
          axis.line   = ggplot2::element_blank(),
          axis.ticks  = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.65)))
}
