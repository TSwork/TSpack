#' Creates graph for multiple item agreement questions
#'
#' \code{ted_agr_plot} Given a tibble of a given multiple level agreement question,
#'     creates graph.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df A dataframe, piped from \code{\link{ted_agr_prep}}
#' @param .cap Text for a caption (optional)
#' @return A graph
#'
#' @examples
#' ted_agr_prep(df, dd, Q27, c("Agree", "Neither agree nor disagree", "Disagree")) %>%
#' ted_agr_plot()
#'
#' @importFrom magrittr %>%
#' @export
ted_agr_plot <- function (.df, .cap = NULL){
  title <- .df %>% dplyr::select(title) %>% dplyr::pull()

  .p <- tagrp(.df, .cap)

  if (is.null(.cap)) {
    .p <- .p + ggplot2::theme(plot.caption = ggplot2::element_blank())
  }
  else {
    .p <- .p
  }
  .p
}
