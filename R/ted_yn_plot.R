#' Creates graph for multiple yes no type questions
#'
#' \code{ted_yn_plot} Given a tibble of a given multiple level yes/no type question,
#'     creates graph.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df A dataframe, piped from \code{\link{ted_yn_prep}}
#' @param .cap Text for a caption (optional)
#' @return A graph
#'
#' @examples
#' ted_yn_prep(df, dd, Q17, .l=c("Strength","Weakness")) %>%
#' ted_yn_plot()
#'
#' @importFrom magrittr %>%
#' @export
ted_yn_plot <- function (.df, .cap = NULL){
  title <- .df %>% dplyr::select(title) %>% dplyr::pull()
  .p <- tynp(.df, .cap)
  if (is.null(.cap)) {
    .p <- .p + ggplot2::theme(plot.caption = ggplot2::element_blank())
  }
  else {
    .p <- .p
  }
  .p
}


