#' Creates graph for multiple item likelihood questions
#'
#' \code{ted_con_plot} Given a tibble of a given multiple level likelihood question,
#'     creates graph.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df A dataframe, piped from \code{\link{ted_con_prep}}
#' @param .cap Text for a caption (optional)
#' @return A graph
#'
#' @examples
#' ted_con_prep(df, dd, Q29, c("Likely", "Neither likely nor unlikely", "Unlikely")) %>%
#' ted_con_plot()
#'
#' @importFrom magrittr %>%
#' @export
ted_con_plot <- function (.df, .cap = NULL){
  title <- .df %>% dplyr::select(title) %>% dplyr::pull()

  .p <- tconp(.df, .cap)

  if (is.null(.cap)) {
    .p <- .p + ggplot2::theme(plot.caption = ggplot2::element_blank())
  }
  else {
    .p <- .p
  }
  .p
}
