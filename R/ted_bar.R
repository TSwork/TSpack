#' Bar graph for multiple response questions
#'
#' \code{ted_bar} Takes a table prepared by \code{\link{ted_sr_data_prep}} or
#' \code{\link{ted_mr_data_prep}}, and outputs a bar graph.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df A dataframe, piped from \code{\link{ted_sr_data_prep}} or
#'   \code{\link{ted_mr_data_prep}}
#' @param .cap Text for a caption (optional)
#' @return A bar graph
#'
#' @examples
#' df %>%
#' ted_mr_data_prep(dd, Q25, .full = F) %>%
#' ted_bar(.cap="2020 Poll, n=")
#'
#' @importFrom magrittr %>%
#' @export
ted_bar <- function(.df, .cap = NULL) {
  .p <- tb(.df, .cap)
  if (is.null(.cap)) {
    .p <- .p + ggplot2::theme(plot.caption = ggplot2::element_blank())
  }
  else {
    .p <- .p
  }
  .p
}
