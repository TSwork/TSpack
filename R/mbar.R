#' Bar graph for multiple response questions
#'
#' \code{mbar} Takes a table prepared by \code{multiple_response_data_prep},
#' and outputs a bar graph.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df A dataframe, piped from \code{multiple_response_data_prep}
#' @param .cap Text for a caption (optional)
#' @return A bar graph
#'
#' @examples
#' df %>%
#' multiple_response_data_prep(dd, Q25, .full = F) %>%
#' mbar(.cap="2020 Poll, n=")
#'
#' @importFrom magrittr %>%
#' @export
mbar <- function(.df, .cap = NULL) {
  .p <- .df %>% ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(labs, value, .desc = FALSE), y = value, ymax = 1)) +
    ggplot2::geom_col(position = "stack", fill = "#003A63", width = 0.43, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) +
    ggplot2::scale_y_continuous(labels = NULL, expand = ggplot2::expand_scale(add = c(0, 0.06), mult = c(0, 0))) +
    ggplot2::scale_x_discrete(expand = ggplot2::expand_scale(add = c(0.1, 0.1))) +
    ggplot2::coord_flip() +
    ggplot2::geom_text(ggplot2::aes(label = scales::percent(value, accuracy = 0.1), y = value), fontface = "bold", color = "#000000", size = 2.5, nudge_x = 0.004, nudge_y = 0.004, hjust = 0, vjust = 0.5) +
    ggplot2::labs(x = NULL,
                  y = NULL,
                  title = stringr::str_wrap(.df$title[[1]], width = ifelse(stringr::str_length(.df$title[[1]] > 65), stringr::str_length(.df$title[[1]])/1.7, 65)),
                  caption = ifelse(is.null(.cap), NA, glue::glue(.cap, .df$n[1]))) +
    ggplot2::theme(plot.title      = ggplot2::element_text(hjust = 0, size = 8, margin = ggplot2::margin(0.05, 5, 0.5, 0, "cm"), face = "bold"),
                   plot.subtitle   = ggplot2::element_blank(),
                   plot.caption    = ggplot2::element_text(size = ggplot2::rel(0.6)),
                   plot.margin     = ggplot2::margin(0.5, 0, 0.25, 1, "cm"),
                   legend.title    = ggplot2::element_blank(),
                   legend.text     = ggplot2::element_text(size = ggplot2::rel(0.6)),
                   legend.key      = ggplot2::element_rect(size = 0.5),
                   legend.key.size = ggplot2::unit(0.3, "cm"),
                   legend.margin   = ggplot2::margin(0, 0, 0, 0, "cm"),
                   legend.position = "bottom",
                   axis.title      = ggplot2::element_blank(),
                   axis.line.x     = ggplot2::element_blank(),
                   axis.ticks      = ggplot2::element_blank(),
                   axis.text.y     = ggplot2::element_text(size = ggplot2::rel(0.75)))
  if (is.null(.cap)) {
    .p <- .p + ggplot2::theme(plot.caption = ggplot2::element_blank())
  }
  else {
    .p <- .p
  }
  .p
}
