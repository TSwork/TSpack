#' Creates graph for multiple item agreement questions
#'
#' \code{ts_agreement_plot} Given a tibble of a given multiple level agreement question,
#'     creates graph.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df A dataframe, piped from \code{ts_agreement_prep}
#' @param .cap Text for a caption (optional)
#' @return A bar graph
#' @return A graph
#'
#' @examples
#' ts_agreement_prep(df, dd, Q27, c("Agree", "Neither agree nor disagree", "Disagree")) %>%
#' ts_agreement_plot()
#'
#' @export
ts_agreement_plot <- function (.df, .cap = NULL){
  title <- .df %>% dplyr::select(title) %>% dplyr::pull()
  .p <- .df %>%
    ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(labs,-temp3), y = Percent, fill = value)) +
    ggplot2::geom_col(position = ggplot2::position_fill(), width = 0.5) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = NULL, expand = ggplot2::expand_scale(add = c(0, 0), mult = c(0, 0))) +
    ggplot2::scale_x_discrete(labels = function(xvar) stringr::str_wrap(xvar, 60), expand = ggplot2::expand_scale(add = c(0.1, 0.1))) +
    ggplot2::scale_fill_manual(values = c("#21578e", "#a7a9ac", "#c15927"), guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::geom_text(ggplot2::aes(label = scales::number(Percent, scale = 100, accuracy = 1), y = Percent), fontface = "bold", color = "#ffffff", size = 3.5, position = ggplot2::position_fill(vjust = 0.5)) +
    ggplot2::labs(x = NULL, y = NULL, title = stringr::str_wrap(.df$title[[1]], width = ifelse(stringr::str_length(.df$title[[1]] > 60), stringr::str_length(.df$title[[1]])/1.8, 60)), caption = ifelse(is.null(.cap), NA, .cap)) +
    ggplot2::theme(plot.title    = ggplot2::element_text(hjust = 0.5, size = 12, margin = ggplot2::margin(0.05, 5, 0.5, 0, "cm"), face = "bold"),
                   plot.subtitle = ggplot2::element_blank(),
                   plot.caption  = ggplot2::element_text(size = ggplot2::rel(0.4)),
                   plot.margin   = ggplot2::margin(0.5, 0, 0.25, 1, "cm"),

                   legend.title  = ggplot2::element_blank(),
                   legend.text   = ggplot2::element_text(size = ggplot2::rel(0.6)),
                   legend.key    = ggplot2::element_rect(size = 0.5),
                   legend.key.size = ggplot2::unit(0.3, "cm"),
                   legend.margin = ggplot2::margin(0, 0, 0, 0, "cm"),
                   legend.position = "top",

                   axis.title  = ggplot2::element_blank(),
                   axis.line   = ggplot2::element_blank(),
                   axis.ticks  = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = ggplot2::rel(0.95)))
  if (is.null(.cap)) {
    .p <- .p + ggplot2::theme(plot.caption = ggplot2::element_blank())
  }
  else {
    .p <- .p
  }
  .p
}
