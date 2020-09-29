#' Makes Graph of Single response questions
#'
#' \code{single_response_respondent_level} returns a bar graph of a given question.
#'
#' This is an NACD function: Should you experience difficulties, see Ted Sikora
#'
#' @param .df An imported dataframe
#' @param .dd A data dictionary
#' @param .x A question number
#' @return A bar chart
#'
#' @examples
#' df %>% newfun(dd, Q8)
#'
#' @export
single_response_respondent_level <- function(.df, .dd, .x){
  x <- enquo(.x)
  qx <- quo_text(x)

  tmptitle <- .dd %>% filter(qnames == qx) %>% pull(title)
  tmptitle <- tmptitle[[1]]

  tempdf <- .df %>%
    filter(!is.na(!!x)) %>%
    pull(!!x) %>%
    fct_count(sort = T, prop = T) %>%
    add_tally(wt = n)

  ggplot(tempdf, aes(x=fct_reorder(f, p), y=p, ymax = 1)) +
    geom_col(width = 0.6, fill = "#003A63") + #"#21578e"
    geom_text(aes(label = scales::percent(p, accuracy = 0.1)), fontface = "bold", color = "#000000", size = 3.5, hjust = 0) +
    coord_flip() +
    scale_y_continuous(labels = NULL, expand = ggplot2::expand_scale(add = c(0, 0), mult = c(0, 0.06))) +
    scale_x_discrete(labels = function(f) stringr::str_wrap(f, 30), expand = ggplot2::expand_scale(add = c(0.1, 0.1))) +
    labs(x = NULL, y = NULL,
         title = stringr::str_wrap(tmptitle, width = ifelse(stringr::str_length(tmptitle > 60), stringr::str_length(tmptitle)/1.8, 60)),
         caption = paste("n = ",tempdf$n[1])) +
    theme(plot.title    = ggplot2::element_text(hjust = 0.5, size = 10, margin = ggplot2::margin(0.05, 5, 0.5, 0, "cm"), face = "bold"),
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
