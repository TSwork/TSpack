tsr <- function(.df, .dd, x){
  #x <-  rlang::enquo(.x)
  qx <- rlang::quo_text(x)

  tmptitle <- .dd %>% dplyr::filter(qnames == qx) %>% dplyr::pull(title)
  tmptitle <- tmptitle[[1]]

  .df %>%
  dplyr::filter(!is.na(!!x)) %>%
  dplyr::pull(!!x) %>%
  forcats::fct_count(sort = T, prop = T) %>%
  dplyr::add_tally(wt = n) %>%
  dplyr::mutate(title=tmptitle) %>%
  dplyr::select(title, labs="f",value="p", dplyr::everything())
}

tmrlabs <- function (.dd, q) {
tibble::tibble(title = .dd[.dd$name %in% str_subset( .dd$name, str_c(q,"_","[0-9]*$")), c("title")][[1]],
               labs =  .dd[.dd$name %in% str_subset( .dd$name, str_c(q,"_","[0-9]*$")), c("value")][[1]],
               qvar =  .dd[.dd$name %in% str_subset( .dd$name, str_c(q,"_","[0-9]*$")), c("name")][[1]])
}


tmr1 <- function (.df, .dd, q) {
.df %>%
  dplyr::select(c(stringr::str_subset( .dd$name, str_c(q,"_","[0-9]*$")), "Company2")) %>%
  dplyr::filter_at(dplyr::vars(dplyr::contains(q)), dplyr::any_vars(!is.na(.))) %>%
  dplyr::mutate_at(vars(-c("Company2")), ~as.integer(!is.na(.))) %>%
  dplyr::group_by(Company2)  %>%
  dplyr::summarise_all( ~ifelse(mean(., na.rm = TRUE) >= 0.5, 1, 0) )  %>%
  dplyr::ungroup() %>%
  dplyr::add_count() %>%
  dplyr::summarise_at(vars(-c("Company2")), mean, na.rm = TRUE) %>%
  tidyr::gather(key = "key", value = "value", -n)
}

tmr2 <- function (.df, .dd, q) {
.df %>%
  dplyr::select(c(stringr::str_subset( .dd$name, str_c(q,"_","[0-9]*$")))) %>%
  dplyr::filter_at(dplyr::vars(dplyr::contains(q)), dplyr::any_vars(!is.na(.))) %>%
  dplyr::mutate_all(~as.integer(!is.na(.)))  %>%
  dplyr::add_count() %>%
  dplyr::summarise_all(~mean(., na.rm = TRUE)) %>%
  tidyr::gather(key = "key", value = "value", -n)
}

tagr <- function (.df, .dd, q, .l, labs) {
.df %>%
  dplyr::select(dplyr::contains(q, ignore.case = FALSE)) %>%
  tidyr::gather() %>%
  dplyr::mutate(value = factor(value, c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"))) %>%
  dplyr::mutate(value = forcats::fct_collapse(value,
                                              Agree = c("Strongly agree", "Agree"),
                                              `Neither agree nor disagree` =  c("Neither agree nor disagree"),
                                              Disagree = c("Disagree", "Strongly disagree")))  %>%
  dplyr::filter(!is.na(value)) %>%
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
  dplyr::ungroup() %>%
  dplyr::mutate(labs = forcats::fct_reorder(labs,-temp3)) %>%
  dplyr::arrange(labs,value) %>%
  dplyr::select(-dplyr::contains("temp"))
}

tb <- function (.df, .cap) {
.df %>% ggplot2::ggplot(ggplot2::aes(x = labs, y = value, ymax = 1)) +
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
}

tagrp <- function (.df, .cap) {
.df %>%
  ggplot2::ggplot(ggplot2::aes(x = labs, y = Percent, fill = value)) +
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
  }



