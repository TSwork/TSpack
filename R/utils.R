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


