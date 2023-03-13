#' Clean data before probabilistic matching.
#'
#' Clean first and last name fields before matching with RecordLinkage or other tools.
#' @param x a dataframe
#' @param first name of variable with first name
#' @param last name of variable with first name
#' @export


clean_match <- function(x, first = "first_name", last = "last_name"){
  x %>%
    mutate(dplyr::across(c(tidyselect::all_of(first), tidyselect::all_of(last)), ~stringr::str_remove_all(.x, "[[:punct:]]")),
           dplyr::across(c(tidyselect::all_of(first), tidyselect::all_of(last)), toupper),
           dplyr::across(c(tidyselect::all_of(first), tidyselect::all_of(last)), ~stringr::str_remove_all(.x, " JR| IV| III| II")), #include SR?
           dplyr::across(c(tidyselect::all_of(first), tidyselect::all_of(last)), ~stringr::str_remove_all(.x, " ")))
}
