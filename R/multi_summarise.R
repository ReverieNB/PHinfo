#' Summarise across multiple variables
#'
#' Summarise across multiple variables
#' @param data dataframe
#' @param group_cols columns used for grouping
#' @param sum_cols columns used for summarizing
#' @export

multi_summarise <- function(data, group_cols, sum_cols){

  purrr::map_df(sum_cols, function(x){
    temp <- data %>%
      dplyr::mutate(dplyr::across(everything(), factor)) %>% #transform data into factors so no categories are dropped
      dplyr::group_by(!!!syms(group_cols), !!sym(x), .drop = FALSE) %>%
      dplyr::summarise(count = n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(type = x) %>%
      dplyr::rename(value = length(group_cols)+1)
  })
}
