#' Catch logical values.
#'
#' Columns with many missing values may be read in as logical, losing data. This function will catch any unnoticed instances of this.
#' @param x a dataframe
#' @export

catch_logical <- function(x){
  temp <- x %>%
    purrr::map_df(class) %>%
    tidyr::pivot_longer(cols = tidyselect::everything(), names_to = "columns", values_to = "type") %>%
    dplyr::filter(type == "logical")

  if (nrow(temp) == 0){
    print("No logical columns found")
    return(x)
  } else {
    stop(paste(nrow(temp), ifelse(nrow(temp)==1, "column is", "columns are"), "logical:", unique(temp$columns)))
  }
}
