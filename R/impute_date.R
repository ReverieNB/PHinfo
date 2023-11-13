#' Impute missing date information.
#'
#' Impute missing information for dates in character, YYYYMMDD, format. Used in HIV and HIV D2C scripts.
#' @param x a dataframe
#' @param month name of variable with first name
#' @param year name of variable with first name
#' @export

impute_date <- function(x, month = "06", day = "15"){
  x2 <- str_remove_all(x, "[.]")

  case_when(
    is.na(x2) ~ as.character(NA),
    nchar(x2)==8 ~ x2,
    nchar(x2)==6 ~ paste0(x2, month),
    nchar(x2)==4 ~ paste0(x2, day),
    TRUE ~ "ERROR"
  )
}
