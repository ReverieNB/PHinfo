#' Read xlsx files.
#'
#' Read xlsx files, and convert all columns that would be read in as logical as character. This prevents losing information in columns that are mostly blank.
#' @param x filepath
#' @export

read_xlsx_nologic <- function(x, ...){ #this will be a slow function because things are being read in twice

  #get list of all column types, adjust logical types
  fixes <- readxl::read_xlsx(x,
                             n_max = 1001,
                             ...) %>%
    purrr::map_df(class) %>%
    dplyr::slice(1) %>%
    dplyr::mutate(dplyr::across(everything(), ~ ifelse(. == "logical", "text", "guess"))) %>%
    purrr::as_vector() %>%
    unname()

  #read in file, correct logical column types
  temp <- readxl::read_xlsx(x,
                           col_types = fixes,
                           ...)

  return(temp)
}
