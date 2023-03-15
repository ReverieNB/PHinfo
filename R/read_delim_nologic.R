#' Read delimited files.
#'
#' Read delimited files, and convert all columns that would be read in as logical as character. This prevents losing information in columns that are mostly blank.
#' @param x filepath
#' @export

read_delim_nologic <- function(x, ...){ #this will be a slow function because things are being read in twice

  #get list of all logical columns
  logic <- readr::read_delim(x,
                           ...) %>%
    dplyr::select(where(is.logical))

  fixes <- list(rep("c", length(names(logic))))
  names(fixes[[1]]) = names(logic)

  #read in file, correct logical column types
  temp <- readr::read_delim(x,
                          col_types = fixes[[1]],
                          ...)

  print(problems(temp))

  return(temp)
}
