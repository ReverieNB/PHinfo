#' Generate age groups.
#'
#' Generate age groups based on 'age' variable, up to 55+. Commonly used in HC reports.
#' @param x variable
#' @param nval cleaned value for numeric vars
#' @param cval cleaned value for text vars
#' @export

clean_missing <- function(x, nval = 9, cval= ""){

  if (is.numeric(x) == TRUE){

  ifelse(is.na(as.numeric(x))|as.numeric(x) == 0, nval, as.numeric(x))

    } else {

  ifelse(is.na(as.character(x)), cval, as.character(x))
    }
}
