#' Forest cruise input
#'
#' Process forest inventory data from a properly formatted Excel workbook.
#'
#' @param path path to .xls or .xlsx file with inventory data. See example
#'  workbook for proper formatting.
#'
#' @return cruise object
#' @export
cruise <- function(path) {
  if(!stringr::str_detect(path, "\\.xlsx?$")) {
    stop("path must point to .xls or .xlsx file")
  }

  x <- new_cruise(path)
  validate_cruise(x)
  return(x)
}
