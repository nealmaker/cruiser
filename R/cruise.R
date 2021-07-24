cruise <- function(path) {

  # ensure spreadsheet is in supported format
  if(!stringr::str_detect(path, "\\.xlsx?$")) {
    stop("path must point to .xls or .xlsx file")
  }

  x <- new_cruise(path)
  validate_cruise(x)
  return(x)
}
