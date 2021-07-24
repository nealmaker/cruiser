validate_cruise <- function(x) {
  # ensure proper structure

  # property warnings ----------------------------------------------------------

  # trees warnings -------------------------------------------------------------

  if(any(is.na(x$trees$spp))) {
    message("Trees with no species id in cruise workbook.")
    # cruise$trees$spp[!is.na(cruise$trees$spp)] <- "other hardwood"
  }

  if(any(is.na(x$trees$dbh)) | any(x$trees$dbh < 0)) {
    message("Invalid or missing dbh in cruise workbook.")
  }

  if(any(x$trees$dbh > 40, na.rm = T)) {
    message('Trees in cruise workbook with dbh > 40".')
  }

  if(any(x$trees$cr < 0 | x$trees$cr > 10, na.rm = T)) {
    message("Invalid crown ratio(s) in cruise workbook.")
  }

  if(any(is.na(x$trees$cr)) & !(all(is.na(x$trees$cr)))) {
    message("Some trees in cruise workbook have cr, but not all.")
  }

  if(any(is.na(x$trees$logs)) & !(all(is.na(x$trees$logs)))) {
    message("Some trees in cruise workbook have log grade data, but not all.")
    # cruise$trees$logs[is.na(cruise$trees$logs)] <- "2"
  }

  if(any(stringr::str_detect(x$trees$logs, "[^,\\*12356]"), na.rm = T)) {
    message("Invalid log grade(s) in cruise workbook.")
  }

  # stands warnings ------------------------------------------------------------

  # regen warnings -------------------------------------------------------------

  # habitat warnings -----------------------------------------------------------
}
