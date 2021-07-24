validate_cruise <- function(x) {
  if(!(all(c("property", "stands", "trees", "regen", "habitat") %in% names(x)) &
     inherits(x, "cruise"))) {
    stop("cruise lacks proper elements.")
  }

  # property warnings ----------------------------------------------------------
  if(!(all(c("prop_file_id", "inv_type", "plot_size") %in%
           x$property[,1]))) {
    stop("Critical fields missing from property worksheet.")
  }

  if(is.na(x$property[,2][x$property[,1] == "prop_file_id"])) {
    message("Property file id is missing.")
  }

  if(is.na(x$property[,2][x$property[,1] == "month_inv"]) |
     is.na(x$property[,2][x$property[,1] == "year_inv"])) {
    warning("Cruise date is missing.")
  }

  if(is.na(x$property[,2][x$property[,1] == "latitude"]) |
     is.na(x$property[,2][x$property[,1] == "longitude"])) {
    warning("Latitude and/or longitude are missing.")
  }

  if(is.na(x$property[,2][x$property[,1] == "elev_min"]) |
     is.na(x$property[,2][x$property[,1] == "elev_max"])) {
    warning("Elevation is missing.")
  }

  # trees warnings -------------------------------------------------------------
  if(!(typeof(x$trees) == "logical")) {
    if(!all(c("spp", "stand", "plot", "dbh", "cr", "logs", "vigor", "cut",
              "live", "ags", "tpa", "ba_ac") %in% names(x$trees))) {
      stop("trees table is missing variables.")
    }

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
  }

  # stands warnings ------------------------------------------------------------
  if(!(typeof(x$stands) == "logical")) {

  }

  # regen warnings -------------------------------------------------------------
  if(!(typeof(x$regen) == "logical")) {

  }

  # habitat warnings -----------------------------------------------------------
  if(!(typeof(x$habitat) == "logical")) {

  }
}
