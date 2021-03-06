 new_cruise <- function(path){

  # Read in inventory worksheets -----------------------------------------------

  wkbk <- XLConnect::loadWorkbook(path)

  cruise <- vector(mode = "list", length = 5)
  names(cruise) <- c("property", "stands", "trees", "regen", "habitat")

  cruise[[1]] <- XLConnect::readWorksheet(wkbk, sheet = 1, header = FALSE,
                                          endCol = 3)
  cruise[[2]] <- XLConnect::readWorksheet(wkbk, sheet = 2, header = TRUE)
  cruise[[3]] <- XLConnect::readWorksheet(wkbk, sheet = 3, header = TRUE)
  cruise[[4]] <- XLConnect::readWorksheet(wkbk, sheet = 4, header = TRUE)
  cruise[[5]] <- XLConnect::readWorksheet(wkbk, sheet = 5, header = TRUE)

  for(i in 1:length(cruise)) {
    # remove empty rows (workbook autopopulates "" instead of NA sometimes)
    if(nrow(cruise[[i]]) > 0) {
      cruise[[i]][cruise[[i]] == ""] <- NA
      cruise[[i]] <- cruise[[i]][rowSums(!is.na(cruise[[i]])) != 0, ]
    }

    # remove empty data frames
    if(nrow(cruise[[i]]) == 0) cruise[[i]] <- NA
  }


  # property df ----------------------------------------------------------------

  if(!(all(c("prop_file_id", "inv_type", "plot_size") %in%
           cruise$property[,1]))) {
    stop("Critical fields missing from property worksheet.")
  }

  if(!(cruise$property[,2][cruise$property[,1] == "inv_type"] %in% 1:3)) {
    stop("No valid inventory type entered in cruise workbook.")
  }

  if(is.na(cruise$property[,2][cruise$property[,1] == "plot_size"])) {
    stop("No valid plot size entered in cruise workbook.")
  }

  if(cruise$property[,2][cruise$property[,1] == "inv_type"] == 3) {

    if(is.na(cruise$property[,2][cruise$property[,1] == "dbh_cutoffs"])) {
      stop("Nested fixed area plots indicated, but no dbh cutoffs provided.")
    }

    if(length(unlist(strsplit(
      cruise$property[,2][cruise$property[,1] == "dbh_cutoffs"]))) !=
      length(unlist(strsplit(
        cruise$property[,2][cruise$property[,1] == "dbh_cutoffs"])))) {
      stop("Nested fixed area plots indicated, but number of dbh cutoffs
           does not match number of plot sizes.")
    }
  }


  # Stands df ------------------------------------------------------------------

  if(!(typeof(cruise$stands) == "logical")) {
    if(!all((c("stand", "site_class", "structure", "history", "regen", "health",
         "access", "soils_comma_separated", "acres_calc", "acres_legal",
         "ineligible_wetlands", "ineligible_roads_landings", "wildlife",
         "narrative", "type", "fia_type", "structure.1") %in%
       names(cruise$stands)))) {
      stop("One or more fields missing from stands sheet. Please use standard cruise
           woorkbook for data entry.")
    }

    cruise$stands <- cruise$stands %>%
      dplyr::select(-c("type_code", "structure")) %>%
      dplyr::rename(structure = structure.1) %>%
      dplyr::mutate(stand = as.character(stand),
                    site_class = stringr::str_trim(as.character(site_class)),
                    acres_calc = as.numeric(acres_calc),
                    acres_legal = as.numeric(acres_legal))

    # Warnings
    if(!all(cruise$stands$site_class %in%
         c("I", "II", "III", "IV", "4", "5", "6", "7", NA))){
      warning("One or more unsupported site classes in stands sheet.")
    }

    # forest types match model format
    # soils will return real soil listings (maybe named for county and soil #)
  }


  # trees df -------------------------------------------------------------------

  if(!(typeof(cruise$trees) == "logical")) {
    # ensure column headers are right (accomodate 'ugs' column instead of 'vigor' & 'logs', etc.)
    if(!all((c("spp", "stand", "plot", "dbh") %in% names(cruise$trees)))) {
      stop("One or more critical fields are missing from trees sheet.")
      }
    # what about making sure there's data to figure out ugs?

    # warning if there are stands without entries in cruise$stands

    # fill default vigor and cut if some data was collected
    if(!all(is.na(cruise$trees$vigor))) {
      if(!all(cruise$trees$vigor %in% 1:5, na.rm = T)) {
        message("Invalid tree vigor(s) in cruise workbook.")
      }
      cruise$trees$vigor[is.na(cruise$trees$vigor)] <- 2
      message("Missing tree vigors: defaulting to '2'.")
    }

    if(!all(is.na(cruise$trees$cut))) {
      if(!all(cruise$trees$cut %in% c(0, 1, TRUE, FALSE), na.rm = T)) {
        message("Invalid 'cut' entries in trees worksheet: defaulting to NA.")
      }
      cruise$trees$cut <- as.logical(cruise$trees$cut)
      cruise$trees$cut[is.na(cruise$trees$cut)] <- FALSE
      message("Missing tree vigors defaulting to '2'.")
    }

    # first, general stuff unrelated to ugs, vigor, logs
    # if there's ugs data, fill "0"s & use to populate ags
    # else use vigor and logs to populate ags
    # some scenario with vigor and no logs data? (Would need ugs instead) or one field for vigor that indicates ugs b/c poor quality.
    # deal with vigor and logs data if they exist
    cruise$trees <- cruise$trees %>%
      tidyr::fill(stand, plot) %>%
      dplyr::select(-code) %>%
      dplyr::mutate(dbh = as.numeric(dbh),
                    logs = as.character(logs),
                    vigor = as.numeric(vigor),
                    stand = as.character(stand),
                    cr = as.numeric(cr) * 10,
                    live = dplyr::if_else(vigor == 5, 0, 1),
                    ags = dplyr::if_else(vigor %in% c(1:3, NA) &
                                           (stringr::str_detect(logs, "2") |
                                              stringr::str_detect(logs, "1") |
                                              stringr::str_detect(logs, "\\*")),
                                         1, 0),
                    spp = as.factor(spp))

    # tpa depends on inventory type
    if(cruise$property[,2][cruise$property[,1] == "inv_type"] == 1) {
      # prism
      cruise$trees$tpa <-
        as.numeric(cruise$property[,2][cruise$property[,1] == "plot_size"]) /
        (0.005454 * cruise$trees$dbh ^ 2)
    } else if((cruise$property[,2][cruise$property[,1] == "inv_type"] == 2)) {
      # fixed
      cruise$trees$tpa <-
        1 / as.numeric(cruise$property[,2][cruise$property[,1] == "plot_size"])
    } else if((cruise$property[,2][cruise$property[,1] == "inv_type"] == 3)) {
      # nested fxed
      stop("Nested fixed area plots are not supported yet.")
    }

    cruise$trees$ba_ac <- .005454 * cruise$trees$dbh ^ 2 * cruise$trees$tpa
  }


  # regen df -------------------------------------------------------------------

  if(!(typeof(cruise$regen) == "logical")) {
    # ensure column headers are right

    # Warnings *******

  }


  # habitat df -----------------------------------------------------------------

  if(!(typeof(cruise$habitat) == "logical")) {
    # ensure column headers are right

    # Warnings *******

  }


  # Return S3 object -----------------------------------------------------------

  class(cruise) <- "cruise"
  return(cruise)
}
