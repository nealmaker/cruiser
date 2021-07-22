new_cruise <- function(path){

  # ensure spreadsheet is in supported format
  if(!stringr::str_detect(path, "\\.xlsx?$")) {
    stop("path must point to .xls or .xlsx file")
  }


  # Read in inventory worksheets -----------------------------------------------

  wkbk <- XLConnect::loadWorkbook(path)

  cruise <- vector(mode = "list", length = 5)
  names(cruise) <- c("property", "stands", "trees", "regen", "habitat")

  cruise[[1]] <- XLConnect::readWorksheet(wkbk, sheet = 1, header = FALSE,
                                          endCol = 2)
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


 # Data entry errors -----------------------------------------------------------

  if(is.na(cruise$property[,2][cruise$property[,1] == "prop_file_id"])) {
    message("Property file id missing from cruise workbook.")
  }

  if(!(cruise$property[,2][cruise$property[,1] == "inv_type"] %in% 1:3)) {
    stop("No valid inventory type entered in cruise workbook.")
  }

  if(is.na(cruise$property[,2][cruise$property[,1] == "plot_size"])) {
    stop("No valid plot size entered in cruise workbook.")
  }

  if(!(cruise$property[,2][cruise$property[,1] == "inv_type"] == 3)) {
    if(is.na(cruise$property[,2][cruise$property[,1] == "dbh_cutoffs"])) {
      stop("Nested fixed area plots indicated, but no dbh cutoffs provided.")
    }

    # error if length of cutoffs != length of plot sizes
  }


  # Stands df ------------------------------------------------------------------

  if(!is.na(cruise$stands)) {
    # ensure column headers are right

    cruise$stands <- cruise$stands %>%
      dplyr::select(-c("type_code", "structure")) %>%
      dplyr::rename(structure = structure.1) %>%
      dplyr::mutate(stand = as.character(stand),
                    acres_calc = as.numeric(acres_calc),
                    acres_legal = as.numeric(acres_legal))

    # Warnings *******
  }


  # trees df -------------------------------------------------------------------

  if(!is.na(cruise$trees)) {
    # ensure column headers are right

    cruise$trees <- cruise$trees %>%
      tidyr::fill(cruise$trees, stand, plot) %>%
      dplyr::select(-code) %>%
      dplyr::mutate(dbh = as.numeric(dbh),
                    logs = as.character(logs),
                    stand = as.character(stand),
                    cr = as.numeric(cr) * 10,
                    live = dplyr::if_else(vigor == 5, 0, 1),
                    ags = dplyr::if_else(vigor %in% c(1:3, NA) &
                                           (stringr::str_detect(logs, "2") |
                                              stringr::str_detect(logs, "1") |
                                              stringr::str_detect(logs, "*")),
                                         1, 0),
                    spp = as.factor(spp))

    # tpa depends on inventory type
    if(cruise$property[,2][cruise$property[,1] == "inv_type"] == 1) {
      # prism
      cruise$trees$tpa <-
        cruise$property[,2][cruise$property[,1] == "plot_size"] /
        (0.005454 * cruise$trees$dbh ^ 2)
    } else if((cruise$property[,2][cruise$property[,1] == "inv_type"] == 2)) {
      # fixed
      cruise$trees$tpa <-
        1 / cruise$property[,2][cruise$property[,1] == "plot_size"]
    } else if((cruise$property[,2][cruise$property[,1] == "inv_type"] == 3)) {
      # nested fxed
      stop("Nested fixed area plots are not supported yet.")
    }

    cruise$trees$ba_ac <- .005454 * cruise$trees$dbh ^ 2 * cruise$trees$tpa


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

    # Warnings *******

    if(any(is.na(cruise$trees$spp))) {
      message("Trees with no species id in cruise workbook.")
      # cruise$trees$spp[!is.na(cruise$trees$spp)] <- "other hardwood"
    }

    if(any(is.na(cruise$trees$dbh)) | any(cruise$trees$dbh < 0)) {
      message("Invalid or missing dbh in cruise workbook.")
    }

    if(any(cruise$trees$dbh > 40, na.rm = T)) {
      message('Trees in cruise workbook with dbh > 40".')
    }

    if(any(cruise$trees$cr < 0 | cruise$trees$cr > 10, na.rm = T)) {
      message("Invalid crown ratio(s) in cruise workbook.")
    }

    if(any(is.na(cruise$trees$cr)) & !(all(is.na(cruise$trees$cr)))) {
      message("Some trees in cruise workbook have cr, but not all.")
    }

    if(any(is.na(cruise$trees$logs)) & !(all(is.na(cruise$trees$logs)))) {
      message("Some trees in cruise workbook have log grade data, but not all.")
      # cruise$trees$logs[is.na(cruise$trees$logs)] <- "2"
    }

    if(any(stringr::str_detect(cruise$trees$logs, "[^,\\*12356]"), na.rm = T)) {
      message("Invalid log grade(s) in cruise workbook.")
    }
  }


  # regen df -------------------------------------------------------------------

  if(!is.na(cruise$regen)) {
    # ensure column headers are right

    # Warnings *******

  }


  # habitat df -----------------------------------------------------------------

  if(!is.na(cruise$habitat)) {
    # ensure column headers are right

    # Warnings *******

  }


  # Return S3 object -----------------------------------------------------------

  class(cruise) <- c("cruise", "list")
  return(cruise)
}
