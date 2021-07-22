new_cruise <- function(path){

  # ensure spreadsheet is in supported format
  if(!stringr::str_detect(path, "\\.xlsx?$")) {
    stop("path must point to .xls or .xlsx file", call. = FALSE)
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


  # Minor wrangling ------------------------------------------------------------

  cruise$stands <- cruise$stands %>%
    dplyr::select(-c("type_code", "structure")) %>%
    dplyr::rename(structure = structure.1) %>%
    dplyr::mutate(stand = as.character(stand),
                  acres_calc = as.numeric(acres_calc),
                  acres_legal = as.numeric(acres_legal))

  cruise$trees <- cruise$trees %>%
    tidyr::fill(cruise$trees, stand, plot) %>%
    dplyr::select(-code) %>%
    dplyr::left_join(dplyr::select(cruise$stands, stand, structure),
                     by = "stand") %>%
    dplyr::mutate(logs = as.character(logs),
                  stand = as.character(stand),
                  cr = as.numeric(cr) * 10,
                  live = dplyr::if_else(vigor == 5, 0, 1),
                  include = structure == "Even-aged" | dbh >= 6,
                  ags = dplyr::if_else(vigor %in% c(1:3, NA) &
                                  (stringr::str_detect(logs, "2") |
                                     stringr::str_detect(logs, "1") |
                                     stringr::str_detect(logs, "*")),
                                1, 0),
                  snag = dplyr::if_else(vigor == 5, 1, 0),
                  spp = as.factor(spp),
                  # add softwood function
                  sft = spp %in% c("fir", "cedar", "hemlock", "red pine",
                                   "scots pine", "spruce", "tamarack",
                                   "white pine", "other softwood"))

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
  }

  cruise$trees$ba_ac <- .005454 * cruise$trees$dbh ^ 2 * cruise$trees$tpa


  # Condition handling ---------------------------------------------------------
  # Leave data as is, but warn user of potential problems

  if(any(is.na(trees$spp))) {
    warning("Some trees have no species id.")
    # trees$spp[!is.na(trees$spp)] <- "other hardwood"
  }

  # blank logs field handling
  if(any(is.na(trees$logs))) {
    warning("Missing data in trees$logs")
    # trees$logs[is.na(trees$logs)] <- "2"
  }
}
