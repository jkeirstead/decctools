#' Get LAD energy consumption data
#'
#' Gets LAD (Local Administrative District) energy data from the DECC website, \url{http://www.decc.gov.uk}.
#' Since the format of these files tends to change just enough to be irritating for dependent code, there is
#' also an option that allows you to cache a local copy.
#'
#' @param id the unique id of the LAD to fetch.  If not specified, then all LADs are retrieved.
#' @param year the year to fetch. If not specified, then the most recent year is retrieved.  Only single years currently supported
#' @param sector a vector of economic sectors to fetch.  Valid values are 'domestic', 'industrial', 'total'.
#' @param fuel a vector of fuel types to fetch.  Valid values are 'coal', 'manufactured', 'petrol', 'gas', 'electricity', 'bioenergy', 'total'
#' @param dir an optional directory in which to cache the data
#' @return a long data frame with the requested data.  The 'energy' column is measured in GWh.
#' @keywords data energy
#' @export
#' @examples
#' # Gets energy data for total fuels and sectors for most recent year
#' get_LAD_data()
#' # Gets energy data for electricity and gas use in the domestic sector in the most recent year
#' get_LAD_data(sector="domestic", fuel=c("electricity", "gas")) 
get_LAD_data <- function(id=NA, year=NA, sector='total', fuel='total', dir=NA) {

  ## Download the data if necessary
  url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/172997/Sub-national_total_final_energy_consumption_statistics__2005_-_2010.xlsx"
  file_name <- get_remote_file(url, dir)
  
  ## Load in the raw data from the spreadsheet
  wb <- loadWorkbook(file_name)

  ## Prep the year
  unit <- "GWh"
  valid_years <- get_valid_years(wb, unit)
  year_error <- FALSE

  ## Trap various error conditions
  if (is.na(year)) {
    year_error <- TRUE
  } else if (length(year)!=1) {
    warning("Only one year currently supported.  Defaulting to most recent year.")
    year_error <- TRUE
  } else if (!(year %in% valid_years)) {
    min_valid <- min(valid_years)
    max_valid <- max(valid_years)
    msg <- sprintf("%d is not a valid year.  Try a value in the range %d--%d.", year, min_valid, max_valid)
    warning(msg)
    year_error <- TRUE
  }
  
  if (year_error) year <- max(valid_years)
  
  ## Read in the data
  sheet_name <- paste(year, unit)
  df <- readWorksheet(wb, sheet_name, 4, 1, 421, 27)
  rm(wb)
  
  ## Since tmp contains all of the data, we now need to work some magic to get the right values
  df <- clean_decc_data(df, sector, fuel)

  ## Subset on target ids
  if (!is.na(id)) df <- df[which(df$LAU1_code %in% id),]
  
  ## Renumber rows and return the result
  row.names(df) <- 1:nrow(df)
  return(df)
}


#' Determines which year values are valid
#' @param wb the workbook to search
#' @param unit the measurement unit of interest.  Defaults to GWh
#' @return a vector of years with data in the spreadsheet
get_valid_years <- function(wb, unit="GWh") {
  
  ## Get a list of the sheets
  sheets <- getSheets(wb)

  ## Keep only those with the units in the name
  sheets <- sheets[grep(unit, sheets)]

  ## Drop the unit suffix and make numeric
  years <- as.numeric(str_trim(str_replace(sheets, unit, "")))
  return(years)
}
  

#' Adds DECC energy consumption data to a specified data frame.  Currently
#' only selects domestic electricity and gas.
#'
#' @param df the raw DECC data frame from the spreadsheet
#' @param sector the sector to extract
#' @param fuel the fuels to extract
#' @return a long database
clean_decc_data <- function(df, sector, fuel) {

  ## Manually define the column names
  sectors <- c(NA, NA, rep(c("industrial", "domestic", "total", NA), 2),
              "industrial", "domestic", "transport", "transport", "total", NA,
              rep(c("industrial", "domestic", "total", NA), 2),
              "total", NA, "total")
  fuels <- c(NA, NA, rep("coal", 3), NA, rep("manufactured", 3), NA,
             rep("petrol", 5), NA, rep("gas", 3), NA, rep("electricity", 3), NA,
             "bioenergy", NA, "total")


  ## Make sure we've got valid inputs
  sector <- unique(replace(sector, which(is.na(sector)), "total"))
  fuel <- unique(replace(fuel, which(is.na(fuel)), "total"))

  ## Calculate the columns that meet both criteria
  sector_cols <- which(sectors %in% sector)
  fuel_cols <- which(fuels %in% fuel)
  common_cols <- intersect(sector_cols, fuel_cols)
  
  ## Columns 1 and 2 are the LAU code and name
  good_cols <- c(1, 2, common_cols)  
  df <- df[, good_cols]

  ## Define the new column names
  sector_names <- sectors[common_cols]
  fuel_names <- fuels[common_cols]
  selection_cols <- paste(sector_names, fuel_names, sep=".")

  new_col_names <- c("LAU1_code", "name", selection_cols)
  names(df) <- new_col_names

  ## Now melt the data and final tidy
  df.m <- melt(df, id=c("LAU1_code", "name"), value.name="energy")
  df.m <- cbind(df.m, colsplit(df.m$variable, "\\.", c("sector", "fuel")))
  df.m <- df.m[,-3]
  df.m <- mutate(df.m, energy=as.numeric(str_replace(energy, "\\.\\.", NA)))
  ## Return the result
  return(df.m)
}
