#' Get LAD energy consumption data
#'
#' Gets LAD (Local Administrative District) energy data from the DECC website, \url{http://www.decc.gov.uk}.
#' Since the format of these files tends to change just enough to be irritating for dependent code, there is
#' also an option that allows you to cache a local copy.  Note that the total values do not contain the
#' unallocated fuel values, which represent approximately 0.3% of the total demand.
#'
#' @param id the unique id of the LAD to fetch.  If not specified, then all LADs are retrieved.
#' @param year the year to fetch. If not specified, then the most recent year is retrieved.  Only single years currently supported
#' @param sector a vector of economic sectors to fetch.  Valid values are 'domestic', 'industrial', 'transport', 'total', 'all'.  Default = 'total'
#' @param fuel a vector of fuel types to fetch.  Valid values are 'coal', 'manufactured', 'petrol', 'gas', 'electricity', 'bioenergy', 'total', 'all'. Default = 'total'
#' @param dir an optional directory in which to cache the data
#' @return a long data frame with the requested data.  The 'energy' column is measured in GWh.
#' @keywords data energy
#' @export
#' @examples
#' \dontrun{
#' # Gets energy data for total fuels and sectors for most recent year
#' lad_data <- get_LAD_data()
#' }
#' # Gets energy data for electricity and gas use in the domestic sector in the most recent year
#' # Depending on the status of DECC servers, this can sometimes fail.  In which case, an empty
#' # data frame is returned
#' 
#' df <- get_LAD_data(sector="domestic", fuel=c("electricity", "gas"))
#'    
get_LAD_data <- function(id, year, sector='total', fuel='total', dir) {

  ## Download the data if necessary
  url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/274024/sub_national_total_final_energy_consumption_statistics_2005_2011.xlsx"
  file_name <- get_remote_file(url, dir)

  ## Load in the raw data from the spreadsheet
  wb <- tryCatch({
    loadWorkbook(file_name)
  }, error=function(e) {
    message(sprintf("Error loading workbook:\n\n%s\nTried download file from %s.  Email package maintainer to see if URL has changed.  Returning an empty data frame.", e, url))
    return(NULL)
  })

  ## If a valid workbook isn't found, return an empty data frame
  if (is.null(wb)) return(data.frame())
  
  ## Prep the year
  unit <- "GWh"
  valid_years <- get_valid_years(wb, unit)
  year_error <- FALSE

  ## Trap various error conditions
  if (missing(year)) {
    year_error <- TRUE
  } else if (is.na(year)) {
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
  df <- readWorksheet(wb, sheet_name, 4, 1, 421, 31)
  rm(wb)
  
  ## Since tmp contains all of the data, we now need to work some magic to get the right values
  df <- clean_decc_data(df)

  ## Subset on target ids
  if (!missing(id)) {
    if (!is.na(id)) {
      df <- df[which(df$LAU1_code %in% id),]
    }
  }

  ## Subset to select only those sectors and fuels of interest
  ## If all is given as either sector or fuel then all options are returned
  if (!is.element("all", sector) & !is.element("all", fuel)) {
    df <- df[which(df$sector %in% sector & df$fuel %in% fuel), ]
  } else if (is.element("all", sector) & !is.element("all", fuel)) {
    df <- df[which(df$fuel %in% fuel),] 
  } else if (!is.element("all", sector) & is.element("all", fuel)) {
    df <- df[which(df$sector %in% sector),]
  }
  
  ## Renumber rows and return the result
  row.names(df) <- 1:nrow(df)
  return(df)
}


#' Determines which year values are valid
#' @param wb the workbook to search
#' @param unit the measurement unit of interest.  Defaults to GWh
#' @return a vector of years with data in the spreadsheet
#' @import stringr
get_valid_years <- function(wb, unit="GWh") {
  
  ## Get a list of the sheets
  sheets <- getSheets(wb)

  ## Keep only those with the units in the name
  sheets <- sheets[grep(unit, sheets)]

  ## Drop the unit suffix and make numeric
  years <- as.numeric(str_trim(str_replace(sheets, unit, "")))
  return(years)
}
  

#' Cleans the raw DECC energy consumption data for easier use
#'
#' @param df the raw DECC data frame from the spreadsheet
#' @return a long database with headers by sector and fuel
#' @import reshape2
clean_decc_data <- function(df) {

  ## Manually define the column names
  sectors <- c(NA, NA, rep(c("industrial", "domestic", "total", NA), 2),
              "industrial", "domestic", "transport", "transport", "total", NA,
              rep(c("industrial", "domestic", "total", NA), 2),
              "total", NA, "total", NA, "industrial", "domestic", "transport")
  fuels <- c(NA, NA, rep("coal", 3), NA, rep("manufactured", 3), NA,
             rep("petrol", 5), NA, rep("gas", 3), NA, rep("electricity", 3), NA,
             "bioenergy", NA, "total", NA, rep("total", 3))

  ## Define the new column names
  col_names<- paste(sectors, fuels, sep=".")
  names(df) <- c("LAU1_code", "name", col_names[-c(1:2)])

  ## Now melt the data and final tidy
  df.m <- melt(df, id=c("LAU1_code", "name"), value.name="energy")
  df.m <- cbind(df.m, colsplit(df.m$variable, "\\.", c("sector", "fuel")))
  df.m <- df.m[,-3]

  ## Replace .. matches with NA
  df.m$energy <- gsub("\\.\\.", NA, df.m$energy)
  ## Replace , with nothing
  df.m$energy <- gsub(",", "", df.m$energy)
  ## Get rid of leading asterisks
  df.m$energy <- gsub("\\*\\s?", "", df.m$energy)
  df.m$energy <- as.numeric(df.m$energy)

  ## Remove NA
  df.m <- df.m[which(!is.na(df.m$energy)),]

  ## Remove the regional totals
  df.m <- df.m[-grep("TOTAL", df.m$name),]
  
  ## Return the result
  return(df.m)
}

#' Gets the 2010 population estimates for all local authority districts
#'
#' This is a convenience function that returns the population data from \code{\link{LAD_metadata}}.
#'
#' @seealso \code{\link{LAD_metadata}} 
#' @export
#' @return a data frame with the LAU1_code and population
get_LAD_population <- function() {

  ## This is now stored in the embedded file so it's a quick job.
  return(LAD_metadata[,c("LAU1_code", "population")])

}
  
