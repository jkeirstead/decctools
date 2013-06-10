#' Get MSOA energy consumption data
#'
#' This function fetches MSOA (Middle Super Output Area) data from the DECC website from 2010.  Since the format of
#' these files tends to change just enough to be irritating for dependent code, there is also an option
#' that allows you to cache a local copy.
#'
#' @param id the unique id of the MSOA to fetch.  If not specified, then all MSOAs are retrieved.
#' @param sector the economic sector to fetch.  Valid values are 'domestic', 'nondomestic'
#' @param fuel the fuel type to fetch.  Valid values are 'electricity', 'gas'
#' @param dir an optional directory in which to save a copy of the data
#' @return a long data frame with the requested data.  The 'energy' column is measured in GWh.
#' @keywords data energy
#' @export
#' @examples
#' \dontrun{
#' get_MSOA_data() # Gets all data
#' }
get_MSOA_data <- function(id, sector=c("domestic", "nondomestic"), fuel=c("electricity", "gas"), dir) {

  ## The easiest way to do this might be to just download all of the data, prepare the
  ## appropriate data frame and then return the subset.

  ## Because the format of each spreadsheet is slightly different we have to do some ugly hacking in
  ## the parse_raw_MSOA_data function below
  params <- get_master_MSOA_params_list(dir)
  
  ## Subset this to only those sectors that we care about
  cond <- lapply(params, function(l) return(l$sector %in% sector & l$fuel %in% fuel))
  params <- params[unlist(cond)]
  
  ## Now actually go and get the data
  tmp <- llply(params, parse_raw_MSOA_data)
  all_data <- do.call("rbind", tmp)

  ## Remove the unallocated MSOAs
  all_data <- subset(all_data, MSOA_code!="Unallocated")

  ## Subset on the target ids
  if (!missing(id)) {
    if (!is.na(id)) {
      all_data <- all_data[which(all_data$MSOA_code %in% id),]
    }
  }
    
  ## Renumber rows and return the result
  row.names(all_data) <- 1:nrow(all_data)
  return(all_data)
}

## Now for each row in this data frame we need to get a data frame with the right data
##
## Params: 	l	a list giving the function parameters including
## url, ws, start_row, start_col, end_row, end_col, custom_function, data_col
parse_raw_MSOA_data <- function(l) {
  
  ## Get the file name (and download if necessary)
  file_name <- get_remote_file(l$url, l$dir)
    
  ## Load it into memory
  wb <- loadWorkbook(file_name)
  data <- readWorksheet(wb, l$sheet_name)
  rm(wb)
  
  ## Perform any custom changes to the data set
  msoa <- data[,3]
  energy <- l$custom_function(data)

  ## Set the names
  data <- data.frame(msoa, energy, s=l$sector, f=l$fuel, row.names=1:length(msoa))
  names(data) <- c("MSOA_code", "energy", "sector", "fuel")

  ## Convert kWh to GWh
  data <- mutate(data, energy=energy/1e6)
  
  ## Remove empty rows and return
  data <- subset(data, !is.na(MSOA_code))    
  return(data)
}

#' Builds a master set of parameters for MSOA data
#'
#' Creates a list of various parameters needed to download and extract
#' MSOA data from the DECC website.
#'
#' @param dir the directory to use for saving the data
get_master_MSOA_params_list <- function(dir) {

    ## First specify the urls
  urls <- c("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49424/4828-mlsoa-dom-elec-2010-alldata.xls",
            "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49426/4842-mlsoa-dom-gas-2010-all-data.xls",
            "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49425/4830-mlsoa-non-dom-elec-2010-alldata.xls",
            "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49427/4831-mlsoa-non-dom-gas-2010-alldata.xls")

  ## And the corresponding worksheet names are
  worksheet_names <- c("MLSOA Domestic Electricity", "MLSOA Domestic Gas",
                      "MLSOA Non-domestic Electricity", "Non-domestic gas MLSOA")

  sectors <- c(rep("domestic", 2), rep("nondomestic", 2))
  fuels <- rep(c("electricity", "gas"), 2)
  
  ## Then custom functions to retrieve the data since the domestic electricity is split
  ## These returns the code column first and then the energy value
  null_function <- function(df) return(df[,4])
  dom_elec_function <- function(df) return(apply(df[,4:5],1,sum,na.rm=TRUE))

  dir <- validate_directory(dir)
  
  ## Build a list summarizing everything
  df.l <- data.frame(url=urls, sheet_name=worksheet_names, sector=sectors, fuel=fuels, dir=dir,
                     stringsAsFactors=FALSE)
  df.l <- dlply(df.l, .(sheet_name), as.list)
  
  ## Add the custom functions to to the list
  df.l <- lapply(df.l, function(l) c(l, list(custom_function=null_function)))
  df.l[[1]]['custom_function'] <- list(custom_function=dom_elec_function)
  return(df.l)
}

#' Gets the 2011 population estimates for all MSOAs (including Scottish IGZs)
#'
#' This function gets the socio-demographic data associated with each Middle Super Output Area (MSOA).  
#'
#' @param dir an (optional) directory in which to save the downloaded data
#' @source \url{https://www.gov.uk/government/statistical-data-sets/socio-economic-data-for-mlsoa-igz-and-llsoa-electricity-and-gas-estimates}
#' @export
#' @return a data frame with the MSOA_code, population, area (in hectares), and number of households
get_MSOA_population <- function(dir) {

  ## Download the file
  url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/175644/Socio-economic_data_2013.xls"
  file_name <- get_remote_file(url, dir)

  ## Now open up the file and read the data
  ## Note that it is in two parts: English MLSOAs and Scottish IGZs
  wb <- loadWorkbook(file_name)
  england_data <- readWorksheet(wb, "MLSOA England and Wales", startRow=2, startCol=1)
  scotland_data <- readWorksheet(wb, "IGZ Scotland", startRow=2, startCol=1)
  pop_data <- list(england_data, scotland_data)
  rm(wb)

  ## Tidy up both files
  pop_data <- lapply(pop_data, function(l) {
    names(l) <- c("MSOA_code", "name", "population", "area", "households")
    empty_rows <- which(is.na(l$population))
    if (length(empty_rows)>0) l <- l[-empty_rows,]
    return(l[,-2])
  })
  pop_data <- do.call("rbind", pop_data)
  
  ## Return the result
  return(pop_data)
}
  
