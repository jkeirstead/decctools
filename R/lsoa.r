#' Get LSOA energy consumption data
#'
#' This function fetches LSOA (Lower Super Output Area) data from the DECC website.
#'
#' @param id a vector of LSOA ids to fetch.  If not specified, then all LSOAs are retrieved.
#' @param fuel the fuel type to fetch.  Valid values are 'electricity', 'gas'
#' @param dir an optional directory in which to store a copy of the data
#' @return a long data frame with the requested data.  The 'energy' column is measured in GWh.
#' @keywords data energy
#' @export
#' @examples
#' get_LSOA_data() # Gets all data
get_LSOA_data <- function(id, fuel=c("electricity", "gas"), dir) {

  ## As with the MSOA stuff, we'll prepare a list of all the parameters and then process them one by one.
  sector <- "domestic" # because at the moment, DECC doesn't provide other sectors
  
  ## Fetch the master parameters list
  params <- get_master_LSOA_params_list(dir)

  ## Subset to only those ones we care about
  cond <- lapply(params, function(l) return(l$sector %in% sector & l$fuel %in% fuel))
  params <- params[unlist(cond)]

  ## Now actually go and get the data
  tmp <- llply(params, parse_raw_LSOA_data)
  all_data <- do.call("rbind", tmp)

  ## Remove the unallocated LSOAs
  all_data <- subset(all_data, LSOA_code!="Unallocated")

  ## Subset on the target ids
  if (!missing(id)) {
    if (!is.na(id))  {
      all_data <- all_data[which(all_data$LSOA_code %in% id),]
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
parse_raw_LSOA_data <- function(l) {

  ## Get the file name (and download if necessary)
  file_name <- get_remote_file(l$url, l$dir)
  
  ## Load it into memory
  wb <- loadWorkbook(file_name)
  data <- readWorksheet(wb, l$sheet_name)
  
  ## Perform any custom changes to the data set
  lsoa <- data[,4]
  energy <- l$custom_function(data)
  
  ## Assemble the final data frame
  data <- data.frame(lsoa, energy, s=l$sector, f=l$fuel, row.names=1:length(lsoa))
  names(data) <- c("LSOA_code", "energy", "sector", "fuel")

  ## Convert kWh to GWh
  data <- mutate(data, energy=energy/1e6)
  
  ## Remove empty rows
  data <- subset(data, !is.na(LSOA_code))

  return(data)
}

#' Builds a master set of parameters for MSOA data
#'
#' Creates a list of various parameters needed to download and extract
#' MSOA data from the DECC website.
#'
#' @param dir the directory in which to store a copy of the data
get_master_LSOA_params_list <- function(dir) {

  ## First specify the urls
  urls <- c("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49432/4813-llsoa-domestic-elec-est-2010.xls",
           "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49433/4814-llsoa-domestic-gas-est-2010.xls")

  ## And the corresponding worksheet names are
  worksheet_names <- c("LLSOA Electricity Domestic", "LLSOA Domestic gas")

  ## Define the sectors that these cover
  sectors <- rep("domestic", 2)
  fuels <- c("electricity", "gas")
  
  ## Then custom functions to retrieve the data since the domestic electricity is split
  ## These returns the code column first and then the energy value
  dom_gas_function <- function(df) return(df[,5])
  dom_elec_function <- function(df) return(apply(df[,5:6],1,sum,na.rm=TRUE))

  ## Build a list summarizing everything
  df.l <- data.frame(url=urls, sheet_name=worksheet_names, sector=sectors, fuel=fuels,
                     dir=dir, stringsAsFactors=FALSE)
  df.l <- dlply(df.l, .(sheet_name), as.list)
  
  ## Add the custom functions to to the list
  df.l <- lapply(df.l, function(l) c(l, list(custom_function=dom_gas_function)))
  df.l[[1]]['custom_function'] <- list(custom_function=dom_elec_function)
  return(df.l)

}

#' Gets the 2011 population estimates for all LSOAs
#'
#' This function gets the socio-demographic data associated with each LSOA.  The original data can be found at \url{https://www.gov.uk/government/statistical-data-sets/socio-economic-data-for-mlsoa-igz-and-llsoa-electricity-and-gas-estimates}.  As a reminder, this data only covers England and Wales.
#'
#' @param dir an (optional) directory in which to save the downloaded data
#' @export
#' @return a data frame with the LSOA_code, population, area (in hectares), and number of households
get_LSOA_population <- function(dir) {

  ## Download the file
  url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/175644/Socio-economic_data_2013.xls"
  file_name <- get_remote_file(url, dir)

  ## Now open up the file and read the data
  ## Note that it is in two parts: English MLSOAs and Scottish IGZs
  wb <- loadWorkbook(file_name)
  pop_data <- readWorksheet(wb, "LLSOA England and Wales", startRow=2, startCol=3)
  rm(wb)

  ## Tidy up both files
  tidy_pop_data <- function(l) {
    names(l) <- c("LSOA_code", "name", "population", "area", "households")
    empty_rows <- which(is.na(l$population))
    if (length(empty_rows)>0) l <- l[-empty_rows,]
    return(l[,-2])
  }

  pop_data <- tidy_pop_data(pop_data)
  
  ## Return the result
  return(pop_data)
}
  
