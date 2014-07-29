##' Get MSOA energy consumption data
##'
##' This function fetches MSOA (Middle Super Output Area) data from the
##' DECC website.  Since the format of these files tends to change just
##' enough to be irritating for dependent code, there is also an option
##' that allows you to cache a local copy.
##'
##' @param id the unique id of the MSOA to fetch.  If not specified,
##' then all MSOAs are retrieved.
##' @param year the year for which you want data.  Defaults to the most
##' recent year available.
##' @param sector the economic sector to fetch.  Valid values are
##' 'domestic', 'nondomestic'
##' @param fuel the fuel type to fetch.  Valid values are
##' 'electricity', 'gas'
##' @param dir an optional directory in which to save a copy of the
##' data
##' @return a long data frame with the requested data.  The 'energy'
##' column is measured in GWh.
##' @keywords data energy
##' @export
##' @import plyr
##' @examples
##' \dontrun{
##' msoa_data <- get_MSOA_data() # Gets all data
##' }
##' 
get_MSOA_data <- function(id, year=max(get_msoa_years()), sector=c("domestic", "nondomestic"), fuel=c("electricity", "gas"), dir) {

    ## Check for valid years
    valid <- get_msoa_years()
    if (length(setdiff(year, valid))>0) {
        warning("Invalid years detected.  Using available values; see get_msoa_years()")
        year <- intersect(year, valid)
        if(length(year)==0) year <- max(valid)
    }
    
    ## Because the format of each spreadsheet is slightly different we
    ## have to do some ugly hacking in the parse_raw_MSOA_data function
    ## below
    params <- get_master_MSOA_params_list(dir)
  
    ## Subset this to only those sectors that we care about
    cond <- lapply(params, function(l) return(l$year %in% year & l$sector %in% sector & l$fuel %in% fuel))
    params <- params[unlist(cond)]
  
    ## Now actually go and get the data
    tmp <- llply(params, parse_raw_MSOA_data)
    all_data <- do.call("rbind", tmp)
    
    ## Remove the unallocated MSOAs
    all_data <- all_data[all_data$MSOA_code!="Unallocated", ]

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

##' Gets the years for which MSOA data are available
##'
##' Gets the years for which MSOA data are available
##'
##' @return a numeric vector of valid years
##' @import XML RCurl
##' @export
get_msoa_years <- function() {

    url <- "https://www.gov.uk/government/collections/mlsoa-and-llsoa-electricity-and-gas-estimates"
    download.file(url, destfile=tf <- tempfile(fileext=".html"), method="curl")
    doc <- htmlParse(tf)
    links <- xpathSApply(doc, "//a[contains(text(), 'MLSOA')]/text()")
    links <- unlist(lapply(links, xmlValue))
    years <- suppressWarnings(as.numeric(gsub(".*([0-9]{4})$", "\\1", links)))
    years <- years[!is.na(years)]
    return(sort(years))
}
    
##' Parses raw MSOA data
##' 
##' Parses the raw MSOA data for a set of given parameters.  
##'
##' @param l list giving the function parameters including
## url, ws, start_row, start_col, end_row, end_col, custom_function, data_col
parse_raw_MSOA_data <- function(l) {
  
    ## Get the file name (and download if necessary)
    file_name <- get_remote_file(l$url, l$dir)
    
    ## Load it into memory
    wb <- tryCatch({
        loadWorkbook(file_name)
    }, error=function(e) {
        message(sprintf("Error loading workbook:\n\n%s\nTried download file from %s.  Email package maintainer to see if URL has changed.  Returning an empty data frame.", e, l$url))
        return(NULL)
    })

    ## If a valid workbook isn't found, return an empty data frame
    if (is.null(wb)) return(data.frame())
    
    data <- readWorksheet(wb, l$sheet_name, startRow=2)
    rm(wb)
    
    ## Perform any custom changes to the data set
    msoa <- data[,3]
    energy <- l$custom_function(data)
    
    ## Set the names
    data <- data.frame(msoa, energy, s=l$sector, f=l$fuel, y=l$year, row.names=1:length(msoa))
    names(data) <- c("MSOA_code", "energy", "sector", "fuel", "year")

    ## Convert kWh to GWh
    data <- mutate(data, energy=energy/1e6)
    
    ## Remove empty rows and return
    data <- data[!is.na(data$MSOA_code), ]

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
    urls <- c("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/296230/MSOA_non-domestic_electricity_estimates__2012_.xlsx",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/296237/MSOA_non_-_domestic_gas_estimates__2012_.xlsx",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/296234/MSOA_domestic_electricity_estimates__2012_.xlsx",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/296241/MSOA_domestic_gas_estimates__2012_.xlsx",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/303114/MSOA_non-domestic_electricity_estimates__2011_.xlsx",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/303113/MSOA_non-domestic_gas_estimates__2011_.xlsx",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/303110/MSOA_domestic_electricity_estimates__2011_.xlsx",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/303112/MSOA_domestic_gas_estimates__2011_.xlsx",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49425/4830-mlsoa-non-dom-elec-2010-alldata.xls",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49427/4831-mlsoa-non-dom-gas-2010-alldata.xls",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49424/4828-mlsoa-dom-elec-2010-alldata.xls",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49426/4842-mlsoa-dom-gas-2010-all-data.xls")
             
    ## And the corresponding worksheet names are
    worksheet_names <- c("MSOA non-domestic elec 2012",
                         "MSOA non-domestic gas",
                         "MSOA domestic electricity 2012",
                         "MSOA domestic gas",
                         "MSOA non-domestic elec 2011",
                         "MSOA non-domestic gas 2011",
                         "MSOA domestic electricity 2011",
                         "MSOA domestic gas (2011)",
                         "MLSOA Non-domestic Electricity",
                         "Non-domestic gas MLSOA",
                         "MLSOA Domestic Electricity",
                         "MLSOA Domestic Gas")
    
    sectors <- rep(c("nondomestic", "domestic"), each=2, 3)
    fuels <- rep(c("electricity", "gas"), 2*3)
    years <- rep(2012:2010, each=4)
    
    ## Then custom functions to retrieve the data since the domestic electricity is split
    ## These returns the code column first and then the energy value
    null_function <- function(df) return(df[,4])
    dom_elec_function <- function(df) return(apply(df[,4:5],1,sum,na.rm=TRUE))
    
    dir <- validate_directory(dir)
  
    ## Build a data.frame summarizing everything
    df <- data.frame(url=urls, sheet_name=worksheet_names, year=years,
                       sector=sectors, fuel=fuels, dir=dir, stringsAsFactors=FALSE)
    df.l <- dlply(df, c("year", "sector", "fuel"), as.list)
  
    ## Add the custom function to this list.  For most sheets it's simply retrieving
    ## the fourth column, but domestic electricity is a bit different.
    df.l <- lapply(df.l, function(l) {
        if(l$sector=="domestic" & l$fuel=="electricity") {
            return(c(l, list(custom_function=dom_elec_function)))
        } else {
            return(c(l, list(custom_function=null_function)))
        }
    })

    return(df.l)
}

#' Gets the 2011 population estimates for all MSOAs (including Scottish IGZs)
#'
#' This function gets the socio-demographic data associated with each
#' Middle Super Output Area (MSOA).
#'
#' @param dir an (optional) directory in which to save the downloaded
#' data
#' @source
#' \url{https://www.gov.uk/government/statistical-data-sets/socio-economic-data-for-mlsoa-igz-and-llsoa-electricity-and-gas-estimates}
#' @export
#' @return a data frame with the MSOA_code, population, area (in
#' hectares), and number of households
get_MSOA_population <- function(dir) {

    ## Download the file
    url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/175644/Socio-economic_data_2013.xls"
    file_name <- get_remote_file(url, dir)

    ## Now open up the file and read the data
    ## Note that it is in two parts: English MLSOAs and Scottish IGZs
    wb <- tryCatch({
        loadWorkbook(file_name)
    }, error=function(e) {
        message(sprintf("Error loading workbook:\n\n%s\nTried download file from %s.  Email package maintainer to see if URL has changed.  Returning an empty data frame.", e, url))
        return(NULL)
    })

    ## If a valid workbook isn't found, return an empty data frame
    if (is.null(wb)) return(data.frame())
    
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
  

    
