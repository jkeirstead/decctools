##' Get LSOA energy consumption data
##'
##' This function fetches LSOA (Lower Super Output Area) data from the
##' DECC website.
##'
##' @param id a vector of LSOA ids to fetch.  If not specified, then
##' all LSOAs are retrieved.
##' @param year the years of data to fetch.  The default is the most
##' recent year.
##' @param fuel the fuel type to fetch.  Valid values are
##' 'electricity', 'gas'
##' @param dir an optional directory in which to store a copy of the
##' data
##' @return a long data frame with the requested data.  The 'energy'
##' column is measured in GWh.
##' @keywords data energy
##' @export
##' @import plyr
##' @examples
##' \dontrun{
##' lsoa_data <- get_LSOA_data() # Gets all data
##' }
get_LSOA_data <- function(id, year=max(get_LSOA_years()), fuel=c("electricity", "gas"), dir) {

    ## As with the MSOA stuff, we'll prepare a list of all the
    ## parameters and then process them one by one.
    
    ## Check for valid years
    valid <- get_LSOA_years()
    if (length(setdiff(year, valid))>0) {
        warning("Invalid years detected.  Using available values; see get_lsoa_years()")
        year <- intersect(year, valid)
    }

    
    ## At the moment, DECC only provides these statistics for the
    ## domestic sector
    sector <- "domestic"

    ## Fetch the master parameters list
    params <- get_master_LSOA_params_list(dir)

    ## Subset to only those ones we care about
    cond <- lapply(params, function(l) return(l$year %in% year & l$sector %in% sector & l$fuel %in% fuel))
    params <- params[unlist(cond)]

    ## Now actually go and get the data
    tmp <- llply(params, parse_raw_LSOA_data)
    all_data <- do.call("rbind", tmp)

    ## Remove the unallocated LSOAs
    all_data <- all_data[all_data$LSOA_code!="Unallocated", ]

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


##' Parses raw LSOA data
##'
##' Parses the raw LSOA data for a set of given parameters
##'
##' @param l list giving the function parameters including url, ws,
##' start_row, start_col, end_row, end_col, custom_function, data_col
##' @import plyr
parse_raw_LSOA_data <- function(l) {

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
    lsoa <- data[,4]
    energy <- l$custom_function(data)
  
    ## Assemble the final data frame
    data <- data.frame(lsoa, energy, s=l$sector, f=l$fuel, y=l$year, row.names=1:length(lsoa))
    names(data) <- c("LSOA_code", "energy", "sector", "fuel", "year")

    ## Convert kWh to GWh
    data <- mutate(data, energy=energy/1e6)
  
    ## Remove empty rows
    data <- data[!is.na(data$LSOA_code), ]

    return(data)
}

##' Builds a master set of parameters for MSOA data
##'
##' Creates a list of various parameters needed to download and
##' extract MSOA data from the DECC website.
##'
##' @param dir the directory in which to store a copy of the data
get_master_LSOA_params_list <- function(dir) {

  ## First specify the urls
    urls <- c("https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49432/4813-llsoa-domestic-elec-est-2010.xls",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49433/4814-llsoa-domestic-gas-est-2010.xls",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/303117/LSOA_domestic_electricity_estimates__2011_.xlsx",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/303118/LSOA_domestic_gas__2011_.xlsx",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/296257/LSOA_domestic_electricity_estimates__2012_.xlsx",
              "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/317540/LSOA_domestic_gas__2012_.xlsx")

    ## And the corresponding worksheet names are
    worksheet_names <- c("LLSOA Electricity Domestic", "LLSOA Domestic gas",
                       "LSOA domestic electricity 2011", "2011 LSOA Domestic Gas",
                       "LSOA domestic electricity 2012", "LSOA domestic gas")

    ## Define the sectors that these cover
    sectors <- rep("domestic", 6)
    fuels <- rep(c("electricity", "gas"), 3)
    years <- rep(2010:2012, each=2)
  
    ## Then custom functions to retrieve the data since the domestic electricity is split
    ## These returns the code column first and then the energy value
    dom_gas_function <- function(df) return(df[,5])
    dom_elec_function <- function(df) return(apply(df[,5:6],1,sum,na.rm=TRUE))

    dir <- validate_directory(dir)
  
    ## Build a list summarizing everything
    df <- data.frame(url=urls, sheet_name=worksheet_names, sector=sectors, fuel=fuels,
                     year=years, dir=dir, stringsAsFactors=FALSE)
    df.l <- dlply(df, c("year", "sector", "fuel"), as.list)
  
    ## Add the custom functions to to the list
    df.l <- lapply(df.l, function(l) {
        if (l$fuel=="electricity") {
            return(c(l, list(custom_function=dom_elec_function)))
        } else {
            return(c(l, list(custom_function=dom_gas_function)))
        }})
    return(df.l)
}

##' Gets the years for which LSOA data are available
##'
##' Gets the years for which LSOA data are available
##'
##' @return a numeric vector of valid years
##' @import XML RCurl
##' @export
get_LSOA_years <- function() {

    url <- "https://www.gov.uk/government/collections/mlsoa-and-llsoa-electricity-and-gas-estimates"
    download.file(url, destfile=tf <- tempfile(fileext=".html"), method="curl")
    doc <- htmlParse(tf)
    links <- xpathSApply(doc, "//a[contains(text(), 'LLSOA')]/text()")
    links <- unlist(lapply(links, xmlValue))
    years <- suppressWarnings(as.numeric(gsub(".*([0-9]{4}) \\(experimental\\)$", "\\1", links)))
    years <- years[!is.na(years)]
    return(sort(years))
}

##' Gets the 2011 population estimates for all LSOAs
##'
##' This function gets the socio-demographic data associated with each
##' Lower Super Output Area (LSOA). These data only cover England and
##' Wales.
##'
##' @param dir an (optional) directory in which to save the downloaded
##' data
##' @source
##' \url{https://www.gov.uk/government/statistical-data-sets/socio-economic-data-for-mlsoa-igz-and-llsoa-electricity-and-gas-estimates}
##' @export
##' @return a data frame with the LSOA_code, population, area (in
##' hectares), and number of households
get_LSOA_population <- function(dir) {

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
  
