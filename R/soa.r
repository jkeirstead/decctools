##' Get SOA energy consumption data
##'
##' Gets SOA (Super Output Area) energy consumption data from the DECC
##' website.  You can select which SOA level, sector, year, and fuel
##' type to fetch, as well as specifying a directory for local
##' caching.
##'
##' @param level either "LSOA" or "MSOA"
##' @param year the year for which you want data.  Defaults to the
##' most recent year available.
##' @param sector the economic sector to fetch.  Valid values are
##' 'domestic', 'nondomestic'
##' @param fuel the fuel type to fetch.  Valid values are
##' 'electricity', 'gas'
##' @param id the unique id of the SOA to fetch.  If not specified,
##' then all SOAs are retrieved.
##' @param dir an optional directory in which to save a copy of the
##' data
##' @return a long data frame with the requested data.  The 'energy'
##' column is measured in GWh.
##' @keywords data energy
##' 
get_SOA_data <- function(level, year=max(get_SOA_years()), sector=c("domestic", "nondomestic"), fuel=c("electricity", "gas"), id, dir) {

    ## Check for valid years
    valid <- get_SOA_years(level)
    if (length(setdiff(year, valid))>0) {
        warning("Invalid years detected.  Using available values; see get_SOA_years()")
        year <- intersect(year, valid)
        if(length(year)==0) year <- max(valid)
    }

    ## At the moment, DECC only provides these statistics for the
    ## domestic sector
    if (level=="LSOA") sector <- "domestic"
    
    ## Because the format of each spreadsheet is slightly different we
    ## have to do some ugly hacking in the parse_raw_SOA_data function
    ## below
    params <- get_params_list(level)
    dir <- validate_directory(dir)
    params <- lapply(params, function(l) c(l, list(dir=dir)))

    ## Subset this to only those sectors that we care about
    cond <- lapply(params, function(l) return(l$year %in% year & l$sector %in% sector & l$fuel %in% fuel))
    params <- params[unlist(cond)]
  
    ## Now actually go and get the data
    tmp <- llply(params, function(l) parse_raw_SOA_data(level, l))    
    all_data <- do.call("rbind", tmp)
    
    ## Remove the unallocated SOAs
    all_data <- all_data[all_data[level]!="Unallocated", ]

    ## Subset on the target ids
    if (!missing(id)) {
        if (!is.na(id)) {
            all_data <- all_data[which(all_data[level] %in% id),]
        }
    }
    
    ## Renumber rows and return the result
    row.names(all_data) <- 1:nrow(all_data)
    return(all_data)
}


##' Gets the years for which SOA data are available
##'
##' Gets the years for which SOA data are available
##'
##' @param level one of "LSOA" (default) or "MSOA" specifying the
##' output area level
##' @return a numeric vector of valid years for \code{level}
##' @import XML RCurl
get_SOA_years <- function(level="LSOA") {

    ## Validate the input
    level <- validate_SOA_level(level)

    ## Use a flag for the rest of this
    lower <- (level=="LSOA")

    ## Get the available years from the index website.  Unfortunately
    ## this has been be downloaded to actually parse it.
    url <- "https://www.gov.uk/government/collections/mlsoa-and-llsoa-electricity-and-gas-estimates"
    tf <- get_remote_file(url, dir=tempdir())

    ## Parse the document and extra the relevant links
    doc <- htmlParse(tf)
    links <- xpathSApply(doc, sprintf("//a[contains(text(), '%s')]/text()", ifelse(lower, 'LLSOA', 'MLSOA')))
    links <- unlist(lapply(links, xmlValue))
    pattern <- ifelse(lower, ".*([0-9]{4}) \\(experimental\\)$", ".*([0-9]{4})$")
    years <- suppressWarnings(as.numeric(gsub(pattern, "\\1", links)))
    years <- years[!is.na(years)]

    ## Return the result
    return(sort(years))
}

##' Validates a SOA level
##'
##' Only one entry is allowed, either LSOA or MSOA
##'
##' @param level a character vector of unprocessed SOA level values
##' @return a single valid level value
validate_SOA_level <- function(level) {
    valid <- c("LSOA", "MSOA")
    level <- intersect(unique(level), valid)
    if (length(level)>1) {
        warning("Only one level at a time please; using LSOA.")
        level <- "LSOA"
    } else if (length(level)==0) {
        warning("No valid levels specified, defaulting to LSOA.")
        level <- "LSOA"
    }

    return(level)
}

##' Gets metadata for Super Output Areas
##'
##' Gets the socio-demographic data associated with each Super Output
##' Area.
##'
##' @param level one of "LSOA" (default) or "MSOA" specifying the
##' output area level
##' @param dir an (optional) directory in which to save the downloaded
##' data
##' @source
##' \url{https://www.gov.uk/government/statistical-data-sets/socio-economic-data-for-mlsoa-igz-and-llsoa-electricity-and-gas-estimates}
##' @return a data frame with the SOA id code, population, area (in
##' hectares), and number of households
##' @import XLConnect
get_SOA_metadata <- function(level, dir) {

    ## Validate the input
    level <- validate_SOA_level(level)
    
    ## Download the file
    url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/175644/Socio-economic_data_2013.xls"
    file_name <- get_remote_file(url, dir)

    ## Now open up the file and read the data
    ## Note that it is in two parts: English MLSOAs and Scottish IGZs
    wb <- tryCatch({
        loadWorkbook(file_name)
    }, error=function(e) {
        message(e)
        return(NULL)
    })

    ## If a valid workbook isn't found, return an empty data frame
    if (is.null(wb)) return(data.frame())

    if (level=="MSOA") {
        england_data <- readWorksheet(wb, "MLSOA England and Wales", startRow=2, startCol=1)
        scotland_data <- readWorksheet(wb, "IGZ Scotland", startRow=2, startCol=1)
        pop_data <- list(england_data, scotland_data)
    } else {
        pop_data <- readWorksheet(wb, "LLSOA England and Wales", startRow=2, startCol=3)
        pop_data <- list(pop_data)
    }
    rm(wb)
    
    ## Tidy up all of the files
    pop_data <- lapply(pop_data, function(l) {
        names(l) <- c(level, "name", "population", "area", "households")
        empty_rows <- which(is.na(l$population))
        if (length(empty_rows)>0) l <- l[-empty_rows,]
        return(l[,-2])
    })
    pop_data <- do.call("rbind", pop_data)
    
    ## Return the result
    return(pop_data)
}
  

    
##' Parses raw SOA data
##' 
##' Parses the raw SOA data for a set of given parameters.  This
##' function deals with the fact that every DECC spreadsheet has a
##' slightly different layout, but the extraction is the same.
##' Namely, open up an Excel spreadsheet, get the right tab, extract
##' the data from the right columns, tidy, and return.
##'
##' @param level one of "LSOA" or "MSOA" specifying the
##' output area level
##' @param params a list giving the function parameters including
## url, dir, sheet_name, custom_function, sector, fuel, and year
##' @return a data frame with the energy data.  NULL if not all of the
##' required parameters are specified
##' @import XLConnect
##' @importFrom plyr mutate
parse_raw_SOA_data <- function(level, params) {

    ## Validate the inputs
    level <- validate_SOA_level(level)
    reqd_params <- c("url", "dir", "sheet_name", "cols",
                     "sector", "fuel", "year", "start_row")
    if (!all(reqd_params %in% names(params))) {
        missing_pars <- setdiff(reqd_params, names(params))
        warning(sprintf("Required parameters '%s' are missing: returning NULL",
                        paste(missing_pars, collapse=", ")))
        return(NULL)
    }
    
    ## Get the file name (and download if necessary)
    file_name <- get_remote_file(params$url, params$dir)
    
    ## Load it into memory
    wb <- tryCatch({
        loadWorkbook(file_name)
    }, error=function(e) {
        message(e)
        return(NULL)
    })

    ## If a valid workbook isn't found, return an empty data frame
    if (is.null(wb)) return(data.frame())

    data <- readWorksheet(wb, params$sheet_name, startRow=params$start_row, header=FALSE)
    rm(wb)
    
    ## Perform any custom changes to the data set
    soa <- data[,ifelse(level=="LSOA", 4, 3)]
    energy <- apply(data[,eval(parse(text=sprintf("c(%s)", params$cols))), drop=FALSE], 1, sum, na.rm=TRUE)
    
    ## Set the names
    data <- data.frame(soa, energy, s=params$sector, f=params$fuel, y=params$year, row.names=1:length(soa))
    names(data) <- c(level, "energy", "sector", "fuel", "year")

    ## Convert kWh to GWh
    data <- mutate(data, energy=energy/1e6)
    
    ## Remove empty rows and return
    data <- data[!is.na(data[level]), ]

    return(data)
}

##' Builds a master set of parameters for SOA data
##'
##' Creates a list of various parameters needed to download and
##' extract SOA data from the DECC website.
##'
##' @param level one of "LSOA" or "MSOA" specifying the output area
##' level
##' @return a list containing the parameters necessary to read each
##' LSOA data file
get_params_list <- function(level) {

    ## Validate the inputs
    level <- validate_SOA_level(level)

    ## Build a data.frame summarizing everything
    data(params, envir=environment())
    
    ## Fix automatic factorisation of the text file
    factor_cols <- sapply(params, is.factor)
    params[factor_cols] <- lapply(params[factor_cols], as.character)
    
    ## Subset for the right level and remove key column
    params <- params[which(params$level==level), ]
    params <- params[, -1]

    df.l <- dlply(params, c("year", "sector", "fuel"), as.list)
  
    return(df.l)
}
