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
        if(length(year)==0) year <- max(valid)
    }
    
    ## At the moment, DECC only provides these statistics for the
    ## domestic sector
    sector <- "domestic"

    ## Fetch the master parameters list
    params <- get_params_list("LSOA")
    dir <- validate_directory(dir)
    params <- lapply(params, function(l) c(l, list(dir=dir)))

    ## Subset to only those ones we care about
    cond <- lapply(params, function(l) return(l$year %in% year & l$sector %in% sector & l$fuel %in% fuel))
    params <- params[unlist(cond)]

    ## Now actually go and get the data
    tmp <- llply(params, function(l) parse_raw_SOA_data("LSOA", l))
    all_data <- do.call("rbind", tmp)

    ## Remove the unallocated LSOAs
    all_data <- all_data[all_data$LSOA!="Unallocated", ]

    ## Subset on the target ids
    if (!missing(id)) {
        if (!is.na(id))  {
            all_data <- all_data[which(all_data$LSOA %in% id),]
        }
    }

    ## Renumber rows and return the result
    row.names(all_data) <- 1:nrow(all_data)
    return(all_data)
    
}

##' Gets the years for which LSOA data are available
##'
##' Gets the years for which LSOA data are available
##'
##' @return a numeric vector of valid years
##' @export
get_LSOA_years <- function() {
    get_SOA_years("LSOA")
}

##' Gets metadata for all LSOAs
##'
##' Gets the socio-demographic data associated with each Lower Super
##' Output Area (LSOA). These data only cover England and Wales.
##'
##' @param dir an (optional) directory in which to save the downloaded
##' data
##' @source
##' \url{https://www.gov.uk/government/statistical-data-sets/socio-economic-data-for-mlsoa-igz-and-llsoa-electricity-and-gas-estimates}
##' @export
##' @return a data frame with the LSOA id code, population, area (in
##' hectares), and number of households
get_LSOA_metadata <- function(dir) {
    get_SOA_metadata("LSOA", dir)
}
  
