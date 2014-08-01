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
##' @importFrom plyr llply
##' @examples
##' \dontrun{
##' msoa_data <- get_MSOA_data() # Gets all data
##' }
##' 
get_MSOA_data <- function(id, year=max(get_MSOA_years()), sector=c("domestic", "nondomestic"), fuel=c("electricity", "gas"), dir) {
    return(get_SOA_data("MSOA", id, year, sector, fuel, dir))
}

##' Gets the years for which MSOA data are available
##'
##' Gets the years for which MSOA data are available
##'
##' @return a numeric vector of valid years
##' @export
get_MSOA_years <- function() {
    get_SOA_years("MSOA")
}
    
#' Gets metadata for all MSOAs (including Scottish IGZs)
#'
#' Gets the socio-demographic data associated with each Middle Super
#' Output Area (MSOA).
#'
#' @param dir an (optional) directory in which to save the downloaded
#' data
#' @source
#' \url{https://www.gov.uk/government/statistical-data-sets/socio-economic-data-for-mlsoa-igz-and-llsoa-electricity-and-gas-estimates}
#' @export
#' @return a data frame with the MSOA id code, population, area (in
#' hectares), and number of households
get_MSOA_metadata <- function(dir) {
    get_SOA_metadata("MSOA", dir)
}
  

    
