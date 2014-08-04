##' Get LSOA energy consumption data
##'
##' This function fetches LSOA (Lower Super Output Area) data from the
##' DECC website.
##'
##' @param year the years of data to fetch.  The default is the most
##' recent year.
##' @param sector a vector of economic sectors to fetch.  For LSOA
##' data, only 'domestic' is allowed.
##' @param fuel the fuel type to fetch.  Valid values are
##' 'electricity', 'gas'
##' @param id a vector of LSOA ids to fetch.  If not specified, then
##' all LSOAs are retrieved.
##' @param dir an optional directory in which to store a copy of the
##' data
##' @return a long data frame with the requested data.  The 'energy'
##' column is measured in GWh.
##' @keywords data energy
##' @export
##' @examples
##' \dontrun{
##' lsoa_data <- get_LSOA_data() # Gets all data
##' }
get_LSOA_data <- function(year=max(get_LSOA_years()), fuel=c("electricity", "gas"), sector="domestic", id, dir) {
    return(get_SOA_data("LSOA", year, "domestic", fuel, id, dir))
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
  
