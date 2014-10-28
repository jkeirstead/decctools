##' Get LAD energy consumption data
##'
##' Gets LAD (Local Administrative District) energy data from the DECC
##' website, \url{http://www.decc.gov.uk}.  Since the format of these
##' files tends to change just enough to be irritating for dependent
##' code, there is also an option that allows you to cache a local
##' copy.  Note that the total values do not contain the unallocated
##' fuel values, which represent approximately 0.3% of the total
##' demand.
##'
##' @param year the year to fetch. If not specified, then the most
##' recent year is retrieved.  Only single years currently supported
##' @param sector a vector of economic sectors to fetch.  Valid values
##' are 'domestic', 'industrial', 'transport', 'total', 'all'.
##' Default = 'total'
##' @param fuel a vector of fuel types to fetch.  Valid values are
##' 'coal', 'manufactured', 'petrol', 'gas', 'electricity',
##' 'bioenergy', 'total', 'all'. Default = 'total'.  'All' includes
##' the total as well.  
##' @param id the unique id of the LAD to fetch.  If not specified,
##' then all LADs are retrieved.
##' ##' @param dir an optional directory in which to cache the data
##' @return a long data frame with the requested data.  The 'energy'
##' column is measured in GWh.
##' @keywords data energy
##' @export
##' @import XLConnect
##' @examples
##' \dontrun{
##' # Gets energy data for total fuels and sectors for most recent year
##' lad_data <- get_LAD_data()
##' }
##' 
##' \dontrun{
##' 
##' # Gets energy data for electricity and gas use in the domestic
##' sector in the most recent year # Depending on the status of DECC
##' servers, this can sometimes fail.  In which case, an empty # data
##' frame is returned.
##'
##' df <- get_LAD_data(sector="domestic", fuel=c("electricity",
##' "gas"))
##'
##' }
##'    
get_LAD_data <- function(year=max(get_LAD_years()), sector='total', fuel='total', id, dir) {

    ## Download the data if necessary
    url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/360876/Sep_2014_sub_national_total_final_energy_consumption_statistics.xlsx"
    file_name <- get_remote_file(url, dir)

    ## A fix for bioenergy
    if (fuel=="bioenergy" & sector!="total") {
        sector <- "total"
        warning("Bioenergy is not divided by sector.  Total value will be reported.")
    }
    
    ## Check for valid years
    valid <- get_LAD_years()
    if (length(setdiff(year, valid))>0) {
        warning("Invalid years detected.  Using available values; see get_lsoa_years()")
        year <- intersect(year, valid)
        if(length(year)==0) year <- max(valid)
    }
  
    ## Read in the data
    wb <- tryCatch({
        loadWorkbook(file_name)
    }, error=function(e) {
        message(e)
        print(file_name)
        return(NULL)
    })

    ## If a valid workbook isn't found, return NULL
    if (is.null(wb)) return(NULL)
    params <- list(id=ifelse(missing(id), NA, id), sector=sector, fuel=fuel)
    df <- lapply(year, function(y) process_tab(wb, y, params))
    rm(wb)

    ## Combine back into a data frame
    df <- do.call("rbind", df)

    return(df)

}

##' Loads data from a specified tab
##'
##' Loads energy data from a specified tab in the LAD workbook.
##'
##' @param wb the open workbook containing all of the tabs
##' @param y the year of data to fetch
##' @param params a list of parameters giving the target id, sector,
##' and fuel
##'
##' @return a data frame with the data
##' @import XLConnect
process_tab <- function(wb, y, params) {
    
    unit <- "GWh"
    sheet_name <- paste(y, unit)
    
    df <- readWorksheet(wb, sheet_name, 4, 1, 421, 31)
    
    ## Since tmp contains all of the data, we now need to work some
    ## magic to get the right values
    df <- clean_decc_data(df)
    df <- cbind(df, year=y)

    ## Subset on target ids
    if (!is.na(params$id)) {
        df <- df[which(df$LAU1_code %in% params$id),]
    }    

    ## Subset to select only those sectors and fuels of interest If
    ## all is given as either sector or fuel then all options are
    ## returned           
    if (!is.element("all", params$sector) & !is.element("all", params$fuel)) {
        df <- df[which(df$sector %in% params$sector & df$fuel %in% params$fuel), ]
    } else if (is.element("all", params$sector) & !is.element("all", params$fuel)) {
        df <- df[which(df$fuel %in% params$fuel),] 
    } else if (!is.element("all", params$sector) & is.element("all", params$fuel)) {
        df <- df[which(df$sector %in% params$sector),]
    }
    
    ## Renumber rows and return the result
    row.names(df) <- 1:nrow(df)
    return(df)
}

##' Get the years for which LAD data is available
##' 
##'
##' @return a vector of years
##' @import stringr XLConnect
##' @export
get_LAD_years <- function() {

    url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/360876/Sep_2014_sub_national_total_final_energy_consumption_statistics.xlsx"
    file_name <- get_remote_file(url)

    ## Load in the raw data from the spreadsheet
    wb <- tryCatch({
      loadWorkbook(file_name)
  }, error=function(e) {
      message(e)
      return(NULL)
  })
    

    ## Get a list of the sheets
    sheets <- getSheets(wb)
    rm(wb)
    
    ## Keep only those with the units in the name
    sheets <- sheets[grep("GWh", sheets)]

    ## Drop the unit suffix and make numeric
    years <- as.numeric(str_trim(str_replace(sheets, "GWh", "")))
    return(years)
}
  

##' Cleans the raw DECC energy consumption data for easier use
##'
##' @param df the raw DECC data frame from the spreadsheet
##' @return a long database with headers by sector and fuel
##' @import reshape2 stringr
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

    ## Tidy the name fields so they will match get_geo_lookup
    name <- NULL # R CRAN check hack for below
    df.m <- transform(df.m, name=str_replace(name, "&", "and"))
    df.m <- transform(df.m, name=str_replace(name, " on Trent", "-on-Trent"))
    df.m <- transform(df.m, name=str_replace(name, ", Cynon, Taff", " Cynon Taf"))
    df.m <- transform(df.m, name=str_replace(name, "Vale of Glamorgan, The", "The Vale of Glamorgan"))
    df.m <- transform(df.m, name=str_replace(name, "Edinburgh, City of", "City of Edinburgh"))
    df.m <- transform(df.m, name=str_replace(name, " \\(Western Isles\\)", ""))
    df.m <- transform(df.m, name=str_replace(name, "Dungannon.*", "Dungannon"))
    df.m <- transform(df.m, name=str_replace(name, " upon Avon", "-on-Avon"))
    
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

##' Gets metadata to describe LADs
##'
##' Gets metadata to decribe the local authority districts
##'
##' @param dir an optional directory in which to save a copy of the metadata
##' @return a data frame giving metadata for each LAD.  See \link{LAD_metadata}
##' @export
get_LAD_metadata <- function(dir) {
    data(LAD_metadata, envir=environment())

    if (!missing(dir)) {
        dir <- validate_directory(dir)
        write.csv(get("LAD_metadata"), file=file.path(dir, "LAD_metadata.csv"), row.names=FALSE)
    }

    return(get("LAD_metadata"))
}
