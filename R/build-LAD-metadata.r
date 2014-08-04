##' Builds the LAD metadata
##'
##' Builds a metadata set for the package
build_LAD_metadata <- function() {
    lookup <- get_code_lookup()
    pop <- get_population()
    urban <- get_urban_rural_classes()
    group <- get_area_classifications()
    climate <- get_climate_data()

    ## Assemble everything
    tmp <- merge(lookup, pop, by.x="new", by.y="code")
    tmp <- merge(tmp, urban)
    tmp <- merge(tmp, group, all.x=TRUE)
    tmp <- merge(tmp, climate)

    ## Drop the name.y column (with Unitary Authority on the end)
    tmp <- tmp[, -which(names(tmp)=="name.y")]
    names(tmp)[4] <- "name"

    ## Reorder
    tmp <- tmp[,c("name", "country", "new", "old", "Classification", "supergroup", "population", "area", "hdd", "cdd")]
    names(tmp)[5] <- "urban_class"
    names(tmp)[6] <- "group_name"
    tmp <- arrange(tmp, new)

    ## Need to tidy names to match lookups?
    ## df.m <- transform(df.m, name=str_replace(name, "&", "and"))
    ## df.m <- transform(df.m, name=str_replace(name, "Edinburgh, City of", "City of Edinburgh"))

    return(tmp)      
}

##' Downloads population estimates
##'
##' @return a data frame with the LAD id, name, population, and area (ha)
##'
##' @source Office for National Statistics
##' \url{http://www.ons.gov.uk/ons/rel/census/2011-census/population-and-household-estimates-for-the-united-kingdom}
get_population <- function() {
    library(XLConnect)

    ## Download the spreadsheet and tidy
    url <- "http://www.ons.gov.uk/ons/rel/census/2011-census/population-and-household-estimates-for-the-united-kingdom/rft-table-2-census-2011.xls"
    download.file(url, destfile=tf <- tempfile(), method="curl")
    
    wb <- loadWorkbook(tf)
    df <- readWorksheet(wb, sheet="Table 2", startRow=26, endRow=533, header=FALSE)
    rm(wb)

    ## Subset for the useful columns
    df <- df[,c(1,2,3,4,6,7)]

    ## Remove all of the NA rows
    df <- df[-which(is.na(df$Col1)),]

    ## Remove the English agglomerations and national totals
    aggs <- grep("^E1|([S|W|N]9)", df$Col1)
    df <- df[-aggs,]
    
             
    ## There should only be one valid name per row
    names <- apply(df, 1, function(x) as.character(na.omit(x[2:4])))
    df <- cbind(df, name=names)

    ## Tidy the names
    df <- df[,c(1,7,5,6)]
    names(df) <- c("code", "name", "population", "area")

    ## Remove any trailing notes and spaces
    library(stringr)
    df <- transform(df, name=str_replace(name, "[0-9]*$", ""))
    df <- transform(df, name=str_trim(name), code=str_trim(code))
        
    ## Replace King's Lynn apostrophe
    id <- grep("^King.*Lynn.*", df$name)
    df$name[id] <- "King's Lynn and West Norfolk"

    return(df)
}

##' Gets a look-up between old and new LA codes
##'
##' Gets a look-up table between old and new Local Authority codes.
##' For Northern Ireland, we cheat and reuse the old codes as new
##' codes to make later merging easy.
##' 
##' @source Office for National Statistics
##' 
##' @return a data frame giving the old and new lookup codes, as well
##' as country and name
get_code_lookup <- function() {

    url <- "https://geoportal.statistics.gov.uk/Docs/Lookups/Countries_(2011)_to_government_office_regions_(2010)_to_counties_(2011)_to_local_authority_districts_(2011)_to_wards_(2011)_UK_lookup.zip"
    file <- get_remote_file(url)

    unzip(file, exdir=tempdir())
    contents <- "CTRY11_GOR10_CTY11_LAD11_WD11_UK_LU.csv"
    df <- read.csv(file.path(tempdir(), contents), stringsAsFactors=FALSE)

    ## Drop unused columns
    df <- df[, c(3, 10, 11, 12)]

    ## Just take one for each code
    library(plyr)
    df <- ddply(df, .(LAD11NM), head, n=1)

    names(df) <- c("country", "new", "old", "name")
    df <- arrange(df, old)

    ni <- which(df$country=="Northern Ireland")
    df$new[ni] <- df$old[ni]
    
    return(df)
}

##' Gets urban/rural classifications
##'
##' Gets the urban/rural classification of each local authority.  The
##' data are from 2009.
##' 
##' @note The urban/rural classifications from the 2011 Census are not
##' yet available; see
##' \url{http://www.ons.gov.uk/ons/guide-method/geography/products/area-classifications/2011-rural-urban/index.html}.
##' The data used here are from
##' \url{http://www.ons.gov.uk/ons/guide-method/geography/products/area-classifications/rural-urban-definition-and-la/rural-urban-local-authority--la--classification--england-/index.html}
##' @return a data frame giving the urban rural classification for
##' each local authority with its old id code
##' 
get_urban_rural_classes <- function() {
   
    url <- "http://www.ons.gov.uk/ons/guide-method/geography/products/area-classifications/rural-urban-definition-and-la/rural-urban-local-authority--la--classification--england-/la-classification--post-april-2009-.xls"
    library(XLConnect)
    file <- get_remote_file(url)

    ## Get the classes for England
    wb <- loadWorkbook(file)
    df <- readWorksheet(wb, "England")
    df <- df[,c("Name", "District.Code", "Classification")]
    rm(wb)
    
    ## For the other countries these values are estimated. See the
    ## decctools documentation and benchmarking paper for more info.
    urban_wales <- c("Bridgend", "Cardiff", "Merthyr Tydfil", "Neath Port Talbot", "Newport",
                     "Rhondda Cynon Taf", "Torfaen", "Wrexham")    
    urban_ireland <- c("Derry", "Newtownabbey", "Carrickfergus", "Belfast", "North Down", "Craigavon")
    urban_scotland <- c("Clackmannanshire","East Renfrewshire","Falkirk","Inverclyde","Midlothian",
                        "North Ayrshire","South Lanarkshire","Aberdeen City","Edinburgh, City of",
                        "Renfrewshire","West Dunbartonshire","West Lothian","Dundee City","North Lanarkshire",
                        "East Dunbartonshire","Glasgow City")

    ## Get the lookup table and merge by old code
    lookup <- get_code_lookup()
    tmp <- merge(lookup, df, by.x="old", by.y="District.Code", all.x=TRUE)

    urban_ids <- which(tmp$name %in% c(urban_wales, urban_ireland, urban_scotland))
    tmp[urban_ids,]$Classification <- "LU"

    rural <- which(is.na(tmp$Classification))
    tmp[rural,]$Classification <- "SR"

    tmp <- tmp[,c("old", "Classification")]
    return(tmp)
    
}
    

##' Gets Output Area classifications
##'
##' Gets 2011 Output Area classification data from Office for National
##' Statistics.  At the moment, these are only available at ward level
##' and so the representative classification for the local authority is
##' simply taken as the most common ward classification.
##'
##' @note In all of the other Office of National Statistics products
##' used in this package, Northern Ireland local authorities are not
##' assigned a "new" style code.  However in the area classifications,
##' they do have this new code but corresponding to differently named
##' authorities.  As a result, no area classifications are returned
##' for NI towns.
##' 
##' @return a data frame giving the new LAD code and the most common
##' Supergroup classification name
##' @source \url{http://www.ons.gov.uk/ons/guide-method/geography/products/area-classifications/ns-area-classifications/ns-2011-area-classifications/index.html}
get_area_classifications <- function() {
    url <- "http://www.ons.gov.uk/ons/guide-method/geography/products/area-classifications/ns-area-classifications/ns-2011-area-classifications/datasets/2011-oac-clusters-and-names-csv.zip"

    download.file(url, destfile=tf <- tempfile(), method="curl")
    zip <- "2011 OAC Clusters and Names.csv"
    unzip(tf, files=zip, exdir=tempdir())
    df <- read.csv(file=file.path(tempdir(), zip))

    ## Only take the most common category for each authority
    library(dplyr)
    tmp <- df %.% group_by(Local.Authority.Code, Supergroup.Name) %.%
        summarize(count=n()) %.% arrange(Local.Authority.Code, desc(count))
    tmp2 <- ddply(tmp, .(Local.Authority.Code), head, 1)[,-3]

    names(tmp2) <- c("new", "supergroup")
    return(tmp2)

}

##' Gets climate data for the local authorities
##'
##' Gets heating and cooling degree day data for each local authority.
##' This is based on the benchmarking paper.
##'
##' @return a data frame giving the new code and heating and cooling degree days.
get_climate_data <- function() {
    data(climate, envir=environment())
    names(climate)[1] <- "new"
    return(climate)    
}
