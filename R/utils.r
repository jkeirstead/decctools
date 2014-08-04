##' Gets a file from a remote URL
##' 
##' Since the URLs of UK government data appear to change quite
##' frequently, the method will automatically cache a local copy.
##'
##' @param url the address of the file to download
##' @param dir the directory to store the cached file in.  If
##' undefined, it defaults to $TEMP/decctools.
##' @param update_cache boolean.  Should the cached file be
##' overwritten?  Default = FALSE
##' @return the file name (local path)
##' @import stringr RCurl
get_remote_file <- function(url, dir, update_cache=FALSE) {

  ## Currently only processes one URL
  if (length(url)>1) {
    warning("Only one URL at a time currently supported.  Using first URL only.")
    url <- url[1]
  } else if (is.na(url)) {
    stop("A valid URL must be specified.")
  } 

  dir <- validate_directory(dir)
  
  ## Split the URL to identify the filename
  tmp <- unlist(str_split(url, "/"))
  file_name <- file.path(dir, tmp[length(tmp)])

  ## If the file doesn't exist, then download it
  if (!file.exists(file_name) | update_cache) {

      ## see if the certificates are installed
      certs <- system.file("CurlSSL", "cacert.pem", package="RCurl")
      if (file.exists(certs)) {
          options(RCurlOptions=list(cainfo=certs))
      } else {
          warning(sprintf("Unable to find SSL certificates file.  Running without SSL validation.",
                          certs))
          options(RCurlOptions=list(ssl.verifypeer=FALSE))
      }

      if (!url.exists("http://www.google.com")) {
          stop("You must be connected to the internet to download this file.")
      } else {
          f <- CFILE(file_name, mode="wb")
          curlPerform(url=url, writedata=f@ref)
          close(f)
      }    
  }
  
  return(file_name)
}

##' Validates a user-specified directory
##'
##' Ensures that a user-specified directory exists.  If the argument is
##' missing then a placeholder directory is created in $TEMP.
##' 
##' @param dir the directory name
##' @return the validated directory name
validate_directory <- function(dir) {
    
  ## Define the cache directory
  if (missing(dir)) dir=file.path(tempdir(), get_package_name())
  if (is.na(dir)) dir=file.path(tempdir(), get_package_name())

  ## Make sure it exists before trying to download
  if (!file.exists(dir)) dir.create(dir)

  return(dir)
}

##' Gets the name of this package
##'
##' @return a character string
get_package_name <- function() {
  return ("decctools")
}  

##' Gets a lookup table to match IDs across geographies
##'
##' Provides a lookup table giving the local authority name and
##' corresponding IDs for LAD, MSOA, and LSOA geographies.
##'
##' @note As SOA energy data are not available for Northern Ireland,
##' this method only returns a lookup for England, Wales, and Scotland.
##'
##' @return a data frame
##' @export
get_geo_lookup <- function() {

    ## For England and Wales, this is quite straight forward as ONS
    ## provides a single lookup table.
    url <- "https://geoportal.statistics.gov.uk/Docs/Lookups/Output_areas_(2011)_to_lower_layer_super_output_areas_(2011)_to_middle_layer_super_output_areas_(2011)_to_local_authority_districts_(2011)_E+W_lookup.zip"
    file_name <- get_remote_file(url, NA)
    unzip(file_name, exdir=tempdir())
    df <- read.csv(file.path(tempdir(), "OA11_LSOA11_MSOA11_LAD11_EW_LUv2.csv"),
                   stringsAsFactors=FALSE)
    df <- df[,c("LSOA11CD", "MSOA11CD", "LAD11CD")]
    names(df) <- c("LSOA", "MSOA", "LAD")
    
    ## Scotland however does things its own way. The first file
    ## provides the lookup between IGZ/MSOA geographies and local
    ## authorities.  But their Scottish Executive Local Authority
    ## codes don't match up to the ONS codes.  I've manually built a
    ## lookup table from
    ## \url{https://www.scotxed.net/Survey%20documentation/List%20of%20local%20authority%20codes.xls}
    url <- "http://www.scotland.gov.uk/Resource/Doc/933/0109431.txt"
    file_name <- get_remote_file(url, NA)
    scot.df <- read.csv(file_name)
    data(scotland_igz, envir=environment())
    scot.df <- merge(scot.df, scotland_igz, by.x="LA_CODE", by.y="scotex")
    scot.df <- scot.df[,c("IZ_CODE", "lad")]
    names(scot.df) <- c("MSOA", "LAD")
    scot.df <- cbind(LSOA=NA, scot.df)

    ## Fortunately Northern Ireland doesn't even provide lower level
    ## energy data so we don't need to worry about this.

    tmp <- rbind(df, scot.df)
    return(tmp)
}

##' Checks if a specified local authority is urban
##'
##' Checks if a local authority is urban according to a user specified
##' classification.  See the details for more, well, detail.
##'
##' @details The urban classifications are described at
##' \url{http://www.ons.gov.uk/ons/guide-method/geography/products/area-classifications/rural-urban-definition-and-la/rural-urban-local-authority--la--classification--england-/index.html}
##' and are summarized as follows:
##'
##' \itemize{
##' 
##' \item MU = Major Urban: districts with either 100,000 people or 50
##' per cent of their population in urban areas with a population of
##' more than 750,000
##' 
##' \item LU = Large Urban: districts with either 50,000 people or 50
##' per cent of their population in one of 17 urban areas with a
##' population between 250,000 and 750,000
##' 
##' \item OU = Other Urban: districts with fewer than 37,000 people or
##' less than 26 per cent of their population in rural settlements and
##' larger market towns
##' 
##' \item SR = Significant Rural: districts with more than 37,000
##' people or more than 26 per cent of their population in rural
##' settlements and larger market towns
##' 
##' \item R50 = Rural-50: districts with at least 50 per cent but less
##' than 80 per cent of their population in rural settlements and
##' larger market towns
##' 
##' \item Rural-80: districts with at least 80 per cent of their
##' population in rural settlements and larger market towns
##' 
##' }
##'
##'
##' Unfortunately these classifications are only defined for
##' England. For all other countries, assumptions have been made about
##' which LAUs are urban (LU) or rural (SR) using the following data
##' sources:
##' 
##' \itemize{
##' 
##' \item Wales,
##' \url{http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/administrative/wales/unitary-authorities/index.html}
##' 
##' \item Scotland,
##' \url{http://www.scotland.gov.uk/Publications/2004/06/19498/38788}
##'
##' \item Northern Ireland,
##' \url{http://www.nisra.gov.uk/geography/default.asp10.htm}
##'
##' }
##'
##' @param lad a character vector a LAD ids (post-2011 format)
##' @param urban a character vector specifying which of the above
##' classifications to consider as a urban
##' @return a boolean vector of \code{length(lad)} indicating whether
##' the corresponding local authority is urban
##' @export
is_urban <- function(lad, urban=c("MU", "LU", "OU")) {
    data(LAD_metadata, envir=environment())
    tmp <- LAD_metadata[which(LAD_metadata$new %in% lad),]
    return(tmp$urban_class %in% urban)
}
