#' Gets a file from a remote URL
#' 
#' Since the URLs of UK government data appear to change quite frequently, the method
#' will automatically cache a local copy.  
#'
#' @param url the address of the file to download
#' @param dir the directory to store the cached file in.  If undefined, it defaults to $TEMP/decctools.
#' @param update_cache boolean.  Should the cached file be overwritten?  Default = FALSE
#' @return the file name (path)
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
    method <- "curl"
    if (system(method)==127) {
      method <- "wget"
      if (system(method)==127) {
        stop("You must have either curl or wget installed.")
      }
    }
    
    if (!url.exists("http://www.google.com")) {
      stop("You must be connected to the internet to download this file.")
    } else {
      download.file(url, file_name, mode="wb", method=method)
    }
  }
  
  return(file_name)
}

#' Validates a user-specified directory
#'
#' Ensures that a user-specified directory exists.  If the argument is missing
#' then a placeholder directory is created in $TEMP.
#' @param dir the directory name
validate_directory <- function(dir) {
  ## Define the cache directory
  if (missing(dir)) dir=file.path(tempdir(), get_package_name())
  if (is.na(dir)) dir=file.path(tempdir(), get_package_name())

  ## Make sure it exists before trying to download
  if (!file.exists(dir)) dir.create(dir)

  return(dir)
}

#' Gets the name of this package
#'
#' @return a character string
get_package_name <- function() {
  return ("decctools")
}  

#' Gets a lookup table to match IDs across geographies
#'
#' Provides a lookup table giving the local authority name and corresponding IDs for LAU, LAD,
#' MSOA, and LSOA geographies.  Also returns a flag indicating whether or
#' not a given LAU is urban.  See \code{\link{LAD_metadata}} for more information.
#'
#' @param urban_classes classifications to be considered as urban.  Possible values include 'OU' (other urban), 'LU' (large urban), 'MU' (major urban), 'SR' (significant rural), 'R80' (80\% rural), 'R50' (50\% rural)
#' @param dir (optional) directory to save the lookup table information
#' @return a data frame
#' @export
get_lookup_table <- function(urban_classes=c("LU", "MU"), dir) {

  ## The lookup data is available in the LSOA electricity demands data from DECC
  ## so we'll use this rather than rooting around a master copy from ONS
  url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49432/4813-llsoa-domestic-elec-est-2010.xls"

  file_name <- get_remote_file(url, dir)

  ## Load it into memory.  Only need first 4 columns
  wb <- loadWorkbook(file_name)
  data <- readWorksheet(wb, "LLSOA Electricity Domestic", startCol=1, endCol=4)
  names(data) <- c("LAU1_name", "LAU1_code", "MSOA_code", "LSOA_code")

  ## Get rid of empty lines
  data <- data[!is.na(data$LAU1_name), ]

  ## Also need to add in the Scottish IGZ codes
  ## Download the file
  url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49424/4828-mlsoa-dom-elec-2010-alldata.xls"
  file_name <- get_remote_file(url, dir)

  ## Now open up the file and read the data
  ## Note that it is in two parts: English MLSOAs and Scottish IGZs
  wb <- loadWorkbook(file_name)
  scotland_data <- readWorksheet(wb, "MLSOA Domestic Electricity")
  rm(wb)
  scotland_data <- scotland_data[,1:3]
  names(scotland_data) <- c("LAU1_name", "LAU1_code", "MSOA_code")
  scotland_data <- scotland_data[!is.na(scotland_data$LAU1_name) &
                                 str_sub(scotland_data$MSOA_code, end=1)=="S", ]
  scotland_data <- cbind(scotland_data, LSOA_code=NA)

  ## Combine the two data sets
  data <- rbind(data, scotland_data)
  
  ## Some manual checking found a couple discrepancies in the names
  ## Tweak these to match
  ## unique(subset(tmp, is.na(LAU1_code) & !(country%in%c("Northern Ireland")))$LAU1_name)
  data <- mutate(data, LAU1_name=str_replace(data$LAU1_name, "Argyll \\& Bute UA Island", "Argyll and Bute"))
  data <- mutate(data, LAU1_name=str_replace(data$LAU1_name, "Edinburgh, City of", "City of Edinburgh"))
  data <- mutate(data, LAU1_name=str_replace(data$LAU1_name, "Dumfries \\& Galloway", "Dumfries and Galloway"))
  data <- mutate(data, LAU1_name=str_replace(data$LAU1_name, "Eilean Siar \\(Western Isles\\)", "Eilean Siar"))
  data <- mutate(data, LAU1_name=str_replace(data$LAU1_name, "Perth \\& Kinross", "Perth and Kinross"))
  data <- mutate(data, LAU1_name=str_replace(data$LAU1_name, "Cheshire West \\& Chester", "Cheshire West and Chester"))
  data <- mutate(data, LAU1_name=str_replace(data$LAU1_name, "Bristol, City of", "City of Bristol"))
  data <- mutate(data, LAU1_name=str_replace(data$LAU1_name, "Kingston upon Hull, City of", "City of Kingston upon Hull"))
  data <- mutate(data, LAU1_name=str_replace(data$LAU1_name, "Rhondda, Cynon, Taff", "Rhondda Cynon Taf"))
  data <- mutate(data, LAU1_name=str_replace(data$LAU1_name, "Vale of Glamorgan, The", "The Vale of Glamorgan"))

  ## Now load in the urban classification.  All the detailed processing for this is in the benchmarking paper
  ## This is lazy loaded as part of the package
  meta <- mutate(LAD_metadata, urban=(LAD_metadata$urban_class %in% urban_classes))
  meta <- meta[,c("name", "short_code", "long_code", "urban", "country")]

   ## Need to keep Scotland and Northern Ireland too
  tmp <- merge(data, meta, by.x="LAU1_name", by.y="name", all.y=TRUE) 
  return(tmp)
}
