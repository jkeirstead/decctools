#' Gets a file from a remote URL
#' 
#' Since the URLs of UK government data appear to change quite frequently, the method
#' will automatically cache a local copy.  
#'
#' @param url the address of the file to download
#' @param dir the directory to store the cached file in.  If undefined, it defaults to $TEMP/decctools.
#' @param update_cache boolean.  Should the cached file be overwritten?  Default = FALSE
#' @return the file name (path)
get_remote_file <- function(url, dir=NA, update_cache=FALSE) {

  ## Currently only processes one URL
  if (length(url)>1) {
    warning("Only one URL at a time currently supported.  Using first URL only.")
    url <- url[1]
  } else if (is.na(url)) {
    stop("A valid URL must be specified.")
  }
  
  ## Define the cache directory
  if (is.na(dir)) dir=file.path(tempdir(), get_package_name())
  ## Make sure it exists before trying to download
  if (!file.exists(dir)) dir.create(dir)
  
  ## Split the URL to identify the filename
  tmp <- unlist(str_split(url, "/"))
  file_name <- file.path(dir, tmp[length(tmp)])

  ## If the file doesn't exist, then download it
  if (!file.exists(file_name) | update_cache) download.file(url, file_name, mode="wb", method="curl")
  
  return(file_name)
}

#' Gets the name of this package
#'
#' @return a character string
get_package_name <- function() {
  return ("decctools")
}  

#' Gets a lookup table of local authorities to MSOA and LSOA
#'
#' Provides a lookup table giving the corresponding local authority name and ID
#' for MSOA and LSOA geographies.  Also returns a flag indicating whether or
#' not a given LAU is urban.  See \link{LAU_classes} for more information.
#'
#' @param urban_classes classifications to be considered as urban.
#' @param dir (optional) directory to save the lookup table information
#' @return a data frame
#' @export
get_lookup <- function(urban_classes=c("LU", "MU"), dir=NA) {
  ## I'll take this from the domestic electricity page
  url <- "https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/49432/4813-llsoa-domestic-elec-est-2010.xls"

  file_name <- get_remote_file(url, dir=dir)

  ## Load it into memory.  Only need first 4 columns
  wb <- loadWorkbook(file_name)
  data <- readWorksheet(wb, "LLSOA Electricity Domestic", startCol=1, endCol=4)
  names(data) <- c("LAU1_name", "LAU1_code", "MSOA_code", "LSOA_code")

  ## Get rid of empty lines
  data <- subset(data, !is.na(LAU1_name))

  ## Now load in the urban classification.  All the detailed processing for this is in the benchmarking paper
  ## This is lazy loaded as part of the package
  tmp <- mutate(LAU_classes, urban=(Classification %in% urban_classes))
  data <- merge(data, tmp)
  return(data)
}
