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
    valid <- c("LSOA", "MSOA")
    level <- intersect(unique(level), valid)
    if (length(level)>1) {
        warning("Only one level at a time please; using LSOA.")
        level <- "LSOA"
    } else if (length(level)==0) {
        warning("No valid levels specified, defaulting to LSOA.")
        level <- "LSOA"
    }

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
    
