#' decctools
#'
#' Provides easy access to United Kingdom energy statistics for R.  The raw data can be found on the Department of Energy and Climate Change website, \url{http://www.decc.gov.uk}
#'
#' @seealso \code{\link{get_LAD_data}}, \code{\link{get_MSOA_data}}, \code{\link{get_LSOA_data}}, \code{\link{get_lookup}}
#' @name decctools
#' @docType package
NULL

#' Urban/rural classifications of local administrative units
#'
#' A data frame providing the urban/rural classification of local administrative units in the United Kingdom.
#' These classifications are described at \url{http://www.ons.gov.uk/ons/guide-method/geography/products/area-classifications/rural-urban-definition-and-la/rural-urban-local-authority--la--classification--england-/index.html} and are summarized as follows:
#' \itemize{
#'	\item MU = Major Urban: districts with either 100,000 people or 50 per cent of their population in urban areas with a population of more than 750,000
#' 	\item LU = Large Urban: districts with either 50,000 people or 50 per cent of their population in one of 17 urban areas with a population between 250,000 and 750,000
#'	\item OU = Other Urban: districts with fewer than 37,000 people or less than 26 per cent of their population in rural settlements and larger market towns
#' 	\item SR = Significant Rural: districts with more than 37,000 people or more than 26 per cent of their population in rural settlements and larger market towns
#'	\item R50 = Rural-50: districts with at least 50 per cent but less than 80 per cent of their population in rural settlements and larger market towns
#' 	\item Rural-80: districts with at least 80 per cent of their population in rural settlements and larger market towns
#' }
#'
#' @note Unfortunately these classifications are only defined for England. For all other countries, assumptions have been made about which LAUs are urban (LU) or rural (SR) using the following data sources:
#' \itemize{
#'	\item Wales, \url{http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/administrative/wales/unitary-authorities/index.html}
#'	\item Scotland, \url{http://www.scotland.gov.uk/Publications/2004/06/19498/38788}
#'	\item Northern Ireland, \url{http://www.nisra.gov.uk/geography/default.asp10.htm}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name LAU_classes
#' @format A data frame with 406 rows and 2 columns
NULL
