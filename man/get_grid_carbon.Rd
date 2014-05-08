\name{get_grid_carbon}
\alias{get_grid_carbon}
\title{Calculates the carbon intensity of electricity generation}
\usage{
get_grid_carbon(start, end)
}
\arguments{
  \item{start}{the start date for retrieving data.}

  \item{end}{the end date for retrieving data}
}
\value{
a data frame with a datetime stamp and average carbon
intensity of electricity measured in kg CO2/kWh
}
\description{
Calculates the carbon intensity of electricity generation
in the UK between specified dates.
}
\examples{
# These require a working internet connection
start <- "2010-01-01"
end <- "2010-01-03"
library(RCurl)
if (url.exists("http://www.google.com")) {
  carbon <- get_grid_carbon(start, end) # Gets grid carbon for 1--3 January 2010
}
}
\seealso{
\code{\link{get_grid_mix}} and
\code{\link{carbon_intensities}} for data sources
}

