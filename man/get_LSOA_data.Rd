\name{get_LSOA_data}
\alias{get_LSOA_data}
\title{Get LSOA energy consumption data}
\usage{
get_LSOA_data(id, fuel = c("electricity", "gas"), dir)
}
\arguments{
  \item{id}{a vector of LSOA ids to fetch.  If not
  specified, then all LSOAs are retrieved.}

  \item{fuel}{the fuel type to fetch.  Valid values are
  'electricity', 'gas'}

  \item{dir}{an optional directory in which to store a copy
  of the data}
}
\value{
a long data frame with the requested data.  The 'energy'
column is measured in GWh.
}
\description{
This function fetches LSOA (Lower Super Output Area) data
from the DECC website.
}
\examples{
\dontrun{
lsoa_data <- get_LSOA_data() # Gets all data
}
}
\keyword{data}
\keyword{energy}

