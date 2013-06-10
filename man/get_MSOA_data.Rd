\name{get_MSOA_data}
\alias{get_MSOA_data}
\title{Get MSOA energy consumption data}
\usage{
  get_MSOA_data(id, sector = c("domestic", "nondomestic"),
    fuel = c("electricity", "gas"), dir)
}
\arguments{
  \item{id}{the unique id of the MSOA to fetch.  If not
  specified, then all MSOAs are retrieved.}

  \item{sector}{the economic sector to fetch.  Valid values
  are 'domestic', 'nondomestic'}

  \item{fuel}{the fuel type to fetch.  Valid values are
  'electricity', 'gas'}

  \item{dir}{an optional directory in which to save a copy
  of the data}
}
\value{
  a long data frame with the requested data.  The 'energy'
  column is measured in GWh.
}
\description{
  This function fetches MSOA (Middle Super Output Area)
  data from the DECC website from 2010.  Since the format
  of these files tends to change just enough to be
  irritating for dependent code, there is also an option
  that allows you to cache a local copy.
}
\examples{
\dontrun{
get_MSOA_data() # Gets all data
}
}
\keyword{data}
\keyword{energy}

