\name{get_LAD_data}
\alias{get_LAD_data}
\title{Get LAD energy consumption data}
\usage{
  get_LAD_data(id, year, sector = "total", fuel = "total",
    dir)
}
\arguments{
  \item{id}{the unique id of the LAD to fetch.  If not
  specified, then all LADs are retrieved.}

  \item{year}{the year to fetch. If not specified, then the
  most recent year is retrieved.  Only single years
  currently supported}

  \item{sector}{a vector of economic sectors to fetch.
  Valid values are 'domestic', 'industrial', 'total'.}

  \item{fuel}{a vector of fuel types to fetch.  Valid
  values are 'coal', 'manufactured', 'petrol', 'gas',
  'electricity', 'bioenergy', 'total'}

  \item{dir}{an optional directory in which to cache the
  data}
}
\value{
  a long data frame with the requested data.  The 'energy'
  column is measured in GWh.
}
\description{
  Gets LAD (Local Administrative District) energy data from
  the DECC website, \url{http://www.decc.gov.uk}. Since the
  format of these files tends to change just enough to be
  irritating for dependent code, there is also an option
  that allows you to cache a local copy.
}
\examples{
\dontrun{
# Gets energy data for total fuels and sectors for most recent year
lad_data <- get_LAD_data()
}
# Gets energy data for electricity and gas use in the domestic sector in the most recent year
# Depending on the status of DECC servers, this can sometimes fail.  In which case, an empty
# data frame is returned

df <- get_LAD_data(sector="domestic", fuel=c("electricity", "gas"))
}
\keyword{data}
\keyword{energy}

