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
\seealso{
  \code{\link{get_grid_mix}} and
  \code{\link{carbon_intensities}} for data sources
}

