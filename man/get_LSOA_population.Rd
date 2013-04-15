\name{get_LSOA_population}
\alias{get_LSOA_population}
\title{Gets the 2011 population estimates for all LSOAs}
\usage{
  get_LSOA_population(dir)
}
\arguments{
  \item{dir}{an (optional) directory in which to save the
  downloaded data}
}
\value{
  a data frame with the LSOA_code, population, area (in
  hectares), and number of households
}
\description{
  This function gets the socio-demographic data associated
  with each LSOA.  The original data can be found at
  \url{https://www.gov.uk/government/statistical-data-sets/socio-economic-data-for-mlsoa-igz-and-llsoa-electricity-and-gas-estimates}.
  As a reminder, this data only covers England and Wales.
}

