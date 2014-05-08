\name{get_lookup_table}
\alias{get_lookup_table}
\title{Gets a lookup table to match IDs across geographies}
\usage{
get_lookup_table(urban_classes = c("LU", "MU"), dir)
}
\arguments{
  \item{urban_classes}{classifications to be considered as
  urban.  Possible values include 'OU' (other urban), 'LU'
  (large urban), 'MU' (major urban), 'SR' (significant
  rural), 'R80' (80\% rural), 'R50' (50\% rural)}

  \item{dir}{(optional) directory to save the lookup table
  information}
}
\value{
a data frame
}
\description{
Provides a lookup table giving the local authority name and
corresponding IDs for LAU, LAD, MSOA, and LSOA geographies.
Also returns a flag indicating whether or not a given LAU
is urban.  See \code{\link{LAD_metadata}} for more
information.
}

