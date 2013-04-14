\name{get_lookup}
\alias{get_lookup}
\title{Gets a lookup table of local authorities to MSOA and LSOA}
\usage{
  get_lookup(urban_classes = c("LU", "MU"), dir = NA)
}
\arguments{
  \item{urban_classes}{classifications to be considered as
  urban.}

  \item{dir}{(optional) directory to save the lookup table
  information}
}
\value{
  a data frame
}
\description{
  Provides a lookup table giving the corresponding local
  authority name and ID for MSOA and LSOA geographies.
  Also returns a flag indicating whether or not a given LAU
  is urban.  See \link{LAU_classes} for more information.
}

