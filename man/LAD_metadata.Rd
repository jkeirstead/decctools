\docType{data}
\name{LAD_metadata}
\alias{LAD_metadata}
\title{Descriptive data on local administrative districts}
\format{A data frame with 406 rows and 11 columns}
\source{
  Various; see URLs in detail above.  Lookup codes from
  \url{http://www.ons.gov.uk/ons/guide-method/geography/products/names--codes-and-look-ups/names-and-codes-listings/names-and-codes-for-eurostat-geography/index.html}
  (Eurostat LAU) and
  \url{http://www.ons.gov.uk/ons/guide-method/geography/products/names--codes-and-look-ups/names-and-codes-listings/names-and-codes-for-administrative-geography/index.html}
  for UK LADs.  Population data from
  \url{http://www.ons.gov.uk/ons/publications/re-reference-tables.html?edition=tcm\%3A77-231847}.
}
\description{
  A data frame providing extra information about local
  administrative districts (LADs) in the United Kingdom.
  This includes the following fields: \itemize{ \item name:
  the name of the LAD \item country: the country in which
  the LAD is located \item LAU1_code: the Eurostat LAU code
  associated with each LAD \item short_code: the UK
  geography local administrative district short code
  (LAD11CDO) \item long_code: the UK geography local
  administrative district long code (LAD11CD) \item
  urban_class: the urban/rural classification \item
  group_name: a descriptive label describing the character
  of each LAD \item population: mid-year 2010 population
  estimates for each LAD \item area: the area of the LAU in
  square kilometres \item hdd: the heating degree days of
  the local climate \item cdd: the cooling degree days of
  the local climate }
}
\details{
  There are multiple subnational geographies available for
  the UK; please see
  \url{http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/index.html}
  for an overview.  This data set tries to make it easier
  to handle the links between two of these geographies:
  local administrative districts (LADs, the UK
  administrative geography for local authorities) and local
  administrative units (LAUs, the pan-European equivalent
  for EuroStat).

  The first five data fields, and the \code{area} field,
  should be self-explanatory and can be used to link other
  data sets, most notably the energy statistics provided by
  \code{\link{get_LAD_data}}.  However the other items
  require a bit of explanation.

  \code{urban_class}: These classifications are described
  at
  \url{http://www.ons.gov.uk/ons/guide-method/geography/products/area-classifications/rural-urban-definition-and-la/rural-urban-local-authority--la--classification--england-/index.html}
  and are summarized as follows: \itemize{ \item MU = Major
  Urban: districts with either 100,000 people or 50 per
  cent of their population in urban areas with a population
  of more than 750,000 \item LU = Large Urban: districts
  with either 50,000 people or 50 per cent of their
  population in one of 17 urban areas with a population
  between 250,000 and 750,000 \item OU = Other Urban:
  districts with fewer than 37,000 people or less than 26
  per cent of their population in rural settlements and
  larger market towns \item SR = Significant Rural:
  districts with more than 37,000 people or more than 26
  per cent of their population in rural settlements and
  larger market towns \item R50 = Rural-50: districts with
  at least 50 per cent but less than 80 per cent of their
  population in rural settlements and larger market towns
  \item Rural-80: districts with at least 80 per cent of
  their population in rural settlements and larger market
  towns } Unfortunately these classifications are only
  defined for England. For all other countries, assumptions
  have been made about which LAUs are urban (LU) or rural
  (SR) using the following data sources: \itemize{ \item
  Wales,
  \url{http://www.ons.gov.uk/ons/guide-method/geography/beginner-s-guide/administrative/wales/unitary-authorities/index.html}
  \item Scotland,
  \url{http://www.scotland.gov.uk/Publications/2004/06/19498/38788}
  \item Northern Ireland,
  \url{http://www.nisra.gov.uk/geography/default.asp10.htm}
  }

  \code{group_name} The Office for National Statistics
  calculated an area classification for each local
  authority; based on 2001 census data; see
  \url{http://www.ons.gov.uk/ons/guide-method/geography/products/area-classifications/ns-area-classifications/index/index.html}.
  This field presents the group name, which is simply a
  descriptive label of the character of each LAD.

  \code{hdd}, \code{cdd}: These fields represent annual
  heating and cooling degree days for each LAD; the raw
  data come from
  \url{http://www.eci.ox.ac.uk/research/energy/degreedays.php}.
  Heating degree days are assumed to be measured against a
  15.5 degrees C base temperature and cooling degree days
  against 13 degrees C.  The Oxford data set provides point
  estimates for 77 locations around the UK; an inverse
  distance weighting interpolation was performed (with p =
  0.2) to give a smooth surface of degree days across the
  UK and this was sampled using the LAD polygons to
  estimate the heating and cooling degree days in each
  location.
}
\note{
  Please be aware that the LAD codes change from time to
  time and so if you join this data with new data sets, you
  should manually check that the merge has been successful.
  Subtle variations in name also exist so beware if merging
  by \code{name}.
}
\keyword{datasets}

