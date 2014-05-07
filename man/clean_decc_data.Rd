\name{clean_decc_data}
\alias{clean_decc_data}
\title{Adds DECC energy consumption data to a specified data frame.  Currently
only selects domestic electricity and gas.}
\usage{
clean_decc_data(df, sector, fuel)
}
\arguments{
  \item{df}{the raw DECC data frame from the spreadsheet}

  \item{sector}{the sector to extract}

  \item{fuel}{the fuels to extract}
}
\value{
a long database with headers by sector and fuel
}
\description{
Adds DECC energy consumption data to a specified data
frame.  Currently only selects domestic electricity and
gas.
}

