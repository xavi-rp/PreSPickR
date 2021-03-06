% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gbif_data.R
\name{GetBIF()}
\alias{GetBIF()}
\alias{GetBIF}
\title{Downloading species ocurrence data (presences) from GBIF}
\usage{
GetBIF(
  gbif_usr = NULL,
  gbif_pwrd = NULL,
  email = NULL,
  credentials = NULL,
  sp_dir = NULL,
  sp_list = NULL,
  out_name = "sp_records"
)
}
\arguments{
\item{gbif_usr}{User name in GBIF}

\item{gbif_pwrd}{Password in GBIF}

\item{email}{email in GBIF}

\item{credentials}{.RData file containing a list with gbif_usr, gbif_pwrd and email}

\item{out_name}{Name to the output data set (csv file)}
}
\value{
An csv file with the occurrences in Lat/Long Geographic Coordinates System WGS84.
}
\description{
Download species occurrences from GBIF (Global Biodiversity Information Facility ), and saves them as a csv data set. It is based on several functions included in the package "rgbif" (Chamberlain, 2017). GetBIF() retrieve your GBIF credentials (user and password) and automatically checks in a loop until the request of data made to GBIF is ready and starts the download. Finally, it saves the data in a csv file.
}
\details{
The aim of this script is to define the function GetBIF(), which is used to download species occurrences from GBIF (Global Biodiversity Information Facility ), and saves them as a csv data set. It is based on several functions included in the package "rgbif" (Chamberlain, 2017). GetBIF() retrieve your GBIF credentials (user and password) and automatically checks in a loop until the request of data made to GBIF is ready and starts the download. Finally, it saves the data in a csv file.
}
\author{
Xavier Rotllan-Puig
}
