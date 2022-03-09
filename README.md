# PreSPickR

The aim of this package is to download species presences (occurrences) data from public repositories. For now it is implemented for [Bioatles](http://bioatles.caib.es), which has data from the Balearic Islands, and for [GBIF](https://www.gbif.org/).

Bioatles data is projected in European Datum 1950 (31N). The function bioatles(), which downloads the occurrences, also transform the data set in Lat/Long Geographic Coordinates System WGS84. It is based on several functions included in the packages "rvest" (Wickham, 2016) and "xml2" (Wickham et al., 2017). Finally, it saves the data in a csv file.

Equally, GetBIF() is used to download species occurrences from GBIF. It is based on several functions included in the package "rgbif" (Chamberlain, 2017). It uses the so-called "/occurrence/download route" (see vignettes of the "rgbif" package) which is more appropriate for big data sets.
GetBIF() retrieve your GBIF credentials (user, password and email) and automatically makes checks using a loop until the request of the data set made to GBIF is ready. In that moment it starts the download. Finally, it saves the data in a csv file. The data is in Lat/Long Geographic Coordinates System WGS84.

See vignette for some examples.

Download the source package  [here](https://www.researchgate.net/publication/326440673_PreSPickR_Downloading_Species_Presences_Occurrences_From_Public_Repositories). Or install the latest version:

```
library(devtools)
install_github("xavi-rp/PreSPickR")
```


In case you want to install v.2 (under development):

```
library(devtools)
install_github("xavi-rp/PreSPickR",
               ref = "v2",
               INSTALL_opts = c("--no-multiarch")  # https://github.com/rstudio/renv/issues/162
               )

```


## References

  - Scott Chamberlain (2017). rgbif: Interface to the Global 'Biodiversity' Information Facility 'API'. R package version 0.9.8. https://CRAN.R-project.org/package=rgbif
  - Hadley Wickham (2016). rvest: Easily Harvest (Scrape) Web Pages. R package version 0.3.2. https://CRAN.R-project.org/package=rvest
  - Hadley Wickham, James Hester and Jeroen Ooms (2017). xml2: Parse XML. R package version 1.1.1. https://CRAN.R-project.org/package=xml2

