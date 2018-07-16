# PreSPickR

The aim of this package is to download species presences (occurrences) data from public repositories. For now it is implemented for Bioatles, which has data from the Balearic Islands (http://bioatles.caib.es), and for GBIF.

Bioatles data is projected in European Datum 1950 (31N). The function bioatles(), which downloads the occurrences, also transform the data set in Lat/Long Geographic Coordinates System WGS84. It is based on several functions included in the packages "rvest" (Wickham, 2016) and "xml2" (Wickham et al., 2017). Finally, it saves the data in a csv file.

Equally, GetBIF() is used to download species occurrences from GBIF. It is based on several functions included in the package "rgbif" (Chamberlain, 2017).It uses the so-called "/occurrence/download route" (see vignettes of the "rgbif" package) which is more appropriate for big data sets.
GetBIF() retrieve your GBIF credentials (user, password and email) and automatically makes checks using a loop until the request of the data set made to GBIF is ready. In that moment it starts the download. Finally, it saves the data in a csv file. The data is in Lat/Long Geographic Coordinates System WGS84.


