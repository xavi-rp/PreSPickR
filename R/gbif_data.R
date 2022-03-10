
###########################################################################
########                                                       ############
########               Downloading data from GBIF              ############
########                                                       ############
########                                                       ############
###########################################################################


#' Downloading species ocurrence data (presences) from GBIF
#'
#' The aim of this script is to define the function GetBIF(), which is used to download species occurrences from GBIF (Global Biodiversity Information Facility ), and generate a csv data set with the coordinates ready to use. It is based on several functions included in the package "rgbif" (Chamberlain, 2017). GetBIF() retrieve your GBIF credentials (user and password) and automatically checks in a loop until the request of data made to GBIF is ready and starts the download. Finally, it saves the data in a csv file.
#'
#' @author Xavier Rotllan-Puig
#' @title PrepBIF
#' @description Download species occurrences from GBIF (Global Biodiversity Information Facility ), and generates a csv data set with the coordinates ready to be used. It is based on several functions included in the package "rgbif" (Chamberlain, 2017). GetBIF() retrieve your GBIF credentials (user and password) and automatically checks in a loop until the request of data made to GBIF is ready and starts the download. Finally, it extracts and saves the coordinates of the occurrences in a csv file.
#' @import rgbif dplyr
#' @param gbif_usr User name in GBIF
#' @param gbif_pwrd Password in GBIF
#' @param email Email in GBIF
#' @param credentials .RData file containing a list with gbif_usr, gbif_pwrd and email
#' @param taxon_dir Directory where to find the taxons list to be downloaded (only if not the working directory)
#' @param taxon_list List of taxons to be downloaded (either a csv file or a vector with the names)
#' @param download_format Default "SIMPLE_CSV"
#' @param download_years Period to be downloaded. If c(NA, NA), the default, all years available
#' @param download_coords Location (xmin, xmax, ymin, ymax). If c(NA, NA, NA, NA), the default, global download
#' @param download_coords_accuracy Range of allowed uncertainty in the coordinates. If c(NA, NA), the default, all uncertainties allowed, even if no uncertainty is reported in GBIF. If, e.g., c(0, 50), will download those occs with not reported uncertainty plus those with uncertainty < 50m
#' @param rm_dupl If TRUE (default), duplicate occurrences (same sp, same coordinates) are removed from the final data set (csv file)
#' @param cols2keep Column names to keep in the final data set. Default, cols2keep = c("species", "decimalLatitude", "decimalLongitude"),
#' @param out_name Name to the output data set (csv file)
#' @param ... Other parameters to be passed mostly to 'occ_download()'. Not used
#' @return A csv file with the occurrences in Lat/Long Geographic Coordinates System WGS84.
#' @name GetBIF()
#' @export
#' @examples
#' \donttest{
#' GetBIF(credentials = "~/gbif_credentials.RData",
#'        taxon_list = "list_taxons.csv",
#'        out_name = "sp_records")
#'}
#'
#'
#'
#
# Created on: Winter 2018 (updated Summer 2021)
#
# Created by: Xavier Rotllan-Puig (xavi.rotllan.puig@gmail.com)
#
# Inputs:
#       - The location of a csv file with the names of the taxons to be downloaded
#         ATTENTION: Make sure that the spelling is exactly the same used by GBIF (e.g. "FAGUS SYLVATICA L.")
#       - For security reasons, your GBIF credentials (user, password and email)
#         can be loaded from a RData file (location needs to be given). Otherwise,
#         they can be passed as arguments.
#
#
# Outputs:
#       - A csv file with 3 columns by default (species name, decimalLatitude, decimalLongitude),
#         Plus a 4th column with species reference name in the form of 3 first letters of genus, underscore,
#         3 first lettes of species.
#
# References:
#       - Scott Chamberlain (2017). rgbif: Interface to the Global 'Biodiversity'
#         Information Facility 'API'. R package version 0.9.8.
#         https://CRAN.R-project.org/package=rgbif


# ------------------------------------------


GetBIF <- function(gbif_usr = NULL, gbif_pwrd = NULL, email = NULL,
                   credentials = NULL,
                   taxon_dir = NULL, taxon_list = NULL,
                   download_format = "SIMPLE_CSV",
                   #download_years = c(2000, 2021),
                   download_years = c(NA, NA),
                   download_coords = c(NA, NA, NA, NA), #order: xmin, xmax, ymin, ymax
                   #download_coords_accuracy = c(0, 50),
                   download_coords_accuracy = c(NA, NA),
                   rm_dupl = TRUE,
                   cols2keep = c("species", "decimalLatitude", "decimalLongitude"),
                   out_name = "sp_records",
                   ...
                   ){

  #### Settings ####

  # Calling GBIF credentials
  if (!is.null(credentials)) load(credentials, verbose = FALSE)

  # List of taxons
  if (any(grepl(".csv", taxon_list))) {
    if (is.null(taxon_dir)) taxon_dir <- getwd()
    taxons <- read.csv(paste0(taxon_dir, "/", taxon_list), header = FALSE)
    taxons <- as.vector(taxons$V1)  # taxons to be downdloaded
  } else if (is.vector(taxon_list)) {
    taxons <- taxon_list
  } else {
    stop("Not supported format (must be .csv file or vector)")
  }

  # Dots
  #dts <- list(...)
  #if(is.null(dts$hasCoordinate)) dts$hasCoordinate <- TRUE
  #if(is.null(dts$type)) dts$type <- "and"
  #if(is.null(dts$POLYGON)) dts$POLYGON <- "POLYGON((-179.99999 -60.00000,180.00000 -60.00000,180.00000 73.00000,-179.99999 73.00000,-179.99999 -60.00000))"
  ## for Europe (more or less)          "POLYGON((-12.69141 33.4901,42.71485 33.4901,42.71485 71.9218,-12.69141 71.9218,-12.69141 33.4901))"
  #if(is.null(dts$year)) dts$year <- c(1996, as.integer(format(Sys.Date(), "%Y")))
  #if(is.null(dts$elevation)) dts$elevation <- c(0, 3000)
  #if(is.null(dts$coordinateUncertaintyInMeters)) dts$coordinateUncertaintyInMeters <- c(0, 50)

  if (sum(is.na(download_coords)) == 4){
    coords <- "POLYGON((-179.99999 -60.00000,180.00000 -60.00000,180.00000 73.00000,-179.99999 73.00000,-179.99999 -60.00000))"
  }else if (sum(is.na(download_coords)) > 0 & sum(is.na(download_coords)) < 4){
    stop("please provide correct coordinates")
  }else if (all(download_coords >= -180 & download_coords <= 180)){
    coords <- paste0("POLYGON((",
                     download_coords[1], " ", download_coords[3], ",",
                     download_coords[2], " ", download_coords[3], ",",
                     download_coords[2], " ", download_coords[4], ",",
                     download_coords[1], " ", download_coords[4], ",",
                     download_coords[1], " ", download_coords[3],
                     "))")
  }else{
    stop("please provide correct coordinates")
  }


  if (sum(is.na(download_years)) == 2){
    download_years <- c(0, as.vector(format(Sys.Date(), "%Y")))
  }else if (sum(is.na(download_years)) > 0 & sum(is.na(download_years)) < 2){
    stop("please provide correct years")
  }else if (all(download_years >= 0 & download_years <= as.vector(format(Sys.Date(), "%Y")))){
    years <- download_years
  }else{
    stop("please provide correct years")
  }


  if (sum(is.na(download_coords_accuracy)) == 2){  # c(NA, NA) all downloaded
    cord_unc <- pred_not(pred("coordinateUncertaintyInMeters", 0))
  }else if (sum(is.na(download_coords_accuracy)) > 0 & sum(is.na(download_coords_accuracy)) < 2){ # c(NA, 50) error
    stop("please provide correct download_coords_accuracy (i.e. coordinateUncertaintyInMeters)")
  }else if (all(download_coords_accuracy != 0)){ # c(20, 50)
    cord_unc <- pred_and(pred_gte("coordinateUncertaintyInMeters", download_coords_accuracy[1]), pred_lte("coordinateUncertaintyInMeters", download_coords_accuracy[2]))
  }else if (any(download_coords_accuracy != 0)){ # c(0, 50) allows not reported uncert
    cord_unc <- pred_and(pred_not(pred("coordinateUncertaintyInMeters", 0)), pred_lte("coordinateUncertaintyInMeters", download_coords_accuracy[2]))
  }else{
    stop("please provide correct download_coords_accuracy (if c(0, 0), nothing downloaded from GBIF)")
  }



  #### Downloading Data ####
  ## Spin up a download request for SEVERAL taxons data

  for (sps in taxons){
    print(paste0("Downloading data for ", sps))
    rqst_02 <- occ_download(pred("taxonKey", name_backbone(name = sps)$usageKey),
                            format = download_format,
                            pred("hasCoordinate", TRUE),
                            pred_and(pred_gte("year", years[1]), pred_lte("year", years[2])),
                            pred_within(coords),
                            #pred_and(pred_gte("elevation", dts$elevation[1]), pred_lte("elevation", dts$elevation[2])),
                            #pred_and(pred_gte("coordinateUncertaintyInMeters", download_coords_accuracy[1]), pred_lte("coordinateUncertaintyInMeters", download_coords_accuracy[2])),
                            cord_unc,
                            ...,
                            user = gbif_usr, pwd = gbif_pwrd, email = email)    #prepares the spin up
    # Creates metadata
    rqst_02_meta <- data.frame(status = "INITIAL")
    round <- 1
    while (rqst_02_meta$status != "SUCCEEDED") {
      cat("\r", paste0("round = ", round, " / ",
                       "status = ", rqst_02_meta$status))
      Sys.sleep(60)
      rqst_02_meta <- rqst_02 %>% occ_download_meta
      cat("\r", paste0("round = ", round, " / ",
                       "status = ", rqst_02_meta$status))
      round <- round + 1
    }

    # Start download when meta says "Status: SUCCEEDED"
    dta <- occ_download_get(key = rqst_02_meta$key, path = ".",
                            overwrite = TRUE, curlopts = list(verbose = TRUE))

    # saving citation
    citation_02 <- dta %>% gbif_citation

    # Saving download info
    save(list = c("rqst_02", "rqst_02_meta", "dta", "citation_02"),
         file = paste0("download_info_", sps, ".RData"))

  }


  #### Retrieving Data ####
  data1 <- Prep_BIF(taxon_dir = paste0(taxon_dir, "/"),
                    taxons = taxons,
                    cols2keep = cols2keep,
                    rm_dupl = rm_dupl
                    )


  #### Saving data ####
  print(paste0("Saving GBIF data as ", out_name, ".csv"))
  write.csv(data1, paste0(out_name, ".csv"),
            quote = FALSE, row.names = FALSE)

  return(data1)
}
