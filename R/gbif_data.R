
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
#' @description Download species occurrences from GBIF (Global Biodiversity Information Facility ), and generates a csv data set with the coordinates ready to be used. It is based on several functions included in the package "rgbif" (Chamberlain, 2017). GetBIF() retrieve your GBIF credentials (user and password) and automatically checks in a loop until the request of data made to GBIF is ready and starts the download. Finally, it extracts and saves the coordinates of the occurrences in a csv file.
#' @param gbif_usr User name in GBIF
#' @param gbif_pwrd Password in GBIF
#' @param email email in GBIF
#' @param credentials .RData file containing a list with gbif_usr, gbif_pwrd and email
#' @param sp_dir directory where to find the species list to be downloaded (only if not the working directory)
#' @param sp_list list of species to be downloaded (either a csv file or a vector with the names)
#' @param out_name Name to the output data set (csv file)
#' @param ... Other parameters to be passed mostly to 'occ_download()'. Notice that not all parameters are supported in this version
#' @return A csv file with the occurrences in Lat/Long Geographic Coordinates System WGS84.
#' @name GetBIF()
#' @export
#' @examples
#' \donttest{
#' GetBIF(credentials = "~/gbif_credentials.RData",
#'        sp_list = "list_taxons.csv",
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
#       - The location of a csv file with the names of the species to be downloaded
#         ATTENTION: Make sure that the spelling is exactly the same used by GBIF (e.g. "FAGUS SYLVATICA L.")
#       - For security reasons, your GBIF credentials (user, password and email)
#         can be loaded from a RData file (location needs to be given). Otherwise,
#         they can be passed as arguments
#
#
# Outputs:
#       - A csv file with 3 columns (species, decimalLatitude, decimalLongitude)
#
# References:
#       - Scott Chamberlain (2017). rgbif: Interface to the Global 'Biodiversity'
#         Information Facility 'API'. R package version 0.9.8.
#         https://CRAN.R-project.org/package=rgbif


# ------------------------------------------


GetBIF <- function(gbif_usr = NULL, gbif_pwrd = NULL, email = NULL,
                   credentials = NULL,
                   sp_dir = NULL, sp_list = NULL,
                   out_name = "sp_records",
                   ...
                   ){

  #### Settings ####
  # Working directory
  wd <- getwd()

  # Calling GBIF credentials
  if (!is.null(credentials)) load(credentials, verbose = FALSE)

  # List of species
  if (any(grepl(".csv", sp_list))) {
    if (is.null(sp_dir)) sp_dir <- wd
    species <- read.csv(paste0(sp_dir, "/", sp_list), header = FALSE)
    species <- as.vector(species$V1)  # species to be downdloaded
  } else if (is.vector(sp_list)) {
    species <- sp_list
  } else {
    stop("Not supported format (must be .csv file or vector)")
  }

  # Dots
  dts <- list(...)
  if(is.null(dts$hasCoordinate)) dts$hasCoordinate <- TRUE
  if(is.null(dts$type)) dts$type <- "and"
  if(is.null(dts$POLYGON)) dts$POLYGON <- "POLYGON((-179.99999 -60.00000,180.00000 -60.00000,180.00000 73.00000,-179.99999 73.00000,-179.99999 -60.00000))"
  # for Europe (more or less)          "POLYGON((-12.69141 33.4901,42.71485 33.4901,42.71485 71.9218,-12.69141 71.9218,-12.69141 33.4901))"
  if(is.null(dts$year)) dts$year <- c(1996, as.integer(format(Sys.Date(), "%Y")))
  if(is.null(dts$elevation)) dts$elevation <- c(0, 3000)
  if(is.null(dts$coordinateUncertaintyInMeters)) dts$coordinateUncertaintyInMeters <- c(0, 50)

  
  #### Downloading Data ####
  ## Spin up a download request for SEVERAL species data

  for (sps in species){
    print(paste0("Downloading data for ", sps))
    rqst_02 <- occ_download(pred("taxonKey", name_backbone(name = sps)$usageKey),
                            pred("hasCoordinate", dts$hasCoordinate),
                            type = dts$type,
                            pred_and(pred_gte("year", dts$year[1]), pred_lte("year", dts$year[2])),
                            pred_within(dts$POLYGON),
                            pred_and(pred_gte("elevation", dts$elevation[1]), pred_lte("elevation", dts$elevation[2])),
                            pred_and(pred_gte("coordinateUncertaintyInMeters", dts$coordinateUncertaintyInMeters[1]), pred_lte("coordinateUncertaintyInMeters", dts$coordinateUncertaintyInMeters[2])),
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
  data1 <- data.frame()

  for (sps in species){
    cat(paste0("Reading data for ", sps), "\n")
    load(paste0("download_info_", sps, ".RData"), verbose = FALSE)

    # Reading in data
    data02 <- occ_download_import(dta)
    data02 <- data02[!duplicated(data02[, c(133:134)]), ]
    data02 <- data02[, names(data02) %in%
                       c("species", "decimalLatitude", "decimalLongitude")]

    data1 <- rbind(data1, data02)
  }

  data1 <- as.data.frame(data1)  #data set with coordinates and name of species
  data1$sp2 <- tolower(paste(substr(data1$species, 1, 3),
                             substr(sub("^\\S+\\s+", '', data1$species), 1, 3),
                             sep = "_"))

  #### Saving data ####
  print(paste0("Saving GBIF data as ", wd, "/", out_name, ".csv"))
  write.csv(data1, paste0(wd, "/", out_name, ".csv"),
            quote = FALSE, row.names = FALSE)
}
