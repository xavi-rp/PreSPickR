###########################################################################
########                                                       ############
########               Preparing data to be saved              ############
########                                                       ############
########                                                       ############
###########################################################################

#' Preparing the species ocurrences data set from GBIF to be saved as a simpler csv
#'
#'
#' @author Xavier Rotllan-Puig
#' @title GetBIF
#' @description Retrieving the data set(s) downloaded with GetBIG() and creating a simpler data set (csv) with only those fields decided by the user. This function is used by GetBIF()
#' @import rgbif dplyr
#' @param rm_dupl If TRUE (default), duplicate occurrences (same sp, same coordinates) are removed from the final data set
#' @param cols2keep Column names to keep in the final data set. Default, cols2keep = c("species", "decimalLatitude", "decimalLongitude"),
#' @return A data frame
#' @name PrepBIF()
#' @export
#' @examples
#' \donttest{
#' Prep_BIF(taxon_dir = NULL,
#'          taxons = NULL,
#'          cols2keep = NULL,
#'          rm_dupl = NULL
#'          )
#'}
#'
#

#----------------------

Prep_BIF <- function(taxon_dir = NULL,
                     taxons = NULL,
                     cols2keep = NULL,
                     rm_dupl = NULL
                     ){

  data1 <- data.frame()

  for (sps in taxons){
    cat(paste0("Reading data for ", sps), "\n")
    load(paste0(taxon_dir, "/download_info_", sps, ".RData"), verbose = FALSE)

    # Reading in data
    data02 <- occ_download_import(dta)
    data02 <- data02[, names(data02) %in% cols2keep]

    data1 <- rbind(data1, data02)
  }

  data1 <- as.data.frame(data1)  #data set with coordinates and name of species
  if(rm_dupl == TRUE)  data1 <- data1[!duplicated(data1), ]

  data1$sp2 <- tolower(paste(substr(data1$species, 1, 3),
                             substr(sub("^\\S+\\s+", '', data1$species), 1, 3),
                             sep = "_"))

  return(data1)

}
