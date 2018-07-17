###########################################################################
########                                                       ############
########             Downloading data from Bioatles            ############
########                                                       ############
###########################################################################

#' Downloading species ocurrence data (presences) from Bioatles
#'
#' The aim of this script is to define the function bioatles(), which is used to download species (presences) data from the Bioatles http://bioatles.caib.es. As data in Bioatles is projected in European Datum 1950 (31N), the function also trensform it in Lat/Long Geographic Coordinates System WGS84. bioatles() is based on several functions included in the packages "rvest" (Wickham, 2016) and "xml2" (Wickham et al., 2017). Finally, it saves the data in a csv file.
#'
#' @author Xavier Rotllan-Puig
#' @description Download species (presences) data from the Bioatles http://bioatles.caib.es. As data in Bioatles is projected in European Datum 1950 (31N), the function also trensform it in Lat/Long Geographic Coordinates System WGS84. bioatles() is based on several functions included in the packages "rvest" (Wickham, 2016) and "xml2" (Wickham et al., 2017). Finally, it saves the data in a csv file.
#' @param sp_dir Directory of the species list
#' @param sp_list A csv file name or a vector containing species to be downloaded
#' @param out_name Name to the output data set (csv file)
#' @return The sum of \code{sp_dir} and \code{sp_list} and \code{out_name}
#' @name bioatles()
#'
#'
#'
#
# Created on: Winter-Spring 2018 (under construction)
#
# Created by:  (xavi.rotllan.puig@gmail.com)
#
# Inputs:
#       - The location of a csv file with the names of the species to be downloaded
#         ATTENTION: Make sure that the spelling is exactly the same used by GBIF (e.g. "Chamaerops humilis")
#
# Outputs:
#       - A csv file with 3 columns (species, decimalLatitude, decimalLongitude)
#
# References:
#       - Hadley Wickham (2016). rvest: Easily Harvest (Scrape) Web Pages. R package
#         version 0.3.2. https://CRAN.R-project.org/package=rvest
#       - Hadley Wickham, James Hester and Jeroen Ooms (2017). xml2: Parse XML. R
#         package version 1.1.1. https://CRAN.R-project.org/package=xml2
#
#
# ------------------------------------------


bioatles <- function(sp_dir = NULL, sp_list = NULL, out_name = "sp_records"){
  #### Settings ####
  # Working directory
  wd <- getwd()


  # Bioatles webpage
  page <- "http://bioatles.caib.es/serproesfront/cuadriculas.do?seccion=distribEspecies" %>%
    read_html()  # web site of Bioatles


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


  #### Downloading Species Name and BIOATLES code ####
  noms <- as.data.frame(page %>% html_nodes('option') %>% html_text())
  chks <- which(grepl("^Selecciona", noms$`page %>% html_nodes("option") %>% html_text()`))
  noms1 <- as.data.frame(noms[(chks[length(chks) - 1] + 1) : (chks[length(chks)] - 1), ])

  nds1 <- page %>% html_nodes('select#selectEspecie')
  x <- list(.name = xml_name(nds1[[1]]))
  attrs <- xml_attrs(nds1[[1]])
  attrs <- attrs[!grepl("xmlns", names(attrs))]
  x <- c(x, attrs)
  children <- xml_children(nds1[[1]])
  code <- as.data.frame(bind_rows(
    lapply(xml_attrs(children),
           function(x) data.frame(as.list(x), stringsAsFactors = FALSE)))$value)

  code_name <- cbind(as.data.frame(code[-1, ]), noms1)
  names(code_name) <- c("code", "name")

  #### Downloading data of species presences ####
  data1 <- data.frame()

  for (sps in species) {
    print(paste0("Downloading data: ", sps))
    spec2 <- tolower(paste(substr(sps, 1, 3), substr(sub(".* ", "", sps), 1, 3), sep = "_"))
    spec <- code_name[code_name$name %in% sps, ]
    if (nrow(spec) == 0) stop("No data for this species in Bioatles, please check name/spelling")
    spec_code <- as.vector(spec$code)

    page2 <- paste0("http://bioatles.caib.es/serproesfront/registros.do?accion=listarRegistros&codiEspecie=", spec_code, "&codiFamilia=0&codiGrupo=0") %>% read_html()
    tbl_sp <- page2 %>% html_table(fill = TRUE, header = T)
    pres2export <- tbl_sp[[2]]
    pres2export <- pres2export[ - c(1, ncol(pres2export))]

    data02 <- pres2export[, c(7, 1, 2)]
    data02 <- data02[!duplicated(data02), ]
    data02[, 2:3] <- data02[, 2:3] * 1000
    names(data02) <- c("species", "decimalLongitude", "decimalLatitude")

    #### Transform presences' projection ####
    coordinates(data02) <- c("decimalLongitude", "decimalLatitude")  # setting spatial coordinates
    proj4string(data02) <- CRS("+init=EPSG:23031")  # define projection: European Datum 1950 (31N)
    CRS.new <- CRS("+init=EPSG:4326") # Lat/Long Geographic Coordinates System WGS84
    data02_WGS84 <- spTransform(data02, CRS.new)  #projecting

    data02 <- as.data.frame(data02_WGS84@coords)
    data02$species <- sps
    data02$sp2 <- spec2

    data1 <- rbind(data1, data02)
  }

  #### Saving data ####
  print(paste0("Saving Bioatles data as ", wd, "/", out_name, ".csv"))
  write.csv(data1[, 1:3], paste0(wd, "/", out_name, ".csv"), quote = FALSE, row.names = FALSE)

}
