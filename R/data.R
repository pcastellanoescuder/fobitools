
#' Food-Biomarker Ontology Chemical Class Sets - May 25, 2020
#'
#' This file contans FOBI's chemical class sets  
#'
#' @format List created with `sigora::makeGPS` function.
#' @source \url{https://github.com/pcastellanoescuder/FoodBiomarkerOntology}
"GPSrepo_chemicals"

#' Food-Biomarker Ontology Food Class Sets - May 25, 2020
#'
#' This file contans FOBI's food class sets  
#'
#' @format List created with `sigora::makeGPS` function.
#' @source \url{https://github.com/pcastellanoescuder/FoodBiomarkerOntology}
"GPSrepo_foods"

#' ID Map File - May 25, 2020
#'
#' This file contains all FOBI metabolites with 8 different possible IDs for each one (if possible).
#'
#' @format A data frame object: 590 metabolites with 8 possible IDs.
#' \describe{
#'   \item{metaboliteNames}{Raw metabolite names.}
#'   \item{FOBI}{FOBI ID.}
#'   \item{HMDB}{HMDB ID.}
#'   \item{KEGG}{KEGG ID.}
#'   \item{PubChemCID}{PubChemCID ID.}
#'   \item{InChIKey}{InChIKey ID.}
#'   \item{InChICode}{InChICode ID.}
#'   \item{ChemSpider}{ChemSpider ID.}
#' }
#' @source \url{https://github.com/pcastellanoescuder/FoodBiomarkerOntology}
"idmap"

