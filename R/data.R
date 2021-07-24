#' FOBI Identifier Map File - Jul 18, 2021
#'
#' Identifier mappings for FOBI metabolites 
#'
#' @format A tibble: 590 FOBI metabolites with 8 different IDs.
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

#' FOBI Table File - Jul 23, 2021
#'
#' FOBI table obtained with `parse_fobi()`
#'
#' @format A tibble: 4069 FOBI entities with their associated information.
#' \describe{
#'   \item{id_code}{FOBI entity ID.}
#'   \item{name}{FOBI entity name.}
#'   \item{is_a_code}{FOBI entity direct parent class ID.}
#'   \item{is_a_name}{FOBI entity direct parent class name.}
#'   \item{id_BiomarkerOf}{FOBI entity associated food ID (in terms of biomarker).}
#'   \item{BiomarkerOf}{FOBI entity associated food name (in terms of biomarker).}
#'   \item{ChemSpider}{ChemSpider ID.}
#'   \item{KEGG}{KEGG ID.}
#'   \item{PubChemCID}{PubChemCID ID.}
#'   \item{InChIKey}{InChIKey ID.}
#'   \item{InChICode}{InChICode ID.}
#'   \item{FOBI}{FOBI ID.}
#'   \item{alias}{FOBI entity alias name.}
#'   \item{HMDB}{HMDB ID.}
#'   \item{id_Contains}{FOBI entity associated food ID (in terms of a recipe).}
#'   \item{Contains}{FOBI entity associated food ID (in terms of a recipe).}
#' }
#' @source \url{https://github.com/pcastellanoescuder/FoodBiomarkerOntology}
"fobi"

#' FOBI Foods Table File - Feb 02, 2021
#'
#' FOBI foods table obtained with `parse_fobi(terms = "FOBI:0001", get = "des")`
#'
#' @format A tibble: 568 FOBI food entities with their associated information.
#' \describe{
#'   \item{id_code}{FOBI entity ID.}
#'   \item{name}{FOBI entity name.}
#'   \item{is_a_code}{FOBI entity direct parent class ID.}
#'   \item{is_a_name}{FOBI entity direct parent class name.}
#'   \item{id_BiomarkerOf}{FOBI entity associated food ID (in terms of biomarker).}
#'   \item{BiomarkerOf}{FOBI entity associated food name (in terms of biomarker).}
#'   \item{ChemSpider}{ChemSpider ID.}
#'   \item{KEGG}{KEGG ID.}
#'   \item{PubChemCID}{PubChemCID ID.}
#'   \item{InChIKey}{InChIKey ID.}
#'   \item{InChICode}{InChICode ID.}
#'   \item{FOBI}{FOBI ID.}
#'   \item{alias}{FOBI entity alias name.}
#'   \item{HMDB}{HMDB ID.}
#'   \item{id_Contains}{FOBI entity associated food ID (in terms of a recipe).}
#'   \item{Contains}{FOBI entity associated food ID (in terms of a recipe).}
#' }
#' @source \url{https://github.com/pcastellanoescuder/FoodBiomarkerOntology}
"foods"

#' FOBI's `ontology_index` object - Jul 23, 2021
#'
#' FOBI terms obtained with `ontologyIndex::get_ontology()`
#'
#' @format An ontology_index object with 1201 terms.
#' \describe{
#'   \item{format-version:}{1.2}
#'   \item{data-version:}{fobi.owl}
#'   \item{ontology}{fobi}
#'   \item{Properties}{
#'   	id: character,
#'   	name: character,
#'   	parents: list,
#'   	children: list,
#'   	ancestors: list,
#'   	obsolete: logical}
#'   \item{Roots}{
#'   FOBI:0001 - Foods,
#'   FOBI:01501 - Biomarkers,
#'   FOBI:00422 - BiomarkerOf,
#'   FOBI:00423 - HasBiomarker,
#'   FOBI:00424 - Contains,
#'   FOBI:00425 - IsIngredientOf}
#' }
#' @source \url{https://github.com/pcastellanoescuder/FoodBiomarkerOntology}
"fobi_terms"

