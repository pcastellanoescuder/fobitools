
#' Convert Metabolite IDs Using FOBI Annotations
#'
#' @description This function can convert metabolite IDs to the other ID types available in FOBI. Note that the input vector can be a combination of different IDs. 
#'
#' @param ids Character vector with metabolite IDs to convert. Input ID types can be FOBI, raw metabolite names (used in FOBI), HMDB, KEGG, PubChemCID, InChIKey, InChICode, ChemSpider or a combination of them.
#' @param to Target ID type. If possible, metabolites will be converted to this ID type. Options are "FOBI" (default), "metaboliteNames", "HMDB", "KEGG", "PubChemCID", "InChIKey", "InChICode" and "ChemSpider".
#' @param stable_version Logical. If it's set to TRUE (default), the function will use an stable version of FOBI. If not, the function will use the `fobi.obo` file from GitHub (\url{https://github.com/pcastellanoescuder/FoodBiomarkerOntology}).
#' 
#' @export
#'
#' @return A data frame with input IDs and converted IDs.
#' @author Pol Castellano-Escuder
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select rename filter mutate_all matches vars select_at filter_all any_vars
#' @importFrom stringr regex str_replace_all str_trim
#' @importFrom crayon red
#' @importFrom clisymbols symbol
id_convert <- function(ids,
                       to = "FOBI",
                       stable_version = TRUE){
  
  if (!(to %in% c("FOBI", "PubChemCID", "KEGG", "InChIKey", "InChICode", "ChemSpider", "metaboliteNames", "HMDB"))) {
    stop(crayon::red(clisymbols::symbol$cross, "Select one valid ID type!"))
  }
  
  if(!stable_version){
    
    fobi <- parse_fobi()
    
    idmap <- fobi %>%
      filter(!BiomarkerOf == "NULL") %>%
      select(name, FOBI, HMDB, KEGG, PubChemCID, InChIKey, InChICode, ChemSpider) %>%
      mutate_all(as.character) %>%
      mutate(HMDB = str_replace_all(HMDB, regex("( ).*"), regex("\\1")),
             KEGG = str_replace_all(KEGG, regex("( ).*"), regex("\\1")),
             PubChemCID = str_replace_all(PubChemCID, regex("( ).*"), regex("\\1")),
             ChemSpider = str_replace_all(ChemSpider, regex("( ).*"), regex("\\1"))) %>%
      rename(metaboliteNames = name) %>%
      mutate_all(~ ifelse(. == "NULL", NA, .)) %>%
      mutate_all(~ stringr::str_trim(.)) %>%
      filter(!duplicated(metaboliteNames))
    
    }
  
  else {
    
    data("idmap")
    
  }
  
  ids <- ids[!is.na(ids)]
  from <- data.frame(presence = apply(idmap, 2, function(x) any(ids %in% x))) %>%
    filter(presence == "TRUE")
    
  conversion <- idmap %>%
    select_at(vars(dplyr::matches(rownames(from)), dplyr::matches(to))) %>%
    filter_all(any_vars(. %in% ids))
  
  return(conversion)
  
}

