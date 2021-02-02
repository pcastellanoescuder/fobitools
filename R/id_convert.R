
#' Convert Metabolite Identifiers
#'
#' @description This function can convert metabolite identifiers to other available IDs in FOBI. Input vector can be a combination of different IDs. 
#'
#' @param ids A vector with metabolite IDs to convert. Input ID types can be FOBI, raw metabolite names (used in FOBI), HMDB, KEGG, PubChemCID, InChIKey, InChICode, ChemSpider, and a combination of them.
#' @param to Target ID type. If possible, metabolites will be converted to this ID type. Options are "FOBI" (default), "metaboliteNames", "HMDB", "KEGG", "PubChemCID", "InChIKey", "InChICode", and "ChemSpider".
#' @param fobi FOBI table obtained with `parse_fobi()`. If this value is set to NULL, the last version of FOBI will be downloaded from GitHub.
#' 
#' @export
#'
#' @return A tibble with input IDs and converted IDs.
#' @references Pol Castellano-Escuder, Raúl González-Domínguez, David S Wishart, Cristina Andrés-Lacueva, Alex Sánchez-Pla, FOBI: an ontology to represent food intake data and associate it with metabolomic data, Database, Volume 2020, 2020, baaa033, https://doi.org/10.1093/databa/baaa033.
#' @author Pol Castellano-Escuder
#'
#' @examples
#' 
#' ids <- c(fobitools::idmap$HMDB[1:10], 
#'          fobitools::idmap$KEGG[11:23], 
#'          fobitools::idmap$InChIKey[100:150])
#' fobitools::id_convert(ids, to = "FOBI")
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate select rename filter mutate_all matches vars select_at filter_all any_vars as_tibble relocate last_col
#' @importFrom stringr regex str_replace_all str_trim
id_convert <- function(ids,
                       to = "FOBI",
                       fobi = fobitools::fobi){
  
  if (!(to %in% c("FOBI", "PubChemCID", "KEGG", "InChIKey", "InChICode", "ChemSpider", "metaboliteNames", "HMDB"))) {
    stop("Select a valid ID type!")
  }
  
  if (is.null(fobi)){
    fobi <- parse_fobi()
  }
  
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

  ids <- ids[!is.na(ids)]
  from <- data.frame(presence = apply(idmap, 2, function(x) any(ids %in% x))) %>%
    filter(presence == "TRUE")
    
  conversion <- idmap %>%
    select_at(vars(dplyr::matches(rownames(from)), dplyr::matches(to))) %>%
    filter_all(any_vars(. %in% ids)) %>%
    relocate(last_col()) %>%
    as_tibble()
  
  return(conversion)
  
}

