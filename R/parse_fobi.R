
#' Parse FOBI into Table
#'
#' @description This function parse the `fobi.obo` file from GitHub (\url{https://github.com/pcastellanoescuder/FoodBiomarkerOntology}) into readable table format.
#'
#' @param terms Default is NULL. By changing this parameter for one or more entity IDs (in FOBI), this function will parse only the selected FOBI entities.
#' @param get Only if `terms` is not NULL. Include in the resultant table the upper (ancestors) or lower (decendants) nodes for selected entities in `terms`. Options are "descendants" (default) and "ancestors".
#' 
#' @export
#'
#' @return A data frame with desired FOBI information.
#' @author Pol Castellano-Escuder
#'
#' @examples
#' 
#' # Download and parse whole FOBI
#' fobi <- parse_fobi()
#' 
#' # Download and parse all Foods
#' foods <- parse_fobi(terms = "FOBI:0001")
#' 
#' # Download and parse all Biomarkers
#' biomarkers <- parse_fobi(terms = "FOBI:01501")
#' 
#' # Download and parse 'apple' entity and its ancestors
#' apple <- parse_fobi(terms = "FOODON:00002473", get = "ancestors")
#' 
#' @importFrom magrittr %>%
#' @importFrom vroom vroom
#' @importFrom tidyr drop_na separate_rows pivot_wider
#' @importFrom dplyr slice mutate select rename filter n rowwise ungroup mutate_all group_by
#' @importFrom stringr str_remove_all regex str_replace_all str_detect str_remove str_split
#' @importFrom purrr map
#' @importFrom ontologyIndex get_ontology get_descendants get_ancestors
#' @importFrom crayon red
#' @importFrom clisymbols symbol
parse_fobi <- function(terms = NULL, 
                       get = "descendants"){
  
  if (!(get %in% c("descendants", "ancestors"))) {
    stop(crayon::red(clisymbols::symbol$cross, "Incorrect value for get argument!"))
  }
  
  path <- "https://raw.github.com/pcastellanoescuder/FoodBiomarkerOntology/master/src/ontology/fobi.obo"
  suppressMessages({
    raw_lines <- vroom::vroom(path, delim = ": ")
  })
  
  parsed_fobi <- raw_lines %>% 
    slice(13:n()) %>%
    rename(V1 = 1, V2 = 2) %>%
    separate_rows(V2, sep = " ! ") %>% 
    mutate(V2 = str_replace_all(V2, pattern = "FOBI_", "FOBI:"),
           V2 = str_remove_all(V2, pattern = regex("xsd:string")),
           V1 = ifelse(V1 == "property_value", map(str_split(V2, pattern = " "), 1), V1),
           V1 = ifelse(V1 == "relationship", map(str_split(V2, pattern = " "), 1), V1),
           V1 = ifelse(str_detect(V1, pattern = "id") & str_detect(V2, pattern = regex(":")), "id_code", V1),
           V1 = ifelse(V1 == "id", "name", V1),
           V1 = ifelse(str_detect(V1, pattern = "is_a") & str_detect(V2, pattern = regex(":")), "is_a_code", V1),
           V1 = ifelse(V1 == "is_a", "is_a_name", V1),
           V2 = str_remove_all(V2, pattern = regex('["]'))) %>%
    drop_na() %>%
    rowwise() %>%
    mutate(V2 = str_remove(V2, pattern = V1)) %>%
    ungroup() %>%
    mutate_all(~ str_trim(.)) %>%
    mutate(gf = NA)
  
  for(i in 1:nrow(parsed_fobi)){
    ifelse(parsed_fobi$V1[i] == "id_code", parsed_fobi$gf[i] <- i, parsed_fobi$gf[i] <- parsed_fobi$gf[i - 1])
  }
  
  fobi <- parsed_fobi %>%
    group_by(gf) %>%
    pivot_wider(names_from = V1, values_from = V2, values_fn = list(V2 = list)) %>%
    ungroup() %>%
    select(-gf) %>%
    filter(!name %in% c("BiomarkerOf", "HasBiomarker", "Contains", "IsIngredientOf"))
  
  if(!is.null(terms) & get == "descendants"){
    
    descs <- ontologyIndex::get_ontology(path) %>%
      ontologyIndex::get_descendants(roots = terms)
    
    fobi <- fobi %>%
      filter(id_code %in% descs)
    
  }
  
  if(!is.null(terms) & get == "ancestors"){
    
    ances <- ontologyIndex::get_ontology(path) %>%
      ontologyIndex::get_ancestors(terms = terms)
    
    fobi <- fobi %>%
      filter(id_code %in% ances)
    
  }
  
  fobi <- fobi %>%
    select(id_code:Contains) %>%
    rename(id_BiomarkerOf = "FOBI:00422",
           ChemSpider = "FOBI:040232",
           KEGG = "FOBI:040235",
           PubChemCID = "FOBI:040236",
           InChIKey = "FOBI:040237",
           InChICode = "FOBI:040238",
           FOBI = "FOBI:043480",
           alias = "IAO:0000118",
           HMDB = "FOBI:040233",
           id_Contains = "FOBI:00424") %>%
    unnest(1:16)
  
  if (nrow(fobi) < 1){
    warning("No entities found...")
  }
  
  return(fobi)
  
}

