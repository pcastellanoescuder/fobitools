
#' Parse FOBI in a Readable Tabular Format
#'
#' @description This function allows users to download and parse the last version of the Food-Biomarker Ontology into a readable table format.
#'
#' @param terms A character vector with FOBI term IDs. Default is NULL.
#' @param get A character string indicating desired relationships between provided terms. Options are 'anc' (for ancestors) and 'des' (for descendants). Default is NULL and only information of single input terms will be provided.
#' 
#' @export
#'
#' @return A tibble with FOBI terms information.
#' @references Pol Castellano-Escuder, Raúl González-Domínguez, David S Wishart, Cristina Andrés-Lacueva, Alex Sánchez-Pla, FOBI: an ontology to represent food intake data and associate it with metabolomic data, Database, Volume 2020, 2020, baaa033, https://doi.org/10.1093/databa/baaa033.
#' @author Pol Castellano-Escuder
#'
#' @examples
#' 
#' # Download and parse whole FOBI
#' fobi <- parse_fobi()
#' 
#' # Download and parse 'apple' related terms
#' fobi_apple <- parse_fobi(terms = "FOODON:00002473")
#'
#' # Download and parse 'apple' term ancestors
#' fobi_apple_anc <- parse_fobi(terms = "FOODON:00002473", get = "anc")
#' 
#' # Download and parse 'apple' and 'alpha-Chaconine' related terms
#' fobi_subset <- parse_fobi(terms = c("FOODON:00002473", "CHEBI:10219"))
#' 
#' @importFrom magrittr %>%
#' @importFrom vroom vroom
#' @importFrom ontologyIndex get_ontology get_descendants
#' @importFrom tidyr drop_na separate_rows pivot_wider
#' @importFrom dplyr slice mutate select rename filter n rowwise ungroup mutate_all group_by
#' @importFrom stringr str_remove_all regex str_replace_all str_detect str_remove str_split
#' @importFrom purrr map
parse_fobi <- function(terms = NULL,
                       get = NULL){

  if (!is.null(get)){
    if (!(get %in% c("anc", "des"))) {
      stop("Incorrect value for get argument. Options are 'anc' (for ancestors) and 'des' (for descendants)")
    }
  }
  
  path <- "https://raw.github.com/pcastellanoescuder/FoodBiomarkerOntology/master/fobi.obo"
  
  fobi_terms <- ontologyIndex::get_ontology(path)
  
  if (!all(terms %in% fobi_terms$id)) {
    stop("Some of provided terms were not found in FOBI")
  }
  
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
  
  for(i in seq_len(nrow(parsed_fobi))){
    ifelse(parsed_fobi$V1[i] == "id_code", parsed_fobi$gf[i] <- i, parsed_fobi$gf[i] <- parsed_fobi$gf[i - 1])
  }
  
  fobi <- parsed_fobi %>%
    group_by(gf) %>%
    pivot_wider(names_from = V1, values_from = V2, values_fn = list(V2 = list)) %>%
    ungroup() %>%
    select(-gf) %>%
    filter(!name %in% c("BiomarkerOf", "HasBiomarker", "Contains", "IsIngredientOf")) %>%
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

  ##
  
  if (!is.null(terms) & is.null(get)){
    fobi <- fobi %>%
      filter(id_code %in% terms | is_a_code %in% terms | id_BiomarkerOf %in% terms | id_Contains %in% terms)
  }
  
  else if (!is.null(terms) & !is.null(get)){
    if(get == "des"){
      fobi_des <- fobi_terms %>%
        ontologyIndex::get_descendants(roots = terms, exclude_roots = TRUE)
      
      fobi <- fobi %>%
        filter(id_code %in% fobi_des)
    }
    else {
      fobi_anc <- fobi_terms %>%
        ontologyIndex::get_ancestors(terms = terms)
      
      fobi <- fobi %>%
        filter(id_code %in% fobi_anc)
    }
  }
  
  else if (is.null(terms) & !is.null(get)) {
    stop("No terms provided!")
  }
  
  return(fobi)
  
}

