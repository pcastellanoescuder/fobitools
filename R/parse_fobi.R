
parse_fobi <- function(terms = NULL, output = "descendants"){
  
  path <- "https://raw.github.com/pcastellanoescuder/FoodBiomarkerOntology/master/src/ontology/fobi.obo"
  suppressMessages({
    raw_lines <- vroom::vroom(path)
  })
  colnames(raw_lines)[1:2] <- c("V1", "V2")
  
  suppressWarnings({
    parsed_fobi <- raw_lines %>% 
      slice(13:n()) %>%
      separate_rows(V2, sep = " ! ") %>% 
      mutate(V2 = str_remove_all(V2, pattern = regex("xsd:string")), # !!!
             V3 = ifelse(V1 == "property_value:", map(str_split(V2, pattern = " "), 1), V1),
             V2 = str_remove_all(V2, pattern = regex(V1)),
             V3 = ifelse(V3 == "relationship:", map(str_split(V2, pattern = " "), 1), V3),
             V2 = str_remove_all(V2, pattern = regex(V3)),
             V2 = str_remove_all(V2, pattern = regex('["]')),
             V1 = str_remove_all(V1, pattern = regex('[:]')),
             V2 = str_replace_all(V2, pattern = regex('[_]'), replacement = ':')) %>%
      rowwise() %>%
      mutate_all(~ str_trim(.)) %>%
      ungroup() %>%
      select(V1, V3, V2) %>%
      drop_na() %>%
      mutate(V3 = ifelse(str_detect(V2, pattern = regex(":")) & str_detect(V1, pattern = regex("id")), "id_code", V3),
             V3 = ifelse(V3 == "id:", "name", V3),
             V3 = ifelse(str_detect(V2, pattern = regex(":")) & str_detect(V1, pattern = regex("is_a")), "is_a_code", V3),
             V3 = ifelse(V3 == "is_a:", "is_a_name", V3),
             V3 = str_replace_all(V3, pattern = regex('name:'), replacement = regex('name'))) %>%
      rename(type = V1,
             description = V3,
             target = V2) %>%
      mutate(gf = NA)
  })
  
  for(i in 1:nrow(parsed_fobi)){
    ifelse(parsed_fobi$description[i] == "id_code", parsed_fobi$gf[i] <- i, parsed_fobi$gf[i] <- parsed_fobi$gf[i - 1])
  }
  
  fobi <- parsed_fobi %>%
    select(-type) %>%
    group_by(gf) %>%
    pivot_wider(names_from = description, values_from = target, values_fn = list(target = list)) %>%
    ungroup() %>%
    select(-gf) %>%
    filter(!name %in% c("BiomarkerOf", "HasBiomarker", "Contains", "IsIngredientOf"))
  
  if(!is.null(terms) & output == "descendants"){
    
    descs <- ontologyIndex::get_ontology(path) %>%
      ontologyIndex::get_descendants(roots = terms)
    
    fobi <- fobi %>%
      filter(id_code %in% descs)
    
  }
  
  if(!is.null(terms) & output == "ancestors"){
    
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
           InChI_Code = "FOBI:040238",
           FOBI = "FOBI:043480",
           alias = "IAO:0000118",
           HMDB = "FOBI:040233",
           id_Contains = "FOBI:00424")
  
  return(fobi)
  
}

