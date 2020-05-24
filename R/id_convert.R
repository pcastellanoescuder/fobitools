
id_convert <- function(ids,
                       to = "FOBI",
                       stable_version = FALSE){
  
  if(!isTRUE(stable_version)){
    
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
      mutate_all(~ stringr::str_trim(.))
    
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

