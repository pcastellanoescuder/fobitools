
#' Text Mining Pipeline to Annotate Free Nutritional Text with FOBI
#'
#' @description This function provides a text mining pipeline to map nutritional free text to Food-Biomarker Ontology. This pipeline is composed of five sequential layers to map food items to FOBI with the maximum accuracy as possible.
#'
#' @param foods A two column data frame. First column must contain the ID (should be unique) and the second column must contain food items (it can be a word or a string).
#' @param similarity Numeric between 0 (low) and 1 (high). This value indicates the semantic similarity cutoff used at the last layer of the text mining pipeline. 1 = exact match; 0 = very poor match. Values below 0.85 are not recommended.
#' @param reference FOBI foods table obtained with `parse_fobi(terms = "FOBI:0001", get = "des")`. If this value is set to NULL, the last version of FOBI will be downloaded from GitHub.
#' 
#' @export
#'
#' @return A list containing two tibble objects: annotated and unannotated food items.
#' @references Pol Castellano-Escuder, Raúl González-Domínguez, David S Wishart, Cristina Andrés-Lacueva, Alex Sánchez-Pla, FOBI: an ontology to represent food intake data and associate it with metabolomic data, Database, Volume 2020, 2020, baaa033, https://doi.org/10.1093/databa/baaa033.
#' @author Pol Castellano-Escuder
#'
#' @examples
#' 
#' # Free text annotation in FOBI
#' free_text <- data.frame(id = c(101, 102, 103, 104),
#'                         text = c("Yesterday I ate eggs and bacon with a butter toast and black tea", 
#'                                  "Crisp bread and rice crackers with wholegrain", 
#'                                  "Beef and veal, one apple", "pizza without meat"))
#' annotate_foods(free_text)
#' 
#' @importFrom magrittr %>%
#' @importFrom clisymbols symbol
#' @importFrom tictoc tic toc
#' @importFrom tidyr separate_rows expand_grid
#' @importFrom dplyr mutate select rename filter ungroup mutate_all group_by bind_rows summarise as_tibble slice left_join
#' @importFrom stringr str_replace_all str_trim str_squish str_remove_all str_detect str_replace
#' @importFrom textclean make_plural
#' @importFrom RecordLinkage jarowinkler 
#' @importFrom crayon red yellow green
annotate_foods <- function(foods,
                           similarity = 0.85,
                           reference = fobitools::foods){
  
  tictoc::tic()
  
  if(ncol(foods) != 2){
    stop("Input must be a two-column data frame")
  }
  if(similarity > 1 | similarity < 0){
    stop("Similarity parameter must be a numeric value between 0 and 1")
  }
  
  foods <- foods %>%
    dplyr::rename(FOOD_ID = 1, FOOD_NAME = 2) %>%
    filter(!duplicated(FOOD_NAME))
  
  dup_ids <- foods %>%
    filter(duplicated(FOOD_ID))
  
  if(nrow(dup_ids) > 0){
    stop("Duplicated IDs are not allowed")
  }
  
  if (is.null(reference)){
    reference <- fobitools::parse_fobi(terms = "FOBI:0001", get = "des")
  }
  
  remove_patterns <- c("\\bproducts\\b",
                       "\\bproduct\\b",
                       "\\bwhole\\b",
                       "\\braw\\b",
                       "\\bfoods\\b",
                       "\\bfood\\b",
                       "\\bplant\\b",
                       "\\bbased\\b",
                       "\\bbeverage\\b",
                       "\\bor\\b",
                       "\\bon\\b",
                       "\\bother\\b",
                       "\\bfried\\b",
                       "\\bboiled\\b")
  
  ffq <- foods %>%
    mutate(words = str_replace_all(FOOD_NAME, "," , " and "),
           words = str_replace_all(words, ";" , " and "),
           words = str_replace_all(words, "[[:punct:]]" , " "),
           words = str_replace_all(words, "[[:digit:]]", " "),
           words = str_trim(words),
           words = str_squish(words),
           words = tolower(words)) %>%
    separate_rows(words, sep = "\\bwith\\b") %>%
    separate_rows(words, sep = "\\band\\b") %>%
    mutate(words = str_remove_all(words, pattern = paste(remove_patterns, collapse = "|")),
           words = ifelse(str_detect(words, "\\bwithout meat\\b"), paste0("vegetarian ", words), words), # without meat
           words = str_remove_all(words, "\\without.*"),
           words = str_remove_all(words, "\\for.*"), # to discuss
           words = str_remove_all(words, ".*free"), # to discuss
           words = str_trim(words),
           words = str_squish(words)) %>%
    group_by(FOOD_ID) %>%
    mutate(words_joint = paste(words, collapse = " "),
           words_joint = str_trim(words_joint),
           words_joint = str_squish(words_joint)) %>%
    ungroup() %>%
    filter(!duplicated(FOOD_ID)) %>%
    select(-words) %>%
    dplyr::rename(words = words_joint)
  
  remove_patterns2 <- c("\\bproducts\\b",
                        "\\bproduct\\b",
                        "\\bwhole\\b",
                        "\\braw\\b",
                        "\\bfoods\\b",
                        "\\bfood\\b",
                        "\\bplant\\b",
                        "\\bbased\\b",
                        "\\bbeverage\\b",
                        "\\bor\\b",
                        "\\band\\b",
                        "\\bother\\b")

  fobi_foods <- reference %>% 
    select(id_code, name) %>%
    mutate_all(unlist) %>%
    mutate(ref = tolower(name),
           ref = str_replace(ref, "\\(.*\\)", ""),
           ref = str_remove_all(ref, pattern = paste(remove_patterns2, collapse = "|")),
           ref = str_replace_all(ref, pattern = "\\brice grain\\b", replacement = "rice"), # specific case
           ref = str_replace_all(ref, pattern = "\\bsoybean\\b", replacement = "soy"), # specific case
           ref = str_replace_all(ref, pattern = "\\bbarley grain \\b", replacement = "barley"), # specific case
           ref = str_replace_all(ref, pattern = "\\bpork ribs\\b", replacement = "pork"), # specific case
           ref = str_trim(ref),
           ref = str_squish(ref)) %>%
    filter(ref != "")
  
  #### START
  
  # RAW MATCH
  
  wordlist <- expand_grid(words = ffq$words, ref = fobi_foods$ref) %>%
    mutate(detect = stringr::str_detect(words, paste0("\\b", ref, "\\b"))) %>%
    filter(ref == words | detect == "TRUE") %>%
    select(-detect)
  
  result0 <- merge(ffq, wordlist, by = "words")
  result0 <- merge(result0, fobi_foods, by = "ref") %>%
    select(FOOD_ID, FOOD_NAME, id_code, name) %>%
    filter(!duplicated(.))
  
  no_matched <- ffq %>% filter(!FOOD_NAME %in% result0$FOOD_NAME)
  
  # RAW MATCH (SINGULARS AND PLURALS WITHOUT WHITESPACES)
  
  ffq_sing <- ffq %>% 
    filter(FOOD_NAME %in% no_matched$FOOD_NAME) %>%
    mutate(words = str_replace_all(words, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)")))
  
  ffq_sing_nospace <- ffq_sing %>%
    mutate(words = str_replace_all(words, " ", ""))
    
  ffq_plural <- ffq %>% 
    filter(FOOD_NAME %in% no_matched$FOOD_NAME) %>%
    mutate(words = textclean::make_plural(words))
  
  ffq_plural_nospace <- ffq_plural %>%
    mutate(words = str_replace_all(words, " ", ""))
  
  ffq_no_space <- ffq %>% 
    filter(FOOD_NAME %in% no_matched$FOOD_NAME) %>%
    mutate(words = str_replace_all(words, " ", ""))
  
  ffq1 <- bind_rows(ffq_sing, ffq_sing_nospace, ffq_plural, ffq_plural_nospace, ffq_no_space)
  
  wordlist <- expand_grid(words = ffq1$words, ref = fobi_foods$ref) %>%
    mutate(detect = stringr::str_detect(words, paste0("\\b", ref, "\\b"))) %>%
    filter(ref == words | detect == "TRUE") %>%
    select(-detect)
  
  result1 <- merge(ffq1, wordlist, by = "words")
  result1 <- merge(result1, fobi_foods, by = "ref") %>%
    select(FOOD_ID, FOOD_NAME, id_code, name) %>%
    filter(!duplicated(.))
  
  no_matched <- ffq1 %>% filter(!FOOD_NAME %in% result0$FOOD_NAME, 
                                !FOOD_NAME %in% result1$FOOD_NAME)
  
  # SPLIT INPUT (SINGULARS AND PLURALS)
  
  ffq_sep <- ffq %>%
    filter(FOOD_NAME %in% no_matched$FOOD_NAME) %>%
    separate_rows(words, sep = " ")
  
  ffq_sep_sing <- ffq_sep %>%
    mutate(words = str_replace_all(words, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)")))
  
  ffq_sep_plural <- ffq_sep %>%
    mutate(words = textclean::make_plural(words))
  
  ffq_sep <- bind_rows(ffq_sep, ffq_sep_sing, ffq_sep_plural)
  
  wordlist <- expand_grid(words = ffq_sep$words, ref = fobi_foods$ref) %>%
    mutate(detect = stringr::str_detect(words, paste0("\\b", ref, "\\b"))) %>%
    filter(ref == words | detect == "TRUE") %>%
    select(-detect)
  
  result2 <- merge(ffq_sep, wordlist, by = "words")
  result2 <- merge(result2, fobi_foods, by = "ref") %>%
    filter(!duplicated(.)) %>%
    group_by(FOOD_ID) %>%
    mutate(id_code = paste(id_code, collapse = ";"),
           name = paste(name, collapse = ";")) %>%
    select(FOOD_ID, FOOD_NAME, id_code, name) %>%
    ungroup() %>%
    filter(!duplicated(.))
  
  no_matched <- ffq %>% filter(!FOOD_NAME %in% result0$FOOD_NAME, 
                               !FOOD_NAME %in% result1$FOOD_NAME,
                               !FOOD_NAME %in% result2$FOOD_NAME)
  
  # SPLIT REFERENCE (SINGULARS AND PLURALS)
  
  fobi_foods_sep <- fobi_foods %>% 
    separate_rows(ref, sep = " ")
  
  fobi_foods_sep_sing <- fobi_foods_sep %>%
    mutate(ref = str_replace_all(ref, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)")))
  
  fobi_foods_sep_plural <- fobi_foods_sep %>%
    mutate(ref = textclean::make_plural(ref))
  
  fobi_foods_sep <- bind_rows(fobi_foods_sep, fobi_foods_sep_sing, fobi_foods_sep_plural)
  
  ffq2 <- ffq %>% 
    filter(FOOD_NAME %in% no_matched$FOOD_NAME)
  
  wordlist <- expand_grid(words = ffq2$words, ref = fobi_foods_sep$ref) %>%
    mutate(detect = stringr::str_detect(words, paste0("\\b", ref, "\\b"))) %>%
    filter(ref == words | detect == "TRUE") %>%
    select(-detect)
  
  result3 <- merge(ffq2, wordlist, by = "words")
  result3 <- merge(result3, fobi_foods_sep, by = "ref") %>%
    select(FOOD_ID, FOOD_NAME, id_code, name) %>%
    filter(!duplicated(.))
  
  no_matched <- ffq %>% filter(!FOOD_NAME %in% result0$FOOD_NAME, 
                               !FOOD_NAME %in% result1$FOOD_NAME,
                               !FOOD_NAME %in% result2$FOOD_NAME,
                               !FOOD_NAME %in% result3$FOOD_NAME)
  
  # RAW SEMANTIC SIMILARITY
  
  ffq3 <- ffq %>%
    filter(FOOD_NAME %in% no_matched$FOOD_NAME)
  
  wordlist <- expand_grid(words = ffq3$words, ref = fobi_foods$ref) %>%
    group_by(words) %>%
    mutate(match_score = RecordLinkage::jarowinkler(words, ref)) %>%
    summarise(match = match_score[which.max(match_score)], ref = ref[which.max(match_score)])
  
  result4 <- merge(ffq3, wordlist, by = "words")
  result4 <- merge(result4, fobi_foods, by = "ref") %>%
    filter(match >= similarity) %>%
    select(FOOD_ID, FOOD_NAME, id_code, name) %>%
    filter(!duplicated(.))
  
  no_matched <- ffq3 %>% filter(!FOOD_NAME %in% result0$FOOD_NAME,
                                !FOOD_NAME %in% result1$FOOD_NAME,
                                !FOOD_NAME %in% result2$FOOD_NAME,
                                !FOOD_NAME %in% result3$FOOD_NAME,
                                !FOOD_NAME %in% result4$FOOD_NAME) %>%
    select(-words) %>%
    as_tibble()
  
  ## MERGE RESULTS
  
  annotated_input <- bind_rows(result0, result1, result2, result3, result4) %>% 
    separate_rows(id_code, sep = ";") %>%
    separate_rows(name, sep = ";") %>%
    mutate_all(as.character) %>%
    select(-id_code) %>%
    mutate(grouping = paste0(FOOD_ID, "_", name)) %>% 
    group_by(grouping) %>%
    dplyr::slice(1) %>%
    ungroup() %>%
    select(-grouping) %>%
    left_join(reference, by = "name") %>%
    select(FOOD_ID, FOOD_NAME, id_code, name) %>%
    dplyr::rename(FOBI_ID = id_code, FOBI_NAME = name) %>%
    filter(!duplicated(.)) %>%
    as_tibble()
  
  ## OUTPUT MESSAGE
  
  if (round(100 - ((nrow(no_matched)/nrow(foods))*100), 2) > 75){
    cat(crayon::green(paste0(round(100 - ((nrow(no_matched)/nrow(foods))*100), 2), "% annotated\n")))
  }
  else if (round(100 - ((nrow(no_matched)/nrow(foods))*100), 2) > 25){
    cat(crayon::yellow(paste0(round(100 - ((nrow(no_matched)/nrow(foods))*100), 2), "% annotated\n")))
  }
  else {
    cat(crayon::red(paste0(round(100 - ((nrow(no_matched)/nrow(foods))*100), 2), "% annotated\n")))
  }
  
  tictoc::toc()
  
  return(list(annotated = annotated_input, unannotated = no_matched))
  
}

