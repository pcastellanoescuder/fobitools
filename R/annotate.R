
annotate <- function(data, 
                     reference){
  
  ffq <- data %>%
    filter(!duplicated(FOOD_NAME)) %>%
    mutate(words = str_trim(FOOD_NAME),
           words = str_squish(words),
           words = str_replace_all(words, "[[:punct:]]" , " "),
           words = str_replace_all(words, "[[:digit:]]", " "),
           words = tolower(words)) %>%
    separate_rows(words, sep = "\\bwith\\b") %>%
    separate_rows(words, sep = "\\band\\b") %>%
    mutate(words = str_remove_all(words, pattern = "\\bproducts\\b"),
           words = str_remove_all(words, pattern = "\\bproduct\\b"),
           words = str_remove_all(words, pattern = "\\bwhole\\b"),
           words = str_remove_all(words, pattern = "\\braw\\b"),
           words = str_remove_all(words, pattern = "\\bfoods\\b"),
           words = str_remove_all(words, pattern = "\\bfood\\b"),
           words = str_remove_all(words, pattern = "\\bplant\\b"),
           words = str_remove_all(words, pattern = "\\bbased\\b"),
           words = str_remove_all(words, pattern = "\\bbeverage\\b"),
           words = str_remove_all(words, pattern = "\\bor\\b"), # to discuss
           # words = str_remove_all(words, pattern = "\\band\\b"),
           # words = str_remove_all(words, pattern = "\\bwith\\b"),
           words = str_remove_all(words, pattern = "\\bon\\b"),
           words = str_remove_all(words, pattern = "\\bother\\b"),
           words = str_remove_all(words, pattern = "\\bfried\\b"), # to discuss
           words = str_remove_all(words, pattern = "\\bboiled\\b"),
           words = ifelse(str_detect(words, "\\bwithout meat\\b"), paste0("vegetarian ", words), words), # without meat
           words = str_remove_all(words, "\\without.*"),
           words = str_remove_all(words, "\\for.*"), # to discuss
           words = str_remove_all(words, ".*free"), # to discuss
           words = str_trim(words),
           words = str_squish(words)) %>%
    group_by(FOOD_ID) %>%
    mutate(words_joint = paste(words, collapse = " "),
           words_joint = str_trim(words_joint),
           words_joint = str_squish(words_joint))%>%
    ungroup() %>%
    filter(!duplicated(FOOD_ID)) %>%
    select(-words) %>%
    rename(words = words_joint)
  
  fobi_foods <- reference %>% 
    select(1:2) %>%
    mutate_all(unlist) %>%
    mutate(ref = tolower(name),
           ref = str_replace(ref, "\\(.*\\)", ""),
           ref = str_remove_all(ref, pattern = "\\bproducts\\b"),
           ref = str_remove_all(ref, pattern = "\\bproduct\\b"),
           ref = str_remove_all(ref, pattern = "\\bwhole\\b"),
           ref = str_remove_all(ref, pattern = "\\braw\\b"),
           ref = str_remove_all(ref, pattern = "\\bfoods\\b"),
           ref = str_remove_all(ref, pattern = "\\bfood\\b"),
           ref = str_remove_all(ref, pattern = "\\bplant\\b"),
           ref = str_remove_all(ref, pattern = "\\bbased\\b"),
           ref = str_remove_all(ref, pattern = "\\bbeverage\\b"),
           ref = str_remove_all(ref, pattern = "\\bor\\b"),
           ref = str_remove_all(ref, pattern = "\\band\\b"),
           ref = str_remove_all(ref, pattern = "\\bother\\b"), # to discuss
           ref = str_replace_all(ref, pattern = "\\brice grain\\b", replacement = "rice"), # specific cases
           ref = str_replace_all(ref, pattern = "\\bsoybean\\b", replacement = "soy"), # specific cases
           ref = str_replace_all(ref, pattern = "\\bbarley grain \\b", replacement = "barley"), # specific cases
           ref = str_replace_all(ref, pattern = "\\bpork ribs\\b", replacement = "pork"), # specific cases
           ref = str_trim(ref),
           ref = str_squish(ref)) %>%
    filter(ref != "")
  
  #### START
  
  # RAW MATCH
  
  wordlist <- expand_grid(words = ffq$words, ref = fobi_foods$ref) %>% 
    filter(ref == words)
  
  result0 <- merge(ffq, wordlist, by = "words")
  result0 <- merge(result0, fobi_foods, by = "ref") %>%
    select(FOOD_ID, FOOD_NAME, id_code, name) %>%
    filter(!duplicated(FOOD_NAME))
  
  no_matched <- ffq %>% filter(!FOOD_NAME %in% result0$FOOD_NAME)
  
  # RAW MATCH (SINGULARS AND PLURALS)
  
  ffq_sing <- ffq %>% 
    filter(FOOD_NAME %in% no_matched$FOOD_NAME) %>%
    mutate(words = str_replace_all(words, c("[^[:alnum:]]$" = "",  "s$" = "", "(\\(\\d*)" = "\\1\\)")))
  
  ffq_plural <- ffq %>% 
    filter(FOOD_NAME %in% no_matched$FOOD_NAME) %>%
    mutate(words = textclean::make_plural(words))
  
  ffq1 <- bind_rows(ffq_sing, ffq_plural)
  
  wordlist <- expand_grid(words = ffq1$words, ref = fobi_foods$ref) %>% 
    filter(ref == words)
  
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
    filter(ref == words)
  
  result2 <- merge(ffq_sep, wordlist, by = "words")
  result2 <- merge(result2, fobi_foods, by = "ref") %>%
    filter(!duplicated(.)) %>%
    group_by(FOOD_ID) %>%
    mutate(id_code = paste(id_code, collapse = " | "),
           name = paste(name, collapse = " | ")) %>%
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
    filter(ref == words)
  
  result3 <- merge(ffq2, wordlist, by = "words")
  result3 <- merge(result3, fobi_foods_sep, by = "ref") %>%
    filter(!duplicated(FOOD_ID)) %>% 
    select(FOOD_ID, FOOD_NAME, id_code, name)
  
  no_matched <- ffq %>% filter(!FOOD_NAME %in% result0$FOOD_NAME, 
                               !FOOD_NAME %in% result1$FOOD_NAME,
                               !FOOD_NAME %in% result2$FOOD_NAME,
                               !FOOD_NAME %in% result3$FOOD_NAME)
  
  # RAW SEMANTIC SIMILARITY
  
  ffq3 <- ffq %>%
    filter(FOOD_NAME %in% no_matched$FOOD_NAME)
  
  wordlist <- expand_grid(words = ffq3$words, ref = fobi_foods$ref) %>%
    group_by(words) %>%
    mutate(match_score = jarowinkler(words, ref)) %>%
    summarise(match = match_score[which.max(match_score)], ref = ref[which.max(match_score)])
  
  result4 <- merge(ffq3, wordlist, by = "words")
  result4 <- merge(result4, fobi_foods, by = "ref") %>%
    filter(match >= 1) %>%
    select(FOOD_ID, FOOD_NAME, id_code, name) %>%
    filter(!duplicated(.))
  
  no_matched <- ffq3 %>% filter(!FOOD_NAME %in% result0$FOOD_NAME,
                                !FOOD_NAME %in% result1$FOOD_NAME,
                                !FOOD_NAME %in% result2$FOOD_NAME,
                                !FOOD_NAME %in% result3$FOOD_NAME,
                                !FOOD_NAME %in% result4$FOOD_NAME)
  
  #######
  
  final_result <- bind_rows(result0, result1, result2, result3, result4) %>% 
    filter(!duplicated(FOOD_ID)) # random pick first one if two or more have been annotated
  # paste0(round(100 - ((nrow(no_matched)/nrow(ffq))*100), 2), "% annotated!")
  
  return(final_result)
  
}

