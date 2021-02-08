context("fobi_graph")

test_that("fobi_graph works", {
  
  terms <- c("CHEBI:16164", "CHEBI:16243", "FOODON:00001139", "FOODON:00003274", "FOODON:00003275")
  
  a <- fobi_graph(terms, get = "anc")
  b <- fobi_graph(terms, get = "anc", property = c("is_a", "BiomarkerOf", "Contains"), labels = TRUE)
  c <- fobi_graph(terms, get = "anc", labels = TRUE, legend = TRUE, fobi = NULL)
  d <- fobi_graph(terms, get = "anc", property = c("is_a", "BiomarkerOf"), labels = TRUE, legend = TRUE, curved = TRUE)
  e <- fobi_graph(terms = "FOODON:00002473", property = c("is_a", "Contains"), labels = TRUE, legend = TRUE, labelsize = 5, fobi = NULL)
  
  ##
  
  expect_true(class(a)[1] == "ggraph")
  expect_true(class(b)[1] == "ggraph")
  expect_true(class(c)[1] == "ggraph")
  expect_true(class(d)[1] == "ggraph")
  expect_true(class(e)[1] == "ggraph")
  
  ##
  
  expect_error(fobi_graph(get = "anc"))
  expect_error(fobi_graph(terms, get = "ance"))
  expect_error(fobi_graph(terms, get = "anc", layout = "fr"))
  expect_error(fobi_graph(terms, get = "anc", legendPos = "left"))
  expect_error(fobi_graph(terms = c("FOBI:01501", "FOBI:0001"), get = "des", property = c("is_a", "Biomarker_of", "Contains"))) # Biomarker_of instead of BiomarkerOf
  expect_error(fobi_graph(terms = c("FOBI:01501", "FOBI:0001"), get = "anc", legend = TRUE))
  
  })