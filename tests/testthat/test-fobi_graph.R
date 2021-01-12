context("fobi_graph")

test_that("fobi_graph works", {
  
  terms <- c("CHEBI:16164", "CHEBI:16243", "FOODON:00001139", "FOODON:00003274", "FOODON:00003275")
  
  a <- fobi_graph(terms, get = "anc")
  b <- fobi_graph(terms, get = "anc", labels = TRUE)
  c <- fobi_graph(terms, get = "anc", labels = TRUE, legend = TRUE)
  d <- fobi_graph(terms, get = "anc", labels = TRUE, legend = TRUE, curved = TRUE)
  e <- fobi_graph(terms = "FOODON:00002473", labels = TRUE, legend = TRUE, labelsize = 5)
  
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
  
  })