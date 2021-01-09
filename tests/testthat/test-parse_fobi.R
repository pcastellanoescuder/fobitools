context("parse_fobi")

test_that("parse_fobi works", {
  
  a <- fobitools::parse_fobi(terms = "FOODON:00002473")
  b <- fobitools::parse_fobi(terms = "FOBI:01501")
  c <- fobitools::parse_fobi(terms = "FOBI:0001")
  d <- fobitools::parse_fobi()
  e <- fobitools::parse_fobi(terms = c("FOODON:00002473", "CHEBI:10219"))
    
  ##
  
  expect_equal(ncol(a), ncol(b))
  expect_equal(ncol(b), ncol(c))
  expect_equal(ncol(c), ncol(d))
  
  ##
  
  expect_true(nrow(e) > nrow(a))
  expect_true(nrow(a) < nrow(d))
  expect_true(nrow(b) < nrow(d))
  expect_true(nrow(c) < nrow(d))
  
  ##
  
  nuts_id <- "FOBI:0135"
  expect_true(nuts_id %in% c$id_code)
  
  alkaloids_id <- "FOBI:040010"
  expect_true(alkaloids_id %in% b$id_code)
  
  ##
  
  expect_warning(fobitools::parse_fobi(terms = "FOODON:0000"))
  
})
