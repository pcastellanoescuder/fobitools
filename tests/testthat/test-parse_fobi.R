context("parse_fobi")

test_that("parse_fobi works", {
  
  a <- fobitools::parse_fobi(terms = "FOODON:00002473", get = "ancestors")
  a_desc <- fobitools::parse_fobi(terms = "FOODON:00002473", get = "descendants")
  b <- fobitools::parse_fobi(terms = "FOBI:01501")
  c <- fobitools::parse_fobi(terms = "FOBI:0001")
  d <- fobitools::parse_fobi()
  
  ##
  
  expect_equal(ncol(a), ncol(b))
  expect_equal(ncol(b), ncol(c))
  expect_equal(ncol(c), ncol(d))
  expect_equal(ncol(d), ncol(a_desc))
  
  ##
  
  expect_true(nrow(a_desc) < nrow(a))
  expect_true(nrow(a) < nrow(d))
  expect_true(nrow(b) < nrow(d))
  expect_true(nrow(c) < nrow(d))
  
  ##
  
  apple_id <- "FOODON:00002473"
  expect_true(apple_id %in% c$id_code)
  
  luteolin_id <- "CHEBI:15864"
  expect_true(luteolin_id %in% b$id_code)
  
  ##
  
  expect_error(fobitools::parse_fobi(terms = "FOODON:00002473", get = "ance"))
  
})

