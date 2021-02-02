context("id_convert")

test_that("id_convert works", {
  
  ids <- c(fobitools::idmap$HMDB[1:10], fobitools::idmap$KEGG[11:23], fobitools::idmap$InChIKey[100:150])
  nas <- sum(is.na(ids))
    
  ##
    
  a <- fobitools::id_convert(ids, to = "FOBI")
  b <- fobitools::id_convert(ids, to = "PubChemCID", fobi = NULL)
  
  ##
  
  expect_equal(ncol(a), 4)
  expect_equal(ncol(b), 4)
  
  ##
  
  expect_true(class(a)[2] == "tbl")
  expect_true(class(b)[2] == "tbl")
  
  ##
  
  expect_equal(nrow(a), length(ids) - nas)
  expect_equal(nrow(b), length(ids) - nas)
  
  ##
  
  expect_error(fobitools::id_convert(ids, to = "FOB"))
  
})

