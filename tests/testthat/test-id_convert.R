context("id_convert")

test_that("id_convert works", {
  
  ids <- c(fobitools::idmap$HMDB[1:10], fobitools::idmap$KEGG[11:23], fobitools::idmap$InChIKey[100:150])
  nas <- sum(is.na(ids))
    
  ##
    
  tictoc::tic()
  a <- fobitools::id_convert(ids, to = "FOBI")
  time1 <- tictoc::toc()
  
  b <- fobitools::id_convert(ids, to = "PubChemCID")
  
  tictoc::tic()
  c <- fobitools::id_convert(ids, to = "FOBI", stable_version = FALSE)
  time2 <- tictoc::toc()
  
  ##
  
  expect_false((time1$toc-time1$tic) == (time2$toc-time2$tic))
  expect_true((time1$toc-time1$tic) < (time2$toc-time2$tic))
  
  ##
  
  expect_equal(ncol(a), 4)
  expect_equal(ncol(b), 4)
  expect_equal(ncol(c), 4)
  
  expect_equal(nrow(a), length(ids) - nas)
  expect_equal(nrow(b), length(ids) - nas)
  expect_equal(nrow(c), length(ids) - nas)
  
  ##
  
  expect_error(fobitools::id_convert(ids, to = "FOB"))
  
})

