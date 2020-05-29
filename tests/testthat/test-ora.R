context("ora")

test_that("ora works", {

  data("idmap")

  idx <- sample(1:nrow(idmap), 100)
  metabolites <- idmap$InChIKey[idx]

  data <- data.frame(A = metabolites[1:50], B = metabolites[51:100])

  tictoc::tic()
  a <- fobitools::ora(metabolites = metabolites)
  time1 <- tictoc::toc()
  
  b <- fobitools::ora(metabolites = metabolites, fobi_sets = "foods", method = "fdr")
  c <- fobitools::ora(metabolites = metabolites, fobi_sets = "chemicals", method = "fdr")
  d <- fobitools::ora(metabolites = metabolites, fobi_sets = "foods", method = "bonferroni")
  
  tictoc::tic()
  e <- fobitools::ora(metabolites = metabolites, stable_version = FALSE)
  time2 <- tictoc::toc()
  
  ##

  expect_false((time1$toc-time1$tic) == (time2$toc-time2$tic))
  expect_true((time1$toc-time1$tic) < (time2$toc-time2$tic))
  
  ##
  
  expect_true(class(a) == "data.frame")
  expect_true(class(b) == "data.frame")
  expect_true(class(c) == "data.frame")
  expect_true(class(d) == "data.frame")
  
  ##
  
  expect_equal(dim(a), dim(b))
  expect_equal(ncol(b), ncol(c))
  expect_equal(ncol(c), ncol(d))

  ##
  
  expect_equal(a, d)
  expect_equal(a$pvalue, b$pvalue)
  expect_equal(b$pvalue, d$pvalue)
  
  ##

  expect_error(fobitools::ora(metabolites, fobi_sets = "chem"))
  expect_error(fobitools::ora(metabolites, method = "fd"))
  expect_error(fobitools::ora())
  expect_error(fobitools::ora(data))

})

