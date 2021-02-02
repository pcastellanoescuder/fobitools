context("ora")

test_that("ora works", {

  metaboliteUniverse <- c(fobitools::idmap$FOBI[1:200], fobitools::idmap$FOBI[400:450])
  metaboliteList <- c(fobitools::idmap$FOBI[1:50], fobitools::idmap$FOBI[70:80])

  data <- data.frame(A = metaboliteList[1:50], B = metaboliteList[51:100])

  a <- fobitools::ora(metaboliteList = metaboliteList, metaboliteUniverse = metaboliteUniverse, pvalCutoff = 1)
  b <- fobitools::ora(metaboliteList = metaboliteList, metaboliteUniverse = metaboliteUniverse, subOntology = "food", pvalCutoff = 1, adjust = "fdr", fobi = NULL)
  c <- fobitools::ora(metaboliteList = metaboliteList, metaboliteUniverse = metaboliteUniverse, subOntology = "biomarker", pvalCutoff = 1, adjust = "fdr")
  d <- fobitools::ora(metaboliteList = metaboliteList, metaboliteUniverse = metaboliteUniverse, subOntology = "food", pvalCutoff = 1, adjust = "fdr", fobi = NULL)
  
  ##
  
  expect_true(class(a)[2] == "tbl")
  expect_true(class(b)[2] == "tbl")
  expect_true(class(c)[2] == "tbl")
  expect_true(class(d)[2] == "tbl")
  
  ##
  
  expect_equal(dim(a), dim(b))
  expect_equal(ncol(b), ncol(c))
  expect_equal(ncol(c), ncol(d))

  ##
  
  expect_equal(a, d)
  expect_equal(a$pvalue, b$pvalue)
  expect_equal(b$pvalue, d$pvalue)
  
  ##

  expect_error(fobitools::ora(metaboliteList, metaboliteUniverse = metaboliteUniverse, subOntology = "bio", pvalCutoff = 1))
  expect_error(fobitools::ora(metaboliteList, metaboliteUniverse = metaboliteUniverse, pvalCutoff = 1, adjust = "fd"))
  expect_error(fobitools::ora())
  expect_error(fobitools::ora(data))

})

