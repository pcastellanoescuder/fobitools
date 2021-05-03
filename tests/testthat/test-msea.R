context("msea")

test_that("msea works", {
  
  metabolites <- c(fobitools::idmap$FOBI[1:49], fobitools::idmap$FOBI[70:80])
  random_pvals <- c(runif(n = length(metabolites)*0.3, min = 0.001, max = 0.05), runif(n = length(metabolites)*0.7, min = 0.05, max = 0.99))
  names(random_pvals) <- metabolites
  
  metaboliteRanks <- random_pvals[order(random_pvals)]
    
  data <- data.frame(A = metaboliteRanks[1:50], B = metaboliteRanks[51:100])
  
  a <- fobitools::msea(metaboliteRanks = metaboliteRanks, pvalCutoff = 1)
  b <- fobitools::msea(metaboliteRanks = metaboliteRanks, subOntology = "food", pvalCutoff = 1, fobi = NULL)
  c <- fobitools::msea(metaboliteRanks = metaboliteRanks, subOntology = "biomarker", pvalCutoff = 1)
  
  ##
  
  expect_true(class(a)[2] == "tbl")
  expect_true(class(b)[2] == "tbl")
  expect_true(class(c)[2] == "tbl")
  
  ##
  
  expect_equal(dim(a), dim(b))
  expect_equal(ncol(b), ncol(c))
  expect_equal(a$pvalue, b$pvalue)
  
  ##
  
  expect_error(fobitools::msea(metaboliteRanks, subOntology = "bio", pvalCutoff = 1))
  expect_error(fobitools::msea())
  expect_error(fobitools::msea(data))
  expect_error(fobitools::msea(metabolites))
  
})