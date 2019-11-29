context("ORA")

test_that("ORA works", {

  metabolites <- c("acesulfame k",
                   "saccharin",
                   "ergothioneine",
                   "3-methylxanthine",
                   "3-hydroxybenzoic acid sulfate",
                   "3-hydroxyphenylacetic acid sulfate",
                   "4-hydroxyphenylacetic acid glucuronide",
                   "5-(3'-methoxy-4'-hydroxyphenyl)-γ-valerolactone sulfate")

  data <- data.frame(A = metabolites[1:4], B = metabolites[5:8])

  a <- FOBIEnrichR::ORA(metabolites = metabolites)
  b <- FOBIEnrichR::ORA(metabolites = metabolites, method = "fdr")
  c <- FOBIEnrichR::ORA(metabolites = metabolites, method = "bonferroni")
  d <- FOBIEnrichR::ORA(metabolites = metabolites[1:6], method = "bonferroni")

  ##

  expect_equal(dim(a), dim(b))
  expect_equal(dim(b), dim(c))
  expect_equal(dim(c), dim(d))

  expect_equal(a, c)
  expect_false(all(a == b))
  expect_false(all(b == c))
  expect_false(all(c == d))

  ##

  expect_warning(FOBIEnrichR::ORA(metabolites))
  expect_error(FOBIEnrichR::ORA(metabolites, method = "fd"))
  expect_error(FOBIEnrichR::ORA())
  expect_error(FOBIEnrichR::ORA(data))

})

