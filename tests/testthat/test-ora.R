context("ora")

test_that("ora works", {

  data("idmap")

  idx <- sample(1:nrow(idmap), 100)
  metabolites <- idmap$InChIKey[idx]

  data <- data.frame(A = metabolites[1:50], B = metabolites[51:100])

  a <- fobibsa::ora(metabolites = metabolites)
  b <- fobibsa::ora(metabolites = metabolites, method = "fdr")
  c <- fobibsa::ora(metabolites = metabolites, method = "bonferroni")
  d <- fobibsa::ora(metabolites = metabolites[1:50], method = "bonferroni")

  ##

  expect_equal(dim(a), dim(b))
  expect_equal(dim(b), dim(c))
  # expect_equal(dim(c), dim(d))

  expect_equal(a, c)
  expect_false(all(a == b))
  expect_false(all(b == c))
  # expect_false(all(c == d))

  ##

  expect_warning(fobibsa::ora(metabolites))
  expect_error(fobibsa::ora(metabolites, method = "fd"))
  expect_error(fobibsa::ora())
  expect_error(fobibsa::ora(data))

})

