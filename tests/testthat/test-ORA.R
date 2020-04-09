context("ora")

test_that("ora works", {

  data("idmap")

  idx <- sample(1:nrow(idmap), 100)
  metabolites <- idmap$InChIKey[idx]

  data <- data.frame(A = metabolites[1:50], B = metabolites[51:100])

  a <- fobienrich::ora(metabolites = metabolites)
  b <- fobienrich::ora(metabolites = metabolites, method = "fdr")
  c <- fobienrich::ora(metabolites = metabolites, method = "bonferroni")
  d <- fobienrich::ora(metabolites = metabolites[1:50], method = "bonferroni")

  ##

  expect_equal(dim(a), dim(b))
  expect_equal(dim(b), dim(c))
  expect_equal(dim(c), dim(d))

  expect_equal(a, c)
  expect_false(all(a == b))
  expect_false(all(b == c))
  expect_false(all(c == d))

  ##

  expect_warning(fobienrich::ora(metabolites))
  expect_error(fobienrich::ora(metabolites, method = "fd"))
  expect_error(fobienrich::ora())
  expect_error(fobienrich::ora(data))

})

