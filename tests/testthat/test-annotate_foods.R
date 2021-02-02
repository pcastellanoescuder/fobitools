context("annotate_foods")

test_that("annotate_foods works", {
  
  # Dummy free text
  free_text <- data.frame(id = c(101, 102, 103, 104),
                          text = c("Yesterday I ate eggs and bacon with a butter toast and black tea", 
                                   "Crisp bread and rice crackers with wholegrain", 
                                   "Beef and veal, one apple", "pizza without meat"))
  
  free_text_wrong <- data.frame(id = c(101, 102, 103, 104),
                                text = c("Yesterday I ate eggs and bacon with a butter toast and black tea", 
                                         "Crisp bread and rice crackers with wholegrain", 
                                         "Beef and veal, one apple", "pizza without meat"),
                                id_2 = c(1,2,3,4))
  
  free_text_dup <- data.frame(id = c(101, 101, 103, 104),
                              text = c("Yesterday I ate eggs and bacon with a butter toast and black tea", 
                                       "Crisp bread and rice crackers with wholegrain", 
                                       "Beef and veal, one apple", "pizza without meat"))
  
  a <- annotate_foods(free_text)
  b <- annotate_foods(free_text, similarity = 0.7)
  c <- annotate_foods(free_text, similarity = 1, reference = NULL)
  
  ##
  
  expect_equal(ncol(a), ncol(b))
  expect_equal(ncol(b), ncol(c))
  
  ##
  
  expect_true(class(a) == "list")
  expect_true(class(b) == "list")
  expect_true(class(c) == "list")
  
  expect_true(class(a$annotated)[2] == "tbl")
  expect_true(class(b$annotated)[2] == "tbl")
  expect_true(class(c$annotated)[2] == "tbl")
  
  expect_true(class(a$unannotated)[2] == "tbl")
  expect_true(class(b$unannotated)[2] == "tbl")
  expect_true(class(c$unannotated)[2] == "tbl")
  
  ##
  
  expect_error(annotate_foods(free_text, similarity = 1.01))
  expect_error(annotate_foods(free_text, similarity = -1))
  expect_error(annotate_foods(free_text_wrong))
  expect_error(annotate_foods(free_text_dup))
  
})

