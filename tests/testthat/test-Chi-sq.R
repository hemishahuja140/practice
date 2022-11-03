test_that("hypothesis works", {
  expect_equal(hypothesis_chi(project2022), cat("NULL HYPOTHESIS","Gender and amount of physical activity are independent from each other.",sep = "\n"))
})
