context("test-pointEstimate")

testthat::test_that("outcome.type is matched correcly (is not a family like regular glm)", {
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "gaussian"))
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "binomial"))
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "Gamma"))
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "Poisson"))
})

testthat::test_that("Y and X vars are both provided if no formula is given", {
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", Z = c("BMI", "SEX"), outcome.type = "binary"))
  testthat::expect_error(pointEstimate(data = cvdd, X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "binary"))
})


testthat::test_that("X variable is a binary (either a factor with 2 levels, or numeric with only 2 values)", {
  testthat::expect_error(pointEstimate(data = cvdd, formula = CVD ~ BMI + AGE, outcome.type = "binary"))
})


testthat::test_that("outcome is expected value", {
  testthat::expect_equal(round(pointEstimate(data = cvdd, formula = CVD ~ SEX + BMI + AGE, outcome.type = "binary")$diff,3), -0.315)
  testthat::expect_equal(round(pointEstimate(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type = "binary")$ratio, 3), 1.259)
  testthat::expect_equal(pointEstimate(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type  = "binary")$family, "binomial")
})