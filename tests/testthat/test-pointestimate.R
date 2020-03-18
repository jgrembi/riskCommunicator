context("test-pointestimate")

test_that("model specification family is matched correcly", {
  expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX"), family = "gaussian"))
  expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX"), family = "poisson"))
  expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX"), family = "Gamma"))
})

test_that("Y and X vars are both provided if no formula is given", {
  expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", Z = c("BMI", "SEX"), family = "binomial"))
  expect_error(pointEstimate(data = cvdd, X = "DIABETES", Z = c("AGE", "SEX"), family = "binomial"))
})


test_that("X variable is a binary (either a factor with 2 levels, or numeric with only 2 values)", {
  expect_error(pointEstimate(data = cvdd, formula = CVD ~ BMI + AGE, family = "binomial"))
})


test_that("outcome is expected value", {
  expect_equal(round(pointEstimate(data = cvdd, formula = CVD ~ SEX + BMI + AGE, family = "binomial")$diff,3), -0.315)
  expect_equal(round(pointEstimate(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), family = "binomial")$ratio, 3), 1.259)
})