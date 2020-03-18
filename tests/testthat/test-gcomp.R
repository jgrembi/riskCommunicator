context("test-gcomp")

test_that("Y and X vars are both provided if no formula is given", {
  expect_error(gComp(data = cvdd, Y = "cvd_dth", Z = c("BMI", "SEX"), family = "binomial", R = 25))
  expect_error(pointEstimate(data = cvdd, X = "DIABETES", Z = c("AGE", "SEX"), family = "binomial"))
})


test_that("outcome is expected value", {
  testthat::expect_equal(round(gComp(data = cvdd, formula = cvd_dth ~ DIABETES + BMI, family = "binomial", R = 4)$paramEst[[1]], 2), 3.63)
  testthat::expect_equal(gComp(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), family = "binomial", R = 4)$R, 4)
})

test_that("class is correct", {
  testthat::expect_is(gComp(data = cvdd, formula = cvd_dth ~ DIABETES + AGE, family = "binomial", R = 4), "gComp")
})

