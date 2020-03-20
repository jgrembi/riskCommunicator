context("test-gComp")

testthat::test_that("outcome.type is matched correcly (is not a family like regular glm)", {
  testthat::expect_error(gComp(data = cvdd, formula = cvd_dth ~ DIABETES + BMI, outcome.type = "binomial", R = 4))
  testthat::expect_error(gComp(data = cvdd, formula = nhosp ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP, outcome.type = "poisson", R = 4))
  testthat::expect_error(gComp(data = cvdd, Y = "glucoseyear6", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), outcome.type = "gaussian", R = 4))
})


testthat::test_that("Y and X vars are both provided if no formula is given", {
  testthat::expect_error(gComp(data = cvdd, Y = "cvd_dth", Z = c("BMI", "SEX"), outcome.type = "binary", R = 25))
  testthat::expect_error(pointEstimate(data = cvdd, X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "binary"))
})


testthat::test_that("outcome is expected value", {
  testthat::expect_equal(round(gComp(data = cvdd, formula = cvd_dth ~ DIABETES + BMI, outcome.type = "binary", R = 4)$paramEst[[1]], 2), 3.63)
  testthat::expect_equal(gComp(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type  = "binary", R = 4)$R, 4)

})

testthat::test_that("class is correct", {
  testthat::expect_is(gComp(data = cvdd, formula = cvd_dth ~ DIABETES + AGE, outcome.type = "binary", R = 4), "gComp")
})

