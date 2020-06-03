context("test-pointestimate")

testthat::test_that("outcome.type is matched correcly (is not a family like regular glm)", {
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "gaussian"))
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "binomial"))
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "Gamma"))
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "Poisson"))
  testthat::expect_error(pointEstimate(data = cvdd %>% dplyr::mutate(AGE = as.character(AGE)), formula = "DEATH ~ DIABETES + SEX + AGE", outcome.type = "binary"))
})

testthat::test_that("Y and X vars are both provided if no formula is given", {
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", Z = c("BMI", "SEX"), outcome.type = "binary"))
  testthat::expect_error(pointEstimate(data = cvdd, X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "binary"))
})


testthat::test_that("X variable is either a factor with 2+ levels or numeric)", {
  testthat::expect_error(pointEstimate(data = cvdd %>% mutate(DIABETES = as.character(DIABETES)), formula = cvd_dth ~ DIABETES + SEX, outcome.type = "binary"))
})

testthat::test_that("If an exposure.scalar is provided, X variable is numeric)", {
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "bmicat", Z = c("AGE", "SEX", "DIABETES", "CURSMOKE", "PREVHYP"), outcome.type = "binary", exposure.scalar = 10))
})

testthat::test_that("outcome is expected value", {
  testthat::expect_equal(round(pointEstimate(data = cvdd, formula = CVD ~ SEX + BMI + AGE, outcome.type = "binary")$parameter.estimates[[1,1]],3), -0.164)
  testthat::expect_equal(round(pointEstimate(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type = "binary")$parameter.estimates[[2,1]], 3), 1.187)
  testthat::expect_equal(pointEstimate(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type  = "binary")$formula, formula(DEATH ~ DIABETES + SEX + AGE))
  testthat::expect_equal(pointEstimate(data = cvdd, formula = cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP, outcome.type  = "binary")$parameter.estimates$Estimate[7], 3.3304)
  testthat::expect_equal(round(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), subgroup = "SEX", outcome.type = "binary")$parameter.estimates$SEX0[[1]],6), round(0.2395556, 6))
  testthat::expect_equal(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), subgroup = "SEX", outcome.type = "binary")$parameter.estimates$SEX1[[7]], 2.7900080)
  testthat::expect_equal(round(pointEstimate(data = cvdd, Y = "cvd_dth", X = "bmicat", Z = c("AGE", "SEX", "DIABETES", "CURSMOKE", "PREVHYP"), outcome.type = "binary")$parameter.estimates$bmicat3_v._bmicat0[[1]] ,6), round(0.1157133, 6))
  testthat::expect_equal(pointEstimate(data = cvdd, Y = "glucoseyear6", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), outcome.type = "continuous")$parameter.estimates$Estimate[[6]], 61.6257)
  testthat::expect_equal(pointEstimate(data = cvdd, formula = cvd_dth ~ AGE + DIABETES + SEX + BMI + CURSMOKE + PREVHYP, outcome.type = "binary", exposure.scalar = 10)$parameter.estimates$Estimate[[1]], 0.2426)
  testthat::expect_equal(pointEstimate(data = cvdd, formula = cvd_dth ~ AGE + DIABETES + SEX + BMI + CURSMOKE + PREVHYP, outcome.type = "binary", exposure.scalar = 1)$parameter.estimates$Estimate[[2]], 1.0549)
  testthat::expect_equal(pointEstimate(data = cvdd %>% dplyr::mutate(cvd_dth = as.numeric(as.character(cvd_dth)),
                                              timeout = as.numeric(timeout)), Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), 
                                              outcome.type = "rate", rate.multiplier = 365.25*100, offset = "timeout")$parameter.estimates$Estimate[[4]], 2.1892)
})