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


testthat::test_that("X variable is either a factor with 2+ levels or numeric", {
  testthat::expect_error(pointEstimate(data = cvdd %>% mutate(DIABETES = as.character(DIABETES)), formula = cvd_dth ~ DIABETES + SEX, outcome.type = "binary"))
})

testthat::test_that("If an exposure.scalar is provided, X variable is numeric", {
  testthat::expect_error(pointEstimate(data = cvdd, Y = "cvd_dth", X = "bmicat", Z = c("AGE", "SEX", "DIABETES", "CURSMOKE", "PREVHYP"), outcome.type = "binary", exposure.scalar = 10))
})

testthat::test_that("Offset is provided for rate or rate_nb outcomes", {
  testthat::expect_error(pointEstimate(data = cvdd %>% 
                                          dplyr::rowwise() %>%
                                          dplyr::mutate(outpt_clinic_visits = ifelse(DIABETES == 0, rnbinom(n = 1, mu = 6, size = 1.22), rnbinom(n = 1, mu = 14, size = 4.2))) %>%
                                          dplyr::ungroup(),
                                        formula = outpt_clinic_visits ~ DIABETES + AGE + SEX + BMI + PREVHYP, outcome.type = "rate_nb"))
})

testthat::test_that("If a categorical Y with more than 2 categories is provided, error is returned", {
  testthat::expect_error(pointEstimate(data = cvdd, Y = "bmicat", X = "cvd_dth", Z = c("AGE", "SEX", "DIABETES", "CURSMOKE", "PREVHYP"), outcome.type = "binary"))
})

testthat::test_that("No interaction terms provided in formula", {
  testthat::expect_error(pointEstimate(data = cvdd, formula = "cvd_dth ~ AGE + DIABETES + SEX + AGE*SEX", outcome.type = "binary"))
})

testthat::test_that("All variables are in dataset", {
  testthat::expect_error(pointEstimate(data = cvdd %>% select(-AGE), formula = CVD ~ SEX + BMI + AGE, outcome.type = "binary"))
})


testthat::test_that("Y is numeric if otucome.type is in count, rate, continuous, count_nb, rate_nb", {
  testthat::expect_error(pointEstimate(data = cvdd, Y = "bmicat", X = "DIABETES", Z = c("AGE", "SEX",  "CURSMOKE", "PREVHYP"), outcome.type = "continuous"))
  testthat::expect_error(pointEstimate(data = cvdd, Y = "bmicat", X = "DIABETES", Z = c("AGE", "SEX",  "CURSMOKE", "PREVHYP"), outcome.type = "count"))
  testthat::expect_error(pointEstimate(data = cvdd, Y = "bmicat", X = "DIABETES", Z = c("AGE", "SEX",  "CURSMOKE", "PREVHYP"), outcome.type = "count_nb"))
  testthat::expect_error(pointEstimate(data = cvdd, Y = "bmicat", X = "DIABETES", Z = c("AGE", "SEX",  "CURSMOKE", "PREVHYP"), outcome.type = "rate"))
  testthat::expect_error(pointEstimate(data = cvdd, Y = "bmicat", X = "DIABETES", Z = c("AGE", "SEX",  "CURSMOKE", "PREVHYP"), outcome.type = "rate_nb"))
  })

testthat::test_that("outcome is expected value", {
  testthat::expect_equal(round(pointEstimate(data = cvdd, formula = CVD ~ SEX + BMI + AGE, outcome.type = "binary")$parameter.estimates[[1,1]],3), -0.145)
  testthat::expect_equal(round(pointEstimate(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type = "binary")$parameter.estimates[[2,1]], 3), 1.176)
  testthat::expect_equal(pointEstimate(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type  = "binary")$formula, formula(DEATH ~ DIABETES + SEX + AGE))
  testthat::expect_equal(pointEstimate(data = cvdd, formula = cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP, outcome.type  = "binary")$parameter.estimates$Estimate[7], 3.484)
  testthat::expect_equal(round(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), subgroup = "SEX", outcome.type = "binary")$parameter.estimates$SEX0[[1]],6), round(0.259017, 6))
  testthat::expect_equal(round(pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), subgroup = "SEX", outcome.type = "binary")$parameter.estimates$SEX1[[7]], 4), 3.1695)
  testthat::expect_equal(round(pointEstimate(data = cvdd, Y = "cvd_dth", X = "bmicat", Z = c("AGE", "SEX", "DIABETES", "CURSMOKE", "PREVHYP"), outcome.type = "binary")$parameter.estimates$bmicat3_v._bmicat0[[1]] ,6), round(0.093172, 6))
  testthat::expect_equal(round(pointEstimate(data = cvdd, Y = "cvd_dth", X = "bmicat", Z = c("AGE", "SEX", "DIABETES", "CURSMOKE", "PREVHYP"), subgroup = "SEX", outcome.type = "binary")$parameter.estimates$bmicat2_v._bmicat0_SEX1[[3]], 3), round(1.585692, 3))
  testthat::expect_equal(pointEstimate(data = cvdd, Y = "glucoseyear6", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), outcome.type = "continuous")$parameter.estimates$Estimate[[6]], 61.6257)
  testthat::expect_equal(pointEstimate(data = cvdd, formula = cvd_dth ~ AGE + DIABETES + SEX + BMI + CURSMOKE + PREVHYP, outcome.type = "binary", exposure.scalar = 10)$parameter.estimates$Estimate[[1]], 0.2264)
  testthat::expect_equal(pointEstimate(data = cvdd, formula = cvd_dth ~ AGE + DIABETES + SEX + BMI + CURSMOKE + PREVHYP, outcome.type = "binary", exposure.scalar = 1)$parameter.estimates$Estimate[[2]], 1.0551)
  testthat::expect_equal(pointEstimate(data = cvdd %>% 
                                         dplyr::mutate(cvd_dth = as.numeric(as.character(cvd_dth)),
                                                       timeout = as.numeric(timeout)), Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), 
                                       outcome.type = "rate", rate.multiplier = 365.25*100, offset = "timeout")$parameter.estimates$Estimate[[4]], 2.1892)
  set.seed(424)
  testthat::expect_equal(round(pointEstimate(data = cvdd %>% 
                                               dplyr::rowwise() %>%
                                               dplyr::mutate(outpt_clinic_visits = ifelse(DIABETES == 0, rnbinom(n = 1, mu = 6, size = 1.22), rnbinom(n = 1, mu = 14, size = 4.2))) %>%
                                               dplyr::ungroup(),
                                             formula = outpt_clinic_visits ~ DIABETES + AGE + SEX + BMI + PREVHYP, outcome.type = "count_nb")$parameter.estimates$Estimate[4], 4), 8.4779)
  testthat::expect_equal(round(pointEstimate(data = cvdd %>% 
                                               dplyr::rowwise() %>%
                                               dplyr::mutate(outpt_clinic_visits = ifelse(DIABETES == 0, rnbinom(n = 1, mu = 6, size = 1.22), rnbinom(n = 1, mu = 14, size = 4.2))) %>%
                                               dplyr::ungroup(),
                                             formula = outpt_clinic_visits ~ DIABETES + AGE + SEX + BMI + PREVHYP, outcome.type = "rate_nb", offset = "AGE")$parameter.estimates$Estimate[4], 4), 0.1457)
})