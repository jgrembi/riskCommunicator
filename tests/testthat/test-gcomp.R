context("test-gcomp")

testthat::test_that("outcome.type is matched correcly (is not a family like regular glm)", {
  testthat::expect_error(gComp(data = cvdd, formula = cvd_dth ~ DIABETES + BMI, outcome.type = "binomial", R = 4))
  testthat::expect_error(gComp(data = cvdd, formula = nhosp ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP, outcome.type = "poisson", R = 4))
  testthat::expect_error(gComp(data = cvdd, Y = "glucoseyear6", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), outcome.type = "gaussian", R = 4))
})


testthat::test_that("Y and X vars are both provided if no formula is given", {
  testthat::expect_error(gComp(data = cvdd, Y = "cvd_dth", Z = c("BMI", "SEX"), outcome.type = "binary", R = 5))
  testthat::expect_error(gComp(data = cvdd, X = "DIABETES", Z = c("AGE", "SEX"), outcome.type = "binary", R = 4))
})


testthat::test_that("outcome is expected value", {
  ## binary outcome
  testthat::expect_equal(round(gComp(data = cvdd, formula = cvd_dth ~ DIABETES + BMI, outcome.type = "binary", R = 4)$results.df[[1,4]], 2),  0.4)
  ## binary outcome
  testthat::expect_equal(gComp(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type  = "binary", R = 4)$R, 4)
  ## rate outcome
  testthat::expect_equal(round(gComp(data = cvdd %>%
                                 dplyr::mutate(cvd_dth = as.numeric(as.character(cvd_dth)),
                                               timeout = as.numeric(timeout)), 
                               Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), 
                               outcome.type = "rate", rate.multiplier = 365.25*100, offset = "timeout", R = 5)$results.df[1,4, drop = T], 1), 2.2)
  ## rate outcome with subgroup
  testthat::expect_equal(unname(round(gComp(data = cvdd %>%
                                 dplyr::mutate(cvd_dth = as.numeric(as.character(cvd_dth)),
                                               timeout = as.numeric(timeout)), 
                               Y = "cvd_dth", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), subgroup = "SEX",
                               outcome.type = "rate", rate.multiplier = 365.25*100, offset = "timeout", R = 4)$results.df[4,5, drop = T], 2)), 2.04)
  ## continuous outcome
  testthat::expect_equal(gComp(data = cvdd, Y = "glucoseyear6", X = "DIABETES", Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), outcome.type = "continuous", R = 4)$results.df[1,4, drop = T], 61.6257)
  ## count outcome
  testthat::expect_equal(round(gComp(data = cvdd, formula = "nhosp ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP", outcome.type = "count", R = 4)$results.df[1,4, drop = T], 2), 0.05)
  ## negative binomial outcome
  set.seed(1298)
  testthat::expect_equal(round(gComp(data = cvdd %>%
                                       dplyr::rowwise() %>%
                                       dplyr::mutate(outpt_clinic_visits = ifelse(DIABETES == 0, rnbinom(n = 1, size = 1.22, mu = 6), rnbinom(n = 1, mu = 14, size = 4.2))) %>%
                                       dplyr::ungroup(),
                                     formula = outpt_clinic_visits ~ DIABETES + AGE + SEX + BMI + PREVHYP, outcome.type = "count_nb", R = 10)$results.df[1,4, drop = T], 2), 7.91)
  ## binary outcome, categorical exposure
  testthat::expect_equal(unname(round(gComp(data = cvdd, Y = "cvd_dth", X = "bmicat", outcome.type = "binary", R = 5)$results.df[10,4, drop = T], 2)), 1.58)
  ## binary outcome, continuous exposure
  testthat::expect_equal(round(gComp(data = cvdd, Y = "cvd_dth", X = "AGE", Z = c("BMI", "SEX", "DIABETES", "CURSMOKE", "PREVHYP"), outcome.type = "binary", exposure.scalar = 10, R = 5)$results.df[4,4, drop = T], 2), 4.42)
  ## binary outcome, continuous exposure, subgroups
  testthat::expect_equal(unname(round(gComp(data = cvdd, Y = "cvd_dth", X = "AGE", Z = c("BMI", "SEX", "DIABETES"), subgroup = "SEX", outcome.type = "binary", exposure.scalar = 10, R = 5)$results.df[1,5, drop = T], 2)), 0.24)
  ## using clusterID
  testthat::expect_equal(round(gComp(data = cvdd %>% dplyr::mutate(id = substr(RANDID, 1,2),
                                                                   id.cat = factor(id)), 
                                     formula = "nhosp ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP", clusterID = "id.cat", 
                                     outcome.type = "count", R = 4)$results.df[1,4, drop = T], 2), 0.05)
  ## negative binomial count and rate (count_nb, rate_nb)
  set.seed(424)
  testthat::expect_equal(round(gComp(data = cvdd %>% 
                                               dplyr::rowwise() %>%
                                               dplyr::mutate(outpt_clinic_visits = ifelse(DIABETES == 0, rnbinom(n = 1, mu = 6, size = 1.22), rnbinom(n = 1, mu = 14, size = 4.2))) %>%
                                               dplyr::ungroup(),
                                             formula = "outpt_clinic_visits ~ DIABETES + AGE + SEX + BMI + PREVHYP", outcome.type = "count_nb", R = 10)$results.df[1,6, drop = T], 2), 9.27)
  testthat::expect_equal(round(gComp(data = cvdd %>% 
                                               dplyr::rowwise() %>%
                                               dplyr::mutate(outpt_clinic_visits = ifelse(DIABETES == 0, rnbinom(n = 1, mu = 6, size = 1.22), rnbinom(n = 1, mu = 14, size = 4.2))) %>%
                                               dplyr::ungroup(),
                                             formula = "outpt_clinic_visits ~ DIABETES + AGE + SEX + BMI + PREVHYP", outcome.type = "rate_nb", subgroup = "SEX", offset = "AGE", R = 5)$results.df[4,5, drop = T], 2), 2.26)
  
})

testthat::test_that("class is correct", {
  testthat::expect_is(gComp(data = cvdd, formula = cvd_dth ~ DIABETES + AGE, outcome.type = "binary", R = 4), "gComp")
})

