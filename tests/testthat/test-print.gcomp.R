context("test-print.gcomp")

testthat::expect_output(print(gComp(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type  = "binary", R = 4)))

