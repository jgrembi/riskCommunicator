context("test-summary.gcomp")

testthat::expect_output(print(summary(gComp(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type  = "binary", R = 4))))


testthat::expect_type(summary(gComp(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type  = "binary", R = 4)), "list")

testthat::expect_s3_class(summary(gComp(data = cvdd, Y = "DEATH", X = "DIABETES", Z = c("SEX", "AGE"), outcome.type  = "binary", R = 4)), class = "summary.gComp")
