#' Print estimates of difference and ratio effects obtained in the bootstrap
#' computations of the g-computation
#'
#' @description Print results from bootstrap computations of the g-computation
#'
#' @param x (Required) An object of class \code{gComp} as produced by \code{gComp()}.
#' @param ... (Optional) Further arguments passed to or from other methods.
#'
#' @return For \code{print} returns the formula and resulting point estimate and 95% 
#' confidence intervals of the  difference and ratio. 
#' For \code{summary} resturns the formula, family and link function, contrasts, 
#' parameter estimates with 95% CIs, and a summary of the underlying glm used to 
#' make predictions.
#'   
#' @export
#' @method print gComp summary gComp
#'
#' @examples
#' ## Obtain the risk difference and risk ratio for cardiovascular disease or 
#' ## death between patients with and without diabetes, while controlling for
#' ## age, sex, BMI, whether the individual is currently a smoker, and 
#' ## if they have a history of hypertension.
#' data(cvdd)
#' set.seed(4832)
#' diabetes.result <- gComp(data = cvdd, Y = "cvd_dth", X = "bmicat",
#' Z = c("AGE", "SEX", "DIABETES", "CURSMOKE", "PREVHYP"), outcome.type = "binary", R = 20)
#' print(diabetes.result)
#' summary(diabetes.result)
#'
#'
#' @keywords print.gComp summary.gComp




print.gComp <- function(x, ...) {
  cat("Formula:", "\n")
  cat(format(x$formula, width = 60), "\n") 
  cat("\n")
  cat("Parameter estimates:", "\n")
  print(format(x$summary))
  
}

summary.gComp <- function(object, ...) {
  if(!is(object, "gComp")) stop("Object supplied is not of class 'gComp'")
  
  res <- list(formula = object$formula, 
              family = object$family$family, 
              link = object$family$link,
              contrast = object$contrast,
              summary = object$summary, 
              underlying_glm = object$glm.result)
  
  class(res) <- "summary.gComp"
  return(res)
}

print.summary.gComp <- function(x, ...) {
  cat("Formula:", "\n")
  cat(format(x$formula, width = 60), "\n") 
  cat("\n")
  cat("Family:", x$family, "\n")
  cat("Link function:", x$link, "\n")
  cat("\n")
  cat("Contrast:", x$contrast, "\n")
  cat("\n")
  cat("Parameter estimates:", "\n")
  print(format(x$summary))
  cat("\n")
  cat("Underlying glm:")
  print(x$underlying_glm)
  # NextMethod("print")
  # invisible(x)
}