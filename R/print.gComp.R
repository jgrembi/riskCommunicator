#' Print estimates of difference and ratio effects obtained in the bootstrap
#' computations of the g-computation
#'
#' @description Print results from bootstrap computations of the g-computation
#'
#' @param x (Required) An object of class \code{gComp} as produced by \code{gComp()}.
#' @param ... (Optional) Further arguments passed to or from other methods.
#'
#' @return Returns the formula and resulting point estimate and 95\% 
#' confidence intervals of the difference and ratio. 
#' 
#'   
#' @export
#' @method print gComp 
#'
#' @examples
#' ## Obtain the risk difference and risk ratio for cardiovascular disease or 
#' ## death between patients with and without diabetes, while controlling for
#' ## age, sex, BMI, whether the individual is currently a smoker, and 
#' ## if they have a history of hypertension.
#' data(cvdd)
#' set.seed(4832)
#' diabetes.result <- gComp(cvdd, 
#'    formula = "cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP",
#'    outcome.type = "binary", R = 20)
#' print(diabetes.result)
#'
#'
#' @keywords print.gComp 




print.gComp <- function(x, ...) {
  cat("Formula:", "\n")
  cat(format(x$formula, width = 60), "\n") 
  cat("\n")
  cat("Parameter estimates:", "\n")
  print(format(x$summary))
  
}
