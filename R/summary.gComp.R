#' Print a summary of a gComp class object.
#'
#' @description Takes a \code{gComp} object produced by \code{gComp()} and 
#'   produces various useful summaries from it.
#'  
#' @param object (Required) An object of class \code{gComp} as produced by \code{gComp()}.
#' @param x (Required) An object of class \code{summary.gComp} as produced by \code{summary.gComp()}.
#' @param ... Further arguments passed to or from other methods.
#' 
#' @return Returns the formula, family (with link function), contrast evaluated, resulting  
#'   point estimate and 95\% confidence intervals of the parameters estimated, and the  
#'   underlying glm used for model predictions.
#'   
#' @export
#' @method summary gComp 
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
#' summary(diabetes.result)
#'
#' @importFrom methods is
#'
#' @keywords summary.gComp
#' 
summary.gComp <- function(object, ...) {
  if(!methods::is(object, "gComp")) stop("Object supplied is not of class 'gComp'")
  
  res <- list(formula = object$formula, 
              family = object$family$family, 
              link = object$family$link,
              contrast = object$contrast,
              summary = object$summary, 
              underlying_glm = object$glm.result)
  
  class(res) <- "summary.gComp"
  return(res)
}

#' @rdname summary.gComp
#' @export
#' @method print summary.gComp
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
  invisible(x)
}