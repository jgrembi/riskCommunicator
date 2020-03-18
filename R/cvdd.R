#' A subset of the \code{framingham} data
#'
#' A subset of the \code{framingham} dataset containing the following changes:
#' \itemize{
#' \item{removal of all observations where PERIOD == 2 or PERIOD == 3 (i.e. keep only PERIOD == 1)}
#' \item{removal of all observations where PREVCHD == 1 (i.e. all patients with coronary heart disease)}
#' \item{created a new variable, \code{cvd_dth} signifying an outcome of cardiovascular disease OR death (i.e. if the patient had either CVD or DEATH, this new variable is 1, otherwise 0)}
#' \item{created a new variable, \code{timeout}, which calculates the number of days from the start of the study to cardiovascular #' disease or death}
#' \item{created a new variable, \code{logpdays}, which is the log of \code{timeout}}
#' \item{created a new variable, \code{nhosp}, which is a simulated number of hospitalizations}
#' }
#' @name cvdd
#' @docType data
#' @usage data(cvdd)
#' @keywords cvdd data
#'
NULL