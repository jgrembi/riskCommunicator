#' Take predicted dataframe and calculate the outcome (risk difference/ratio,
#' incidence rate difference/ratio, mean difference, and/or number needed to
#' treat)
#'
#' @param predict.df (Required) A data.frame output from the
#'   \code{make_predict_df} function with predicted outcome for each observation
#'   at each level of treatment/exposure.
#' @param outcome.type (Required) Character argument to describe the outcome
#'   type. Acceptable responses, and the corresponding error distribution and
#'   link function used in the \code{glm}, include: \describe{
#'   \item{binary}{(Default) A binomial distribution with link = 'logit' is
#'   used.} \item{count}{A Poisson distribution with link = 'log' is used.}
#'   \item{rate}{A Poisson distribution with link = 'log' is used.}
#'   \item{continuous}{A gaussian distribution with link = 'identity' is used.}
#'   }
#' @param X (Required) Character argument which provides variable identifying
#'   exposure/treatment group assignment.
#' @param rate.multiplier (Optional, only applicable for rate outcomes) Default
#'   1. Numeric value to multiply to the rate-based effect measures. This option
#'   facilitates reporting effects with interpretable person-time denominators.
#'   For example, if the person-time variable (offset) is in days, a multiplier
#'   of 365*100 would result in estimates of rate differences per 100
#'   person-years.
#'
#' @return A list containing the following components: \describe{
#'   \item{results_tbl}{A tibble of the results for each observation with
#'   treatment/no treatment for both risk and odds.} \item{res}{The calculated
#'   results for the applicable measures (based on the outcome.type): Risk
#'   Difference, Risk Ratio, Odds Ratio, Incidence Risk Difference, Incidence
#'   Risk Ratio, Mean Difference, Number Needed to Treat} }
#' @importFrom dplyr select pull
#' @importFrom tibble tibble
#' @importFrom stringr str_subset str_split
#' @importFrom tidyselect starts_with

get_results_tibble <- function(predict.df, outcome.type, X, rate.multiplier = 1) {

  col.names <- unique(unlist(stringr::str_split(names(predict.df), "_"))) %>%
    stringr::str_subset(pattern = as.character(X))
  noTx.predict = predict.df %>%
    dplyr::select(tidyselect::starts_with(col.names[1])) %>%
    dplyr::pull()
  Tx.predict = predict.df %>%
    dplyr::select(tidyselect::starts_with(col.names[2])) %>%
    dplyr::pull()
  if (outcome.type == "binary") {
    results_tbl <- tibble::tibble(noTx =  exp(noTx.predict)/(1 + exp(noTx.predict)),
                                  Tx = exp(Tx.predict)/(1 + exp(Tx.predict)),
                                  noTx_odds = exp(noTx.predict),
                                  Tx_odds = exp(Tx.predict)) 
  } else if (outcome.type %in% c("rate", "count")) {
    results_tbl <- tibble::tibble(noTx =  exp(noTx.predict),
                                  Tx = exp(Tx.predict),
                                  noTx_odds = NA,
                                  Tx_odds = NA)
  } else if (outcome.type == "continuous") {
    results_tbl <- tibble::tibble(noTx =  noTx.predict,
                                  Tx = Tx.predict,
                                  noTx_odds = NA,
                                  Tx_odds = NA)
  } else {
    stop("outcome.type not supported")
  }
  
  diff <- mean(results_tbl$Tx, na.rm = T) - mean(results_tbl$noTx, na.rm = T)
  ratio <- mean(results_tbl$Tx, na.rm = T)/mean(results_tbl$noTx, na.rm = T)
  ratio_odds <- ifelse(outcome.type == "binary", mean(results_tbl$Tx_odds, na.rm = T)/mean(results_tbl$noTx_odds, na.rm = T), NA)
  res <- c(`Risk Difference` = ifelse(outcome.type %in% c("binary"), diff, NA),
           `Risk Ratio` = ifelse(outcome.type %in% c("binary"), ratio, NA),
           `Odds Ratio` = ifelse(outcome.type %in% c("binary"), ratio_odds, NA),
           `Incidence Rate Difference` = ifelse(outcome.type %in% c("rate"), (diff*rate.multiplier), ifelse(outcome.type == "count", diff, NA)),
           `Incidence Rate Ratio` = ifelse(outcome.type %in% c("rate", "count"), ratio, NA),#only for poisson)
           `Mean Difference` = ifelse(outcome.type %in% c("continuous"), diff, NA),
           `Number needed to treat` = ifelse(outcome.type %in% c("binary"), 1/diff, NA))
  
  return(list(results_tbl, res))
}
