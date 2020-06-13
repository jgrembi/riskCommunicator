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
#' @param rate.multiplier (Optional, only applicable for rate outcomes) Default
#'   1. Numeric value to multiply to the rate-based effect measures. This option
#'   facilitates reporting effects with interpretable person-time denominators.
#'   For example, if the person-time variable (offset) is in days, a multiplier
#'   of 365*100 would result in estimates of rate differences per 100
#'   person-years.
#'
#' @return A list containing the calculated results for the applicable measures
#'   (based on the outcome.type): Risk Difference, Risk Ratio, Odds Ratio,
#'   Incidence Risk Difference, Incidence Risk Ratio, Mean Difference, Number
#'   Needed to Treat, Average Tx (average predicted outcome of all observations with 
#'   treatment/exposure), and Average noTx (average predicted outcome of all 
#'   observations without treatment/exposure)
#'   
#' @importFrom dplyr select pull
#' @importFrom tidyselect starts_with

get_results_dataframe <- function(predict.df, outcome.type, rate.multiplier) {
  
  col.names <- names(predict.df)
  
  noTx.predict <-  predict.df %>%
    dplyr::select(tidyselect::starts_with(col.names[1])) %>%
    dplyr::pull() 
  
  noTx_odds <- noTx.predict %>%
    exp() %>%
    mean(na.rm = T)
  
  Tx.predict <-  predict.df %>%
    dplyr::select(tidyselect::starts_with(col.names[2])) %>%
    dplyr::pull() 
  
  Tx_odds <- Tx.predict %>%
    exp() %>%
    mean(na.rm = T)
  
  if (outcome.type == "binary") {
    results_tbl <- data.frame(noTx =  noTx_odds/(1 + noTx_odds),
                                  Tx = Tx_odds/(1 + Tx_odds),
                                  noTx_odds = noTx_odds,
                                  Tx_odds = Tx_odds) 
  } else if (outcome.type %in% c("rate", "count")) {
    results_tbl <- data.frame(noTx =  noTx_odds,
                                  Tx = Tx_odds,
                                  noTx_odds = NA,
                                  Tx_odds = NA)
  } else if (outcome.type == "continuous") {
    results_tbl <- data.frame(noTx =  mean(noTx.predict, na.rm = T),
                                  Tx = mean(Tx.predict, na.rm = T),
                                  noTx_odds = NA,
                                  Tx_odds = NA)
  } else {
    stop("outcome.type not supported")
  }
  
  diff <- results_tbl$Tx - results_tbl$noTx
  ratio <- results_tbl$Tx/results_tbl$noTx
  ratio_odds <- results_tbl$Tx_odds/results_tbl$noTx_odds
  res <- c(`Risk Difference` = ifelse(outcome.type == "binary", diff, NA),
           `Risk Ratio` = ifelse(outcome.type == "binary", ratio, NA),
           `Odds Ratio` = ifelse(outcome.type == "binary", ratio_odds, NA),
           `Incidence Rate Difference` = ifelse(outcome.type == "rate", (diff*rate.multiplier), ifelse(outcome.type == "count", diff, NA)),
           `Incidence Rate Ratio` = ifelse(outcome.type %in% c("rate", "count"), ratio, NA),
           `Mean Difference` = ifelse(outcome.type == "continuous", diff, NA),
           `Number needed to treat` = ifelse(outcome.type == "binary", 1/diff, NA),
           `Average Tx` = ifelse(outcome.type == "rate", rate.multiplier*results_tbl$Tx, results_tbl$Tx),
           `Average noTx` = ifelse(outcome.type == "rate", rate.multiplier*results_tbl$noTx, results_tbl$noTx))
  
  return(res)
}
