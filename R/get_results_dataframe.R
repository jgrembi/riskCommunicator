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
#'   used.} 
#'   \item{count}{A Poisson distribution with link = 'log' is used.}
#'   \item{count_nb}{A negative binomial model with link = 'log' is used, where the theta 
#'   parameter is estimated internally; ideal for over-dispersed count data.}
#'   \item{rate}{A Poisson distribution with link = 'log' is used; ideal for 
#'    events/person-time outcomes.} 
#'    \item{rate_nb}{A negative binomial model with link = 'log' is used, where the theta 
#'   parameter is estimated internally; ideal for over-dispersed events/person-time outcomes.}
#'    \item{continuous}{A gaussian distribution with link = 'identity' is used.}
#'   }
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

get_results_dataframe <- function(predict.df, outcome.type) {
  
  col.names <- names(predict.df)
  
  noTx.predict <-  predict.df %>%
    dplyr::select(tidyselect::starts_with(col.names[1])) %>%
    dplyr::pull() 
  
  noTx_odds <- noTx.predict %>%
    exp(.) 
  
  Tx.predict <-  predict.df %>%
    dplyr::select(tidyselect::starts_with(col.names[2])) %>%
    dplyr::pull() 
  
  Tx_odds <- Tx.predict %>%
    exp(.) 
  
  if (outcome.type == "binary") {
    results_tbl <- data.frame(noTx =  noTx_odds/(1 + noTx_odds),
                                  Tx = Tx_odds/(1 + Tx_odds),
                                  noTx_odds = noTx_odds,
                                  Tx_odds = Tx_odds) 
  } else if (outcome.type %in% c("rate", "count", "count_nb", "rate_nb")) {
    results_tbl <- data.frame(noTx =  noTx_odds,
                                  Tx = Tx_odds,
                                  noTx_odds = NA,
                                  Tx_odds = NA)
  } else if (outcome.type == "continuous") {
    results_tbl <- data.frame(noTx =  noTx.predict,
                                  Tx = Tx.predict,
                                  noTx_odds = NA,
                                  Tx_odds = NA)
  } else {
    stop("outcome.type not supported")
  }
  
  diff <- mean(results_tbl$Tx, na.rm = T) - mean(results_tbl$noTx, na.rm = T)
  ratio <- mean(results_tbl$Tx, na.rm = T)/mean(results_tbl$noTx, na.rm = T)
  ratio_odds <- mean(results_tbl$Tx_odds, na.rm = T)/mean(results_tbl$noTx_odds, na.rm = T)
  res <- c(`Risk Difference` = ifelse(outcome.type == "binary", diff, NA),
           `Risk Ratio` = ifelse(outcome.type == "binary", ratio, NA),
           `Odds Ratio` = ifelse(outcome.type == "binary", ratio_odds, NA),
           `Incidence Rate Difference` = ifelse(outcome.type %in% c("rate", "rate_nb","count", "count_nb"), diff, NA),
           `Incidence Rate Ratio` = ifelse(outcome.type %in% c("rate", "count", "count_nb", "rate_nb"), ratio, NA),
           `Mean Difference` = ifelse(outcome.type == "continuous", diff, NA),
           `Number needed to treat` = ifelse(outcome.type == "binary", 1/diff, NA),
           `Average Tx` = mean(results_tbl$Tx, na.rm = T),
           `Average noTx` = mean(results_tbl$noTx, na.rm = T))
  
  return(res)
}
