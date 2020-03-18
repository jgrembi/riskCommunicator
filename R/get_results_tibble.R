#' Take predicted dataframe and calculate the outcome (risk difference/ratio, incidence rate difference/ratio, marginal difference, and number needed to treat)
#'
#' @param predict.df 
#' @param outcome.type 
#' @param X 
#' @param rate.multiplier 
#'
#' @value
#' @export
#'
#' @examples
#' 
#'
#' @importFrom dplyr select pull
#' @importFrom tibble tibble
#' @importFrom stringr str_subset str_split
#' @importFrom tidyselect starts_with

get_results_tibble <- function(predict.df, outcome.type, X, rate.multiplier) {
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
           `Marginal Difference` = ifelse(outcome.type %in% c("continuous"), diff, NA),
           `Number needed to treat` = ifelse(outcome.type %in% c("binary","count"), 1/diff, NA))
  
  return(list(results_tbl, res))
}
