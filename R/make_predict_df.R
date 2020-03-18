#' Using the \code{glm} results, predict outcomes for each individual at each level of treatment/exposure
#'
#' @param glm.res 
#' @param df 
#' @param X 
#' @param subgroup 
#' @param offset 
#'
#' @value
#' @export
#'
#' @examples
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map_dfc
#' @importFrom stats predict
#'
make_predict_df <- function(glm.res, df, X, subgroup = NULL, offset = NULL) {
  
  if (!is.null(offset)) {
    df <- df %>%
      dplyr::mutate(offset2 = 1)
  }
  
  result.output.final <- purrr::map_dfc(levels(df[[X]]), function(x) {
    if(!is.null(subgroup)) {
      res.df <- purrr::map_dfc(levels(df[[subgroup]]), function(s) {
        out <- stats::predict(glm.res, newdata = df %>% dplyr::mutate(!!X := factor(x, levels = levels(df[[X]])),
                                                                      !!subgroup := factor(s, levels = levels(df[[subgroup]]))))
        return(out)
      })
    } else {
      res.df <- stats::predict(glm.res, newdata = df %>% dplyr::mutate(!!X := factor(x, levels = levels(df[[X]]))))
       # res.df <- stats::predict(glm.res, newdata = df %>% dplyr::mutate(DIABETES = 1))
    }
    return(res.df)
  })
  
  
  if (!is.null(subgroup)) {
    names(result.output.final) <- sapply(levels(df[[X]]), function(x) paste(paste0(X,x), sapply(levels(df[[subgroup]]), function(s) paste0(subgroup,s)), sep = "_"))
  } else {
    names(result.output.final) <- sapply(levels(df[[X]]), function(x) paste0(X,x))
  }
  
  # result.output.final <- result.output.final %>%
  # dplyr::select(tidyselect::starts_with(paste(X), tidyselect::starts_with(subgroup)))
  
  return(result.output.final)
}


###
#### Make sure this is all happening correctly in the above code
###


# ##Predictions
# df_0 <- data %>%
#   dplyr::mutate(!!X := 0, 
#                 !!X := factor(!!X))
# df_1 <- data %>%
#   dplyr::mutate(!!X := 1, 
#                 !!X := factor(!!X))
# if (!is.null(offset)) {
#   df_0 <- df_0 %>% 
#     dplyr::mutate(offsetlog = 0)
#   df_1 <- df_1 %>%
#     dplyr::mutate(offsetlog = 0)
# }
# predict_noTx <- stats::predict(glm.result, newdata = df_0)
# predict_Tx <- stats::predict(glm.result, newdata = df_1)
# predict_naturalCourse <- stats::predict(glm.result, newdata = data)
# if (outcome.type == "binary") {
#   results_tbl <- tibble::tibble(noTx =  exp(predict_noTx)/(1+exp(predict_noTx)), 
#                                 Tx = exp(predict_Tx)/(1 + exp(predict_Tx)),  
#                                 natCourse = exp(predict_naturalCourse)/(1 + exp(predict_naturalCourse)),
#                                 noTx_odds = exp(predict_noTx),
#                                 Tx_odds = exp(predict_Tx), 
#                                 natCourse_odds = exp(predict_naturalCourse))
# } else if (outcome.type %in% c("count", "rate")) {
#   results_tbl <- tibble::tibble(noTx =  exp(predict_noTx), 
#                                 Tx = exp(predict_Tx), 
#                                 natCourse = exp(predict_naturalCourse), 
#                                 actual = data[,Y])
# } else if (outcome.type == "continuous") {
#   results_tbl <- tibble::tibble(noTx =  predict_noTx, 
#                                 Tx = predict_Tx, 
#                                 natCourse = predict_naturalCourse,  
#                                 actual = data[,Y])
# } else {
#   stop("This package only supports binary/dichotomous, count/rate, or continuous outcome variable models")
# }
#   