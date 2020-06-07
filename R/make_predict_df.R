#' Using \code{glm} results, predict outcomes for each individual at each level
#' of treatment/exposure
#'
#' @param glm.res (Required) A fitted object of class inheriting from "glm" that
#'   will be used with new dataset for prediciton.
#' @param df (Required) A new data frame in which to look for variables with
#'   which to predict. This is equivalent to the \code{newdata} argument in
#'   predict.glm.
#' @param X (Required) Character argument which provides variable identifying
#'   exposure/treatment group assignment.
#' @param subgroup (Optional) Default NULL. Character argument of the variable
#'   name to use for subgroup analyses. Variable automatically transformed to a
#'   factor within the function if not supplied as such.
#' @param offset (Optional, only applicable for rate outcomes) Default NULL.
#'   Character argument which specifies the person-time denominator for rate
#'   outcomes to be included as an offset in the Poisson regression model.
#'   Numeric variable should be on the linear scale; function will take natural
#'   log before including in the model.
#'
#' @return A data.frame of predicted outcomes for each level of
#'   treatment/exposure.  Additional columns are provided for each subgroup
#'   *x*treatment, if specified.
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map_dfc
#' @importFrom stats predict
#'   
make_predict_df <- function(glm.res, df, X, subgroup = NULL, offset = NULL) {
 
  # Define variable offset2 in dataset since it was used in the original glm.  We set it to 1 because log(1) = 0 and therefore no offset is used in the 
  if (!is.null(offset)) {
    df <- df %>%
      dplyr::mutate(offset2 = 1)
  }
  
  if (is.numeric(df[[X]])) {
    predict.levels <- c(0,1)
  } else {
    predict.levels <- levels(df[[X]])
  }
   # For each observation in the dataframe, predict (using the supplied glm result) data for each level of the treatment/exposure.
  result.output.final <- suppressMessages(purrr::map_dfc(predict.levels, function(x) {  # For each level of X (treatment/exposure), do the following...
    if (is.numeric(df[[X]])) {
      newdata <- df %>% dplyr::mutate(!!X := x)
    } else {
      newdata <- df %>% dplyr::mutate(!!X := factor(x, levels = predict.levels))
    }
    if(!is.null(subgroup)) { # If subgroups are present, do for each subgroup separately.
      res.df <- suppressMessages(purrr::map_dfc(levels(df[[subgroup]]), function(s) {
        out <- stats::predict(glm.res, newdata = newdata %>% dplyr::mutate(!!subgroup := factor(s, levels = levels(df[[subgroup]]))))
        return(out)
      }))
    } else {
      res.df <- stats::predict(glm.res, newdata = newdata)
    }
    return(res.df)
  }))
  
  # Change column names to levels of X or level of X "_" level of subgroup, if present. 
  if (!is.null(subgroup)) {
    names(result.output.final) <- sapply(predict.levels, function(x) paste(paste0(X,x), sapply(levels(df[[subgroup]]), function(s) paste0(subgroup,s)), sep = "_"))
  } else {
    names(result.output.final) <- sapply(predict.levels, function(x) paste0(X,x))
  }
  
  return(result.output.final)
}

 