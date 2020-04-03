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
#'
#' @return A data.frame of predicted outcomes for each level of
#'   treatment/exposure.  Additional columns are provided for each subgroup
#'   *x*treatment, if specified.
#'
#' @importFrom dplyr mutate
#' @importFrom purrr map_dfc
#' @importFrom stats predict
#'   
make_predict_df <- function(glm.res, df, X, subgroup = NULL) {
  # For each observation in the dataframe, predict (using the supplied glm result) data for each level of the treatment/exposure.
  result.output.final <- purrr::map_dfc(levels(df[[X]]), function(x) {  # For each level of X (treatment/exposure), do the following...
    if(!is.null(subgroup)) { # If subgroups are present, do for each subgroup separately.
      res.df <- purrr::map_dfc(levels(df[[subgroup]]), function(s) {
        out <- stats::predict(glm.res, newdata = df %>% dplyr::mutate(!!X := factor(x, levels = levels(df[[X]])),
                                                                      !!subgroup := factor(s, levels = levels(df[[subgroup]]))))
        return(out)
      })
    } else {
      res.df <- stats::predict(glm.res, newdata = df %>% dplyr::mutate(!!X := factor(x, levels = levels(df[[X]]))))
    }
    return(res.df)
  })
  
  # Change column names to levels of X or level of X "_" level of subgroup, if present. 
  if (!is.null(subgroup)) {
    names(result.output.final) <- sapply(levels(df[[X]]), function(x) paste(paste0(X,x), sapply(levels(df[[subgroup]]), function(s) paste0(subgroup,s)), sep = "_"))
  } else {
    names(result.output.final) <- sapply(levels(df[[X]]), function(x) paste0(X,x))
  }
  
  return(result.output.final)
}

 