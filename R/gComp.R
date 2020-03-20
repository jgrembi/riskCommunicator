#' Perform g-computation to estimate difference and ratio effects of an outcome exposure
#'
#' @description Obtain a point estimate and 95% confidence limits for the difference and ratio between treatment and non-treatment (or exposed/not exposed) groups.
#'
#' @param data (Required) Data.frame or tibble containing variables for \code{Y}, \code{X}, and \code{Z} or with variables matching the model variables specified in a user-supplied formula.
#' @param outcome.type (Required) Character argument to describe the outcome type. Acceptable responses, and the corresponding error distribution and link function used in the \code{glm}, include:
#'  \describe{
#'  \item{binary} (Default) A binomial distribution with link = 'logit' is used.
#'  \item{count} A Poisson distribution with link = 'log' is used.
#'  \item{rate} A Poisson distribution with link = 'log' is used.
#'  \item{continuous} A gaussian distribution with link = 'identity' is used. 
#' }
#' @param formula (Optional) Default NULL. An object of class "formula" (or one that can be coerced to that class) which provides the model formula for the \code{glm} function to be used internally. 
#' The first predictor (after the "~") is assumed to be the exposure variable.
#' Can be supplied as a character or formula object, the function will internally convert it to a formula if not supplied as such. 
#' If no formula is provided, Y and X must be provided.
#' @param Y (Optional) Default NULL. Character argument which provides the response variable that will be supplied to the \code{glm} function internally.  
#' Must also provide \code{X} in order for the function to work.  Can optionally provide a formula instead of \code{Y} and \code{X} variables.
#' @param X (Optional) Default NULL. Character argument which provides variable identifying exposure/treatment group assignment that will be supplied to the \code{glm} function internally. 
#' Must also provide \code{Y} in order for the function to work.   
#' Preferrably, \code{X} is supplied as a factor with the lowest level set to the desired comparator. 
#' Numeric variables are accepted, and coerced to factor with lowest level being the smallest number. 
#' Character variables are not accepted and will throw an error.
#' Can optinoally provide a formula instead of \code{Y} and \code{X} variables.
#' @param Z (Optional) Default NULL. List or single character vector which provides the names of covariates or other variables to adjust for in the \code{glm} function to be used internally.  
#' For only one covariate, can be a single character object, for multiple a vector of quoted variable names is required. Does not allow interaction terms.
#' @param subgroup (Optional) Default NULL. Character argument of the variable name to use for subgroup analyses. 
#' @param offset (Required if using \code{outcome.type = "rate"}) Default NULL. Character argument which identifies the variable to use for offset. 
#' Internal function converts offset to \code{log} scale, so variable should be provided on the linear scale. 
#' @param rate.multiplier (Optional) Default 1. Numeric argument to identify the multiplier to provide rate outcome in desired units. Only used if outcome.type == "rate." 
#' For example, the rate for an offset provided in days could be converted to years by supplying rate.multiplier = 365. 
#' @param R (Optional) Default 200. The number of bootstraps to be conducted to produce the bootstrap confidence interval of the estimate.
#' @param clusterID (Optional) Default NULL. Character vector of the variable name to use as the level for resampling if the bootstrap resampling should be done at any level other than random resampling of the dataset.
#' @param ... Other named arguments for \code{glm} which are passed unchanged each time it is called. Arguments to \code{glm} should follow the specifications in the \code{\link{glm}} package.
#' 
#' @value the returned value is an object of class \code{gComp} containing the following:
#' \itemize{
#' \item{"summary"} {summary providing parameter estimates and 95% confidence limits of the outcome difference and ratio}
#' \item{"results.df} {data.frame with parameter estimates, 2.5% confidence limit, and 97.5% confidence limit each as a column}
#' \item{"n"} {number of observations in the original dataset}
#' \item{"R"} {number of bootstrap iterations}
#' \item{"boot.result"} {a boot object containing the results of the \code{R} bootstrap iterations of the g-computation} 
#' \item{"contrast"} {the contrast levels compared}
#' \item{"family"} {the error distribution used in the model}
#' \item{"formula"} {the model formula used to fit the \code{glm}}
#' \item{"predictedDat"} {a tibble with the predicted values for the naturnal course, and both treatment and no treatment counterfactual predicitions for each observation in the original dataset}

#' @export
#'
#' @examples
#' ## Obtain the risk difference and risk ratio for cardiovascular disease or death between 
#' ## patients with and without diabetes.  
#' data(cvdd)
#' diabetes <- gComp(cvdd, formula = "cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP", 
#' outcome.type = "binary", R = 200)

#' 
#' @importFrom rsample bootstraps analysis
#' @importFrom stats quantile as.formula
#' @importFrom dplyr rename n_distinct left_join mutate group_by ungroup select summarise_at
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom tidyr nest unnest gather spread unnest_legacy
#' @importFrom purrr map_dfc 
#' @importFrom furrr future_map_dfr 
#' @importFrom tidyselect vars_select starts_with contains matches
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' 
#' 
#' @keywords gComp
gComp <- function(data, 
                  outcome.type =  c("binary", "count","rate", "continuous"), 
                  formula = NULL, 
                  Y = NULL, 
                  X = NULL, 
                  Z = NULL, 
                  subgroup = NULL,
                  offset = NULL, 
                  rate.multiplier = 1, 
                  R = 200,
                  clusterID = NULL,
                  ...) {
  ###need to check if X is categorical or 
  if (!is.null(X)) {
    X.type = ifelse(is.factor(data[[X]]), "categorical", ifelse(is.numeric(data[[X]]), "numeric", stop("X must be a factor or numeric variable")))
  } else {
    formula = stats::as.formula(formula)
    X <- rlang::sym(all.vars(formula[[3]])[1])
    X.type = ifelse(is.factor(data[[X]]), "categorical", ifelse(is.numeric(data[[X]]), "numeric", stop("X must be a factor or numeric variable")))
  }
if (!is.null(clusterID)) clusterID <- rlang::sym(clusterID)
  if (X.type == "numeric") {
    stop("Numeric explanitory variables not allowed at this time, we are still working on that feature!")
  }
  
  ## Get point estimate for diff and ratio
  ptEstimate <- pointEstimate(data, formula = formula, Y = Y, X = X, Z = Z, subgroup = subgroup, outcome.type = outcome.type, offset = offset, rate.multiplier = rate.multiplier, ...)
  ## Nest df by bootstrap resampling unit
  if (is.null(clusterID)) {
    # ## run R bootstrap iterations to get 95% CI for diff and ratio
    # fun.statistic <- function(x, idx, outcome.type = outcome.type, offset = offset, formula = formula, Y = Y, X = X, Z = Z) {
    #   estimate <- pointEstimate(x[idx,], outcome.type = outcome.type, offset = offset, formula = formula, Y = Y, X = X, Z = Z)
    #   if !is.null(subgroup) {
    #     return(estimate$parameterEstimates)
    #   }
    #   return(estimate$parameterEstimates$Results)
    # }
    # ## I want to make sure I'm still allowing for the user to supply extra arguments to the boot call (e.g. weights).  Not sure this will allow that.
    # boot.res <- boot::boot(data = data, statistic = fun.statistic, R = R, parallel = parallel, ncpus = ncpus, formula = formula, outcome.type = outcome.type, offset = offset, Y = Y, X = X, Z = Z)
    # if (!is.null(subgroup)) {
    #   boot.res <- boot::boot(data = data, statistic = fun.statistic, R = R, parallel = parallel, ncpus = ncpus, formula = formula, outcome.type = outcome.type, offset = offset, Y = Y, X = X, Z = Z)  
    # } else {
    #   res.ci.df <- data.frame(ptEstimate$parameterEstimates) %>%
    #     tibble::rownames_to_column("outcome") %>%
    #     left_join(data.frame(outcome = c("Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Marginal Difference", "Number needed to treat"),
    #                          ci.ll = sapply(1:7, function(i) stats::quantile(boot.res$t[,i], probs = 0.025, na.rm = T)),
    #                          ci.ul = sapply(1:7, function(i) stats::quantile(boot.res$t[,i], probs = 0.975, na.rm = T)))) %>%
    #     tibble::column_to_rownames("outcome") %>%
    #     rename(Result = Results, `2.5% CI` = ci.ll, `97.5% CI` = ci.ul)
    # }
    df <- data %>%
      tibble::rownames_to_column("dummy_id") %>%
      dplyr::group_by(dummy_id) %>%
      tidyr::nest()
    } else {
      df <- data %>%
        dplyr::group_by(!!!clusterID) %>%
        tidyr::nest()
      
    }
  ## Generate R bootstrap resampling units
  bs <- rsample::bootstraps(df, times = R)
  names(bs$splits) <- paste0("boot.", seq(1:R)) 
  ## run R bootstrap iterations to get 95% CI for diff and ratio
  
  boot.res <- furrr::future_map_dfr(bs$splits, function(x) {
     #x <- bs$splits[[1]]
     df <- rsample::analysis(x) %>%
      tidyr::unnest_legacy(., cols = c(data)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-tidyselect::matches("dummy_id"))
    estimate <- pointEstimate(df, outcome.type = outcome.type, offset = offset, formula = formula, Y = Y, X = X, Z = Z, subgroup = subgroup, rate.multiplier = rate.multiplier)
    result <- estimate$parameterEstimates %>%
      t() %>%
      as.data.frame() %>% 
      tibble::rownames_to_column("test") %>%
      dplyr::mutate(boot = as.character(x$id))
    names(result) <- c("test","Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Marginal Difference", "Number needed to treat", "boot")
    return(result)
  })
  
  # boot.res <- do.call(c, future.apply::future_apply(bs$splits, boot_fun, future.seed = TRUE))
  
  if(length(unique(boot.res$test)) > 1) {
    ci <- boot.res %>%
      dplyr::group_by(test) %>%
      dplyr::summarise_at(vars(`Risk Difference`:`Number needed to treat`),
                          ~stats::quantile(., probs = 0.025, na.rm = T)) %>%
      dplyr::mutate(test = paste0(test,"_2.5% CL")) %>%
      dplyr::ungroup() %>%
      tidyr::gather(var, value, -test) %>%
      tidyr::spread(test, value) %>%
      dplyr::left_join(boot.res %>%
                         dplyr::group_by(test) %>%
                         dplyr::summarise_at(vars(`Risk Difference`:`Number needed to treat`),
                                             ~stats::quantile(., probs = 0.975, na.rm = T)) %>%
                         dplyr::mutate(test = paste0(test, "_97.5% CL")) %>%
                         dplyr::ungroup() %>%
                         tidyr::gather(var, value, -test) %>%
                         tidyr::spread(test, value), by = "var") %>%
      dplyr::rename(outcome = var) %>%
      dplyr::mutate(outcome = as.character(outcome))
    test.list <- unlist(names(ptEstimate$parameterEstimates)) 

    res.ci.df <- data.frame(ptEstimate$parameterEstimates) %>%
      tibble::rownames_to_column("outcome") %>%
      dplyr::left_join(ci, by = "outcome") %>%
      tibble::column_to_rownames("outcome") %>%
      dplyr::select(tidyselect::vars_select(names(.), 
                                            tidyselect::starts_with(unlist(test.list))))
    summary <- purrr::map_dfc(test.list, function(t) {
      # t = test.list[1]
      df <- res.ci.df %>%
        stats::na.omit() %>%
        dplyr::select(tidyselect::contains(t)) %>%
        dplyr::rename_all(.funs = funs(sub(t, "Result", .))) %>%
        dplyr::mutate(Out = paste0(formatC(round(Result, 3), format = "f", digits = 3), " (", formatC(round(`Result_2.5% CL`, 3), format = "f", digits = 3), ", ", formatC(round(`Result_97.5% CL`, 3), format = "f", digits = 3), ")")) %>%
        dplyr::select(Out)# %>%
      names(df) <- paste0(t, " Estimate (95% CL)")
      return(df)
    })
    rownames(summary) <- rownames(res.ci.df %>% na.omit())
    
  } else {
    res.ci.df <- data.frame(ptEstimate$parameterEstimates) %>%
      tibble::rownames_to_column("outcome") %>%
      dplyr::left_join(data.frame(outcome = rownames(ptEstimate$parameterEstimates),
                                  ci.ll = sapply(2:8, function(i) stats::quantile(boot.res[,i], probs = 0.025, na.rm = T)),
                                  ci.ul = sapply(2:8, function(i) stats::quantile(boot.res[,i], probs = 0.975, na.rm = T))) %>%
                         dplyr::mutate(outcome = as.character(outcome)), by = "outcome") %>%
      tibble::column_to_rownames("outcome") %>%
      dplyr::rename(`2.5% CL` = ci.ll, `97.5% CL` = ci.ul)
    summary <- res.ci.df %>% 
      stats::na.omit() %>%
      dplyr::mutate(Out = paste0(formatC(round(Estimate, 3), format = "f", digits = 3), " (", formatC(round(`2.5% CL`, 3), format = "f", digits = 3), ", ", formatC(round(`97.5% CL`, 3), format = "f", digits = 3), ")")) %>%
      dplyr::select(-(Estimate:`97.5% CL`)) %>%
      dplyr::rename(`Estimate (95% CL)` = Out)
    rownames(summary) <- rownames(res.ci.df %>% na.omit())
  }

  ## output results list
  res <- list(#parameterEstimates = ptEstimate$parameterEstimates,
    summary = summary,
    results.df = res.ci.df %>% na.omit(), 
    n = dplyr::n_distinct(data), 
    R = R, 
    boot.result = boot.res,
    contrast = ptEstimate$contrast,
    family = ptEstimate$family, 
    formula = ptEstimate$formula,
    predictedData = ptEstimate$predictedData)
  
  class(res) <- c("gComp", class(res))
  return(res)
}
  
