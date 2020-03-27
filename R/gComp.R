#' Perform g-computation to estimate difference and ratio effects of an outcome exposure
#'
#' @description Obtain a point estimate and 95% confidence limits for the difference and ratio between treatment and non-treatment (or exposed/not exposed) groups.
#'
#' @param data (Required) A data.frame or tibble containing variables for \code{Y}, \code{X}, and \code{Z} or with variables matching the model variables specified in a user-supplied formula. Data set should also contain variables for the optinal \code{subgroup} and \code{offset}, if they are specified 
#' @param outcome.type (Required) Character argument to describe the outcome type. Acceptable responses, and the corresponding error distribution and link function used in the \code{glm}, include:
#'  \describe{
#'  \item{binary}{(Default) A binomial distribution with link = 'logit' is used.}
#'  \item{count}{A Poisson distribution with link = 'log' is used.}
#'  \item{rate}{A Poisson distribution with link = 'log' is used.}
#'  \item{continuous}{A gaussian distribution with link = 'identity' is used.} 
#' }
#' @param formula (Optional) Default NULL. An object of class "formula" (or one that can be coerced to that class) which provides the the complete model formula, similar to the formula for the glm function in R (e.g. `Y ~ X + Z1 + Z2 + Z3`). 
#' Can be supplied as a character or formula object. If no formula is provided, Y and X must be provided.
#' @param Y (Optional) Default NULL. Character argument which specifies the outcome variable. Can optionally provide a formula instead of \code{Y} and \code{X} variables.
#' @param X (Optional) Default NULL. Character argument which specifies the exposure variable (or treatment group assignment), which can be binary, categorical, or continuous. This variable can be supplied as a factor variable, a numeric variable coded 0 or 1, or a continuous variable. 
#' Preferrably, \code{X} is supplied as a factor with the lowest level set to the desired comparator. 
#' Numeric variables are accepted, and coerced to factor with lowest level being the smallest number. 
#' Character variables are not accepted and will throw an error.
#' Can optionally provide a formula instead of \code{Y} and \code{X} variables. 
#' @param Z (Optional) Default NULL. List or single character vector which specifies the names of covariates or other variables to adjust for in the \code{glm} function to be used internally. Does not allow interaction terms.
#' @param subgroup (Optional) Default NULL. Character argument that indicates subgroups for stratified analysis. Effects will be reported for each category of the subgroup variable. Variable will be automatically converted to a factor if not already.  
#' @param offset (Optional, only applicable for rate outcomes) Default NULL. Character argument which specifies the person-time denominator for rate outcomes to be included as an offset in the Poisson regression model. Numeric variable should be on the linear scale; function will take natural log before including in the model.
#' @param rate.multiplier (Optional, only applicable for rate outcomes) Default 1. Numeric value to multiply to the rate-based effect measures. This option facilitates reporting effects with interpretable person-time denominators. For example, if the person-time variable (offset) is in days, a multiplier of 365*100 would result in estimates of rate differences per 100 person-years.
#' @param R (Optional) Default 200. The number of data resamples to be conducted to produce the bootstrap confidence interval of the estimate.
#' @param clusterID (Optional) Default NULL. Character argument which specifies the variable name for the unique identifier for clusters. This option specifies that clustering should be accounted for in the calculation of confidence intervals. The \code{clusterID} will be used as the level for resampling in the bootstrap procedure.
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
#' @importFrom dplyr rename n_distinct left_join mutate group_by ungroup select summarise_at vars
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom tidyr nest unnest gather spread unnest_legacy
#' @importFrom purrr map_dfc 
#' @importFrom furrr future_map_dfr 
#' @importFrom tidyselect vars_select starts_with contains matches
#' @importFrom rlang sym .data
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
                  clusterID = NULL) {
  ###need to check if X is categorical or 
  if (!is.null(X)) {
    X_type = ifelse(is.factor(data[[X]]), "categorical", ifelse(is.numeric(data[[X]]), "numeric", stop("X must be a factor or numeric variable")))
  } else {
    formula = stats::as.formula(formula)
    X <- rlang::sym(all.vars(formula[[3]])[1])
    X_type = ifelse(is.factor(data[[X]]), "categorical", ifelse(is.numeric(data[[X]]), "numeric", stop("X must be a factor or numeric variable")))
  }
if (!is.null(clusterID)) clusterID <- rlang::sym(clusterID)
  if (X_type == "numeric") {
    stop("Numeric explanitory variables not allowed at this time, we are still working on that feature!")
  }
  
  ## Get point estimate for diff and ratio
  pt_estimate <- pointEstimate(data, outcome.type = outcome.type, formula = formula, Y = Y, X = X, Z = Z, subgroup = subgroup, offset = offset, rate.multiplier = rate.multiplier)
  ## Nest df by bootstrap resampling unit
  if (is.null(clusterID)) {
    # ## run R bootstrap iterations to get 95% CI for diff and ratio
    # fun.statistic <- function(x, idx, outcome.type = outcome.type, offset = offset, formula = formula, Y = Y, X = X, Z = Z) {
    #   estimate <- pointEstimate(x[idx,], outcome.type = outcome.type, offset = offset, formula = formula, Y = Y, X = X, Z = Z)
    #   if !is.null(subgroup) {
    #     return(estimate$parameter.estimates)
    #   }
    #   return(estimate$parameter.estimates$Results)
    # }
    # ## I want to make sure I'm still allowing for the user to supply extra arguments to the boot call (e.g. weights).  Not sure this will allow that.
    # boot_res <- boot::boot(data = data, statistic = fun.statistic, R = R, parallel = parallel, ncpus = ncpus, formula = formula, outcome.type = outcome.type, offset = offset, Y = Y, X = X, Z = Z)
    # if (!is.null(subgroup)) {
    #   boot_res <- boot::boot(data = data, statistic = fun.statistic, R = R, parallel = parallel, ncpus = ncpus, formula = formula, outcome.type = outcome.type, offset = offset, Y = Y, X = X, Z = Z)  
    # } else {
    #   res_ci_df <- data.frame(pt_estimate$parameter.estimates) %>%
    #     tibble::rownames_to_column("outcome") %>%
    #     left_join(data.frame(outcome = c("Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Mean Difference", "Number needed to treat"),
    #                          ci.ll = sapply(1:7, function(i) stats::quantile(boot_res$t[,i], probs = 0.025, na.rm = T)),
    #                          ci.ul = sapply(1:7, function(i) stats::quantile(boot_res$t[,i], probs = 0.975, na.rm = T)))) %>%
    #     tibble::column_to_rownames("outcome") %>%
    #     rename(Result = Results, `2.5% CI` = ci.ll, `97.5% CI` = ci.ul)
    # }
    df <- data %>%
      tibble::rownames_to_column("dummy_id") %>%
      dplyr::group_by(.data$dummy_id) %>%
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
  
  boot_res <- furrr::future_map_dfr(bs$splits, function(x) {
     #x <- bs$splits[[1]]
     df <- rsample::analysis(x) %>%
      tidyr::unnest_legacy(., cols = c(data)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-tidyselect::matches("dummy_id"))
    estimate <- pointEstimate(df, outcome.type = outcome.type, offset = offset, formula = formula, Y = Y, X = X, Z = Z, subgroup = subgroup, rate.multiplier = rate.multiplier)
    result <- estimate$parameter.estimates %>%
      t() %>%
      as.data.frame() %>% 
      tibble::rownames_to_column("test") %>%
      dplyr::mutate(boot = as.character(x$id))
    names(result) <- c("test","Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Mean Difference", "Number needed to treat", "boot")
    return(result)
  })
  
  # boot_res <- do.call(c, future.apply::future_apply(bs$splits, boot_fun, future.seed = TRUE))
  
  if(length(unique(boot_res$test)) > 1) {
    ci <- boot_res %>%
      dplyr::group_by(.data$test) %>%
      dplyr::summarise_at(dplyr::vars(.data$`Risk Difference`:.data$`Number needed to treat`),
                          ~stats::quantile(., probs = 0.025, na.rm = T)) %>%
      dplyr::mutate(test = paste0(.data$test,"_2.5% CL")) %>%
      dplyr::ungroup() %>%
      tidyr::gather(var, value, -.data$test) %>%
      tidyr::spread(.data$test, value) %>%
      dplyr::left_join(boot_res %>%
                         dplyr::group_by(.data$test) %>%
                         dplyr::summarise_at(dplyr::vars(.data$`Risk Difference`:.data$`Number needed to treat`),
                                             ~stats::quantile(., probs = 0.975, na.rm = T)) %>%
                         dplyr::mutate(test = paste0(.data$test, "_97.5% CL")) %>%
                         dplyr::ungroup() %>%
                         tidyr::gather(var, value, -.data$test) %>%
                         tidyr::spread(.data$test, value), by = "var") %>%
      dplyr::rename(outcome = var) %>%
      dplyr::mutate(outcome = as.character(.data$outcome))
    test_list <- unlist(names(pt_estimate$parameter.estimates)) 

    res_ci_df <- data.frame(pt_estimate$parameter.estimates) %>%
      tibble::rownames_to_column("outcome") %>%
      dplyr::left_join(ci, by = "outcome") %>%
      tibble::column_to_rownames("outcome") %>%
      dplyr::select(tidyselect::vars_select(names(.), 
                                            tidyselect::starts_with(unlist(test_list))))
    summary <- purrr::map_dfc(test_list, function(t) {
      # t = test_list[1]
      df <- res_ci_df %>%
        stats::na.omit() %>%
        dplyr::select(tidyselect::contains(t)) %>%
        dplyr::rename_all(.funs = funs(sub(t, "Estimate", .))) %>%
        dplyr::mutate(Out = paste0(formatC(round(.data$Estimate, 3), format = "f", digits = 3), " (", formatC(round(.data$`Estimate_2.5% CL`, 3), format = "f", digits = 3), ", ", formatC(round(.data$`Estimate_97.5% CL`, 3), format = "f", digits = 3), ")")) %>%
        dplyr::select(.data$Out)
        # dplyr::rename(`Estimate (95% CI)` = .data$Out) %>%
        # dplyr::select(.data$`Estimate (95% CI)`)# %>%
      names(df) <- paste0(t, " Estimate (95% CI)")
      return(df)
    })
    rownames(summary) <- rownames(res_ci_df %>% na.omit())
    
  } else {
    res_ci_df <- data.frame(pt_estimate$parameter.estimates) %>%
      tibble::rownames_to_column("outcome") %>%
      dplyr::left_join(data.frame(outcome = rownames(pt_estimate$parameter.estimates),
                                  ci.ll = sapply(2:8, function(i) stats::quantile(boot_res[,i], probs = 0.025, na.rm = T)),
                                  ci.ul = sapply(2:8, function(i) stats::quantile(boot_res[,i], probs = 0.975, na.rm = T))) %>%
                         dplyr::mutate(outcome = as.character(.data$outcome)), by = "outcome") %>%
      tibble::column_to_rownames("outcome") %>%
      dplyr::rename(`2.5% CL` = .data$ci.ll, `97.5% CL` = .data$ci.ul)
    summary <- res_ci_df %>% 
      stats::na.omit() %>%
      dplyr::mutate(Out = paste0(formatC(round(.data$Estimate, 3), format = "f", digits = 3), " (", formatC(round(.data$`2.5% CL`, 3), format = "f", digits = 3), ", ", formatC(round(.data$`97.5% CL`, 3), format = "f", digits = 3), ")")) %>%
      dplyr::select(-(.data$Estimate:.data$`97.5% CL`)) %>%
      dplyr::rename(`Estimate (95% CI)` = .data$Out)
    rownames(summary) <- rownames(res_ci_df %>% na.omit())
  }

  ## output results list
  res <- list(#parameter.estimates = pt_estimate$parameter.estimates,
    summary = summary,
    results.df = res_ci_df %>% na.omit(), 
    n = dplyr::n_distinct(data), 
    R = R, 
    boot.result = boot_res,
    contrast = pt_estimate$contrast,
    family = pt_estimate$family, 
    formula = pt_estimate$formula,
    predicted.data = pt_estimate$predicted.data)
  
  class(res) <- c("gComp", class(res))
  return(res)
}
  
