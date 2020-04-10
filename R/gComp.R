#' Estimate difference and ratio effects with 95\% confidence intervals.
#'
#' @description Obtain a point estimate and 95\% confidence interval for
#'   difference and ratio effects comparing exposed and unexposed (or treatment and non-treatment)
#'   groups using g-computation.
#'
#' @inheritParams pointEstimate
#' @param R (Optional) Default 200. The number of data resamples to be conducted
#'   to produce the bootstrap confidence interval of the estimate.
#' @param clusterID (Optional) Default NULL. Character argument which specifies
#'   the variable name for the unique identifier for clusters. This option
#'   specifies that clustering should be accounted for in the calculation of
#'   confidence intervals. The \code{clusterID} will be used as the level for
#'   resampling in the bootstrap procedure.
#'
#' @return An object of class \code{gComp} which is a list with components:
#'   \item{summary}{Summary providing parameter estimates and 95\% confidence
#'   limits of the outcome difference and ratio} \item{results.df}{Data.frame
#'   with parameter estimates, 2.5\% confidence limit, and 97.5\% confidence
#'   limit each as a column} \item{n}{Number of observations in the original
#'   dataset} \item{R}{Number of bootstrap iterations}
#'   \item{boot.result}{Data.frame containing the results of the \code{R}
#'   bootstrap iterations of the g-computation} \item{contrast}{Contrast levels
#'   compared} \item{family}{Error distribution used in the model}
#'   \item{formula}{Model formula used to fit the \code{glm}}
#'   \item{predicted.data}{A tibble with the predicted values for both exposed
#'   and unexposed counterfactual predictions for each observation in the
#'   original dataset}
#'
#' @details The \code{gComp} function executes the following steps: 
#' \enumerate{
#'   \item Calls the \code{\link{pointEstimate}} function on the data to obtain
#'   an estimate of the difference and ratio effects. 
#'   \item Generates \code{R} bootstrap resamples of the data, with replacement. If
#'   the resampling is to be done at the cluster level (set using the
#'   \code{clusterID} argument), the number of clusters will remain constant but
#'   the total number of observations in each resampled data set might be
#'   different if clusters are not balanced. 
#'   \item Calls the \code{\link{pointEstimate}} function on each of the resampled data sets.
#'   \item Calculates the 95\% confidence interval of the difference and ratio
#'   estimates using the results obtained from the \code{R} resampled parameter
#'   estimates. }   
#'   
#'   As bootstrap resamples are generated with random sampling, users should
#'   set a seed (\code{\link[base]{set.seed}} for reproducible
#'   confidence intervals.
#'
#' @export
#'
#' @examples
#' ## Obtain the risk difference and risk ratio for cardiovascular disease or death between
#' ## patients with and without diabetes.
#' data(cvdd)
#' diabetes <- gComp(cvdd, formula = "cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP",
#' outcome.type = "binary", R = 100)
#'
#' @importFrom rsample bootstraps analysis
#' @importFrom stats quantile as.formula
#' @importFrom dplyr rename n_distinct left_join mutate group_by ungroup select
#'   summarise_at vars
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom tidyr nest unnest gather spread unnest_legacy
#' @importFrom purrr map_dfc
#' @importFrom furrr future_map_dfr
#' @importFrom tidyselect vars_select starts_with contains matches
#' @importFrom rlang sym .data
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link{pointEstimate}}
#'   
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
  
  # Ensure X is categorical (factor) or numeric, throw error if character
  if (!is.null(X)) {
    X_type = ifelse(is.factor(data[[X]]), "categorical", ifelse(is.numeric(data[[X]]), "numeric", stop("X must be a factor or numeric variable")))
  } else {
    formula = stats::as.formula(formula)
    X <- rlang::sym(all.vars(formula[[3]])[1])
    X_type = ifelse(is.factor(data[[X]]), "categorical", ifelse(is.numeric(data[[X]]), "numeric", stop("X must be a factor or numeric variable")))
  }
  
  if (!is.null(clusterID)) clusterID <- rlang::sym(clusterID)
  
  # if (X_type == "numeric") {
  #   stop("Numeric explanitory variables not allowed at this time, we are still working on that feature!")
  # }
  
  # Get point estimate for diff and ratio
  pt_estimate <- pointEstimate(data, outcome.type = outcome.type, formula = formula, Y = Y, X = X, Z = Z, subgroup = subgroup, offset = offset, rate.multiplier = rate.multiplier)
  
  # Nest df by bootstrap resampling unit
  if (is.null(clusterID)) { # By observation
    df <- data %>%
      tibble::rownames_to_column("dummy_id") %>%
      dplyr::group_by(.data$dummy_id) %>%
      tidyr::nest()
    } else { # By specified cluster
      df <- data %>%
        dplyr::group_by(!!!clusterID) %>%
        tidyr::nest()
      
    }
  
  # Generate R bootstrap resampling units
  bs <- rsample::bootstraps(df, times = R)
  names(bs$splits) <- paste0("boot.", seq(1:R)) 
  
  # Run R bootstrap iterations to get R point estimates 
  boot_res <- furrr::future_map_dfr(bs$splits, function(x) {
     #x <- bs$splits[[1]]
     df <- rsample::analysis(x) %>%
      tidyr::unnest_legacy(., cols = c(data)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-tidyselect::matches("dummy_id"))
    estimate <- pointEstimate(df, outcome.type = outcome.type, formula = formula, Y = Y, X = X, Z = Z, subgroup = subgroup, offset = offset, rate.multiplier = rate.multiplier)
    result <- estimate$parameter.estimates %>%
      t() %>%
      as.data.frame() %>% 
      tibble::rownames_to_column("test") %>%
      dplyr::mutate(boot = as.character(x$id))
    names(result) <- c("test","Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Mean Difference", "Number needed to treat", "boot")
    return(result)
  })
  
  # Use bootstrap results (boot_res) to calculate 95% CI for effect estimates
  if(length(unique(boot_res$test)) > 1) { # For subgroups and/or >2 treatment/exposure levels
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
      names(df) <- paste0(t, " Estimate (95% CI)")
      return(df)
    })
    rownames(summary) <- rownames(res_ci_df %>% na.omit())
    
  } else { # For no subgroups and only 2 treatment/exposure levels
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

  # Output results list
  res <- list(
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
  
