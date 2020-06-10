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
#' @param parallel (Optional) Default "no." The type of parallel operation to be used. Available 
#'   options (besides the default of no parallel processing) include multicore" (not available 
#'   for Windows) or "snow." This argument is passed directly to the /href{\code{boot} 
#'   function}{https://cran.r-project.org/web/packages/boot/boot.pdf}.
#' @param ncpus (Optional, only used if parallel is set to "multicore" or "snow") Default 1. 
#'   Integer argument for the number of CPUs available for parallel processing/ number of 
#'   parallel operations to be used.  This argument is passed directly to the /href{\code{boot} 
#'   function}{https://cran.r-project.org/web/packages/boot/boot.pdf}.
#'  
#'
#' @return An object of class \code{gComp} which is a list with components:
#' \itemize{
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
#'   \item{glm.result} {The 
#'   \code{glm} class object returned from the fitted regression of the outcome 
#'   on the exposure and relevant covariates.}
#'   }
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
#' @note
#'  Note that for a protective exposure (risk difference less than 0), the 
#'   'Number needed to treat/harm' is interpreted as the number needed to treat, 
#'   and for a harmful exposure (risk difference greater than 0), it is 
#'   interpreted as the number needed to harm. Note also that confidence intervals 
#'   are not reported for the number needed to treat/harm. If the confidence 
#'   interval (CI) for the risk difference crosses the null, the construction of 
#'   the CI for the number needed to treat/harm is not well defined. Challenges 
#'   and options for reporting the number needed to treat/harm CI are reviewed 
#'   extensively in Altman 1998, Hutton 2000, and Stang 2010, with a consensus 
#'   that an appropriate interval would have two segments, one bounded at negative 
#'   infinity and the other at positive infinity. Because the number needed to 
#'   treat/harm is most useful as a communication tool and is directly derived 
#'   from the risk difference, which has a CI that provides a more interpretable 
#'   measure of precision, we do not report the CI for the number needed to 
#'   treat/harm. If the CI of the risk difference does not cross the null, the 
#'   number needed to treat/harm CI can be calculated straightforwardly by 
#'   taking the inverse of each confidence bound of the risk difference.
#'   
#' @note 
#'  For continuous exposure variables, the default effects are provided 
#'   for a one unit difference in the exposure at the mean value of the exposure 
#'   variable. Because the underlying parametric model for a binary outcome is 
#'   logistic regression, the risks for a continuous exposure will be estimated 
#'   to be linear on the log-odds (logit) scale, such that the odds ratio for 
#'   any one unit increase in the continuous variable is constant. However, 
#'   the risks will not be linear on the linear (risk difference) or log (risk 
#'   ratio) scales, such that these parameters will not be constant across the 
#'   range of the continuous exposure. Users should be aware that the risk 
#'   difference, risk ratio, number needed to treat/harm (for a binary outcome) 
#'   and the incidence rate difference (for a rate/count outcome) reported with 
#'   a continous exposure apply specifically at the mean of the continuous 
#'   exposure. The effects do not necessarily apply across the entire range of 
#'   the variable. However, variations in the effect are likely small, 
#'   especially near the mean.
#'     
#'
#' @export
#' 
#' @references 
#'  Ahern J, Hubbard A, Galea S. Estimating the effects of potential public health 
#'   interventions on population disease burden: a step-by-step illustration of 
#'   causal inference methods. Am. J. Epidemiol. 2009;169(9):1140–1147.
#'  
#'  Altman DG, Deeks JJ, Sackett DL. Odds ratios should be avoided when events 
#'   are common. BMJ. 1998;317(7168):1318.
#' 
#'  Hernán MA, Robins JM (2020). Causal Inference: What If. Boca Raton: 
#'   Chapman & Hall/CRC. /href{https://www.hsph.harvard.edu/miguel-hernan/causal-inference-book/}{A link to the book can be found here}
#' 
#'  Hutton JL. Number needed to treat: properties and problems. Journal of the 
#'   Royal Statistical Society: Series A (Statistics in Society). 2000;163(3):381–402.
#'
#'  Robins J. A new approach to causal inference in mortality studies with a 
#'   sustained exposure period—application to control of the healthy worker 
#'   survivor effect. Mathematical Modelling. 1986;7(9):1393–1512.
#'   
#'  Snowden JM, Rose S, Mortimer KM. Implementation of G-computation on a 
#'   simulated data set: demonstration of a causal inference technique. 
#'   Am. J. Epidemiol. 2011;173(7):731–738.
#'   
#'  Stang A, Poole C, Bender R. Common problems related to the use of number 
#'   needed to treat. Journal of Clinical Epidemiology. 2010;63(8):820–825. 
#'   
#'  Westreich D, Cole SR, Young JG, et al. The parametric g-formula to 
#'   estimate the effect of highly active antiretroviral therapy on incident 
#'    AIDS or death. Stat Med. 2012;31(18):2000–2009.
#'   
#' @examples
#' ## Obtain the risk difference and risk ratio for cardiovascular disease or death between
#' ## patients with and without diabetes.
#' data(cvdd)
#' set.seed(538)
#' diabetes <- gComp(cvdd, formula = "cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP",
#' outcome.type = "binary", R = 20)
#'
#' @importFrom rsample bootstraps analysis
#' @importFrom stats quantile as.formula na.omit
#' @importFrom dplyr rename n_distinct left_join mutate group_by ungroup select summarise_at vars bind_rows
#' @importFrom tibble rownames_to_column column_to_rownames
#' @importFrom tidyr gather spread
#' @importFrom purrr map_dfc map_dfr
#' @importFrom tidyselect vars_select starts_with contains 
#' @importFrom rlang sym .data
#' @importFrom magrittr %>%
#'
#' @seealso \code{\link{pointEstimate}} \href{boot package documentation}{https://cran.r-project.org/web/packages/boot/boot.pdf}
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
                  exposure.scalar = 1,
                  R = 200,
                  clusterID = NULL,
                  parallel = "no",
                  ncpus = getOption("boot.ncpus", 1L)) {
  
  # Ensure X is categorical (factor) or numeric, throw error if character.  Also if X is numeric, calculate the mean value. 
  #  This will be used for centering the continnuous exposure to ensure estimates are within the range of the observed values.
  #  This is done here so that the same mean value can be used across all bootstraps for consistent interpretation.
  if (!is.null(X)) {
    X_mean = ifelse(is.factor(data[[X]]), TRUE, ifelse(is.numeric(data[[X]]), mean(data[[X]]), stop("X must be a factor or numeric variable")))
  } else {
    formula = stats::as.formula(formula)
    X <- rlang::sym(all.vars(formula[[3]])[1])
    X_mean = ifelse(is.factor(data[[X]]), TRUE, ifelse(is.numeric(data[[X]]), mean(data[[X]]), stop("X must be a factor or numeric variable")))
  }
  
  if (!is.null(clusterID)) {
    clusterID <- rlang::sym(clusterID)
    # Get list of clusters for bootstrapping
    clusters <- unique(data[[clusterID]]) 
  } else {
    clusters <-  NULL
  }
  
  # Get point estimate for diff and ratio
  pt_estimate <- pointEstimate(data, outcome.type = outcome.type, formula = formula, Y = Y, X = X, Z = Z, subgroup = subgroup, offset = offset, rate.multiplier = rate.multiplier, exposure.scalar = exposure.scalar, exposure.center = X_mean)
  
  ####### Run bootstrap resampling, calculate point estimate for each resample, and get 95% CI for estimates
  # Define bootsrap statistic
  fun.statistic <- function(x, idx, outcome.type = outcome.type, offset = offset, formula = formula, 
                            Y = Y, X = X, Z = Z, subgroup = subgroup, rate.multiplier = rate.multiplier, 
                            exposure.scalar = exposure.scalar, exposure.center = exposure.center, 
                            clusters = clusters) {
    
    if (is.null(clusters)) {
      estimate <- suppressMessages(pointEstimate(x[idx,], outcome.type = outcome.type, offset = offset, formula = formula, Y = Y, X = X, Z = Z, subgroup = subgroup, rate.multiplier = rate.multiplier, exposure.scalar = exposure.scalar, exposure.center = exposure.center))
    } else {
      cls <- sample(clusters, size = length(clusters), replace = TRUE)
      df.bs <- lapply(cls, function(b) subset(x, x[[clusterID]] == b))
      df.bs <- do.call(rbind, df.bs)
      estimate <- suppressMessages(pointEstimate(df.bs, outcome.type = outcome.type, offset = offset, formula = formula, Y = Y, X = X, Z = Z, subgroup = subgroup, rate.multiplier = rate.multiplier, exposure.scalar = exposure.scalar, exposure.center = exposure.center))
    }
    if (length(names(estimate$parameter.estimates)) > 1) {
      output <- sapply(names(estimate$parameter.estimates), function (n) c(estimate$parameter.estimates[[n]]))
      # output <- do.call(cbind, output)
      # colnames(output)[8] <- "test"
    } else {
      output <- c(estimate$parameter.estimates$Estimate)
    }
    return(output)
  }
  
  # Run bootstrap iterations
  boot_out <- boot::boot(data = data, statistic = fun.statistic, R = R, parallel = parallel, ncpus = ncpus, 
                         outcome.type = outcome.type, formula = formula, Y = Y, X = X, Z = Z,
                         subgroup = subgroup, offset = offset, rate.multiplier = rate.multiplier, 
                         exposure.scalar = exposure.scalar, exposure.center = X_mean, clusters = clusters)
  
  # Format output from boot function
  if (dim(boot_out$t)[2] > 7) {
    # Reformat if more than 1 estimate provided (e.g. if subgroups are used or if categorical exposure 
    # with more than 2 categories). This is necessary because the boot package only outputs a single line 
    # of results for each bootstrap iteration, including all possible exposure/subgroup comparisons.
    boot_res <- purrr::map_dfr(seq(1:(dim(boot_out$t)[2]/7)), function (x) {
       # x = 2
      out <- boot_out$t[, (1+(x-1)*7):(7*x)] %>%
        data.frame() %>%
        dplyr::mutate(test = names(pt_estimate$parameter.estimates)[x],
               boot = paste0("Bootstrap",seq(1:R)))
    })
    names(boot_res) <- c(rownames(pt_estimate$parameter.estimates), "test", "boot")
  } else {
    boot_res <- boot_out$t %>%
      data.frame() %>%
      dplyr::mutate(test = "Estimate", 
             boot = paste0("Bootstrap", seq(1:R)))
    names(boot_res) <- c(rownames(pt_estimate$parameter.estimates), "test", "boot")
  }    
  
  # # Nest df by bootstrap resampling unit
  # if (is.null(clusterID)) { # By observation
  #   df <- data %>%
  #     tibble::rownames_to_column("dummy_id") %>%
  #     dplyr::group_by(.data$dummy_id) %>%
  #     tidyr::nest()
  #   } else { # By specified cluster
  #     df <- data %>%
  #       dplyr::group_by(!!!clusterID) %>%
  #       tidyr::nest()
  #     
  #   }
  # 
  # # Generate R bootstrap resampling units
  # bs <- rsample::bootstraps(df, times = R)
  # names(bs$splits) <- paste0("boot.", seq(1:R)) 
  # 
  # # Run R bootstrap iterations to get R point estimates 
  # boot_res <- furrr::future_map_dfr(bs$splits, function(x) {
  #    df <- rsample::analysis(x) %>%
  #     tidyr::unnest(., cols = c(data)) %>%
  #     dplyr::ungroup() %>%
  #     dplyr::select(-tidyselect::matches("dummy_id"))
  #   estimate <- suppressMessages(pointEstimate(df, outcome.type = outcome.type, formula = formula, Y = Y, X = X, Z = Z, subgroup = subgroup, offset = offset, rate.multiplier = rate.multiplier, exposure.scalar = exposure.scalar, exposure.center = X_mean))
  #   result <- estimate$parameter.estimates %>%
  #     t() %>%
  #     as.data.frame() %>% 
  #     tibble::rownames_to_column("test") %>%
  #     dplyr::mutate(boot = as.character(x$id))
  #   names(result) <- c("test","Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Mean Difference", "Number needed to treat/harm", "boot")
  #   return(result)
  # })
  
  # Use bootstrap results (boot_res) to calculate 95% CI for effect estimates
  if(length(unique(boot_res$test)) > 1) { # For subgroups and/or >2 treatment/exposure levels
    ci <- boot_res %>%
      dplyr::group_by(.data$test) %>%
      dplyr::summarise_at(dplyr::vars(.data$`Risk Difference`:.data$`Mean Difference`),
                          ~stats::quantile(., probs = 0.025, na.rm = T)) %>%
      dplyr::mutate(test = paste0(.data$test,"_2.5% CL")) %>%
      dplyr::ungroup() %>%
      tidyr::gather(var, value, -.data$test) %>%
      tidyr::spread(.data$test, value) %>%
      dplyr::left_join(boot_res %>%
                         dplyr::group_by(.data$test) %>%
                         dplyr::summarise_at(dplyr::vars(.data$`Risk Difference`:.data$`Mean Difference`),
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
    ##Remove outcomes that are NA
    res_ci_df <- res_ci_df[rowSums(is.na(res_ci_df)) != ncol(res_ci_df), ]
    
    summary <- purrr::map_dfc(test_list, function(t) {
      df <- res_ci_df %>%
        stats::na.omit() %>%
        dplyr::select(tidyselect::contains(t)) %>%
        dplyr::rename_all(list(~sub(t, "Estimate", .))) %>%
        dplyr::mutate(Out = paste0(formatC(round(.data$Estimate, 3), format = "f", digits = 3), " (", formatC(round(.data$`Estimate_2.5% CL`, 3), format = "f", digits = 3), ", ", formatC(round(.data$`Estimate_97.5% CL`, 3), format = "f", digits = 3), ")")) %>%
        dplyr::select(.data$Out) %>%
        dplyr::bind_rows(., data.frame(pt_estimate$parameter.estimates) %>% 
                           dplyr::filter(rownames(.) == "Number needed to treat/harm") %>% 
                           dplyr::select(tidyselect::contains(t)) %>%
                           dplyr::rename_all(list(~sub(t, "Out", .))) %>%
                           dplyr::mutate(Out = ifelse(is.na(.data$Out), NA, formatC(round(.data$Out, 3), format = "f", digits = 3))))
      names(df) <- paste0(t, " Estimate (95% CI)") 
      return(df)
    }) %>%
      stats::na.omit()
    rownames(summary) <- rownames(res_ci_df)
    
  } else { # For no subgroups and only 2 treatment/exposure levels
    res_ci_df <- data.frame(pt_estimate$parameter.estimates) %>%
      tibble::rownames_to_column("outcome") %>%
      dplyr::left_join(data.frame(outcome = rownames(pt_estimate$parameter.estimates)[1:6],
                                  ci.ll = sapply(1:6, function(i) stats::quantile(boot_res[,i], probs = 0.025, na.rm = T)),
                                  ci.ul = sapply(1:6, function(i) stats::quantile(boot_res[,i], probs = 0.975, na.rm = T))) %>%
                         dplyr::mutate(outcome = as.character(.data$outcome)), by = "outcome") %>%
      tibble::column_to_rownames("outcome") %>%
      dplyr::rename(`2.5% CL` = .data$ci.ll, `97.5% CL` = .data$ci.ul)
    
    ##Remove outcomes that are NA
    res_ci_df <- res_ci_df[rowSums(is.na(res_ci_df)) != ncol(res_ci_df), ]
    
    summary <- res_ci_df %>% 
      stats::na.omit() %>%
      dplyr::mutate(Out = paste0(formatC(round(.data$Estimate, 3), format = "f", digits = 3), " (", formatC(round(.data$`2.5% CL`, 3), format = "f", digits = 3), ", ", formatC(round(.data$`97.5% CL`, 3), format = "f", digits = 3), ")")) %>%
      dplyr::select(-(.data$Estimate:.data$`97.5% CL`)) %>%
      dplyr::bind_rows(., data.frame(pt_estimate$parameter.estimates) %>% 
                         dplyr::filter(rownames(.) == "Number needed to treat/harm") %>% 
                         dplyr::mutate(Out = ifelse(is.na(.data$Estimate), NA, formatC(round(.data$Estimate, 3), format = "f", digits = 3))) %>%
                         dplyr::select(.data$Out)) %>%
      dplyr::rename(`Estimate (95% CI)` = .data$Out) %>%
      stats::na.omit()
    rownames(summary) <- rownames(res_ci_df)
  }
  
# Output results list
  res <- list(
    summary = summary,
    results.df = res_ci_df, 
    n = dplyr::n_distinct(data), 
    R = R, 
    boot.result = boot_res,
    contrast = pt_estimate$contrast,
    family = pt_estimate$family, 
    formula = pt_estimate$formula,
    predicted.data = pt_estimate$predicted.data, 
    glm.result = pt_estimate$glm.result)
  
  class(res) <- c("gComp", class(res))
  return(res)
}
  
