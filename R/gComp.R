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
#'   options (besides the default of no parallel processing) include "multicore" (not available 
#'   for Windows) or "snow." This argument is passed directly to \code{\link[boot]{boot}}.
#'   See note below about setting seeds and parallel computing.
#' @param ncpus (Optional, only used if parallel is set to "multicore" or "snow") Default 1. 
#'   Integer argument for the number of CPUs available for parallel processing/ number of 
#'   parallel operations to be used.  This argument is passed directly to \code{\link[boot]{boot}} 
#'  
#'
#' @return An object of class \code{gComp} which is a named list with components:
#'
#'   \item{$summary}{Summary providing parameter estimates and 95\% confidence
#'   limits of the outcome difference and ratio (in a print-pretty format)} 
#'   \item{$results.df}{Data.frame with parameter estimates, 2.5\% confidence 
#'   limit, and 97.5\% confidence limit each as a column (which can be used for easy 
#'   incorporation into tables for publication)} 
#'   \item{$n}{Number of unique observations in the original dataset} 
#'   \item{$R}{Number of bootstrap iterations}
#'   \item{$boot.result}{Data.frame containing the results of the \code{R}
#'   bootstrap iterations of the g-computation} 
#'   \item{$contrast}{Contrast levels compared} 
#'   \item{$family}{Error distribution used in the model}
#'   \item{$formula}{Model formula used to fit the \code{glm}}
#'   \item{$predicted.outcome}{A data.frame with the marginal mean predicted outcomes 
#'   (with 95\% confidence limits) for each exposure level (i.e. under both exposed 
#'   and unexposed counterfactual predictions)}
#'   \item{$glm.result}{The \code{glm} class object returned from the 
#'   fitted regression of the outcome on the exposure and relevant covariates.}
#'
#'
#' @details The \code{gComp} function executes the following steps: 
#' \enumerate{
#'   \item Calls the \code{\link{pointEstimate}} function on the data to obtain
#'   the appropriate effect estimates (difference, ratio, etc.). 
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
#'   set a seed (\code{\link{set.seed}} for reproducible
#'   confidence intervals.
#'   
#'   While offsets are used to account for differences in follow-up time 
#'   between individuals in the \code{glm} model, rate differences are 
#'   calculated assuming equivalent follow-up of all individuals (i.e. 
#'   predictions for each exposure are based on all observations having the 
#'   same offset value). The default is 1 (specifying 1 unit of the original 
#'   offset variable) or the user can specify an offset to be used in the 
#'   predictions with the rate.multiplier argument.
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
#'   a continuous exposure apply specifically at the mean of the continuous 
#'   exposure. The effects do not necessarily apply across the entire range of 
#'   the variable. However, variations in the effect are likely small, 
#'   especially near the mean.
#'        
#' @note       
#'  Interaction terms are not allowed in the model formula. The \code{subgroup} 
#'   argument affords interaction between the exposure variable and a single 
#'   covariate (that is forced to categorical if supplied as numeric) to 
#'   estimate effects of the exposure within subgroups defined by the 
#'   interacting covariate. To include additional interaction terms with 
#'   variables other than the exposure, we recommend that users create the 
#'   interaction term as a cross-product of the two interaction variables in  
#'   a data cleaning step prior to running the model.
#'        
#' @note 
#'  The documentation for \code{\link[boot]{boot}} includes details about 
#'   reproducible seeds when using parallel computing.   
#'  
#' @export
#' 
#' @references 
#'  Ahern J, Hubbard A, Galea S. Estimating the effects of potential public health 
#'   interventions on population disease burden: a step-by-step illustration of 
#'   causal inference methods. Am. J. Epidemiol. 2009;169(9):1140–1147.
#'   \doi{10.1093/aje/kwp015}
#'  
#'  Altman DG, Deeks JJ, Sackett DL. Odds ratios should be avoided when events 
#'   are common. BMJ. 1998;317(7168):1318. \doi{10.1136/bmj.317.7168.1318}
#' 
#'  Hernán MA, Robins JM (2020). Causal Inference: What If. Boca Raton: 
#'   Chapman & Hall/CRC. \href{https://www.hsph.harvard.edu/miguel-hernan/causal-inference-book/}{Book link}
#' 
#'  Hutton JL. Number needed to treat: properties and problems. Journal of the 
#'   Royal Statistical Society: Series A (Statistics in Society). 2000;163(3):381–402.
#'   \doi{10.1111/1467-985X.00175}
#'
#'  Robins J. A new approach to causal inference in mortality studies with a 
#'   sustained exposure period—application to control of the healthy worker 
#'   survivor effect. Mathematical Modelling. 1986;7(9):1393–1512. \doi{10.1016/0270-0255(86)90088-6}
#'   
#'  Snowden JM, Rose S, Mortimer KM. Implementation of G-computation on a 
#'   simulated data set: demonstration of a causal inference technique. 
#'   Am. J. Epidemiol. 2011;173(7):731–738. \doi{10.1093/aje/kwq472}
#'   
#'  Stang A, Poole C, Bender R. Common problems related to the use of number 
#'   needed to treat. Journal of Clinical Epidemiology. 2010;63(8):820–825. 
#'   \doi{10.1016/j.jclinepi.2009.08.006}
#'   
#'  Westreich D, Cole SR, Young JG, et al. The parametric g-formula to 
#'   estimate the effect of highly active antiretroviral therapy on incident 
#'    AIDS or death. Stat Med. 2012;31(18):2000–2009. \doi{10.1002/sim.5316}
#'   
#' @examples
#' ## Obtain the risk difference and risk ratio for cardiovascular disease or death between
#' ## patients with and without diabetes.
#' data(cvdd)
#' set.seed(538)
#' diabetes <- gComp(cvdd, formula = "cvd_dth ~ DIABETES + AGE + SEX + BMI + CURSMOKE + PREVHYP",
#' outcome.type = "binary", R = 20)
#'
#' @importFrom stats quantile as.formula na.omit
#' @importFrom dplyr n_distinct left_join mutate group_by select summarise_at vars mutate_at arrange filter across
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom purrr map_dfc map_dfr
#' @importFrom tidyselect contains any_of everything
#' @importFrom rlang sym .data
#' @importFrom magrittr %>%
#' @importFrom boot boot
#'
#' @seealso \code{\link{pointEstimate}} \code{\link[boot]{boot}}
#'   
#'   

gComp <- function(data, 
                  outcome.type =  c("binary", "count","count_nb", "rate", "rate_nb", "continuous"), 
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
  pt_estimate <- pointEstimate(data, outcome.type = outcome.type, formula = formula, Y = Y, X = X, Z = Z, 
                               offset = offset, rate.multiplier = rate.multiplier, 
                               subgroup = subgroup, 
                               exposure.scalar = exposure.scalar, exposure.center = X_mean)
  
  ####### Run bootstrap resampling, calculate point estimate for each resample, and get 95% CI for estimates
  # Define bootstrap statistic
  fun.statistic <- function(x, idx, outcome.type = outcome.type, 
                            formula = formula, Y = Y, X = X, Z = Z, 
                            offset = offset, rate.multiplier = rate.multiplier, 
                            subgroup = subgroup, 
                            exposure.scalar = exposure.scalar, exposure.center = exposure.center, 
                            clusters = clusters) {
    
    if (is.null(clusters)) {
      estimate <- suppressMessages(pointEstimate(x[idx,], outcome.type = outcome.type, 
                                                 formula = formula, Y = Y, X = X, Z = Z, 
                                                 offset = offset, rate.multiplier = rate.multiplier, 
                                                 subgroup = subgroup, 
                                                 exposure.scalar = exposure.scalar, exposure.center = exposure.center))
    } else {
      cls <- sample(clusters, size = length(clusters), replace = TRUE)
      df.bs <- lapply(cls, function(b) subset(x, x[[clusterID]] == b))
      df.bs <- do.call(rbind, df.bs)
      estimate <- suppressMessages(pointEstimate(df.bs, outcome.type = outcome.type, 
                                                 formula = formula, Y = Y, X = X, Z = Z, 
                                                 offset = offset, rate.multiplier = rate.multiplier, 
                                                 subgroup = subgroup, 
                                                 exposure.scalar = exposure.scalar, exposure.center = exposure.center))
    }
    if (length(names(estimate$parameter.estimates)) > 1) {
      output <- sapply(names(estimate$parameter.estimates), function (n) c(estimate$parameter.estimates[[n]], estimate$predicted.outcome[[n]]))
      # output <- do.call(cbind, output)
      # colnames(output)[8] <- "test"
    } else {
      output <- c(estimate$parameter.estimates$Estimate, estimate$predicted.outcome$Estimate)
    }
    return(output)
  }
  
  # Run bootstrap iterations
  boot_out <- boot::boot(data = data, statistic = fun.statistic, R = R, parallel = parallel, ncpus = ncpus, 
                         outcome.type = outcome.type, formula = formula, Y = Y, X = X, Z = Z,
                         offset = offset, rate.multiplier = rate.multiplier, 
                         subgroup = subgroup,  
                         exposure.scalar = exposure.scalar, exposure.center = X_mean, clusters = clusters)
  
  # Format output from boot function
  if (dim(boot_out$t)[2] > 11) {
    # Reformat if more than 1 estimate provided (e.g. if subgroups are used or if categorical exposure 
    # with more than 2 categories). This is necessary because the boot package only outputs a single line 
    # of results for each bootstrap iteration, including all possible exposure/subgroup comparisons.
    boot_res <- purrr::map_dfr(seq(1:(dim(boot_out$t)[2]/11)), function (x) {
       # x = 2
      out <- boot_out$t[, (1+(x-1)*11):(11*x)] %>%
        data.frame() %>%
        dplyr::mutate(boot = paste0("Bootstrap",seq(1:R)))
    })
    names(boot_res) <- c(rownames(pt_estimate$parameter.estimates), rownames(pt_estimate$predicted.outcome), "boot")
  } else {
    boot_res <- boot_out$t %>%
      data.frame() %>%
      dplyr::mutate(boot = paste0("Bootstrap", seq(1:R)))
    names(boot_res) <- c(rownames(pt_estimate$parameter.estimates), rownames(pt_estimate$predicted.outcome), "boot")
  }    
  
  # Use bootstrap results (boot_res) to calculate 95% CI for effect estimates
    ci <- boot_res %>% 
      dplyr::mutate_at(dplyr::vars(.data$`Risk Difference`:.data$`Mean outcome without exposure/treatment`), as.character) %>% 
      dplyr::mutate_at(dplyr::vars(.data$`Risk Difference`:.data$`Mean outcome without exposure/treatment`), as.numeric) %>%
      dplyr::group_by(.data$Comparison, .data$Subgroup) %>%
      dplyr::summarise_at(dplyr::vars(.data$`Risk Difference`:.data$`Mean Difference`, .data$`Mean outcome with exposure/treatment`:.data$`Mean outcome without exposure/treatment`),
                   list(l.cl = ~ stats::quantile(., probs = 0.025, na.rm = T), 
                        u.cl = ~ stats::quantile(., probs = 0.975, na.rm = T))) 
    ci.long <- ci %>% 
      dplyr::select(.data$Subgroup, .data$Comparison, tidyselect::contains("l.cl")) %>%
      tidyr::pivot_longer(tidyselect::contains("l.cl"), names_to = "Parameter", values_to = "2.5% CL", values_drop_na = T) %>%
      dplyr::mutate(Parameter = gsub("_l.cl", "", .data$Parameter)) %>%
      dplyr::left_join(ci %>% 
                         dplyr::select(.data$Subgroup, .data$Comparison, tidyselect::contains("u.cl")) %>%
                         tidyr::pivot_longer(tidyselect::contains("u.cl"), names_to = "Parameter", values_to = "97.5% CL", values_drop_na = T) %>%
                         dplyr::mutate(Parameter = gsub("_u.cl", "", .data$Parameter)), by =c("Comparison","Subgroup", "Parameter"))
    
    
    ## Because predicted.outcomes is a character output (contains comparison and subgroup names), we need to
    # make the parameter estimates into character to start too
    as.char.param.estimates <- pt_estimate$parameter.estimates %>% 
      dplyr::mutate(dplyr::across(tidyselect::everything(), as.character))
    rownames(as.char.param.estimates) <- rownames(pt_estimate$parameter.estimates)
    
    res_ci_df <- data.frame(t(as.char.param.estimates)) %>%
      cbind(., data.frame(t(pt_estimate$predicted.outcome))) %>%
      tidyr::pivot_longer(!tidyselect::contains("Subgroup") & !tidyselect::contains("Comparison"), names_to = "Parameter", values_to = "Estimate", values_drop_na = T)  %>%
      dplyr::mutate(Parameter = gsub("\\.", " ", .data$Parameter)) %>%
      dplyr::mutate(Parameter = ifelse(.data$Parameter == "Mean outcome with exposure treatment", "Mean outcome with exposure/treatment", 
                    ifelse(.data$Parameter == "Mean outcome without exposure treatment", "Mean outcome without exposure/treatment", 
                           ifelse(.data$Parameter == "Number needed to treat harm", "Number needed to treat/harm", .data$Parameter)))) %>%
      dplyr::left_join(ci.long, by = c("Parameter", "Comparison", "Subgroup")) %>%
      dplyr::mutate(Outcome = pt_estimate$Y) %>%
      dplyr::select(.data$Outcome, .data$Comparison, .data$Subgroup, .data$Parameter, .data$Estimate:.data$`97.5% CL`) %>%
      dplyr::mutate_at(dplyr::vars(tidyselect::contains("Estimate") | tidyselect::contains("CL")), as.character) %>% 
      dplyr::mutate_at(dplyr::vars(tidyselect::contains("Estimate") | tidyselect::contains("CL")), as.numeric)
    
    summary <- res_ci_df %>%
      dplyr::mutate(`Estimate (95% CI)` = ifelse(.data$Parameter == "Number needed to treat/harm", 
                                                 formatC(round(.data$Estimate, 3), format = "f", digits = 3), 
                                                 paste0(formatC(round(.data$Estimate, 3), format = "f", digits = 3), 
                                                        " (", 
                                                        formatC(round(.data$`2.5% CL`, 3), format = "f", digits = 3), 
                                                        ", ", 
                                                        formatC(round(.data$`97.5% CL`, 3), format = "f", digits = 3), 
                                                        ")")),
                    name = ifelse(is.na(.data$Subgroup), paste0(.data$Comparison, " Estimate (95% CI)"), paste0(.data$Comparison, "_", .data$Subgroup, " Estimate (95% CI)"))) %>%
      dplyr::select(.data$Parameter, .data$name, .data$`Estimate (95% CI)`) %>%
      tidyr::pivot_wider(names_from = .data$name, values_from = .data$`Estimate (95% CI)`) %>%
      as.data.frame() %>%
      dplyr::filter(!grepl("exposure/treatment", .data$Parameter))
    rownames(summary) <- summary$Parameter
    
    
    ##Remove subgroups if not present (all NA)
    res_ci_df <- res_ci_df[colSums(!is.na(res_ci_df)) > 0]
  
  ##Make predicted.outcome CI
    pred.outcome <- res_ci_df %>%
      dplyr::filter(grepl("exposure/treatment", .data$Parameter)) %>%
      dplyr::mutate(Group = ifelse(.data$Parameter == "Mean outcome with exposure/treatment", gsub("_v._.*", "", .data$Comparison), gsub(".*_v._", "", .data$Comparison))) %>%
      dplyr::select(tidyselect::any_of(c("Parameter", "Outcome", "Group", "Subgroup", "Estimate", "2.5% CL","97.5% CL"))) %>%
      unique() %>%
      dplyr::arrange(dplyr::across(tidyselect::any_of(c("Group", "Subgroup", "Parameter")))) 
    
# Output results list
  res <- list(
    summary = summary[,-1, drop = FALSE],
    results.df = res_ci_df %>%
      dplyr::filter(!grepl("exposure/treatment", .data$Parameter)), 
    n = dplyr::n_distinct(data), 
    R = R, 
    boot.result = boot_res,
    contrast = pt_estimate$contrast,
    family = pt_estimate$family, 
    formula = pt_estimate$formula,
    # predicted.data = pt_estimate$predicted.data, 
    predicted.outcome = pred.outcome,
    glm.result = pt_estimate$glm.result
    )
  
  class(res) <- c("gComp", class(res))
  return(res)
}
  
