#' Perform g-computation to estimate difference and ratio effects of an exposure
#'
#' @description Generate a point estimate of the outcome difference and ratio
#'   using G-computation
#'
#' @param data (Required) A data.frame containing variables for
#'   \code{Y}, \code{X}, and \code{Z} or with variables matching the model
#'   variables specified in a user-supplied formula. Data set should also
#'   contain variables for the optional \code{subgroup} and \code{offset}, if
#'   they are specified.
#' @param outcome.type (Required) Character argument to describe the outcome
#'   type. Acceptable responses, and the corresponding error distribution and
#'   link function used in the \code{glm}, include: \describe{
#'   \item{binary}{(Default) A binomial distribution with link = 'logit' is
#'   used.} \item{count}{A Poisson distribution 
#'   with link = 'log' is used.}
#'   \item{rate}{A Poisson distribution with link = 'log' is used; ideal for 
#'    events/person-time outcomes.} \item{continuous}{A gaussian distribution 
#'    with link = 'identity' is used.}
#'   }
#' @param formula (Optional) Default NULL. An object of class "formula" (or one
#'   that can be coerced to that class) which provides the the complete model
#'   formula, similar to the formula for the glm function in R (e.g. `Y ~ X + Z1
#'   + Z2 + Z3`). Can be supplied as a character or formula object. If no
#'   formula is provided, Y and X must be provided.
#' @param Y (Optional) Default NULL. Character argument which specifies the
#'   outcome variable. Can optionally provide a formula instead of \code{Y} and
#'   \code{X} variables.
#' @param X (Optional) Default NULL. Character argument which specifies the
#'   exposure variable (or treatment group assignment), which can be binary,
#'   categorical, or continuous. This variable can be supplied as a factor
#'   variable (for binary or categorical exposures) or a continuous variable.
#'   For binary/categorical exposures, \code{X} should be supplied as a factor with 
#'   the lowest level set to the desired referent. Numeric variables are 
#'   accepted, but will be centered (see Note). Character variables are not 
#'   accepted and will throw an error. Can optionally provide a formula
#'   instead of \code{Y} and \code{X} variables.
#' @param Z (Optional) Default NULL. List or single character vector which
#'   specifies the names of covariates or other variables to adjust for in the
#'   \code{glm} function. All variables should either be factors, continuous, 
#'   or coded 0/1 (i.e. not character variables). Does not allow interaction terms.
#' @param subgroup (Optional) Default NULL. Character argument that indicates
#'   subgroups for stratified analysis. Effects will be reported for each
#'   category of the subgroup variable. Variable will be automatically converted
#'   to a factor if not already.
#' @param offset (Optional, only applicable for rate outcomes) Default NULL.
#'   Character argument which specifies the person-time denominator for rate
#'   outcomes to be included as an offset in the Poisson regression model.
#'   Numeric variable should be on the linear scale; function will take natural
#'   log before including in the model.
#' @param rate.multiplier (Optional, only applicable for rate outcomes) Default
#'   1. Numeric value to multiply to the rate-based effect measures. This option
#'   facilitates reporting effects with interpretable person-time denominators.
#'   For example, if the person-time variable (offset) is in days, a multiplier
#'   of 365*100 would result in estimates of rate differences per 100
#'   person-years.
#' @param exposure.scalar (Optional, only applicable for continuous exposure)
#'   Default 1. Numeric value to scale effects with a continuous exposure. This 
#'   option facilitates reporting effects for an interpretable contrast (i.e. 
#'   magnitude of difference) within the continuous exposure. For example, if 
#'   the continuous exposure is age in years, a multiplier of 10 would result 
#'   in estimates per 10-year increase in age rather than per a 1-year increase 
#'   in age.
#' @param exposure.center (Optional, only applicable for continuous exposure)
#'   Default TRUE. Logical or numeric value to center a continuous exposure. This
#'   option facilitates reporting effects at the mean value of the exposure 
#'   variable, and allows for a mean value to be provided directly to the function
#'   in cases where bootstrap resampling is being conducted and a standardized 
#'   centering value should be used across all bootstraps. See note below on 
#'   continuous exposure variables for additional details.
#'
#' @return A named list containing the following: 
#' 
#'   \item{$parameter.estimates}{Point estimates for the risk difference, risk
#'   ratio, odds ratio, incidence rate difference, incidence rate ratio, mean
#'   difference and/or number needed to treat/harm, depending on the outcome.type}
#'   \item{$formula}{Model formula used to fit the \code{glm}}    
#'   \item{$contrast}{Contrast levels compared} 
#'   \item{$Y}{The response variable} 
#'   \item{$covariates}{Covariates used in the model} 
#'   \item{$n}{Number of observations provided to the model} 
#'   \item{$family}{Error distribution used in the model} 
#'   \item{$predicted.data}{A data.frame with the predicted values for the exposed 
#'   and unexposed counterfactual predictions for each observation in the original 
#'   dataset (on the log scale)} 
#'    \item{$predicted.outcome}{A data.frame with the marginal mean
#'   predicted outcomes for each exposure level} 
#'   \item{$glm.result}{The \code{glm} class object returned from the 
#'   fitted regression of the outcome on the exposure and relevant covariates.} 
#'   formula = formula, 
#'
#' @details The \code{pointEstimate} function executes the following steps on
#'   the data: 
#'   \enumerate{ 
#'   \item Fit a regression of the outcome on the exposure
#'   and relevant covariates, using the provided data set. 
#'   \item Using the
#'   modelfit in step 1, predict counterfactuals (e.g. calculate predicted
#'   outcomes for each observation in the data set under each level of the
#'   treatment/exposure). 
#'   \item Estimate the marginal difference/ratio of treatment effect by 
#'   taking the difference or ratio of the average of all observations under 
#'   the treatment/no treatment regimes. 
#'   }
#'
#'   As counterfactual predictions are generated with random sampling of the
#'   distribution, users should set a seed (\code{\link{set.seed}}) for
#'   reproducible confidence intervals.
#'
#' @note
#'  Note that for a protective exposure (risk difference less than 0), the 
#'   'Number needed to treat/harm' is interpreted as the number needed to treat, 
#'   and for a harmful exposure (risk difference greater than 0), it is 
#'   interpreted as the number needed to harm. 
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
#' @references 
#'  Ahern J, Hubbard A, Galea S. Estimating the effects of potential public health 
#'   interventions on population disease burden: a step-by-step illustration of 
#'   causal inference methods. Am. J. Epidemiol. 2009;169(9):1140–1147.
#'   \href{https://doi.org/10.1093/aje/kwp015}{Manuscript link}
#'  
#'  Altman DG, Deeks JJ, Sackett DL. Odds ratios should be avoided when events 
#'   are common. BMJ. 1998;317(7168):1318. \href{https://doi.org/10.1136/bmj.317.7168.1318 }{Manuscript link}
#' 
#'  Hernán MA, Robins JM (2020). Causal Inference: What If. Boca Raton: 
#'   Chapman & Hall/CRC. /href{https://www.hsph.harvard.edu/miguel-hernan/causal-inference-book/}{Book link}
#' 
#'  Robins J. A new approach to causal inference in mortality studies with a 
#'   sustained exposure period—application to control of the healthy worker 
#'   survivor effect. Mathematical Modelling. 1986;7(9):1393–1512. \href{https://doi.org/10.1016/0270-0255(86)90088-6}{Manuscript link}
#'   
#'  Snowden JM, Rose S, Mortimer KM. Implementation of G-computation on a 
#'   simulated data set: demonstration of a causal inference technique. 
#'   Am. J. Epidemiol. 2011;173(7):731–738. \href{https://doi.org/10.1093/aje/kwq472}{Manuscript link}
#'   
#'  Westreich D, Cole SR, Young JG, et al. The parametric g-formula to 
#'   estimate the effect of highly active antiretroviral therapy on incident 
#'    AIDS or death. Stat Med. 2012;31(18):2000–2009. \href{https://doi.org/10.1002/sim.5316}{Manuscript link}
#'
#' @export
#'
#' @examples
#' ## Obtain the risk difference and risk ratio for cardiovascular disease or death
#' ## between patients with and without diabetes, while controlling for
#' ## age,
#' ## sex,
#' ## BMI,
#' ## whether the individual is currently a smoker, and
#' ## if they have a history of hypertension.
#' data(cvdd)
#' ptEstimate <- pointEstimate(data = cvdd, Y = "cvd_dth", X = "DIABETES",
#' Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), outcome.type = "binary")
#'
#' @importFrom tidyselect contains all_of everything
#' @importFrom stats as.formula glm model.matrix binomial poisson gaussian predict na.omit
#' @importFrom dplyr select mutate rename summarise across
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' @importFrom purrr map_dfc
#'
#' @seealso \code{\link{gComp}}




pointEstimate <- function(data, 
                          outcome.type = c("binary", "count","rate", "continuous"),
                          formula = NULL, 
                          Y = NULL, 
                          X = NULL, 
                          Z = NULL, 
                          subgroup = NULL,  
                          offset = NULL, 
                          rate.multiplier = 1,
                          exposure.scalar = 1,
                          exposure.center = TRUE) {

  # Bind variable locally to function for offset2
  offset2 <- NULL
  
  # Ensure outcome.type is one of the allowed responses
  outcome.type <- match.arg(outcome.type)
  
  # set new df to modify
  working.df <- data
  
  # Specify model family and link for the given outcome.type
  if (outcome.type %in% c("binary")) {
    family <- stats::binomial(link = 'logit')
  } else if (outcome.type %in% c("count", "rate")) {
    family <- stats::poisson(link = "log")
    if (is.null(offset) & outcome.type == "rate") stop("Offset must be provided for rate outcomes")
  } else if (outcome.type == "continuous") {
    family <- stats::gaussian(link = "identity")
  } else {
    stop("This package only supports binary/dichotomous, count/rate, or continuous outcome variable models")
  }
  
  # Determine model formula, X, Y, and Z from specified terms, ensure sufficient info provided (either X & Y or formula)
  if (!is.null(X)) X <- rlang::sym(X)
  if (is.null(formula)) {
    if (is.null(Y) | is.null(X)) {
      stop("No formula, or Y and X variables provided") 
    }
    if (is.null(Z)) {
      formula <- stats::as.formula(paste(Y,X, sep = " ~ "))
    } else {
      formula <- stats::as.formula(paste(paste(Y,X, sep = " ~ "), paste(Z, collapse = " + "), sep = " + "))   
    }
  } else {
    formula = stats::as.formula(formula)
    if (any(unlist(sapply(formula[[3]], function(x) grepl(":", x)))) | any(unlist(sapply(formula[[3]], function(x) grepl("\\*", x))))) {
      stop("Package not currently able to handle interaction terms")
    } 
    Y <- as.character(formula[[2]])
    X <- rlang::sym(all.vars(formula[[3]])[1])
    Z <- all.vars(formula[[3]])[-1]
  }
  
  # Determine X_mean
  if (is.numeric(data[[X]])) X_mean = ifelse(exposure.center == T, round(mean(data[[X]]), 2), round(exposure.center, 2))
  
  # Specify interaction term in formula if subgroup provided
  if (!is.null(subgroup)) {
    interaction_term <- rlang::sym(paste(as.character(X), subgroup, sep = ":"))
    formula <- stats::as.formula(paste(paste(Y,X, sep = " ~ "), paste(Z, collapse = " + "), interaction_term, sep = " + "))
  }
  
  # Make list of variables that will be used (Y, X, Z, and subgroup/offset if specified)
  if (is.null(offset) & is.null(subgroup)) {
    allVars <- unlist(c(Y, as.character(X), Z))
  } else if (!is.null(offset)) {
    offset <- rlang::sym(offset)
    # data <- data %>%
    #   dplyr::mutate(offset2 = !!offset + 0.00001,
    #                 logOffset = log(offset2))
    if (!is.null(subgroup)){
      subgroup <- rlang::sym(subgroup)
      allVars <- unlist(c(Y, as.character(X), Z, offset, subgroup))
    } else {
      allVars <- unlist(c(Y, as.character(X), Z, offset))
    }
  } else {
    subgroup <- rlang::sym(subgroup)
    allVars <- unlist(c(Y, as.character(X), Z, subgroup))
  }
  
  # Ensure all variables are provided in the dataset
  if (!all(allVars %in% names(working.df))) stop("One or more of the supplied model variables, offset, or subgroup is not included in the data")
  
  # Ensure X is categorical (factor) or numeric, throw error if character
  if (is.numeric(working.df[[X]])) {
    message("Proceeding with X as a continuous variable, if it should be binary/categorical, please reformat so that X is a factor variable")
  } else if (is.factor(working.df[[X]])) {
    if(exposure.scalar != 1) stop("An exposure scaler can not be used with a binary/categorical exposure.  If you intended your exposure to be continuous, ensure it is provided as a numeric variable.")
  } else {
    stop("X must be a factor or numeric variable")
  }
  
  # Ensure Z covariates are NOT character variables in the dataset
  if (!is.null(Z)) {
    test_for_char_df <- sapply(working.df %>% 
      dplyr::select(tidyselect::all_of(Z)), is.character)
    if (any(test_for_char_df)) {
      stop("One of the covariates (Z) is a character variable in the dataset provided.  Please change to a factor or numeric.")
    }
  }
  
  # Force subgroup to be a factor variable
  if (!is.null(subgroup)) {
    working.df <- working.df %>% 
      dplyr::mutate(!!subgroup := factor(!!subgroup))
  }
  
  # Center continuous variable and divide by exposure.scalar
  if(is.numeric(data[[X]])) {
    working.df <- working.df %>%
      dplyr::mutate(!!X := as.vector(scale(!!X, center = exposure.center, scale = exposure.scalar)))
  }
  
  # Run GLM
  if (!is.null(offset)) {
    working.df <- working.df %>%
      dplyr::mutate(offset2 = !!offset + 0.00001)
    glm_result <- stats::glm(formula = formula, data = working.df, family = family, na.action = stats::na.omit, offset = log(offset2))
  } else {
    glm_result <- stats::glm(formula = formula, data = working.df, family = family, na.action = stats::na.omit)
  }
  
  # Predict outcomes for each observation/individual at each level of treatment/exposure
  fn_output <- make_predict_df(glm.res = glm_result, df = working.df, X = X, subgroup = subgroup, offset = offset)
  
  # Rename vars in predicted dataset so it's clear
  results_tbl_all <- fn_output
  names(results_tbl_all) <- c(sapply(names(fn_output), function(x) paste0("predicted value with ",x)))
  
  # Get list of possible treatments/exposures (all levels of X)
  if (is.numeric(data[[X]])) {
    exposure_list <- sapply(c(X_mean, (X_mean + exposure.scalar)), function(x) paste0(X,x))
  } else {
    exposure_list <- sapply(levels(working.df[[X]]), function(x) paste0(X,x), USE.NAMES = F)
  }
                           
                          
  contrasts_list <- sapply(exposure_list[-1], function(x) paste0(x, "_v._", exposure_list[1]), USE.NAMES = F)
  # Calculate estimates for risk/rate/mean differences & ratios
  if (!is.null(subgroup)) { 
    subgroups_list <- sapply(levels(working.df[[subgroup]]), function(x) paste0(subgroup,x), USE.NAMES = F)
      
    if (length(exposure_list) > 2) { # For when subgroups and exposure with more than 2 levels are both specified
      subgroup_contrasts_res <- suppressMessages(purrr::map_dfc(exposure_list[-1], function(e) {
        
        predict_df_e <- fn_output %>%
          dplyr::select(tidyselect::contains(match = c(exposure_list[1], e)))#, tidyselect::contains(e))
        subgroup_res <- suppressMessages(purrr::map_dfc(subgroups_list, function(s) {
          predict_df_s = fn_output %>% 
            dplyr::select(tidyselect::contains(s))
          fn_results_df <- get_results_dataframe(predict.df = predict_df_s, outcome.type = outcome.type, rate.multiplier = rate.multiplier)
          return(c(fn_results_df, subgroup = s, Comparison = paste0(e, "_v._", exposure_list[1])))
        }))
        subgp_results <- subgroup_res %>%
          as.data.frame() 
         colnames(subgp_results) <- paste0(e, "_v._", exposure_list[1],"_", subgroups_list)
        return(subgp_results)
      }))
      results <- subgroup_contrasts_res
    } else { # For when only subgroups are specified
      subgroup_res <- suppressMessages(purrr::map_dfc(subgroups_list, function(s) {
        # s <- subgroups_list[1]
        predict_df_s = fn_output %>% 
          dplyr::select(tidyselect::contains(s))
        fn_results_df <- get_results_dataframe(predict.df = predict_df_s, outcome.type = outcome.type, rate.multiplier = rate.multiplier)
        return(c(fn_results_df, subgroup = s, Comparison = contrasts_list))
      }))
      results <- subgroup_res %>%
        as.data.frame()
      colnames(results) <- subgroups_list
    }
  } else if (length(exposure_list) > 2) { # For when exposure with more than 2 levels is specified (but no subgroups)
    contrasts_res <- suppressMessages(purrr::map_dfc(exposure_list[-1], function(e) {
      # e <- exposure_list[2]
      predict_df_e <- fn_output %>%
        dplyr::select(tidyselect::contains(match = c(exposure_list[1], e))) #contains(exposure_list[1]), tidyselect::contains(e))
      fn_results_df <- get_results_dataframe(predict.df = predict_df_e, outcome.type = outcome.type, rate.multiplier = rate.multiplier)
      return(c(fn_results_df, subgroup = NA, Comparison = paste0(e, "_v._", exposure_list[1])))
    }))
    results <- contrasts_res %>%
      as.data.frame()
    colnames(results) <- contrasts_list
  } else { # For when NO subgroups are specified and exposure has only 2 levels
    fn_results_df <- get_results_dataframe(predict.df = fn_output, outcome.type = outcome.type, rate.multiplier = rate.multiplier)
    
    results <- fn_results_df %>%
      as.data.frame() %>%
      dplyr::rename(Estimate = ".") %>%
      dplyr::mutate_if(is.numeric, round, digits = 4) %>%
      rbind(., NA) %>%
      rbind(., contrasts_list)
  }
  rownames(results) <- c("Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Mean Difference", "Number needed to treat/harm", "Mean outcome with exposure/treatment", "Mean outcome without exposure/treatment", "Subgroup", "Comparison")

  param.est <- results[1:7, , drop = F] %>% 
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.numeric))
  rownames(param.est) <- rownames(results)[1:7]
                    
  # List of items to return to this function call
  res <- list(parameter.estimates = param.est,
              formula = formula, 
              contrast = unname(sapply(exposure_list[-1], function(x) paste(x, exposure_list[1], sep = " v. "))),
              Y = Y,
              covariates = ifelse(length(attr(glm_result$terms , "term.labels")) > 1, do.call(paste,as.list(attr(glm_result$terms , "term.labels")[-1])), NA),
              n = as.numeric(dplyr::summarise(working.df, n = dplyr::n())), 
              family = family,
              predicted.data = results_tbl_all,
              predicted.outcome = results[8:11, , drop = FALSE],
              glm.result = glm_result)
  return(res)
}

