#' Perform g-computation to estimate difference and ratio effects of an outcome.type exposure
#'
#' @description Generate a point estimate of the outcome difference and ratio
#'
#' @param data the data as a data.frame or tibble containing variables for \code{Y}, \code{X}, and \code{Z} or with variables matching the model variables specified in a user-supplied formula.
#' @param formula optional argument which provides the model formula for the \code{glm} function to be used internally. The first predictor (after the "~") is assumed to be the exposure variable.
#' Can be supplied as a character or formula object, the function will internally convert it to a formula if not supplied as such. 
#' If no formula is provided, Y and X must be provided.
#' @param Y optional character argument which provides the response variable that will be supplied to the \code{glm} function internally.  
#' Must also provide \code{X} in order for the function to work.  Can optionally provide a formula instead of \code{Y} and \code{X} variables.
#' @param X optional character argument which provides the binary exposure/treatment group assignment that will be supplied to the \code{glm} function internally.  
#' This variable can be supplied as a factor variable or as a numeric of 0 or 1. Must also provide \code{Y} in order for the function to work.  
#' Can optinoally provide a formula instead of \code{Y} and \code{X} variables.
#' @param Z optional argument which provides the covariates or other variables to adjust for in the \code{glm} function to be used internally.  
#' Can be either a single character expression or vector of quoted variable names.
#' @param outcome.type required argument to describe the outcome type. Acceptable responses include "binary", "count", "rate", and "continuous". 
#' This argument is used to determine the error distribution and link function to be used in the model when calling the \code{glm} function internally. Default is "binary'
#' For "binary" a binomial distribution with link = 'logit' is used, 
#' for "count" or "rate" a Poisson distribution with link = 'log' is used, 
#' for "continuous" a gaussian distribution with link = 'identity' is used. 
#' @param offset character argument which identifies the variable to use for offset. Required if using outcome.type of "rate". Default is NULL. 
#' Internal functions automatically convert offset to log scale, so should be provided in original units.
#' @param rate.multiplier optional numeric argument to identify the multiplier to provide rate outcome in desired units. Only used if outcome.type == "rate." 
#' For example, the rate for an offset provided in days could be converted to years by supplying rate.multiplier = 365. 
#' #' @return a list containing the following:
#' \itemize{
#' \item{"Risk Difference"} {point estimate of the risk difference for binary outcomes, will be NA for other outcome types}
#' \item{"Risk Ratio"} {point estimate of the risk ratio for binary outcomes, will be NA for other outcome types}
#' \item{"Odds Ratio"} {point estimate of the odds ratio for binary outcomes, will be NA for other outcome types}
#' \item{"Incidence Rate Difference"} {point estimate of the rate difference for rate outcomes, will be NA for other outcome types}
#' \item{"Incidence Rate Ratio"} {point estimate of the rate ratio for rate outcomes, will be NA for other outcome types}
#' \item{"Marginal Difference"} {point estimate of the marginal difference for continuous or count outcomes, will be NA for other outcome types}
#' \item{"Number needed to treat"} {1/(Risk Difference) for binary outcomes, 1/(Incidence Rate Difference) for rate outcomes, will be NA for other outcome types}
#' \item{"n} {number of observations provided to the model}
#' \item{"contrast"} {the contrast levels compared}
#' \item{"family"} {the error distribution used in the model}
#' \item{"formula"} {the model formula used to fit the \code{glm}}
#' \item{"Y"} {the response variable}
#' \item{"covariates"} {covariates used in the model}
#' \item{"predictedData"} {a tibble with the predicted values for the naturnal course, and both treatment and no treatment counterfactual predicitions for each observation in the original dataset}
#' }
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
#' @importFrom tidyselect one_of
#' @importFrom stats as.formula glm model.matrix contrasts binomial na.omit predict
#' @importFrom dplyr expr select mutate select_if rowwise funs        
#' @importFrom tibble as_tibble tibble
#' @importFrom rlang sym
#' @importFrom magrittr %>%
#' @importFrom purrr negate
#'
#' @keywords pointEstimate






#formula  an optional an object of class "formula" (or one that can be coerced to that class): a symbolic description of the model to be fitted. The details of model specification are given under ‘Details’.requires a string of the format "Y ~ X + adjustVar1 + adjustVar2 + adjustVar3".  The function will internally convert it to a formula class.
#X a binary treatment indicator; control = 0, treatment = 1 
pointEstimate <- function(data, formula = NULL, Y = NULL, X = NULL, Z = NULL, outcome.type = c("binary", "count","rate", "continuous"), offset = NULL, rate.multiplier = 1, subgroup = NULL) {
  # data = cvdd
  # Y = "cvd_dth"
  # # X = "DIABETES"
  # X = "bmicat"
  # Z = c("AGE", "SEX", "DIABETES", "CURSMOKE", "PREVHYP")
  # # Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP")
  # outcome.type = "binary"
  # offset = NULL
  # rate.multiplier = 1
  # subgroup = "SEX"
  # formula = NULL

  outcome.type <- match.arg(outcome.type)
  
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

  if (!is.null(X)) X <- rlang::sym(X)
  if (is.null(formula)) {
    if (is.null(Y) | is.null(X)) {
      stop("No formula, or Y and X variables provided") 
    }
    if (is.null(Z)) {
      formula <- Y ~ X
    } else {
      formula <- stats::as.formula(paste(paste(Y,X, sep = " ~ "), paste(Z, collapse = " + "), sep = " + "))   
    }
  } else {
    formula = stats::as.formula(formula)
    if (any(unlist(sapply(formula[[3]], function(x) grepl(":", x)))) | any(unlist(sapply(formula[[3]], function(x) grepl("\\*", x))))) {
      stop("g-computation function not currently able to handle interaction terms")
    } 
    Y <- as.character(formula[[2]])
    X <- rlang::sym(all.vars(formula[[3]])[1])
    Z <- all.vars(formula[[3]])[-1]
  }
  
  if (!is.null(subgroup)) {
    # formula <- stats::as.formula(paste(as.character(formula), paste(as.character(X), subgroup, sep = ":"), sep = " + "))
    interaction.term <- rlang::sym(paste(as.character(X), subgroup, sep = ":"))
    formula <- stats::as.formula(paste(paste(Y,X, sep = " ~ "), paste(Z, collapse = " + "), interaction.term, sep = " + "))
    
  }
  
  
  #Ensure all variables are in the dataset
  if (is.null(offset) & is.null(subgroup)) {
    allVars <- unlist(c(Y, as.character(X), Z))
  } else if (!is.null(offset)) {
    offset <- rlang::sym(offset)
    data <- data %>%
      dplyr::mutate(!!offset := !!offset + 0.00001)
    # formula <- stats::as.formula(paste(formula, "offset(offsetlog)", sep = " + "))
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
  
  if(!all(allVars %in% names(data))) stop("One or more of the supplied model variables, offset, or subgroup is not included in the data")
  
  if (!is.null(X)) {
    X.type <- ifelse(is.factor(data[[X]]), "categorical", ifelse(is.numeric(data[[X]]), "numeric", stop("X must be a factor or numeric variable")))
    if(X.type == "numeric") {
      message("Proceeding with X as a continuous variable, if it should be categorical, please reformat so that X is a factor variable")
      # if (nlevels(eval(dplyr::expr(`$`(data, !!X)))) != 2) {
      #   stop("Explanatory variable has more than 2 levels")
      # }
    } #else {
    #   # if (length(unique((eval(dplyr::expr(`$`(data, !!X)))))) == 2) {
    #     ### could write more code to throw an error if the different values are not 0 or 1
    #     data <- data %>% 
    #       dplyr::mutate(!!X := factor(!!X))
    #   # } else {
    #     # stop("Explanatory variable has more than 2 levels")
    #   #}
    # }
  }
  
  if(!is.null(subgroup)) {
    data <- data %>% 
      dplyr::mutate(!!subgroup := factor(!!subgroup))
  }
  
  
  ## Run GLM
  if (!is.null(offset)) {
    data <- data %>%
      dplyr::mutate(offset2 = !!offset + 0.00001)
    glm.result <- stats::glm(formula = formula, data = data, family = family, na.action = stats::na.omit, offset = log(offset2))
  } else {
    glm.result <- stats::glm(formula = formula, data = data, family = family, na.action = stats::na.omit)
  }
  
  fn.output <- make_predict_df(glm.res = glm.result, df = data, X = X, subgroup = subgroup, offset = offset)
  results.tbl_all <- NULL
  exposure.list <- unique(unlist(stringr::str_split(names(fn.output), "_"))) %>%
    stringr::str_subset(pattern = as.character(X))
  
  if (!is.null(subgroup)) {
    subgroups.list <- unique(unlist(stringr::str_split(names(fn.output), "_"))) %>%
      stringr::str_subset(pattern = as.character(subgroup))
    if (length(exposure.list) > 2) {
      contrasts.list <- lapply(exposure.list[-1], function(x) paste0(x, "_v._", exposure.list[1]))
      subgroup.contrasts.res <- purrr::map_dfc(exposure.list[-1], function(e) {
        predict.df.e <- fn.output %>%
          dplyr::select(tidyselect::contains(exposure.list[1]), tidyselect::contains(e))
        subgroup.res <- purrr::map_dfc(subgroups.list, function(s) {
          predict.df.s = fn.output %>% 
            dplyr::select(tidyselect::contains(s))
          fn.results.tibble <- get_results_tibble(predict.df = predict.df.s, outcome.type = outcome.type, X = X, rate.multiplier = rate.multiplier)
          tbl_s <- fn.results.tibble[[1]]
          names(tbl_s) <- 
            x <- c(paste0("pred with ", exposure.list[1], ", ", s), paste0("pred with ", e, ", ", s), paste0("pred odds with ", exposure.list[1], ", ", s), paste0("pred odds with ", e, ", ", s))
          results.tbl_all <- results.tbl_all %>%
            dplyr::bind_cols(tbl_s)
          return(fn.results.tibble[[2]])
        })
        subgp.results <- subgroup.res %>%
          as.data.frame()
        colnames(subgp.results) <- paste0(e, "_v._", exposure.list[1],"_", subgroups.list)
        return(subgp.results)
      })
      results <- subgroup.contrasts.res
    } else {
      subgroup.res <- purrr::map_dfc(subgroups.list, function(s) {
        predict.df.s = fn.output %>% 
          dplyr::select(tidyselect::contains(s))
        fn.results.tibble <- get_results_tibble(predict.df = predict.df.s, outcome.type = outcome.type, X = X, rate.multiplier = rate.multiplier)
        tbl_s <- fn.results.tibble[[1]]
        pred.names <- c(sapply(exposure.list, function(x) paste0("pred with ",x, ", ", s)), sapply(exposure.list, function(x) paste0("pred odds with ",x, ", ", s)))
        names(tbl_s) <- pred.names
        results.tbl_all <- results.tbl_all %>%
          dplyr::bind_cols(tbl_s)
        return(fn.results.tibble[[2]])
      })
      results <- subgroup.res %>%
        as.data.frame()
      colnames(results) <- subgroups.list
    }
  } else if (length(exposure.list) > 2) {
    contrasts.list <- lapply(exposure.list[-1], function(x) paste0(x, "_v._", exposure.list[1]))
    contrasts.res <- purrr::map_dfc(exposure.list[-1], function(e) {
      # e <- exposure.list[2]
      predict.df.e <- fn.output %>%
        dplyr::select(tidyselect::contains(exposure.list[1]), tidyselect::contains(e))
      fn.results.tibble <- get_results_tibble(predict.df = predict.df.e, outcome.type = outcome.type, X = X, rate.multiplier = rate.multiplier)
      tbl_e <- fn.results.tibble[[1]]
      pred.names <- c(sapply(exposure.list, function(x) paste0("pred with ",x, ", ", e)), sapply(exposure.list, function(x) paste0("pred odds with ",x, ", ", e)))
      names(tbl_e) <- pred.names
      results.tbl_all <- results.tbl_all %>%
        dplyr::bind_cols(tbl_e)
      return(fn.results.tibble[[2]])
    })
    results <- contrasts.res %>%
      as.data.frame()
    colnames(results) <- contrasts.list
  } else {
    fn.results.tibble <- get_results_tibble(predict.df = fn.output, outcome.type = outcome.type, X = X, rate.multiplier = rate.multiplier)
    tbl <- fn.results.tibble[[1]]
    pred.names <- c(sapply(exposure.list, function(x) paste0("pred with ",x)), sapply(exposure.list, function(x) paste0("pred odds with ",x)))
    names(tbl) <- pred.names
    results.tbl_all <- results.tbl_all %>%
      dplyr::bind_cols(tbl)
    results <- fn.results.tibble[[2]] %>%
      as.data.frame() %>%
      dplyr::rename(Results = ".") %>%
      dplyr::mutate_if(is.numeric, round, digits = 4)
  }
  rownames(results) <- c("Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Marginal Difference", "Number needed to treat")
  
  
  
  
  res <- list(parameterEstimates = results,
              n = as.numeric(dplyr::summarise(data, n = dplyr::n())), 
              #counterFactuals = c(counterFactControl = counterFactControl, counterFactTrt = counterFactTrt), 
              contrast = paste(paste0(names(glm.result$xlevels[1]), rev(unlist(glm.result$xlevels[1]))), collapse = " v. "), 
              family = family,#paste0(glm.result$family$family, "(link = '", glm.result$family$link,"')"), 
              formula = formula, 
              Y = Y, 
              covariates = ifelse(length(attr(glm.result$terms , "term.labels")) > 1, do.call(paste,as.list(attr(glm.result$terms , "term.labels")[-1])), NA),
              predictedData = results.tbl_all)
  return(res)
}


# framingham <- read_csv("/Users/JGrembi/Classes/STATS290/project/data/frmgham2.csv")
# save(framingham, file = "/Users/JGrembi/Classes/STATS290/project/data/framingham.rda")
