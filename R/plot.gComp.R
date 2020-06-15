#' Plot estimates of difference and ratio effects obtained in the bootstrap
#' computations of the g-computation
#'
#' @description Plot histograms and Q-Q plots for each the difference and ratio
#'   estimates
#'
#' @param x (Required) An object of class \code{gComp}.
#' @param ... (Optional) additional arguments to be supplied to the
#'   `\code{geom_histogram} call (e.g. to adjust binwidth for histogram, assign
#'   colors, etc.).
#'
#' @return a plot containing histograms and Q-Q plots of the difference and
#'   ratio estimates returned from R bootstrap iterations
#' @export
#' @method plot gComp
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
#' set.seed(58)
#' diabetes.result <- gComp(data = cvdd, Y = "cvd_dth", X = "DIABETES",
#' Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), outcome.type = "binary", R = 60)
#' plot(diabetes.result)
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom dplyr group_by mutate select ungroup
#' @importFrom tidyr pivot_longer 
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_histogram facet_wrap geom_qq geom_qq_line
#'   theme_bw labs label_wrap_gen
#' @importFrom rlang .data
#' @importFrom tidyselect contains
#'
#' @keywords plot.gComp




plot.gComp <- function(x, ...) {

  df <- x$boot.result %>%
    dplyr::select(-tidyselect::contains("/")) %>%
    dplyr::group_by(.data$Comparison, .data$Subgroup) %>%
    tidyr::pivot_longer(cols = .data$`Risk Difference`:.data$`Mean Difference`, names_to = "key", values_drop_na = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(key = factor(.data$key, levels = c("Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Mean Difference")),
                  value = as.numeric(.data$value),
                  Comparison = gsub("_v._", " v. ", .data$Comparison),
                  test = ifelse(is.na(.data$Subgroup), .data$Comparison, paste0(.data$Comparison, " ", .data$Subgroup)))
  
    if (length(unique(df$test)) == 1) {
      hist <- ggplot2::ggplot(df) + 
        ggplot2::geom_histogram(ggplot2::aes(x = value), bins = x$R/(x$R*.05)) +
        ggplot2::facet_wrap(~key, scales = "free", nrow = 2) +
        ggplot2::theme_bw() +
        ggplot2::labs(title = "Histograms") 
      qqplot <- ggplot2::ggplot(df, ggplot2::aes(sample = value)) +
        ggplot2::geom_qq(shape = 1, size = 2) +
        ggplot2::facet_wrap(~key, scales = "free", nrow = 2) +
        ggplot2::geom_qq_line(linetype = "dotdash", color = "red") + #line.p = c(0.025, 0.975)
        ggplot2::theme_bw() +
        ggplot2::labs(title = "Q-Q plots") 
    } else if (length(unique(df$Comparison)) == 1) {
      hist <- ggplot2::ggplot(df) + 
        ggplot2::geom_histogram(ggplot2::aes(x = value), bins = x$R/(x$R*.05)) + 
        ggplot2::facet_wrap(Subgroup ~ key, 
                            scales = "free", 
                            ncol = length(unique(df$key)), 
                            labeller = ggplot2::label_wrap_gen(14)) + 
        ggplot2::theme_bw() + 
        ggplot2::labs(title = "Histograms") 
      qqplot <- ggplot2::ggplot(df, ggplot2::aes(sample = value)) + 
        ggplot2::geom_qq(shape = 1, size = 2) + 
        ggplot2::facet_wrap(Subgroup ~ key, 
                            scales = "free", 
                            ncol = length(unique(df$key)),
                            labeller = ggplot2::label_wrap_gen(14)) + 
        ggplot2::geom_qq_line(linetype = "dotdash", color = "red") + #line.p = c(0.025, 0.975)
        ggplot2::theme_bw() + 
        ggplot2::labs(title = "Q-Q plots") 
    } else {
      hist <- ggplot2::ggplot(df) + 
        ggplot2::geom_histogram(ggplot2::aes(x = value), bins = x$R/(x$R*.05)) + 
        ggplot2::facet_wrap(test ~ key, 
                            scales = "free", 
                            ncol = length(unique(df$key)), 
                            labeller = ggplot2::label_wrap_gen(14)) + 
        ggplot2::theme_bw() + 
        ggplot2::labs(title = "Histograms") 
      qqplot <- ggplot2::ggplot(df, ggplot2::aes(sample = value)) + 
        ggplot2::geom_qq(shape = 1, size = 2) + 
        ggplot2::facet_wrap(test ~ key, 
                            scales = "free", 
                            ncol = length(unique(df$key)),
                            labeller = ggplot2::label_wrap_gen(14)) + 
        ggplot2::geom_qq_line(linetype = "dotdash", color = "red") + #line.p = c(0.025, 0.975)
        ggplot2::theme_bw() + 
        ggplot2::labs(title = "Q-Q plots") }
    
  gridExtra::grid.arrange(hist, qqplot, ncol = 2)
  
}
