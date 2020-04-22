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
#' @method gComp plot
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
#' diabetes.result <- gComp(data = cvdd, Y = "cvd_dth", X = "DIABETES",
#' Z = c("AGE", "SEX", "BMI", "CURSMOKE", "PREVHYP"), outcome.type = "binary", R = 10)
#' plot(diabetes.result)
#'
#' @importFrom gridExtra grid.arrange
#' @importFrom dplyr rename group_by mutate
#' @importFrom stats var
#' @importFrom tibble as_tibble
#' @importFrom tidyr pivot_longer gather
#' @importFrom magrittr %>%
#' @importFrom ggplot2 ggplot aes geom_histogram facet_grid facet_wrap geom_qq
#'   theme_bw geom_abline labs
#' @importFrom rlang .data
#'
#' @keywords plot.gComp




plot.gComp <- function(x, ...) {

  if(x$boot.result$test[1] != "Estimate") {
    df <- x$boot.result %>%
      tibble::as_tibble() %>%
      dplyr::group_by(.data$test) %>%
      tidyr::pivot_longer(cols = .data$`Risk Difference`:.data$`Number needed to treat`, names_to = "key", values_drop_na = TRUE) %>%
      dplyr::mutate(key = factor(.data$key, levels = c("Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Mean Difference", "Number needed to treat")),
                    value = as.numeric(value)) %>%
      na.omit() 
    
    hist <- ggplot2::ggplot(df) + 
      ggplot2::geom_histogram(ggplot2::aes(x = value), bins = x$R/(x$R*.05)) + 
      ggplot2::facet_grid(key~test, scales = "free") + 
      ggplot2::theme_bw() + 
      ggplot2::labs(title = "Histograms")
    qqplot <- ggplot2::ggplot(df, ggplot2::aes(sample = value)) + 
      ggplot2::geom_qq(shape = 1, size = 2) + 
      ggplot2::facet_grid(key~test, scales = "free") + 
      ggplot2::geom_qq_line(linetype = "dotdash", color = "red") + #line.p = c(0.025, 0.975)
      ggplot2::theme_bw() + 
      ggplot2::labs(title = "Q-Q plots")
    
  } else {
    df <- x$boot.result %>%
      tibble::as_tibble() %>%
      tidyr::gather() %>%
      dplyr::mutate(key = factor(.data$key, levels = c("Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Mean Difference", "Number needed to treat")),
                    value = as.numeric(value)) %>%
      na.omit() 
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
  }
  gridExtra::grid.arrange(hist, qqplot, ncol = 2)
  
}
