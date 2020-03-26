#' Plot estimates of difference and ratio effects obtained in the bootstrap computations of the g-computation
#'
#' @description Plot histograms and Q-Q plots for each the difference and ratio estimates
#'
#' @param gComp.res (Required) An object of class\code{gComp} with R bootstrap iterations of a parameter estimate for difference and ratio.
#' @param ... additional arguments to be supplied to the \code{geom_histogram} call (e.g. to adjust binwidth for histogram, assign colors, etc.).
#' 
#' @return a plot containing histograms and Q-Q plots of the difference and ratio estimates returned from R bootstrap iterations
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
#' @importFrom ggplot2 ggplot aes geom_histogram facet_grid facet_wrap geom_qq theme_bw geom_abline labs
#'
#' @keywords plot.gComp




plot.gComp <- function(gComp.res) {
  #grepl(":", gComp.res$formula[3])
  if(gComp.res$boot.result$test[1] != "Estimate") {
    df <- gComp.res$boot.result %>%
      tibble::as_tibble() %>%
      dplyr::group_by(test) %>%
      tidyr::pivot_longer(cols = `Risk Difference`:`Number needed to treat`, names_to = "key", values_drop_na = TRUE) %>%
      dplyr::mutate(key = factor(key, levels = c("Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Mean Difference", "Number needed to treat")),
                    value = as.numeric(value)) %>%
      na.omit() 
    
    hist <- ggplot2::ggplot(df) + 
      ggplot2::geom_histogram(ggplot2::aes(x = value), bins = gComp.res$R/(gComp.res$R*.05)) + 
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
    df <- gComp.res$boot.result %>%
      tibble::as_tibble() %>%
      # dplyr::rename(`Risk Difference` = V1, `Risk Ratio` = V2, `Odds Ratio` = V3, `Incidence Rate Difference` = V4, `Incidence Rate Ratio` = V5, `Mrginal Difference` = V6, `Number needed to treat` = V7) %>% 
      tidyr::gather() %>%
      dplyr::mutate(key = factor(key, levels = c("Risk Difference", "Risk Ratio", "Odds Ratio", "Incidence Rate Difference", "Incidence Rate Ratio", "Mean Difference", "Number needed to treat")),
             value = as.numeric(value)) %>%
      na.omit() 
    hist <- ggplot2::ggplot(df) + 
      ggplot2::geom_histogram(ggplot2::aes(x = value), bins = gComp.res$R/(gComp.res$R*.05)) + 
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
