#' Plot a trial simulation
#'
#' @param trial An object of class 'tte'
#'
#' @export
plot.tte <- function(trial, strat = "trt", xlab = "Time", ylab = "Survival",
                          type = "survival", legend = "Treatment", CI = TRUE,
                          censor = TRUE, median = FALSE) {
  f <- as.formula(paste0("survival::Surv(time, status) ~ ", strat))

  p <- ggsurvfit::survfit2(f, data = trial) |>
    ggsurvfit::ggsurvfit(linewidth = 1, type = type) +
    ggsurvfit::add_risktable(
      risktable_stats = "{n.risk} ({cum.event})") +
    ggsurvfit::scale_ggsurvfit() +
    ggsurvfit::add_pvalue(caption = "Log-rank {p.value}") +
    ggsurvfit::add_legend_title(legend) +
    ggplot2::labs(xlab, ylab) +
    ggplot2::theme_bw()

  if (CI) {
    p <- p + ggsurvfit::add_confidence_interval()
  }

  if (censor) {
    p <- p + ggsurvfit::add_censor_mark(size = 2, alpha = 0.2)
  }

  if (median) {
    p <- p + ggsurvfit::add_quantile(y_value = 0.5, linetype = "dotted")
  }

  p
}
