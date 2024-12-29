#' Plot a trial simulation
#'
#' @param trial An object of class 'tte'
#'
#' @export
plot.tte <- function(trial, strat = "TRT", xlab = "Time", ylab = "Survival",
                          type = "survival", legend = "Treatment", CI = TRUE,
                          censor = TRUE, median = FALSE) {
  f <- as.formula(paste0("survival::Surv(AVAL, CNSR) ~ ", strat))

  p <- ggsurvfit::survfit2(f, data = trial) |>
    ggsurvfit::ggsurvfit(linewidth = 1, type = type) +
    ggsurvfit::add_risktable(
      risktable_stats = "{n.risk}") +
    ggsurvfit::scale_ggsurvfit() +
    ggsurvfit::add_pvalue(caption = "L-R {p.value}") +
    ggsurvfit::add_legend_title(legend) +
    ggplot2::labs(xlab, ylab) +
    ggplot2::theme_bw()

  if (CI) {
    p <- p + ggsurvfit::add_confidence_interval()
  }

  if (censor) {
    p <- p + ggsurvfit::add_censor_mark(size = 4)
  }

  if (median) {
    p <- p + ggsurvfit::add_quantile(y_value = 0.5, linetype = "dotted")
  }

  p
}

#' Median of a trial in a TTE object
#'
#' @param tial An object of class 'tte'
#' @param strat The strata to calculate medians for. Defaults to treatment
#'
#' @export
median.tte <- function(trial, strat = "TRT") {
  survObj <- survival::survfit(
    as.formula(paste0("survival::Surv(AVAL, CNSR) ~ ", strat)),
    data = trial)

  meds <- survival:::median.survfit(survObj)
  out <- data.frame(x = levels(as.factor(trial[[strat]])),
                    y = c(meds[1], meds[2]))
  colnames(out) <- c(strat, "Median")
  out
}
