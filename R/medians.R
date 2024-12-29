#' Differences in medians
#'
#' @param trial An object of class 'tte'
#' @param strat The strata on which to calculate the differences between
#'
#' @export
median_difference <- function(trial, strat = "TRT") {
  if (!inherits(trial, "tte")) {
    rlang::abort("Trial data is not of class tte")
  }

  survObj <- survival::survfit(
    as.formula(paste0("survival::Surv(AVAL, CNSR) ~ ", strat)),
    data = trial)

  meds <- survival:::median.survfit(survObj)
  out <- meds[1] - meds[2]
  out
}
