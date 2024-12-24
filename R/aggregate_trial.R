#' Create an aggregate dataset from a trial
#'
#' @param trial An object of class 'tte'
#'
#' @export
aggregate.tte <- function(trial) {
  out <- trial |>
    dplyr::group_by(trt) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_age = median(age),
      prop_male = mean(sex)
    )
  out
}
