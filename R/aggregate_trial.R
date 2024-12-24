#' Create an aggregate dataset from a trial
#'
#' @param trial An object of class 'tte'
#'
#' @export
aggregate.tte <- function(trial) {
  out <- trial |>
    dplyr::group_by(TRT) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_age = median(AGE),
      prop_male = mean(SEX)
    )
  out
}
