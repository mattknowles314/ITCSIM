#' Create a population summay table
#'
#' @param trial An object of class 'tte'
#'
#' @export
table.tte <- function(trial){
  Tplyr::tplyr_table(trial, TRT) |>
    Tplyr::add_layer(
      Tplyr::group_desc(AGE, by = "Age") |>
        Tplyr::set_format_strings(
          'Mean (SD)' = Tplyr::f_str('xx.xx (xx.xxx)', mean, sd)
        )
    ) |>
    Tplyr::build() |>
    knitr::kable()
}
