#' Simulate a trial
#'
#' Wrapper around [simsurv::simsurv] to simulate a clinical trial
#'
#' @param n Number of patients in total.
#' @param covs A data frame containing covariate information for each individual.
#' @param lambdas See [simsurv::simsurv].
#' @param gammas See [simsurv::simsurv].
#' @param pbeta Log hazard ratio for treatment.
#' @param sizebinom For the [stats::rbinom function].
#' @param pbinom Probability of being assigned to a treatment.
#' @param maxt Maximim observation time.
#' @param trts Vector of treatments.
#'
#' @returns An object of class simtrial
#'
#' @export
simulate_trial <- function(n = 50, covs = NULL, lambdas = 0.1, gammas = 1.5, pbeta = -0.5, sizebnom = 1L, pbinom = 0.5, maxt = 10, trts = c("A", "B")){
  if (is.null(covs)){
    covs <- data.frame(id = 1:n, trt = stats::rbinom(n, sizebnom, pbinom))
  }
  simdata <- simsurv::simsurv(
    lambdas = lambdas,
    gammas = gammas,
    betas = c(trt = -0.5),
    x = covs,
    maxt = maxt
  ) |>
    dplyr::rename(time = eventtime)

  out <- merge(covs, simdata, by = "id") |>
    dplyr::mutate(trt = dplyr::case_when(
      trt == 1 ~ trts[1],
      trt == 0 ~ trts[2]
    ))
  class(out) <- c("simtrial", clas(out))
  out

}
