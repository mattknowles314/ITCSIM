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
simulate_trial <- function(n = 50, distribution  = "weibull", covs = NULL, lambdas = 0.1, gammas = 1.5,
                           pBeta = -0.5, sizebnom = 1L, pBinom = 0.5, maxt = NULL,
                           trts = c("A", "B"), pMale = 0.51, tdeVal = 0.15,
                           tdef = "log", seed = 1, ageMean = 65, ageSD = 2.5){
  if (is.null(covs)) {
    covs <- data.frame(id = 1:n,
                       TRT = stats::rbinom(n, sizebnom, pBinom),
                       SEX = stats::rbinom(n, sizebnom, pMale),
                       AGE = stats::rnorm(n, mean = ageMean, sd = ageSD))
  }
  simdata <- simsurv::simsurv(
    lambdas = lambdas,
    dist = distribution,
    gammas = gammas,
    betas = c(TRT = pBeta),
    x = covs,
    tde = c(TRT = tdeVal),
    tdefunction = tdef,
    maxt = maxt,
    seed = seed
  ) |>
    dplyr::rename(AVAL = eventtime) |>
    dplyr::rename(CNSR = status)

  out <- merge(covs, simdata, by = "id") |>
    dplyr::mutate(TRT = dplyr::case_when(
      TRT == 1 ~ trts[1],
      TRT == 0 ~ trts[2]
    )) |>
    dplyr::rename(USUBJID = id)
  class(out) <- c("tte", class(out))
  out
}

