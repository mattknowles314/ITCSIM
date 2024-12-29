#' Simulate a trial
#'
#' Wrapper around [simsurv::simsurv] to simulate a clinical trial
#'
#' @param n Number of patients in total.
#' @param covs A data frame containing covariate information for each individual.
#' @param lambdas See [simsurv::simsurv].
#' @param gammas See [simsurv::simsurv].
#' @param pBetaTrt Log hazard ratio for treatment.
#' @param pBetaSex Log hazard ratio for sex
#' @param sizebinom For the [stats::rbinom function].
#' @param pbinom Probability of being assigned to a treatment.
#' @param maxt Maximim observation time.
#' @param trts Vector of treatments.
#' @param censProb Censoring probability
#'
#' @returns An object of class simtrial
#'
#' @export
simulate_trial <- function(n = 50, distribution  = "weibull", covs = NULL, lambdas = 0.1, gammas = 1.5,
                           pBetaTrt = -0.5, pBetaSex = 0, sizebnom = 1L, pBinom = 0.5, maxt = NULL,
                           trts = c("A", "B"), pMale = 0.51, tdeVal = 0.15,
                           tdef = "log", ageMean = 65, ageSD = 2.5, censProb = 0.3){
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
    betas = c(TRT = pBetaTrt,
              SEX = pBetaSex),
    x = covs,
    tde = c(TRT = tdeVal),
    tdefunction = tdef,
    maxt = maxt
  ) |>
    dplyr::rename(AVAL = eventtime) |>
    dplyr::rename(CNSR = status) |>
    dplyr::mutate(CNSR = rbinom(n, sizebnom, censProb))

  out <- merge(covs, simdata, by = "id") |>
    dplyr::mutate(TRT = dplyr::case_when(
      TRT == 1 ~ trts[1],
      TRT == 0 ~ trts[2]
    )) |>
    dplyr::mutate(SEX = dplyr::case_when(
      SEX == 1 ~ "M",
      SEX == 0 ~ "F"
    )) |>
    dplyr::rename(USUBJID = id)
  class(out) <- c("tte", class(out))
  out
}

