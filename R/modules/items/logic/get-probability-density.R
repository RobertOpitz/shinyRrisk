#' @name getProbabilityDensity
#' @title getProbabilityDensity
#' @description Builds a list containing probability densities.
#' @return List of probability densities.
#' @export
getProbabilityDensity <- function() {
  cat("getProbabilityDensity\n")
  list("Discrete" = list("",
                         "Bernoulli (bern)"             = "bern",
                         "binomial (binom)"             = "binom",
                         "multinomial (multinom)"       = "multinom",
                         "discrete (discrete)"          = "discrete",
                         "geometric (geom)"             = "geom",
                         "hypergeometric (hyper)"       = "hyper",
                         "negative binomial (nbinom)"   = "nbinom",
                         "Poisson (pois)"               = "pois",
                         "uniform discrete (udiscrete)" = "udiscrete"),
       "Continuous" = list("beta (beta)"                       = "beta",
                           "Pert (pert)"                       = "pert",
                           "modified Pert (modpert)"           = "modpert",
                           "Cauchy (cauchy)"                   = "cauchy",
                           "chi-square (chisq)"                = "chisq",
                           "chi-square, non-central (chisqnc)" = "chisqnc",
                           "exponential (exp)"                 = "exp",
                           "F (f)"                             = "f",
                           "gamma (gamma)"                     = "gamma",
                           "logistic (logis)"                  = "logis",
                           "lognormal (lnorm)"                 = "lnorm",
                           "Normal (norm)"                     = "norm",
                           "Truncated Normal (tnorm)"          = "tnorm",
                           "t (t)"                             = "t",
                           "triangular (triang)"               = "triang",
                           "uniform (unif)"                    = "unif",
                           "Weibull (weibull)"                 = "weibull",
                           "Gompertz (gompertz)"               = "gompertz"),
       "Fitting" = list("Fit to data..." = "fitting"))
}
