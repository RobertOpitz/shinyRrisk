#' This function fits the amount of distribution families to given quantiles and returns
#' diagnostics that allow user to choose a most appropriate probability.
#'
#' Both inputs \code{p} and \code{q} should be of the same length. The items of
#' the probability vector \code{p} should lie between 0 and 1.
#'
#' @name rriskFitdist.perc
#' @aliases rriskFitdist.perc
#' @title Fitting an amount of distribution families by given quantiles
#' @author Matthias Greiner \email{matthias.greiner@@bfr.bund.de} (BfR), \cr
#' Kristin Tolksdorf \email{kristin.tolksdorf@@bfr.bund.de} (BfR), \cr
#' Katharina Schueller \email{schueller@@stat-up.de} (\acronym{STAT-UP} Statistical Consulting), \cr
#' Natalia Belgorodski \email{belgorodski@@stat-up.de} (\acronym{STAT-UP} Statistical Consulting)
#' @usage rriskFitdist.perc(p = c(0.025, 0.5, 0.975), q = c(9.68, 29.20, 50.98),
#'    show.output = TRUE, tolConv = 0.001, fit.weights = rep(1, length(p)))
#' @param p numerical vector giving probabilities.
#' @param q numerical vector giving percentiles.
#' @param show.output logical, if \code{TRUE} the \code{optim} result will be printed (default value is \code{TRUE}).
#' @param tolConv positive numerical value, the absolute convergence tolerance for reaching zero by fitting distributions
#' \code{get.norm.par} will be shown.
#' @param fit.weights numerical vector of the same length as a probabilities vector 
#'    \code{p} containing positive values for weighting quantiles. By default all
#'    quantiles will be weighted by 1.
#' @return Returns a list containing the data frame with the input vectors \code{p}
#' and \code{q} and the results matrix giving fitted distributions, estimated
#' parameters and a vector of theoretical percentiles calculated based on the
#' estimated parameters. If the consistency check of input parameters fails
#' the function returns \code{NA}.
#' @keywords fitdistrplus
#' @export
#' @importFrom mc2d qtriang
#' @importFrom mc2d qpert
#' @importFrom eha qgompertz
#' @importFrom msm qtnorm
#' @examples
#' fit.results1 <- rriskFitdist.perc(show.output = FALSE)
#' fit.results1
#'
#' fit.results2 <- rriskFitdist.perc(show.output = FALSE, tolConv = 0.6)
#' fit.results2
#'
#' p <- c(0.2, 0.7)
#' q <- c(2.6, 19.1)
#' fit.results3 <- rriskFitdist.perc(p = p, q = q, show.output = FALSE)
#' fit.results3
#'
#' p <- c(0.3, 0.8, 0.9)
#' q <- c(10, 20, 40)
#' fit.results4 <- rriskFitdist.perc(p = p, q = q, show.output = FALSE)
#' fit.results4
#'
#' ## Example with fitted pert distribution
#' p <- c(0.025, 0.5, 0.6, 0.975)
#' q <- mc2d::qpert(p = p, min = 0, mode = 3, max = 10, shape = 5)
#' fit.results5 <- rriskFitdist.perc(p = p, q = q, show.output = FALSE)
#' fit.results5

assert_rriskFitdist.perc_input <- function(p, q, fit.weights, show.output) {
  if (length(p) != length(q))
    stop(paste("INVALID INPUT, the vectors of probabilities, percentiles", 
               "or/and weights are not of the same length!", call. = FALSE))
  
  if (length(p) == 0 | length(q) == 0)
    stop(paste("INVALID INPUT, either the vector of probabilities or the", 
               "vector of quantiles is empty!", call. = FALSE))
  
  if (length(fit.weights) == 0)
    stop("INVALID INPUT, the vector of weights is empty!", call. = FALSE)
  
  if (any(fit.weights <= 0))
    stop(paste("INVALID INPUT, all items of the argument 'fit.weights'", 
               "should be positive!", call. = FALSE))
         
  if (!is.numeric(p) | !is.numeric(q) | !is.numeric(fit.weights))
    stop(paste("INVALID INPUT, one of the following vectors is not numeric:", 
               "probabilities, percentiles, weights!", call. = FALSE))

  if (min(p) < 0 | max(p) > 1)
    stop(paste("INVALID INPUT, items of the probability vector should lie", 
               "between 0 and 1!", call. = FALSE))
         
  if (prod(order(p) == seq(1:length(p))) == 0 | 
      prod(order(q) == seq(1:length(q))) == 0)
    stop(paste("INVALID INPUT, the vector of probabilities/percentiles is", 
               "not ordered!", call. = FALSE))

  if (!is.logical(show.output))
    stop(paste("INVALID INPUT, the argument 'show.output' should be logical!", 
               call. = FALSE))
}

rriskFitdist.perc <- function(p = c(0.025, 0.5, 0.975), 
                              q = c(9.68, 29.20, 50.98), 
                              show.output = TRUE, 
                              tolConv = 0.001, 
                              fit.weights = rep(1, length(p))) {
  cat("rriskFitdist.perc\n")

  # check general consistency of the input data
  assert_rriskFitdist.perc_input(p, q, fit.weights, show.output)
  # if (length(p) != length(q)) {
  #   on.exit(return(invisible(NA)))
  #   stop("INVALID INPUT, the vectors of probabilities, percentiles or/and weights are not of the same length!", call. = FALSE)
  # }
  # if (length(p) == 0 | length(q) == 0) {
  #   on.exit(return(invisible(NA)))
  #   stop("INVALID INPUT, either the vector of probabilities or the vector of quantiles is empty!", call. = FALSE)
  # }
  # if (length(fit.weights) == 0) {
  #   on.exit(return(invisible(NA)))
  #   stop("INVALID INPUT, the vector of weights is empty!", call. = FALSE)
  # }
  # if (any(fit.weights <= 0)) {
  #   on.exit(return(invisible(NA)))
  #   stop("INVALID INPUT, all items of the argument 'fit.weights' should be positive!", call. = FALSE)
  # }
  # if (!is.numeric(p) | !is.numeric(q) | !is.numeric(fit.weights)) {
  #   on.exit(return(invisible(NA)))
  #   stop("INVALID INPUT, one of the following vectors is not numeric: probabilities, percentiles, weights!", call. = FALSE)
  # }
  # if (min(p) < 0 | max(p) > 1) {
  #   on.exit(return(invisible(NA)))
  #   stop("INVALID INPUT, items of the probability vector should lie between 0 and 1!", call. = FALSE)
  # }
  # if (prod(order(p) == seq(1:length(p))) == 0 | 
  #     prod(order(q) == seq(1:length(q))) == 0) {
  #   on.exit(return(invisible(NA)))
  #   stop("INVALID INPUT, the vector of probabilities/percentiles is not ordered!", call. = FALSE)
  # }
  # if (!is.logical(show.output)) {
  #   on.exit(return(invisible(NA)))
  #   stop("INVALID INPUT, the argument 'show.output' should be logical!", call. = FALSE)
  # }
  
  #---BEGIN: defining help variables--------------------------------------------
  Perc <- c(p, 0.0001, 0.001, 0.01, 0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975, 
            0.99, 0.999, 0.9999)
  Quantiles <- c(q, rep(NA, 13))
  Quantiles <- Quantiles[order(Perc)]
  Perc <- Perc[order(Perc)]
  Quantiles <- Quantiles[!duplicated(Perc)]
  Perc <- Perc[!duplicated(Perc)]
  Perc <- round(Perc, digits = 4)
  res.mat <- data.frame(weight    = rep(0, length(Perc) + 4), 
                        Quantiles = c(rep(NA, 4), Quantiles),
                        norm      = NA, 
                        beta      = NA, 
                        cauchy    = NA, 
                        logis     = NA, 
                        t         = NA, 
                        chisq     = NA, 
                        chisqnc   = NA, 
                        exp       = NA, 
                        f         = NA, 
                        gamma     = NA, 
                        lnorm     = NA, 
                        unif      = NA, 
                        weibull   = NA, 
                        triang    = NA, 
                        gompertz  = NA, 
                        pert      = NA, 
                        tnorm     = NA)
  rownames(res.mat) <- c(paste0("Para", 1:4), as.character(Perc * 100))
  res.mat[as.character(p * 100), "weight"] <- fit.weights
  #---END: defining help variables----------------------------------------------
  
  message("\nBegin fitting distributions ---------------------------------------")
  
  #---BEGIN: internal helper functions------------------------------------------
  my_try_fit <- function(f) {
    #try(suppressMessages(suppressWarnings( # overkill? try is already silent
    try(expr   = f(p           = p, 
                   q           = q, 
                   show.output = show.output, 
                   plot        = FALSE, 
                   tol         = tolConv, 
                   fit.weights = fit.weights), #)),
        silent = TRUE)
  }
  
  is.ok <- function(x) !inherits(x, "try-error")
  #---END: internal helper functions--------------------------------------------
  
  ## tnorm
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.tnorm.par(p = p, q = q, 
  #                  show.output = show.output, 
  #                  plot = FALSE, 
  #                  tol = tolConv, 
  #                  fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.tnorm.par)
  if (is.ok(parameters)) {
    res.mat$tnorm[1:4] <- parameters # ?
    res.mat$tnorm[-c(1:4)] <- msm::qtnorm(p     = Perc, # -c(1:4) ?
                                          mean  = parameters["mean"], 
                                          sd    = parameters["sd"], 
                                          lower = parameters["lower"], 
                                          upper = parameters["upper"])
  }
  message("* fitting truncated normal distribution ... ", res)
  
  ## chisqnc
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.chisqnc.par(p = p, q = q, 
  #                    show.output = show.output, 
  #                    plot = FALSE, 
  #                    tol = tolConv, 
  #                    fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.chisqnc.par)
  if (is.ok(parameters)) {
    res.mat$chisqnc[1:2] <- parameters
    res.mat$chisqnc[-c(1:4)] <- stats::qchisq(p = Perc, 
                                              df = parameters["df"], 
                                              ncp = parameters["ncp"])
  } 
  message("* fitting non-central chi-square distribution ... ", res)
  
  ## pert
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.pert.par(p = p, q = q, 
  #                 show.output = show.output, 
  #                 plot = FALSE, 
  #                 tol = tolConv, 
  #                 fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.pert.par)
  if (is.ok(parameters)) {
    res.mat$pert[1:4] <- parameters
    res.mat$pert[-c(1:4)] <- mc2d::qpert(p = Perc, 
                                         min = parameters["min"], 
                                         mode = parameters["mode"], 
                                         max = parameters["max"], 
                                         shape = parameters["shape"])
  }
  message("* fitting PERT distribution ... ", res)
  
  ## triang
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.triang.par(p = p, q = q, 
  #                   show.output = show.output, 
  #                   plot = FALSE, 
  #                   tol = tolConv, 
  #                   fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.triang.par)
  if (is.ok(parameters)) {
    res.mat$triang[1:3] <- parameters
    res.mat$triang[-c(1:4)] <- mc2d::qtriang(p = Perc, 
                                             min = parameters["min"], 
                                             mode = parameters["mode"], 
                                             max = parameters["max"])
  }
  message("* fitting triangular distribution ... ", res)
  
  ## gompertz
 # parameters <- NA
 # try({
 #   parameters <- suppressMessages(suppressWarnings({
 #     get.gompertz.par(p = p, q = q, 
 #                      show.output = show.output, 
 #                      plot = FALSE, 
 #                       tol = tolConv, 
 #                       fit.weights = fit.weights)
 #   }))},
 #   silent = TRUE
 # )
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.gompertz.par)
  if (is.ok(parameters)) {
    res.mat$gompertz[1:2] <- parameters
    res.mat$gompertz[-c(1:4)] <- eha::qgompertz(p = Perc, 
                                                shape = parameters["shape"], 
                                                scale = parameters["scale"])
  }
  message("* fitting Gompertz distribution ... ", res)
  
  ## normal
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.norm.par(p = p, q = q, 
  #                 show.output = show.output, 
  #                 plot = FALSE, 
  #                 tol = tolConv, 
  #                 fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.norm.par)
  if (is.ok(parameters)) {
    res.mat$norm[1:2] <- parameters
    res.mat$norm[-c(1:4)] <- stats::qnorm(p = Perc, 
                                          mean = parameters["mean"], 
                                          sd = parameters["sd"])
  }
  message("* fitting normal distribution ... ", res)
  
  ## beta
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.beta.par(p = p, q = q, 
  #                 show.output = show.output, 
  #                 plot = FALSE, 
  #                 tol = tolConv, 
  #                 fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.beta.par)
  if (is.ok(parameters)) {
    res.mat$beta[1:2] <- parameters
    res.mat$beta[-c(1:4)] <- stats::qbeta(p = Perc, 
                                          shape1 = parameters["shape1"], 
                                          shape2 = parameters["shape2"])
  }
  message("* fitting beta distribution ... ", res)
  
  ## cauchy
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.cauchy.par(p = p, q = q, 
  #                   show.output = show.output, 
  #                   plot = FALSE, 
  #                   tol = tolConv, 
  #                   fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.cauchy.par)
  if (is.ok(parameters)) {
    res.mat$cauchy[1:2] <- parameters
    res.mat$cauchy[-c(1:4)] <- stats::qcauchy(p = Perc, 
                                              location = parameters["location"], 
                                              scale = parameters["scale"])
  }
  message("* fitting Cauchy distribution ... ", res)
  
  ## chisq
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.chisq.par(p = p, q = q, 
  #                  show.output = show.output, 
  #                  plot = FALSE, 
  #                  tol = tolConv, 
  #                  fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.chisq.par)
  if (is.ok(parameters)) {
    res.mat$chisq[1] <- parameters
    res.mat$chisq[-c(1:4)] <- stats::qchisq(p = Perc, 
                                            df = parameters["df"])
  }
  message("* fitting chi-square distribution ... ", res)
  
  ## logis
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.logis.par(p = p, q = q, 
  #                  show.output = show.output, 
  #                  plot = FALSE, 
  #                  tol = tolConv, 
  #                  fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.logis.par)
  if (is.ok(parameters)) {
    res.mat$logis[1:2] <- parameters
    res.mat$logis[-c(1:4)] <- stats::qlogis(p = Perc, 
                                            location = parameters["location"], 
                                            scale = parameters["scale"])
  }
  message("* fitting logistic distribution ... ", res)
  
  ## t
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.t.par(p = p, q = q, 
  #              show.output = show.output, 
  #              plot = FALSE, 
  #              tol = tolConv, 
  #              fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.t.par)
  if (is.ok(parameters)) {
    res.mat$t[1] <- parameters
    res.mat$t[-c(1:4)] <- stats::qt(p = Perc, 
                                    df = parameters["df"])
  }
  message("* fitting Student's t-distribution ... ", res)
  
  ## exp
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.exp.par(p = p, q = q, 
  #                show.output = show.output, 
  #                plot = FALSE, 
  #                tol = tolConv, 
  #                fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.exp.par)
  if (is.ok(parameters)) {
    res.mat$exp[1] <- parameters
    res.mat$exp[-c(1:4)] <- stats::qexp(p = Perc, 
                                        rate = parameters["rate"])
  }
  message("* fitting exponential distribution ... ", res)
  
  ## F
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.f.par(p = p, q = q, 
  #              show.output = show.output, 
  #              plot = FALSE, 
  #              tol = tolConv, 
  #              fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.f.par)
  if (is.ok(parameters)) {
    res.mat$f[1:2] <- parameters
    res.mat$f[-c(1:4)] <- stats::qf(p = Perc, 
                                    df1 = parameters["df1"], 
                                    df2 = parameters["df2"])
  }
  message("* fitting F-distribution ... ", res)
  
  ## gamma
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.gamma.par(p = p, q = q, 
  #                  show.output = show.output, 
  #                  plot = FALSE, 
  #                  tol = tolConv, 
  #                  fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.f.par)
  if (is.ok(parameters)) {
    res.mat$gamma[1:2] <- parameters
    res.mat$gamma[-c(1:4)] <- stats::qgamma(p = Perc, 
                                            shape = parameters["shape"], 
                                            rate = parameters["rate"])
  }
  message("* fitting gamma distribution ... ", res)
  
  ## Weibull
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.weibull.par(p = p, q = q, 
  #                    show.output = show.output, 
  #                    plot = FALSE, 
  #                    tol = tolConv, 
  #                    fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.weibull.par)
  if (is.ok(parameters)) {
    res.mat$weibull[1:2] <- parameters
    res.mat$weibull[-c(1:4)] <- stats::qweibull(p = Perc, 
                                                shape = parameters["shape"], 
                                                scale = parameters["scale"])
  }
  message("* fitting Weibull distribution ... ", res)
  
  ## lognormal
  #parameters <- NA
  #try({
  #  parameters <- suppressMessages(suppressWarnings({
  #    get.lnorm.par(p = p, q = q, 
  #                  show.output = show.output, 
  #                  plot = FALSE, 
  #                  tol = tolConv, 
  #                  fit.weights = fit.weights)
  #  }))},
  #  silent = TRUE
  #)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  parameters <- my_try_fit(get.lnorm.par)
  if (is.ok(parameters)) {
    res.mat$lnorm[1:2] <- parameters
    res.mat$lnorm[-c(1:4)] <- stats::qlnorm(p = Perc, 
                                            meanlog = parameters["meanlog"], 
                                            sdlog = parameters["sdlog"])
  }
  message("* fitting lognormal distribution ... ", res)
  
  ## uniform
  parameters <- try(expr = get.unif.par(p = p, q = q, plot = FALSE),
                    silent = TRUE)
  #res <- ifelse(any(is.na(parameters)), "failed", "OK")
  #if (res == "OK") {
  if (is.ok(parameters)) {
    res.mat$unif[1:2] <- parameters
    res.mat$unif[-c(1:4)] <- stats::qunif(p = Perc, 
                                          min = parameters["min"], 
                                          max = parameters["max"])
  }
  message("* fitting uniform distribution ... ", res)
  
  message("End fitting distributions -----------------------------------------\n")
  
  if (all(is.na(res.mat[1:4, -1]))) {
    if (is.element("package:rrisk", search())) { # wenn "rrisk" vorhanden, mache weiter. sonst breche ab.
      #on.exit(.generate.newitem())
      stop("\n\nSorry, no pdfs found that match with the specified percentiles\n", call. = FALSE)
    } else {
      on.exit(return(invisible(NA)))
      stop("\n\nSorry, no pdfs found that match with the specified percentiles\n", call. = FALSE)
    }
  }
  res.mat <- data.frame(apply(res.mat, c(1, 2), function(x) round(x, digits = 2)))
  output <- list(data.frame(p, q), res.mat)
  names(output) <- c("p/q", "results")
  return(output)
} # end of rriskFitdist.perc()