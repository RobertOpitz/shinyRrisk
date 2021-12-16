#' Fitting amount continuous distributions to given univariate data
#'
#' This function is not intended to be called directly but is internally called
#' in \code{fit.cont}.
#'
#' @name useFitdist
#' @aliases useFitdist
#' @title Fitting amount continuous distributions to given univariate data.
#' @author Matthias Greiner \email{matthias.greiner@@bfr.bund.de} (BfR), \cr
#' Kristin Tolksdorf \email{kristin.tolksdorf@@bfr.bund.de} (BfR), \cr
#' Katharina Schueller \email{schueller@@stat-up.de} (\acronym{STAT-UP} Statistical Consulting), \cr
#' Natalia Belgorodski \email{belgorodski@@stat-up.de} (\acronym{STAT-UP} Statistical Consulting)
#' @usage useFitdist(data2fit, show.output = TRUE, distributions)
#' @param data2fit numerical vector, data to be fitted.
#' @param show.output logical value, if \code{TRUE} the output will be printed.
#' @param distributions simple character or character vector giving the names of
#' distribution families, that should be fitted to the data. The possible values
#' are: \code{norm}, \code{cauchy}, \code{logis}, \code{beta}, \code{exp},
#' \code{chisq}, \code{unif}, \code{gamma}, \code{lnorm}, \code{weibull},
#' \code{f}, \code{t}, \code{gompertz}, \code{triang}.
#' @return Returns matrix with fitting results. More information...
#' @keywords others
#' @export
#' @examples
#' x1 <- rgamma(374, 4,0.08)
#' res1 <- useFitdist(data2fit = x1)
#' res1
#'
#' x2 <- rbeta(300, shape1 = 1, shape2 = 2)
#' res2 <- useFitdist(data2fit = x2)
#' res2
#'
useFitdist <- function(data2fit, show.output = TRUE,
                       distributions = c("norm", "cauchy", "logis", "beta", "exp", 
                                         "chisq", "unif", "gamma", "lnorm", "weibull", 
                                         "f", "t", "gompertz", "triang")) {
  cat("useFitdist\n")
  #-----------------------------------------------------------------------------
  # checking input
  #-----------------------------------------------------------------------------
  if (missing(data2fit)) {
    stop("Argument 'data2fit' ist empty!", call. = FALSE)
  }
  if (!is.null(dim(data2fit))) {
    stop("Argument 'data2fit' should be a simple numerical vector!", call. = FALSE)
  }
  if (!all(is.numeric(data2fit))) {
    stop("One or more items of 'data2fit' is/are not numeric!", call. = FALSE)
  }
  if (any(is.na(data2fit))) {
    stop("'data2fit' contains missing values, fitting procedure is not possible!", call. = FALSE)
  }
  if (!is.logical(show.output)) {
    stop("'show.output' should be a simple logical value!", call. = FALSE)
  }
  
  #-----------------------------------------------------------------------------
  # define variables
  #-----------------------------------------------------------------------------
  fit.list <- list()
  index <- 1
  
  #-----------------------------------------------------------------------------
  # internal function
  #-----------------------------------------------------------------------------
  trim.whitespace <- function(char) {
    char <- as.character(char)
    n <- length(char)
    out <- NULL
    for (i in 1:n) {
      c <- unlist(strsplit(char[i], split = " "))
      b <- which(c == "")
      if (length(b) > 0) c <- c[-b]
      c <- paste(c, collapse = " ")
      out <- c(out, c)
    }
    return(out)
  }
  
  #-----------------------------------------------------------------------------
  # fitting procedures
  #-----------------------------------------------------------------------------
  #     if (show.output) cat("\n-------------------------------------------------------------------\n") 
  #     if (show.output) cat("Begin fitting distributions... \n") 
  if (show.output) message("\nBegin fitting distributions ---------------------------------------")
  
  # fit normal distributions
  if (is.element("norm", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "norm")), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting normal distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Normal"
      index <- index + 1
    } else if (show.output) message("* fitting normal distribution ... failed") 
  }
  
  # fit cauchy distribution
  if (is.element("cauchy", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "cauchy")), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting Cauchy  distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Cauchy"
      index <- index + 1
    } else if (show.output) message("* fitting Cauchy distribution ... failed") 
  }
  
  # fit logistic distribution
  if (is.element("logis", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "logis")), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting logistic distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Logistic"
      index <- index + 1
    } else if (show.output) message("* fitting logistic distribution ... failed") 
  }
  
  # fit beta distribution
  if (is.element("beta", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "beta")), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting beta distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Beta"
      index <- index + 1
    } else if (show.output) message("* fitting beta distribution ... failed") 
  }
  
  # fit exponential distribution
  if (is.element("exp", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "exp")), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting exponential distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Exponential"
      index <- index + 1
    } else if (show.output) message("* fitting exponential distribution ... failed") 
  }
  
  # fit exponential distribution
  if (is.element("chisq", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "chisq", start = mean(data2fit))), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting chi-square distribution ... OK")
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Chi-square"
      index <- index + 1
    } else if (show.output) message("* fitting chi-square distribution ... failed")
  }
  
  # fit uniform distribution
  if (is.element("unif", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "unif", method = "mme")), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting uniform distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Uniform"
      index <- index + 1
    } else if (show.output) message("* fitting uniform distribution ... failed") 
  }
  
  # fit gamma distribution
  if (is.element("gamma", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "gamma")), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting gamma distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Gamma"
      index <- index + 1
    } else if (show.output) message("* fitting gamma distribution ... failed") 
  }
  
  # fit lognormal distribution
  if (is.element("lnorm", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "lnorm")), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting lognormal distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Lognormal"
      index <- index + 1
    } else if (show.output) message("* fitting lognormal distribution ... failed") 
  }
  
  # fit weibull distribution
  if (is.element("weibull", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "weibull")), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting Weibull distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Weibull"
      index <- index + 1
    } else if (show.output) message("* fitting Weibull distribution ... failed") 
  }
  
  # fit F distribution
  if (is.element("f", distributions)) {if (mean(data2fit) != 1) {
    n <- abs(2 * mean(data2fit)/(mean(data2fit) - 1))
  } else if (mean(data2fit) == 1) {
    n <- 1
  }
    if ((stats::var(data2fit) * (n - 2)^2 * (n - 4) - 2 * n^2) == 0) {
      m <- 1
    } else if ((stats::var(data2fit) * (n - 2)^2 * (n - 4) - 2 * n^2) != 0) {
      m <- abs(2 * n^2 * (n - 2)/(stats::var(data2fit) * (n - 2)^2 * (n - 4) - 2 * n^2))
    }
    if (m == 0) m <- m + 0.001
    if (n == 0) n <- n + 0.001
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "f", start = c(m, n))), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting F-distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "F"
      index <- index + 1
    } else if (show.output) message("* fitting F-distribution ... failed") 
  }
  
  # fit t distribution
  if (is.element("t", distributions)) {
    if (stats::var(data2fit) != 1) {
      start.val <- abs(stats::var(data2fit) * 2/(stats::var(data2fit) - 1))
    } else if (stats::var(data2fit) == 1) {
      start.val <- 1
    }
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "t", start = start.val)), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting Student's t-distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Student"
      index <- index + 1
    } else if (show.output) message("* fitting Student's t-distribution ... failed") 
  }
  
  # fit gompertz distribution
  if (is.element("gompertz", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "gompertz")), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting Gompertz distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Gompertz"
      index <- index + 1
    } else if (show.output) message("* fitting Gompertz distribution ... failed") 
  }
  
  # fit triangular distribution
  if (is.element("triang", distributions)) {
    try.result <- try(temp <- suppressWarnings(rriskFitdist.cont(data2fit, "triang")), silent = TRUE)
    if (!inherits(try.result, "try-error")) {
      if (show.output) message("* fitting triangular distribution ... OK") 
      fit.list[[index]] <- temp
      names(fit.list)[index] <- "Triangular"
      index <- index + 1
    } else if (show.output) message("* fitting triangular distribution ... failed") 
  }
  #     if (show.output) cat("End fitting distributions... \n") 
  #     if (show.output) cat("------------------------------------------------------------------- \n") 
  if (show.output) message("End fitting distributions -----------------------------------------\n")
  
  #-----------------------------------------------------------------------------
  # exit, if neither distribution could be fitted
  #-----------------------------------------------------------------------------
  if (length(fit.list) == 0) {
    on.exit(return(invisible(NULL)))
    stop("\n Neither continuous distribution could be fitted to the data \n", call. = FALSE)
  }
  
  #-----------------------------------------------------------------------------
  # formatting results
  #-----------------------------------------------------------------------------
  temp <- lapply(fit.list, function(x) {
    result <- data.frame(
      ifelse(!is.null(x$loglik), round(x$loglik, digits = 2), "NULL"),
      ifelse(!is.null(x$aic), round(x$aic, digits = 2), "NULL"),
      ifelse(!is.null(x$bic), round(x$bic, digits = 2), "NULL"),
      ifelse(!is.null(x$chisq), round(x$chisq, digits = 2), "NULL"),
      ifelse(!is.null(x$chisqpvalue), round(x$chisqpvalue, digits = 2), "NULL"),
      ifelse(!is.null(x$ad), round(x$ad, digits = 2), "NULL"),
      ifelse(!is.null(x$adtest), x$adtest, "NULL"),
      ifelse(!is.null(x$ks), round(x$ks, digits = 2), "NULL"),
      ifelse(!is.null(x$kstest), x$kstest, "NULL"))  })
  
  for (i in 1:length(temp)) {
    if (i == 1) {
      res.matrix <- temp[[i]]
    } else {
      res.matrix <- rbind(res.matrix, temp[[i]])
    }
  }
  dimnames(res.matrix) <- list(names(fit.list), c("logL", "AIC", "BIC",
                                                  "Chisq(value)", "Chisq(p)", 
                                                  "AD(value)", "H(AD)", 
                                                  "KS(value)", "H(KS)"))
  res.matrix <- apply(res.matrix, c(1, 2), function(x) trim.whitespace(x))
  
  if (show.output) print(as.data.frame(res.matrix)) 
  
  #-----------------------------------------------------------------------------
  # create output
  #-----------------------------------------------------------------------------
  output <- list(res.matrix, fit.list)
  names(output) <- c("res.matrix", "fit.list")
  
  return(invisible(output))
} # end of useFitdist()