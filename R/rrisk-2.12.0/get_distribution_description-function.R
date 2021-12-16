get_distribution_description <- function(dfamily, values, batch) {
  cat("get_distribution_description\n")
  
  # output object
  result <- list(dist_type = NULL, definition = NULL, fullc = NULL)
  
  # template for matrix
  matrix_template      <- c("matrix(data=", ", ncol=", batch, ")")
  # template for number of random numbers
  nb_of_random_numbers <- c(batch, "*", "rriskModel@settings@N")
  
  result <- switch(dfamily,
         "bern"     = {prob              <- values[["txbValue1"]]
                       result$dist_type  <- "discrete" 
                       result$definition <- paste0("binom(size=1", 
                                                   ",prob=", prob, ")")
                       result$fullc      <- c("rbinom(n=", 
                                              ",size=1,prob=", prob, ")")
                       result},
         "binom"    = {size              <- values[["txbValue1"]]
                       prob              <- values[["txbValue2"]]
                       result$dist_type  <- "discrete" 
                       result$definition <- paste0("binom(size=1", 
                                                   ",prob=", prob, ")")
                       result$fullc      <- c("rbinom(n=", 
                                              ",size=", size,
                                              ",prob=", prob, ")")
                       result},
         "nbinom"   = {size <- values[["txbValue1"]]
                       prob <- values[["txbValue2"]]
                       result$dist_type  <- "discrete"
                       result$definition <- paste0("nbinom(size=", size, 
                                                   ",prob=", prob, ")")
                       result$fullc      <- c("rnbinom(n=", 
                                              ",size=", size, 
                                              ",prob=", prob, ")")
                       result},
         "multinom" = {probs             <- values[["txbValue1"]]
                       result$dist_type  <- "discrete"
                       result$definition <- paste0("multinom(size=1", 
                                                   ",probs=c(", probs, "))")
                       result$fullc      <- c("rrisk_rmultinom(n=", 
                                              ",prob=c(", probs, "))")
                       result},
         "discrete" = {validInput <- FALSE
                       valuesprobs <- data.frame(values = c("value1", "value2", 
                                                            "value3"), 
                                                 probs  = c("prob1", "prob2", 
                                                            "prob3"))
                       valuesprobs <- values # ?
           
                       valuesprobs$values <- as.numeric(as.character(valuesprobs$values)) # ?
                       valuesprobs$probs  <- as.numeric(as.character(valuesprobs$probs)) # ?
           
                       # check input data
                       try.result <- try(na.fail(valuesprobs), 
                                         silent = TRUE)
           
                       if (inherits(try.result, "try-error"))
                         stop(message = paste("Invalid input data!", 
                                              "filDiscrete", sep = "|"), 
                              call.   = TRUE)
                       else 
                         validInput <- TRUE
           
                       values.def <- paste(valuesprobs$values, collapse = ",")
                       probs.def  <- paste(valuesprobs$probs, collapse = ",")
           
                       result$dist_type  <- "discrete" 
                       result$definition <- paste0("discrete(values=c(",
                                                   values.def, "),probs=c(", 
                                                   probs.def, "))")
                       result$fullc      <- c("rdiscrete(n=", 
                                              ",values=c(", values.def, "),", 
                                              "probs=c(", probs.def, "))")
                       result},
         "hyper"    = {m <- values[["txbValue1"]]
                       n <- values[["txbValue2"]]
                       k <- values[["txbValue3"]]
                       result$dist_type  <- "discrete"
                       result$definition <- paste0("hyper(m=", m, ",n=", n, 
                                                   ",k=", k, ")")
                       result$fullc      <- c("rhyper(nn=", 
                                              ",m=", m, ",n=", n, ",k=", k, ")")
                       result},
         "geom"     = {prob <- values[["txbValue1"]]
                       result$dist_type  <- "discrete"
                       result$definition <- paste0("geom(prob=", prob, ")")
                       result$fullc      <- c("rgeom(n=", 
                                              ",prob=", prob, ")")
                       result},
         "pois"     = {lambda <- values[["txbValue1"]]
                       result$dist_type  <- "discrete"
                       result$definition <- paste0("pois(lambda=", lambda, ")")
                       result$fullc      <- c("rpois(n=", 
                                              ",lambda=", lambda, ")")
                       result},
         "udiscrete"= {minu <- values[["txbValue1"]]
                       maxu <- values[["txbValue2"]]
                       result$dist_type  <- "discrete"
                       result$definition <- paste0("udiscrete(min=", minu, 
                                                   ",max=", maxu, ")")
                       result$fullc      <- c("rudiscrete(n=", 
                                              ",min=", minu, 
                                              ",max=", maxu, ")")
                       result},
         "beta"     = {shape1 <- values[["txbValue1"]]
                       shape2 <- values[["txbValue2"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("beta(shape1=", shape1, 
                                                   ",shape2=", shape2, ")")
                       result$fullc      <- c("rbeta(n=", 
                                              ",shape1=", shape1, 
                                              ",shape2=", shape2, ")")
                       result},
         "cauchy"   = {location <- values[["txbValue1"]]
                       scale    <- values[["txbValue2"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("cauchy(scale =", scale, 
                                                  ", location =", location, ")")
                       result$fullc      <- c("rcauchy(n=", 
                                              ",location=", location, 
                                              ",scale =", scale, ")")
                       result},
         "chisq"    = {df <- values[["txbValue1"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("chisq(df =", df, ")")
                       result$fullc      <- c("rchisq(n=", 
                                              ",df=", df, ")")
                       result},
         "chisqnc"  = {df  <- values[["txbValue1"]]
                       ncp <- values[["txbValue2"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("chisqnc(df=", df, 
                                                   ",ncp=", ncp, ")")
                       result$fullc      <- c("rchisq(n=", 
                                              ",df=", df, ",ncp=", ncp, ")")
                       result},
         "exp"      = {rate <- values[["txbValue1"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("exp(rate=", rate, ")")
                       result$fullc      <- c("rexp(n=", 
                                              ",rate=", rate, ")")
                       result},
         "f"        = {df1 <- values[["txbValue1"]]
                       df2 <- values[["txbValue2"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("f(df1=", df1, 
                                                   ",df2=", df2, ")")
                       result$fullc      <- c("rf(n=", 
                                              ",df=", df1, ",df2=", df2, ")")
                       result},
         "gamma"    = {shape <- values[["txbValue1"]]
                       rate  <- values[["txbValue2"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("gamma(shape=", shape, 
                                                   ",rate=", rate, ")")
                       result$fullc      <- c("rgamma(n=", 
                                              ",shape=", shape, 
                                              ",rate=", rate, ")")
                       result},
         "logis"    = {location <- values[["txbValue1"]]
                       scale    <- values[["txbValue2"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("logis(location=", location, 
                                                   ",scale=", scale, ")")
                       result$fullc      <- c("rlogis(n=", 
                                              ",location=", location, 
                                              ",scale=", scale, ")")
                       result},
         "lnorm"    = {meanlog <- values[["txbValue1"]]
                       sdlog   <- values[["txbValue2"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("lnorm(meanlog=", meanlog, 
                                                   ",sdlog=", sdlog, ")")
                       result$fullc      <- c("rlnorm(n=", 
                                              ",meanlog=", meanlog, 
                                              ",sdlog=", sdlog, ")")
                       result},
         "norm"     = {mean_value <- values[["txbValue1"]]
                       sd_value   <- values[["txbValue2"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("norm(mean=", mean_value, 
                                                   ",sd=", sd_value, ")")
                       result$fullc      <- c("rnorm(n=", 
                                              ",mean=", mean_value, 
                                              ",sd=", sd_value, ")")
                       result},
         "tnorm"    = {mean_value <- values[["txbValue1"]]
                       sd_value   <- values[["txbValue2"]]
                       lower      <- values[["txbValue3"]]
                       upper      <- values[["txbValue4"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("tnorm(mean=", mean_value, 
                                                   ",sd=", sd_value, 
                                                   ",lower=", lower, 
                                                   ",upper=", upper, ")")
                       result$fullc      <- c("rrisk_rtnorm(n=", 
                                              ",mean=", mean_value, 
                                              ",sd=", sd_value, 
                                              ",lower=", lower, 
                                              ",upper=", upper, ")")
                       result},
         "t"        = {df <- values[["txbValue1"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("t(df=", df, ")")
                       result$fullc      <- c("rt(n=", 
                                              ",df=", df, ")")
                       result},
         "triang"   = {min_value  <- values[["txbValue1"]]
                       mode_value <- values[["txbValue2"]]
                       max_value  <- values[["txbValue3"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("triang(min=", min_value, 
                                                   ",mode=", mode_value, 
                                                   ",max=", max_value, ")")
                       result$fullc      <- c("rtriang(n=", 
                                              ",min=", min_value, 
                                              ",mode=", mode_value, 
                                              ",max=", max_value, ")")
                       result},
         "unif"     = {min_value <- values[["txbValue1"]]
                       max_value <- values[["txbValue2"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("unif(min=", min_value, 
                                                   ",max=", max_value, ")")
                       result$fullc      <- c("runif(n=", 
                                              ",min=", min_value, 
                                              ",max=", max_value, ")")
                       result},
         "weibull"  = {shape        <- values[["txbValue1"]]
                       scale_value  <- values[["txbValue2"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("weibull(shape=", shape, 
                                                   ",scale=", scale_value, ")")
                       result$fullc      <- c("rweibull(n=", 
                                              ",shape=", shape, 
                                              ",scale=", scale_value, ")")
                       result},
         "pert"     = {min_value  <- values[["txbValue1"]]
                       mode_value <- values[["txbValue2"]]
                       max_value  <- values[["txbValue3"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("pert(min=", min_value, 
                                                   ",mode=", mode_value, 
                                                   ",max=", max_value, ")")
                       result$fullc      <- c("rrisk_mod_rpert(n=", 
                                              ",min_value=", min_value, 
                                              ",mode_value=", mode_value, 
                                              ",max_value=", max_value, ")")
                       result},
         "modpert"  = {min_value  <- values[["txbValue1"]]
                       mode_value <- values[["txbValue2"]]
                       max_value  <- values[["txbValue3"]]
                       shape      <- values[["txbValue4"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("modpert(min=", min_value, 
                                                   ",mode=", mode_value, 
                                                   ",max=", max_value, 
                                                   ",shape=", shape, ")")
                       result$fullc      <- c("rrisk_mod_rpert(n=", 
                                              ",min_value=", min_value, 
                                              ",mode_value=", mode_value, 
                                              ",max_value=", max_value, 
                                              ",shape=", shape, ")")
                       result},
         "gompertz" = {shape       <- values[["txbValue1"]]
                       scale_value <- values[["txbValue2"]]
                       result$dist_type  <- "continuous"
                       result$definition <- paste0("gompertz(shape=", shape,
                                                   ",scale=", scale_value,")")
                       result$fullc      <- c("rgompertz(n=", 
                                              ",shape=", shape, 
                                              ",scale=",scale_value, ")")
                       result})
  
  # add the number of random numbers we want to the ranom number function
  result$fullc <- append(result$fullc, nb_of_random_numbers, after = 1)
  # embed the final random number function into the matrix template
  result$fullc <- append(matrix_template, result$fullc, after = 1)
  # bake the character vector into one string
  result$fullc <- paste(result$fullc, collapse = "") # not necessary for parse and eval
  # now return result
  result
}