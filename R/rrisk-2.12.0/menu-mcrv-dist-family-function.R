#' @name menu.mcrv.dfamily
#' @aliases menu.mcrv.dfamily
#' @title Non-executable auxiliary function
#' @usage menu.mcrv.dfamily(item,rriskModel,dfamily="pois",menuLevel=1)
#' @param item ...
#' @param rriskModel ...
#' @param dfamily ...
#' @param menuLevel ...
#' @keywords items
#' @export

menu.mcrv.distFamily <- function(item, rriskModel, 
                                 dfamily   = "pois",
                                 menuLevel = 1, 
                                 values) {
  cat("menu.mcrv.distFamily; family =", dfamily, "\n")
  
  batch <- 1
  
  # ist ein geschichteter Item?
  if (item@stratum != "") {
    for (i in seq_along(rriskModel@items@items)) { # why a loop?
      if (rriskModel@items@items[[i]]@typecode == "stra") {
        stratum.item <- rriskModel@items@items[[i]]
        batch        <- nrow(stratum.item@data$stratum)
        stid         <- as.character(stratum.item@data$stratum$stid)   
        break
      } # end if
    } # end for
  } # end if
  
  # enter distribution parameters (discrete)
  result <- get_distribution_description(dfamily, values, batch)
  #result$dist_type (discrete or continuous)
  #result$definition
  #result$fullc
  
  #---BEGIN: checking if fullc is executable------------------------------------
  item.temp <- item
  item.temp@fullc <- result$fullc
  
  # hier wird die Ausführbarkeit von fullc geprüft
  fullcValues <- itemsEvaluation(item.temp, rriskModel)
  
  if (inherits(fullcValues, "try-error"))
    stop("Full command expression", result$fullc, "could not be evaluated !\n")
  else {
    
    # Weise Item-Slots 'definition' und 'fullc' Inhalte zu
    if (batch > 1)
      colnames(fullcValues) <- stid
    else
      colnames(fullcValues) <- item@name

    item@fullc <- result$fullc
    item@definition <- result$definition
    item@data <- summary(fullcValues) # save simulated item value in data-slot
  }
  #---END: checking if fullc is executable--------------------------------------
  
  #---BEGIN: check if relaxc is executable--------------------------------------
  if (result$dist_type == "discrete")
    relax_dist <- "rudiscrete"
  else if (result$dist_type == "continuous")
    relax_dist <- "runif"
  else
    stop(paste("ERROR in func 'menu-mcrv-dist-family',", 
               "unknown dist_family type =", dist_type))
  
  cat("menu.mcrv.distFamily -> batch =", batch, "\n")
  #readline(prompt="Weiter mit Enter")
  
  # 
  relaxc      <- paste0("matrix(ncol=", batch, 
                        ",data=", relax_dist, "(n=", batch, 
                        "*rriskModel@settings@N,", 
                        "min=", item@plausimin, 
                        ",max=", item@plausimax, "),byrow=TRUE)")
  relaxc.test <- paste0("matrix(ncol=", batch, 
                        ",data=", relax_dist, "(n=", batch, 
                        "*rriskModel@settings@Ntest,",
                        "min=", item@plausimin, 
                        ",max=", item@plausimax, "),byrow=TRUE)")
  relaxcValues <- try(expr   = eval(parse(text = relaxc.test)), 
                      silent = TRUE)
  
  if (inherits(relaxcValues, "try-error"))
    cat("Relax command expression",relaxc,"could not be evaluated !\n")
  else {
    
    if (batch > 1)
      colnames(relaxcValues) <- stid
    else
      colnames(relaxcValues) <- item@name
    
    item@relaxc <- relaxc
  }
  #---END: check if relaxc is executable----------------------------------------
  
  item
}