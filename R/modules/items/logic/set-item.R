#' @name setItem
#' @title setItem
#' @description Adds or replaces an item in the items list.
#' @param input Contains UI input objects.
#' @param rowNum List index to replace or add item.
#' @return NULL
#' @export
setItem <- function(input, rowNum) {
  cat("setItem\n")

  session <- shiny::getDefaultReactiveDomain() # using side effects
  model   <- session$userData$model()
  
  values  <- list("txbValue1" = input$txbValue1,
                  "txbValue2" = input$txbValue2,
                  "txbValue3" = input$txbValue3,
                  "txbValue4" = input$txbValue4) 
  item    <-  new(Class       = "itemClass",
                  name        = input$txbName,
                  title       = input$txbTitle,
                  type        = getItemTypeCodes()[[input$slbType]],
                  typecode    = input$slbType,
                  unit        = input$txbUnit,
                  role        = getItemRoleCodes()[[input$slbRole]],
                  rolecode    = input$slbRole,
                  assumptions = input$txbAssumptions,
                  reference   = input$txbReference)

  cat("item@typecode =", item@typecode, "\n")
  #---BEGIN if: item type code--------------------------------------------------
  if (item@typecode == "mcrv") {

    item@plausimin <- input$txbPlausibleMin
    item@plausimax <- input$txbPlausibleMax

    dfamily <- input$slbProbabilityDensity
    cat("setItem -> mcrv -> dfamily =", dfamily, "\n")

    #---BEGIN if: dfamily-------------------------------------------------------
    if (dfamily == "fitting") {
      item <- menu.mcrv.fittodata(item               = item, 
                                  rriskModel         = model, 
                                  itemId             = input$slbData, 
                                  continuousFunction = input$slbContinuousFunctions)
    } else if (dfamily == "discrete") { #---if: dfamily-------------------------

      # NEEDS TO BE CHANGED TO A FUNCTION CALL WITH TO AN APPROPRIATE GUI
      result <- tryCatch(expr  = {read.csv(input$filDiscrete$datapath, 
                                           header = TRUE, 
                                           sep = ",")}, # changed from ; to , 
                         error = function(e) e)

      if (inherits(result, "simpleError"))
        stop(message = paste("CSV import failed!", "filDataFile", sep = "|"))
      
      #result <- read_data(input$filDiscrete$datapath)

      if (sum(result["probs"]) != 1)
        shinyAlert(title = paste("The SUM of all probabilities is", 
                                 sum(result["probs"]), 
                                 "but it should be 1!"),
                   type = "warning")

      item <- menu.mcrv.distFamily(item       = item, 
                                   rriskModel = model, 
                                   dfamily    = "discrete", 
                                   values     = result)
    } else #---if: dfamily------------------------------------------------------
      # distribution function 'dfamily" from the menu choosen by user
      item <- menu.mcrv.distFamily(item       = item, 
                                   rriskModel = model, 
                                   dfamily    = dfamily, 
                                   values     = values)
    #---END if: dfamily---------------------------------------------------------
  } else if (item@typecode %in% c("numv", "fnrv")) { #---if: item type code-----
    cat("setItem -> 'numv' or 'fnrv' -> input$txbNumvFnrv =", input$txbNumvFnrv, "\n")
    item <- define.numv.fnrv(item       = item, 
                             rriskModel = model, 
                             type       = item@typecode, 
                             value      = input$txbNumvFnrv)
  } else if (item@typecode == "data") { #---if: item type code------------------

    # NEEDS TO BE CHANGED TO A FUNCTION CALL WITH AN APPROBRIATE GUI
    item@data <- tryCatch(expr  = read.csv(input$filDataFile$datapath, 
                                           header = TRUE, 
                                           sep    = ","), # changed from ; to ,
                          error = function(error) return(error))

    if (inherits(item@data, "simpleError"))
      stop(message = paste("CSV import failed!", "filDataFile", sep = "|"))

  } else if (item@typecode == "rsrv") {#---if: item type code--------------------
    item <- menu.define.rsrv(item       = item, 
                             rriskModel = model, 
                             itemId     = input$slbRsrv)

  } else if (item@typecode %in% c("param", "nonparam")) { #---if: item type code
    item <- menu.bsrv(item       = item, 
                      rriskModel = model, 
                      method     = item@typecode, 
                      itemId     = input$slbBsrv)
    item@typecode <- "bsrv"
  } else #---if: item type code-------------------------------------------------
    stop(paste("Something went wrong in function 'set item'.",
               "item@typecode is", item@typecode, 
               ". This type code is unkown."))
  #---END if: item type code----------------------------------------------------
  
  model@items@items[[rowNum]] <- item
  
  for (this_item in model@items@items) # ? loop überschreibt die immer gleiche variable?
    model@items@items <- setDepItems(this_item, model@items@items)

  session$userData$model(model) # using side ffects

  itemsExt  <- session$userData$itemsExt() # using side effects
  itemsExt@items[[rowNum]] <- new(Class              = "itemClassExt",
                                  name               = input$txbName,
                                  definition         = item@definition,
                                  value1             = values$txbValue1,
                                  value2             = values$txbValue2,
                                  value3             = values$txbValue3,
                                  value4             = values$txbValue4,
                                  probabilityDensity = input$slbProbabilityDensity)
  session$userData$itemsExt(itemsExt) # using side effects

  removeModal() # ? why here?
}
