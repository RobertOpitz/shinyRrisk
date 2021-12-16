#' @name serverSetup
#' @title serverSetup
#' @description Provides event handler functions.
#' @param id Namespace identifier.
#' @return NULL
#' @export
serverSetup <- function(id = "setup") {
  cat("serverSetup\n")

  module <- function(input, output, session) {

    observeEvent(input$btnCreateModel, 
                 {if (!validateSetupInput(input))
                   return(NULL)
                  newRriskModel <- new("modelClass", 
                                       name = new("modelNameClass", 
                                                  name = input$txbModelName))
                  newRriskModel@scoring       <- initScoreSystem()
                  newRriskModel@uncertainties <- initUncertainties()
                  session$userData$model(newRriskModel)})

    observeEvent(input$filModelFile, 
                 {if (!is.null(input$filModelFile))
                   loadModel(path = input$filModelFile$datapath)}) 

    observeEvent(input$btnExamplePerfringensLee,
                 loadModel(path = "../data/Clostridum_perfringens_processed_cheeses.rda"))

    observeEvent(input$btnExampleEcoliPouillot,
                 loadModel(path = "../data/ecoli-pouillot.rda"))

    output$labActiveModelName <- renderText(session$userData$model()@name@name)

    output$btnModelSave <- downloadHandler(
      filename = function() {
        paste0(session$userData$model()@name@name, ".rda")
      },
      content = function(file) {
        rriskModel  <- session$userData$model()
        itemsExt    <- session$userData$itemsExt()
        save(file = file,
             list = c(getObjectName(rriskModel),
                      getObjectName(itemsExt)))
      })

    output$btnJsonExport <- downloadHandler(
      filename = function() {
        paste0(session$userData$model()@name@name, ".json")
      },
      content = function(file) {
        rriskModel  <- session$userData$model()
        json        <- RJSONIO::toJSON(rriskModel)
        write(json, file)
      }
    )

    output$btnExportReportZip <- downloadHandler(
      filename  = getExportName("zip"),
      content   = function(file) {
        csv   <- buildCsv()
        plots <- buildPlots()
        files <- c(csv, plots) #append(csv, plots)
        zip(zipfile = file,
            files = unlist(files, use.names = FALSE))
        for (file in files)
          if (file.exists(file))
            file.remove(file)
      }
    )

    output$btnExportReportPdf <- downloadHandler(
      filename  = getExportName("docx"),
      content   = function(file) {
        tempReport  <- file.path(tempdir(), "report.rmd")
        params      <- list(model = session$userData$model())
        params      <- append(params, buildPlots())

        file.copy("./www/rmd/report.rmd", tempReport, overwrite = TRUE)
        
        print(str(params))

        rmarkdown::render(tempReport,
                          output_file = file,
                          params      = params,
                          envir       = new.env(parent = globalenv()))

        for (file in params)
          if (is.character(file))
            if (file.exists(file))
              file.remove(file)
      }
    )
  } # end of module function

  moduleServer(id, module)
}
