# dynamic_require <- function(package){
#   if (eval(parse(text = paste("require(",package,")")))) 
#     return(TRUE)
#   
#   install.packages(package)
#   return(eval(parse(text = paste("require(",package,")"))))
# }
# 
# get_libraries <- function(packages) {
#   for (package in packages)
#     if (!dynamic_require(package)) 
#       install.packages(package)
# }
# 
# get_libraries(c("DT",
#                 "fitdistrplus",
#                 "mc2d",
#                 "network",
#                 "networkD3",
#                 "plotly",
#                 "rmarkdown", "knitr",
#                 "RJSONIO",
#                 "shiny",
#                 "shinyalert",
#                 "shinycssloaders",
#                 "shinydashboard",
#                 "shinyFeedback",
#                 "shinyjs",
#                 "parallel", 
#                 "mgcv"))

# if (!require("DT")) install.packages("DT")
# #library(DT)
# if (!require("fitdistrplus")) install.packages("fitdistrplus")
# library(fitdistrplus)
# library(mc2d)
# library(mgcv)
# library(network)
# library(networkD3)
# library(plotly)
# #library(rapportools)
# library(rmarkdown)
# library(RJSONIO)
# library(shiny)
# library(shinyalert)
# library(shinycssloaders)
# library(shinydashboard)
# library(shinyFeedback)
# library(shinyjs)
# # test for faster plotting of gam-plots
# library(parallel) 

#nb_of_threads <- 3
#cl <- makeCluster(nb_of_threads)
#clusterEvalQ(cl = cl, library(mgcv))

options(shiny.maxRequestSize = 32 * (1024^2))

source(file = "./import.R", local = TRUE)

ui <- dashboardPage(
  title = "Tool for quantitative risk modelling",
  uiHeader(),
  uiSidebar(),
  uiBody()
)

server <- function(input, output, session) {

  # init default model
  model                   <- new(Class = "modelClass", 
                                 name  = new(Class = "modelNameClass", 
                                             name = "NA"))
  model@scoring           <- initScoreSystem()
  model@uncertainties     <- initUncertainties()
  session$userData$model  <- reactiveVal(model)

  # init default items extension
  itemsExtension            <- new(Class = "modelItemsClassExt")
  session$userData$itemsExt <- reactiveVal(itemsExtension)

  # run model logic
  serverAnalysis()
  serverAuthors()
  serverBasics()
  serverUncertainties()
  serverHeader()
  serverItems()
  #serverOverview()
  serverRun()
  serverScoring()
  serverSetup()
  #serverSimulation()
  serverSummary()
  
  # stop app when tab or window is closed
  session$onSessionEnded(function() {
    # dialog if the user really want to stop app
    # stop cluster
    #stopCluster(cl)
    # stop app
    stopApp()
  })
}

shinyApp(ui = ui, server = server)
