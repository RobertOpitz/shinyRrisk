#options(warn = 2, show.error.locations = "bottom")
shiny::runApp('./inst/app.R', 
              port = 3838, 
              launch.browser = TRUE,
              display.mode = "normal")