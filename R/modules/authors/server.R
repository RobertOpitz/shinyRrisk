#' @name serverAuthors
#' @title serverAuthors
#' @description Provides event handler functions.
#' @param id Namespace identifier.
#' @return NULL
#' @export
serverAuthors <- function(id = "authors") {
  cat("serverAuthors\n")
  ns <- NS(id)
  
  module <- function(input, output, session) {

    observeEvent(input$btnModalAdd, {
      showModal(authorModalAdd())
    })

    observeEvent(input$btnAdd, {

      if (!validateAuthorModal(input))
        return(NULL)

      nextRow <- length(session$userData$model()@authors@authors) + 1
      setAuthor(input, nextRow)
    })

    observeEvent(input$btnModalEdit, {
      rowNum <- parseActionButton(input$btnModalEdit)
      author <- session$userData$model()@authors@authors[[rowNum]]
      showModal(authorModalEdit(author = author, 
                                rowNum = rowNum))
    })

    observeEvent(input$btnEdit, {

      if (!validateAuthorModal(input))
        return(NULL)

      setAuthor(input, strtoi(input$txbRowNum))
    })

    observeEvent(input$btnDelete, {
      rowNum <- parseActionButton(input$btnDelete)
      model <- session$userData$model()
      model@authors@authors <- model@authors@authors[-rowNum]
      session$userData$model(model)
    })

    output$tblAuthors <- DT::renderDataTable({

      if (length(session$userData$model()@authors@authors) == 0)
        return(NULL)

      addActionButtonsColumn(
        data.frame(matrix(unlist(toList(session$userData$model()@authors)),
                          nrow = length(session$userData$model()@authors@authors),
                          byrow = TRUE)),
        c("Name", "Institution", "Mail", "Actions"),
        c("edit", "delete"),
        ns)
    })
  }

  moduleServer(id, module)
}
