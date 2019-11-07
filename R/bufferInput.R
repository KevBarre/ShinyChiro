#' @param id un entier de 1 à 10
bufferInputUI <- function(id){
  ns <- NS(id)

  # Ensemble de "tags" UI pour shiny
  fluidRow(
    div(
      # Case à cocher
      checkboxInput(ns("active_buffer"),paste("Buffer",id)),
      style = "display: inline-block;
               vertical-align:bottom;
               width: 100px;"
    ),
    div(
      id = "buffer_width_div",
      # saisie numérique
      # numericInput(ns("buffer_width"),
      #              paste("Width for buffer",id,"in meters"),
      #              value = 1),
      uiOutput(ns("buffer_width")),
      style = "display: inline-block;
             vertical-align:bottom;"
    )
  )

}

bufferInput <- function(input, output, session, id){
  ns <- session$ns

  # traitement depuis l'UI
  output$buffer_width <- renderUI({
    # browser()

    if(input$active_buffer)
      numericInput(ns("buffer_width"),
                   paste("Width for buffer",id,"in meters"),
                   value = 1)
    else
      NULL
  })

  return(
    reactive(input$buffer_width)
  )

}
