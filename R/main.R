library(shiny)
library(data.table)
require(maptools)
library(sp)
library(raster)

source("module.R")

ui <- fluidPage(
  # File inputs
  fileInput("points", "Choisir le shapefile de points"),
  textOutput("points_out"),
  fileInput("habitats", "Choisir le raster habitat"),
  textOutput("habitats_out"),

  # Buffers definition
  lapply(1:10, bufferInputUI),
  actionButton("dev","Dev")
)

server <- function(input, output, session){

  # variable init
  rv = reactiveValues()

  # dev
  observeEvent(input$dev,{
    browser()
  })

  # reactivity

  ## Files reactive
  observeEvent(input$habitats, {

    output$habitats_out <- renderText({
      # validity check
      validate(
        need({grep("jpeg$|jpg$|tiff$|tif$", input$habitats$type)}, "Not an image")

      )

      # if passed
      # set Points reactive
      rv$Hab <- reactive({
        raster(input$habitats$datapath)
      })

      # return message
      return("File loaded")
    })
  })

  observeEvent(input$points, {

    output$habitats_out <- renderText({
      # validity check
      # browser()
      validate(
        need({grep("shp$", input$points$name)}, "Not a shapefile")

      )

      # if passed
      # set Points reactive
      rv$Points <- reactive({
        raster(input$points$datapath)
      })

      # return message
      return("File loaded")
    })
  })

  ## buffer reactives
  lapply(1:10, function(i){
    # receive reactive
    rv[[paste0("buffer_",i)]] <- callModule(bufferInput, i,i)
  })



}

shinyApp(ui, server)
