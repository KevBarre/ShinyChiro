library(rgdal)
library(shiny)
library(data.table)
library(maptools)
library(sp)
library(raster)
library(tools)
library(utils)

source("bufferInput.R")

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

  # upload size
  options(shiny.maxRequestSize=1024*1024^2)

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

      #shinyalert("Please wait", "Raster file is loading", type = "info")

      # if passed
      # set Points reactive
      rv$Hab <- reactive({

        raster::brick(input$habitats$datapath)
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
        need({grep("application/x-zip-compressed$", input$points$type)}, "Not a zipfile")
      )

      # if passed
      # set Points reactive
      pts_unzip <- unzip(input$points$datapath, exdir = dirname(input$points$datapath))
      rv$Points <- reactive({
        readOGR(dsn = dirname(pts_unzip)[1],layer = file_path_sans_ext(basename(pts_unzip))[1])
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
