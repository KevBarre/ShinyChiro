library(rgdal)
library(shiny)
library(shinyFiles)
library(data.table)
library(maptools)
library(sp)
library(raster)
library(tools)
library(utils)
library(maps)
library(gridExtra)
library(rgeos)
library(dplyr)
library(sp)
library(GISTools)
library(sf)

source("bufferInput.R")
source("ExtractingLandUseBuffers_fromRaster.R")

ui <- fluidPage(
  column(4,
         # File inputs
         fileInput("points", "Choisir le fichier zip contenant tous les fichiers de la couche shapefile de points. La couche doit contenir une colonne nommée id et contenant les identifiants des sites."),
         # selectInput
         selectInput("ID","Nom de la colonne contenant les identifiants des sites", choices = c("",""), selected = ""),

         textOutput("points_out"),
         fileInput("habitats", "Choisir le raster habitat (le chargement peut prendre plusieurs minutes)"),
         textOutput("habitats_out"),
         shinyDirButton("saveDir","Emplacement de sauvegarde","Browse"),

         # Buffers definition
         lapply(1:10, bufferInputUI),
         actionButton("dev","Dev"),

         # process data button

         actionButton("proc","Exécuter"),
         fluidRow(
           h6(""),
           helpText("Note: le calcul prend environ 3 minutes pour 20 sites et 4 buffers différents (500, 1000, 2000 et 4000 mètres)"))
  ),
  column(8,
         fluidRow(
           h2("Visualisation spatiale"),
           plotOutput("mapPlot")
         ),
         fluidRow(
           h2("Prévisualisation du calcul des premiers sites (les résultats se trouvent dans l'emplacement de sauvergde sélectionné"),
           tableOutput("preview")
         )
  )

)

server <- function(input, output, session){

  # var init ----
  roots = c(home=normalizePath("~"))
  shinyDirChoose(input, 'saveDir', roots=roots, filetypes=c('', 'txt'))

  # upload size
  options(shiny.maxRequestSize=1024*1024^2)

  # variable init
  rv = reactiveValues()

  # dev
  observeEvent(input$dev,{
    # browser()
  })

  # reactivity ----

  ## Files reactive

  observeEvent(input$points, {

    output$points_out <- renderText({
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

      # check
      validate(
        need({grep("shp$", pts_unzip)}, "Not a shapefile")
      )

      # return message
      return("File loaded")
    })

  })

  # column names
  n <- c("id")

  #rv$n <- reactive(unlist(names(rv$Points()@data)))
  #
  # observe({
  #   n <- unlist(names(rv$Points()))
  # })
  # n <- names(pts)
  # n <- reactive({
  #   names(rv$Points()@data)})
  # browser()
  # n <- reactive({
  #   as.vector(names(rv$Points()))
  # })
  # n <- reactive(names(pts@data))

  # selection of column name used identify sites
  observe({
    updateSelectInput(session,
                      "ID",
                      "Nom de la colonne contenant l'identifiant des sites",
                      choices = n)

  })

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
      # browser()

      # return message
      return("File loaded")
    })
  })

  # Plot output
  output$mapPlot <- renderPlot({
    req(rv$Hab,
        rv$Points)

    plot(rv$Hab())
    plot(rv$Points(), add=TRUE)
  })

  ## buffer reactives ----
  lapply(1:10, function(i){
    # receive reactive
    rv[[paste0("buffer_",i)]] <- callModule(bufferInput, i,i)

  })

  rv$ID <- reactive(input$ID)

  rv$saveDir <- reactive({
    parseDirPath(roots, input$saveDir)
  })

  observeEvent(input$proc, {
    rv_list <- reactiveValuesToList(rv)
    buffers <- rv_list[grepl("buf", names(rv_list))]
    buffers <- unlist(sapply(buffers, function(i) i()))

    message("eval")

    req(rv$Hab,
        rv$Points,
        rv$ID,
        buffers,
        rv$saveDir)

    message("passed")

    output$preview <- renderTable({head(extractLandUseRaster(Points = rv$Points(),
                                                             bufwidth = buffers,
                                                             ID = rv$ID(),
                                                             Hab = rv$Hab(),
                                                             saveDir = rv$saveDir()
    ) # end big dfunction
    ) # end head
    })# end render

  })

}

# launch app ----
shinyApp(ui, server)
