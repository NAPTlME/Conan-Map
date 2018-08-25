# Script to start up a Shiny Server Instance
# Displays a Conan Exiles map and claim information using images and rds files
# Nathan Pratt
# 8/23/18

requiredPkgs = c("stringr", "dplyr", "shiny", "ggplot2", "png", "grid", 
                 "reshape2", "raster", "rgdal", "viridis")
missingPkgs = requiredPkgs[!(requiredPkgs %in% installed.packages()[,"Package"])]
if(length(missingPkgs)) install.packages(missingPkgs)

#### Load Libraries ####
library(stringr)
library(dplyr)
library(shiny)
library(ggplot2)
library(png)
library(grid)
library(reshape2)
library(raster)
library(rgdal)
library(viridis)
source("conanFx.r")

#### Read Images ####
img = readPNG("Images/T_FullscreenMap_BASE.png")
imgDark = readPNG("Images/T_FullscreenMap_NO-CLAIM.png")

#### Get Coordinate Shape Data ####
tmpList = readRDS("MetaData/cmMapCoordsFinal.rds")
claimableCoords = tmpList$Claimable
outOfPlayCoords = tmpList$OutofPlay
poiCoords = tmpList$POI
rm(tmpList)

#### Set up Shiny UI ####
width = 1200
ui = fluidPage(
  fluidRow(
    column(width = 8, plotOutput("plot1", height = round(width * 0.735, 0), width = width, hover = hoverOpts(id = "plot_hover"))),
    column(width = 2, 
           #wellPanel(
           #  actionButton("close", "Stop Session")
           #),
           wellPanel(
             selectInput(inputId = "cmbBoxFrom", label = "Select Starting Cell", choices = cmbBoxChoices, multiple = F, selected = cmbBoxChoices[1]),
             selectInput(inputId = "cmbBoxTo", label = "Select Ending Cell", choices = cmbBoxChoices, multiple = F, selected = cmbBoxChoices[length(cmbBoxChoices)])
           ),
           wellPanel(
             selectInput(inputId = "img", label = "Select Base Image", choices = c("Base", "Darkened", "None"), multiple = F,  selected = "Base"),
             checkboxGroupInput(inputId = "options", label = "Map Options",
                                choices = c("Show Claims" = "showClaim", "Show Grid" = "showGrid", 
                                            "Show Unclaimed" = "showUnclaimed", "Show POIs" = "showPOI",
                                            "Show Out of Play Zone" = "showOOP"),
                                selected = c("showClaim", "showGrid"))
           ),
           wellPanel(
             verbatimTextOutput("hover_info")
           ))
  )
)

#### Set up Shiny Server ####
server = function(input, output) {
  delayInput = reactive({
    list(input$cmbBoxFrom, input$cmbBoxTo, input$img, input$options)
  }) %>% debounce(2000)
  
  output$plot1 = renderPlot({
    tmp = delayInput()
    if (tmp[[3]] == "Base"){
      imgToUse = img
    } else {
      imgToUse = imgDark
    }
    # Set up df with tooltip info
    ar = readRDS(claimMatrixFile)
    gridX = as.numeric(colnames(ar))
    gridY = 1:nrow(ar)
    claimDf = GetDfFromArray(ar)
    claimDf$Var3 = NULL
    claimDf = claimDf %>% mutate(Coord = paste0(LETTERS[Var1], Var2)) %>% dplyr::select(Coord, Var1:value)
    claimDf$Var1 = as.numeric(claimDf$Var1)
    # get inputs
    fromInput = tmp[[1]]
    toInput = tmp[[2]]
    options = tmp[[4]]
    alphas = c(str_match(fromInput, "[A-Z]")[1,1], str_match(toInput, "[A-Z]")[1,1])
    numerics = c(as.numeric(str_match(fromInput, "[\\-\\d]+")[1,1]), as.numeric(str_match(toInput, "[\\-\\d]+")[1,1]))
    if ((fromInput == "A0" | fromInput == "") & (toInput == "T24" | toInput == "")){
      tmpImg = imgToUse
      xMin = 0
      xMax = 24
      yMin = 1
      yMax = 20
    } else {
      i = tmp[1]
      targetXYRatio = 5/4
      y = as.numeric(sapply(alphas, function(x) which(LETTERS == x)))
      yMin = min(y)
      yMax = max(y)
      x = numerics
      xMin = min(x)
      xMax = max(x)
      currentXYRatio = (xMax - xMin + 1) / (yMax - yMin + 1)
      if (currentXYRatio > targetXYRatio){ # Keep aspect ratio as close as possible... unfortunately ggplot doesn't like to add white space with these background images
        # x range is larger
        # increase y range
        yRange = (yMax - yMin + 1)
        optYRange = round((xMax - xMin + 1) / targetXYRatio, 0)
        diffYRange = optYRange - yRange
        #increase range
        for (i in 1:diffYRange){
          if (i %% 2 == 1){
            yMin = yMin -1
          } else {
            yMax = yMax + 1
          }
        }
        # shift within boundaries
        while (yMax > 20){
          yMax = yMax - 1
          yMin = yMin - 1
        }
        while (yMin < 1){
          yMax = yMax + 1
          yMin = yMin + 1
        }
      } else if (currentXYRatio < targetXYRatio){
        # y range is under ratio
        # increase x range
        xRange = (xMax - xMin + 1)
        optXRange = round((yMax - yMin + 1) * targetXYRatio, 0)
        diffXRange = optXRange - xRange
        # increase range
        for (i in 1:diffXRange){
          if (i %% 2 == 1){
            xMin = xMin - 1
          } else {
            xMax = xMax + 1
          }
        }
        # shift to fit boundaries
        while(xMax > 24){
          xMax = xMax - 1
          xMin = xMin - 1
        }
        while (xMin < 0){
          xMax = xMax + 1
          xMin = xMin + 1
        }
      }
      widthBase = dim(imgDark)[1]
      heightBase = dim(imgDark)[2]
      width = widthBase / 25
      height = heightBase / 20
      tmpYMin = 20 - yMax
      tmpYMax = 20 - yMin
      tmpImg = imgToUse[(tmpYMin*height):((tmpYMax+1)*height), ((xMin)*width):((xMax+1)*width),]
    }
    coordRange = paste0(LETTERS[yMin:yMax], rep(xMin:xMax, each = yMax - yMin + 1))
    rg = rasterGrob(tmpImg, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = T)
    uniqueCols = character()
    p = ggplot() +
      geom_vline(xintercept = c(xMin - 0.5, xMax + 0.5), color = "white", alpha = 0.6, size = 1) + 
      geom_hline(yintercept = c(yMin - 0.5, yMax + 0.5), color = "white", alpha = 0.6, size = 1)
    if (tmp[[3]] != "None"){
      p = p + annotation_custom(rg, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
    }
    if ("showGrid" %in% options){
      p = p + geom_vline(xintercept = seq(from = xMin - 0.5, to = xMax + 0.5, by = 1), color = "white", alpha = 0.6, size = 1) +
        geom_hline(yintercept = seq(from = yMin - 0.5, to = yMax + 0.5, by = 1), color = "white", alpha = 0.6, size = 1)
    }
    if ("showClaim" %in% options){
      tmpDf = claimDf %>% filter(value != "None")
      if (nrow(tmpDf) > 0){
        coords = tmpDf$Coord
        owners = tmpDf$value
        uniqueCols = c(uniqueCols, unique(owners))
        xVals = numeric(0)
        yVals = numeric(0)
        owner = character(0)
        id = character(0)
        it = 1
        for (i in 1:length(coords)){
          for(polygon in claimableCoords[[coords[i]]]$Polygons){
            xVals = c(xVals, polygon[,1])
            yVals = c(yVals, polygon[,2])
            owner = c(owner, rep(owners[i], times = length(polygon[,1])))
            id = c(id, rep(it, times = length(polygon[,1])))
            it = it + 1
          }
        }
        tmpDf = data.frame(X = xVals, Y = yVals, Owner = owner, ID = id)
        p = p + geom_polygon(data = tmpDf, aes(X, Y, fill = Owner, group = ID), color = "gray70", alpha = 0.03, 
                             show.legend = F)
      }
    }
    if ("showUnclaimed" %in% options){
      tmpDf = claimDf%>% filter(value == "None")
      if (nrow(tmpDf) > 0){
        coords = tmpDf$Coord
        xVals = numeric(0)
        yVals = numeric(0)
        id = character(0)
        it = 1
        for (i in 1:length(coords)){
          for (polygon in claimableCoords[[coords[i]]]$Polygons){
            xVals = c(xVals, polygon[,1])
            yVals = c(yVals, polygon[,2])
            id = c(id, rep(it, times = length(polygon[,1])))
            it = it + 1
          }
        }
        tmpDf = data.frame(X = xVals, Y = yVals, ID = id)
        p = p + geom_polygon(data = tmpDf, aes(X, Y, group = ID), fill = "lightcyan2", color = "gray70", alpha = 0.03)
      }
    }
    if ("showPOI" %in% options){
      if (length(poiCoords) > 0){
        xVals = numeric(0)
        yVals = numeric(0)
        id = character(0)
        it = 1
        for (i in 1:length(poiCoords)){
          for (polygon in poiCoords[[i]]$Polygons){
            xVals = c(xVals, polygon[,1])
            yVals = c(yVals, polygon[,2])
            id = c(id, rep(it, times = length(polygon[,1])))
            it = it + 1
          }
        }
        tmpDf = data.frame(X = xVals, Y = yVals, ID = id)
        p = p + geom_polygon(data = tmpDf, aes(X, Y, group = ID), fill = "gray", color = "gray70", alpha = 0.6)
      }
    }
    if ("showOOP" %in% options){
      if (length(outOfPlayCoords) > 0){
        xVals = numeric(0)
        yVals = numeric(0)
        id = character(0)
        it = 1
        for (i in 1:length(outOfPlayCoords)){
          for (polygon in outOfPlayCoords[[i]]$Polygons){
            xVals = c(xVals, polygon[,1])
            yVals = c(yVals, polygon[,2])
            id = c(id, rep(it, times = length(polygon[,1])))
            it = it + 1
          }
        }
        tmpDf = data.frame(X = xVals, Y = yVals, ID = id)
        p = p + geom_polygon(data = tmpDf, aes(X, Y, group = ID), fill = "gray15", color = "gray70", alpha = 0.6)
      }
    }
    print(length(as.character(xMin:xMax)) == length(xMin:xMax))
    print(length(LETTERS[yMin:yMax]) == length(yMin:yMax))
    p = p + theme(
      panel.background = element_rect(fill = NA),
      panel.ontop = F,
      panel.grid.minor = element_blank(),
      axis.title = element_blank()) +
      coord_cartesian(xlim = c(xMin - 0.5, xMax + 0.5), ylim = c(yMin - 0.5, yMax + 0.5), expand = F) +
      scale_x_continuous("Var2", labels = as.character(xMin:xMax), breaks = xMin:xMax) + 
      scale_y_continuous("Var1", labels = LETTERS[yMin:yMax], breaks = yMin:yMax) +
      lims(colour = uniqueCols) #+ 
    #scale_fill_viridis()
    p
  })
  output$hover_info = renderPrint({
    if(!is.null(input$plot_hover)){
      hover = input$plot_hover
      y = LETTERS[floor(hover$y + 0.5)]
      x = floor(hover$x + 0.5)
      coord = paste0(y,x)
      cat(paste0("Coord: ", coord, "\n"))
      #cat(paste0("Y: ", hover$y, "\n"))
      #cat(paste0("X: ", hover$x, "\n"))
      claimMatch = claimDf %>% filter(Coord == coord)
      cat(paste0("Is Claimable: ", coord %in% names(claimableCoords), "\n"))
      if (coord %in% names(claimableCoords)){
        cat(paste0("Claimed by: ", claimMatch[1,"value"], "\n"))
      }
    }
  })
  #observeEvent(input$close, {
  #  stopApp()
  #  print("Closed")
  #})
}

#### Run Shiny Instance ####
shinyApp(ui, server)