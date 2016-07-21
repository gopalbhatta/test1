
# Shiny App for Project in coursera's
#   Developing Data Products
#
# G. Bhatta
#   July 21, 2016
#
# Using data from Kaggle's
#   Forest Cover Type Prediction Competition
#
library(shiny)
library(randomForest)
library(caret)
library(plyr)

Data <- read.csv("./data/Data.csv")
shinyServer(function(input, output) {
  
  # Compute the forumla text in a reactive expression since it is 
  # shared by the output$caption and output$CoverPlot expressions
  formulaText <- reactive({
    paste(input$variable,"~Cover_Type")
  })
  
  # Return the formula text for printing as a caption
  output$caption <- renderText({
    formulaText()
  })
  
  # Generate a plot of the requested variable against Cover Type and only 
  # include outliers if requested
  output$CoverPlot <- renderPlot({
    boxplot(as.formula(formulaText()), 
            data = Data,
            outline = input$outliers,
            col="orange",
            xlab="Cover Type",
            ylab="Measured")
  })
  
  ## Generate a prediction of which Cover Type you have selected
  
  sliderValues <- reactive({
    
    data.frame(
      row.names=c("Elevation",
                  "Aspect",
                  "Slope",
                  "Horizontal_Distance_To_Hydrology",
                  "Vertical_Distance_To_Hydrology",
                  "Horizontal_Distance_To_Roadways",
                  "Hillshade_9am",
                  "Hillshade_Noon",
                  "Hillshade_3pm",
                  "Horizontal_Distance_To_Fire_Points",
                  "Wilderness_Area1",
                  "Wilderness_Area2",
                  "Wilderness_Area3",
                  "Wilderness_Area4",
                  "Soil_Type1",
                  "Soil_Type2",
                  "Soil_Type3",
                  "Soil_Type4",
                  "Soil_Type5",
                  "Soil_Type6",
                  "Soil_Type8",
                  "Soil_Type9",
                  "Soil_Type10",
                  "Soil_Type11",
                  "Soil_Type12",
                  "Soil_Type13",
                  "Soil_Type14",
                  "Soil_Type16",
                  "Soil_Type17",
                  "Soil_Type18",
                  "Soil_Type19",
                  "Soil_Type20",
                  "Soil_Type21",
                  "Soil_Type22",
                  "Soil_Type23",
                  "Soil_Type24",
                  "Soil_Type25",
                  "Soil_Type26",
                  "Soil_Type27",
                  "Soil_Type28",
                  "Soil_Type29",
                  "Soil_Type30",
                  "Soil_Type31",
                  "Soil_Type32",
                  "Soil_Type33",
                  "Soil_Type34",
                  "Soil_Type35",
                  "Soil_Type36",
                  "Soil_Type37",
                  "Soil_Type38",
                  "Soil_Type39",
                  "Soil_Type40"),
      
      Value = c(as.integer(input$E),
                as.integer(input$A),
                as.integer(input$S),
                as.integer(input$HW),
                as.integer(input$VW),
                as.integer(input$HR),
                as.integer(input$S9),
                as.integer(input$SN),
                as.integer(input$S3),
                as.integer(input$HF),
                as.integer(if(input$Wilderness=="1"){c("1")}else{c("0")}),
                as.integer(if(input$Wilderness=="2"){c("1")}else{c("0")}),
                as.integer(if(input$Wilderness=="3"){c("1")}else{c("0")}),
                as.integer(if(input$Wilderness=="4"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="1"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="2"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="3"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="4"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="5"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="6"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="8"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="9"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="10"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="11"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="12"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="13"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="14"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="16"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="17"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="18"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="19"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="20"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="21"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="22"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="23"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="24"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="25"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="26"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="27"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="28"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="29"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="30"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="31"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="32"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="33"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="34"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="35"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="36"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="37"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="38"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="39"){c("1")}else{c("0")}),
                as.integer(if(input$Soil=="40"){c("1")}else{c("0")})
                
      ),
      stringsAsFactors=FALSE)        
  })  
  
  output$values <- renderTable({
    sliderValues()
  })
  
  output$ReadMe<-renderPrint({
    paste("This app was created in R using Shiny. The data comes from the Kaggle Forest Cover Type Prediction competition found here: http://www.kaggle.com/c/forest-cover-type-prediction/data. There are three tabs on this app.  The Instruction tab which explains how the app works. The plot tab which allows to explore the data with different features. The prediction tab which allows to change different parameters and it will output a prediction of what type of forest cover is at that location. The prediction model uses a gradient boosted trees method.  This method has a 75.4% accuracy on out of sample data.")
  })
  
  output$Pred<-renderPrint({
    load("./data/fit.rda")
    PredVal<-predict(fit,t(sliderValues()))
    paste("The predicted forest cover type is ",toString(PredVal[1]),sep='')
  })
  
  
})
