
rsconnect::setAccountInfo(name='gopalbhatta', token='096C05EA32029E94A2C64CC8D0AC1C87', secret='sGw0Xx6cyU8tp5IIT+P9/GIrcH0eJkp0gViarrCl')


library(shiny)

# Define UI for Forest Cover application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Forest Cover Type"),
  
  # Sidebar with controls to select the variable to plot against Forest Type
  # and to specify whether outliers should be included
  sidebarPanel(
    h4("Exploratory Graphs"),
    selectInput("variable", "Plot Variable:",
                list("Elevation" = "Elevation", 
                     "Aspect" = "Aspect", 
                     "Slope" = "Slope",
                     "Horizontal Distance to Water"="Horizontal_Distance_To_Hydrology",
                     "Vertical Distance to Water"="Vertical_Distance_To_Hydrology",
                     "Distance to Roadway" = "Horizontal_Distance_To_Roadways",
                     "Shade at 9 am"="Hillshade_9am",
                     "Shade at noon"="Hillshade_Noon",
                     "Shade at 3 pm"="Hillshade_3pm",
                     "Horizontal Distance to Fire"="Horizontal_Distance_To_Fire_Points"
                )),
    
    checkboxInput("outliers", "Show outliers", FALSE),
    
    
    h4("Prediction"),
    
    selectInput("Wilderness","Wilderness Area",
                list("1"="1",
                     "2"="2",
                     "3"="3",
                     "4"="4")),
    selectInput("Soil","Soil Type",
                list("1"="1",
                     "2"="2",
                     "3"="3",
                     "4"="4",
                     "5"="5",
                     "6"="6",
                     "8"="8",
                     "9"="9",
                     "10"="10",
                     "11"="11",
                     "12"="12",
                     "13"="13",
                     "14"="14",
                     "16"="16",
                     "17"="17",
                     "18"="18",
                     "19"="19",
                     "20"="20",
                     "21"="21",
                     "22"="22",
                     "23"="23",
                     "24"="24",
                     "25"="25",
                     "26"="26",
                     "27"="27",
                     "28"="28",
                     "29"="29",
                     "30"="30",
                     "31"="31",
                     "32"="32",
                     "33"="33",
                     "34"="34",
                     "35"="35",
                     "36"="36",
                     "37"="37",
                     "38"="38",
                     "39"="39",
                     "40"="40"
                )),
    
    sliderInput("E","Elevation (ft):",value=2750,min=1863,max=3849),
    sliderInput("A","Aspect (degrees):",value=180,min=0,max=360),
    sliderInput("S","Slope (degrees):",value=17,min=0,max=52),
    sliderInput("HW","Horizontal Distance to Water (ft):",value=230,min=0,max=1343),
    sliderInput("VW","Vertical Distance to Water (ft):",value=0,min=-146,max=554),
    sliderInput("HR","Horizontal Distance to Road (ft):",value=1715, min=0,max=6890),
    sliderInput("S9","Hillshade at 9 am:",value=128,min=0,max=255),
    sliderInput("SN","Hillshade at Noon:",value=128,min=0,max=255),
    sliderInput("S3","Hillshade at 3 pm:",value=128,min=0,max=255),
    sliderInput("HF","Horizontal Distance to Fire (ft):",value=1511,min=0,max=6993)
  ),
  
  # Show the caption and plot of the requested variable against Forest Type
  mainPanel(
    tabsetPanel
    (
    tabPanel("Instructions",h6(verbatimTextOutput("ReadMe"))),
    tabPanel("Plot",h3(textOutput("caption")),plotOutput("CoverPlot")),
    tabPanel("Prediction",h3("Predicted Cover Type"),h6(verbatimTextOutput("Pred")),h6(tableOutput("values")))
    
    )
    
    
  )
  
))

