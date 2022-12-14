# Import libraries
library(tidyverse)
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)


# Read data
soccer <- read.csv(file = "C:/Users/narays64/OneDrive - Pfizer/Desktop/R mini/Shiny/work_dir/soccer_up.csv") 
soccer_1 <- soccer %>%
  mutate (WinTeam= case_when(Winner > 0 ~ 'Away',
                             Winner < 0 ~ 'Home',
                             Winner == '0' ~ 'Draw')) %>%
  select('home_team', 'away_team',  'neutral', 'WinTeam') 

# Read in the RF model
model <- readRDS("soccer.rds")

####################################
# User interface                   #
####################################

ui <- fluidPage(theme = shinytheme("united"),
                
                # Page header
                headerPanel('who is the winner?'),
                
                # Input values
                sidebarPanel(
                  HTML("<h3>Input parameters</h3>"),
                  
                  selectInput("home_team", label = "Home_team:", 
                              choices = c("All", as.character(unique(soccer_1$home_team)) 
                                                  )),
                  selectInput("away_team", label = "Away_team:", 
                              choices = c("All", as.character(unique(soccer_1$away_team))
                              )),
                  #selectInput("country", label = "Host_country:", 
                  #           choices = c("All", unique(soccer_1$country) 
                  #          )),
                  #selectInput("Continent", label = "Continent:", 
                  #           choices = c("All", unique(soccer_1$Continent) 
                  #          )),
                  selectInput("neutral", label = "Neutral_Venue:", 
                              choices = list("TRUE"= "TRUE", 'FALSE'= "FALSE"),
                              selected = "TRUE"),
                  
                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                ),
                
                mainPanel(
                  tags$label(h3('Status/Output')), # Status/Output Text Box
                  verbatimTextOutput('contents'),
                  tableOutput('tabledata') # Prediction results table
                  
                )
)

####################################
# Server                           #
####################################

server <- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df_1 <- data.frame(
      Name = c("home_team",
               "away_team",
               #"country",
               #"Continent",
               "neutral"),
      Value = as.character(c(input$home_team,
                             input$away_team,
                             #input$country,
                             #input$Continent,
                             input$neutral)),
      stringsAsFactors = FALSE)
    
    WinTeam <- "WinTeam"
    df_1 <- rbind(df_1, WinTeam)
    input <-transpose(df_1)
    write.table(input,"input_1.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input_1", ".csv", sep=""), header = TRUE)
    
    test$home_team <- factor(test$home_team, levels=( c(as.character(unique(soccer_1$home_team)) )))
    test$away_team <- factor(test$away_team, levels=( c(as.character(unique(soccer_1$away_team)) )))
    test$neutral <- factor(test$neutral, levels = c("TRUE", "FALSE"))

    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################

shinyApp(ui = ui, server = server)

