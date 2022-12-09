


# Import libraries
library(tidyverse)
library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(randomForest)


# Read data
soccer <- read.csv(file = "C:/Users/srira/OneDrive/Documents/R/work_dir/results_1.csv") 
soccer_1 <- soccer %>%
   filter (tournament == 'FIFA World Cup' &(home_team %in% c("Brazil", "Portugal", "Argentina", "Morocco", "France", "Netherlands", "England") & away_team %in% c("Brazil", "Portugal", "Argentina", "Morocco", "France", "Netherlands", "England") )) %>%
  mutate (WinTeam= case_when(Winner > 0 ~ 'Away',
                             Winner < 0 ~ 'Home',
                             Winner == '0' ~ 'Draw')) %>%
select('home_team', 'away_team',  'neutral', 'WinTeam') 


#weather$outlook <- factor(weather$outlook, levels = c("overcast", "rainy", "sunny"))
soccer_1$WinTeam <- factor(soccer_1$WinTeam, levels = c("Away", "Draw", "Home"))
soccer_1$home_team <- factor(soccer_1$home_team, levels = c( "Argentina","Brazil",  "England","France", "Morocco", "Netherlands", "Portugal"))
soccer_1$away_team <- factor(soccer_1$away_team, levels = c( "Argentina","Brazil",  "England","France", "Morocco", "Netherlands", "Portugal"))
#soccer_1$country <- factor(soccer_1$country, levels = c(unique(soccer_1$country)))
#soccer_1$Continent <- factor(soccer_1$Continent, levels = c(unique(soccer_1$Continent)))
soccer_1$neutral <- factor(soccer_1$neutral, levels = c("FALSE", "TRUE"))
# Build model
model <- randomForest (WinTeam ~ ., data = soccer_1, ntree = 500, mtry = 3, importance = TRUE)

# Save model to RDS file
saveRDS(model, "model.rds")

# Read in the RF model
model <- readRDS("model.rds")

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
                              choices = list("Argentina"="Argentina","Brazil"="Brazil",  "England"="England","France"="France", "Morocco"="Morocco", "Netherlands"="Netherlands", "Portugal"="Portugal"), 
                              selected = "Argentina"),
                  selectInput("away_team", label = "Away_team:", 
                              choices = list("Argentina"="Argentina","Brazil"="Brazil",  "England"="England","France"="France", "Morocco"="Morocco", "Netherlands"="Netherlands", "Portugal"="Portugal"), 
                              selected = "Brazil"),
                  #selectInput("country", label = "Host_country:", 
                   #           choices = c("All", unique(soccer_1$country) 
                    #          )),
                  #selectInput("Continent", label = "Continent:", 
                   #           choices = c("All", unique(soccer_1$Continent) 
                    #          )),
                selectInput("neutral", label = "Neutral_Venue:", 
                            choices = list("Yes"= "TRUE", 'No'= "FALSE"),
                            selected = "TRUE"),
                ),


                  actionButton("submitbutton", "Submit", class = "btn btn-primary")
                )
                
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
    
    test$home_team <- factor(test$home_team, levels = c( "Argentina","Brazil",  "England","France", "Morocco", "Netherland", "Portugal"))
    test$away_team <- factor(test$away_team, levels = c( "Argentina","Brazil",  "England","France", "Morocco", "Netherland", "Portugal"))
    #test$country <- factor(test$country, levels = c(unique(soccer_1$country)))
    #test$Continent <- factor(test$Continent, levels = c(unique(soccer_1$Continent)))
    test$neutral <- factor(test$neutral, levels = c("TRUE", "FALSE"))
   #test$WinTeam <- factor(test$WinTeam, levels = c("Away", "Draw", "Home"))
    
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

.####################################
# Create the shiny app             #
####################################

shinyApp(ui = ui, server = server)

