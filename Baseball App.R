# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(ggpubr)
#library(tidyverse)


d <- read.csv("truncated_sample2.csv", header =T)
d <-d[order(d$Round),]




#Make sure to uncomment these and add them to the data set so that the APP
# has correct functionality.
BAlm <- lm(formula = BA ~ Round, data = d)
EVlm <- lm(formula = exit_velocity_avg ~ Round, data = d)
BBRlm <- lm(formula = barrel_batted_rate ~ Round, data = d)
dtop <- d[1:10,]


# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
    navbarPage(
                  "Baseball App",
                  tabPanel("Batting Average & Draft",
                           sidebarPanel("MLB Baseball teams spend millions of dollars each year 
                   drafting new players to transform their teams. Drafts
                   run in rounds, and in theory the players drafted in 
                   earlier rounds should be the most talented.  But how
                   well does this really hold up?  The graphs on the right
                   give an intro to the data that compare draft round to 
                   batting average (left chart) and barrels (right chart).
                   Use the tabs at the top of the page to find out if
                   draft order is a strong predictor of player success."),
                           mainPanel("Player Stats",
                                     fluidRow(
                                       splitLayout(cellWidths = c("50%", "50%"), 
                                                   plotOutput("plot"),
                                                   plotOutput("plot2"))
                                     ))),
                 
                  tabPanel("Regression Analysis",
                           sidebarPanel(
                             radioButtons(
                               inputId = "reg",
                               label = "Select a Statistic",
                               choices = c(
                                 "Batting Average", "Exit Velocity", "Barrel Batted Rate"
                               )),
                             "Select a player statistic to run a regression analysis comparing
              the round a player was drafted to their performance in that metric.
              Underneath the regression scatterplot are the details of the linear
              regression."
                           ),
                           mainPanel(plotOutput("regPlot"),
                                     verbatimTextOutput("data")
                           )
                  ),
                  tabPanel("Draft of 10 Players Statistics",
                           sidebarPanel(
                             selectInput(
                               inputId = "team",
                               label = "Draft Picks",
                               choices = c("Top", "Middle", "Bottom", "Random")),
                             selectInput(
                               inputId = "stats",
                               label = "Playing Statistic",
                               choices = c("Batting Average", "Exit Velocity", "Barrel Batted")
                             ),
                             "Use the drop-down menu above to conduct a hypothetical of ten 
             players draft from the dataset.  You can select the top players--
             those who were drafted first, the players drafted in the middle,
             the bottom players--those who were drafted last, or you can pull
             a random draft of ten players.  
             Use the second drop-down box to select a playing statistic to 
             examine in the histogram."
                           ),
                           mainPanel(
                             plotOutput("teamPlot")
                           )
                  )
                )
)
# Define server function  

server <- function(input, output) {
  
  #code for home page    
  output$plot <- renderPlot({
    ggplot(d, aes(x=BA, y=Round)) + 
      geom_point(aes(color = pick)) +
      scale_size_area(limits = c(0, 100), max_size = 10, guide = NULL)
  }, res = 96 )
  
  output$plot2 <- renderPlot({
    ggplot(d, aes(x=exit_velocity_avg, y=Round)) + 
      geom_point(aes(color = pick)) +
      scale_size_area(limits = c(0, 100), max_size = 10, guide = NULL)
  }, res = 96 )


  #plots for regression tab 
  output$regPlot <- renderPlot({
    if (input$reg =="Batting Average") {
      ggplot(d, aes(x=Round, y=BA)) + geom_point(size = 3) +
        geom_smooth(method = "lm") + theme(text= element_text(size = 25))
      #plot(BA ~ Round, data = d)
      #abline(lm(BA ~ Round, data = d), col = "blue")
    } else if (input$reg == "Exit Velocity") {
      ggplot(d, aes(x=Round, y=exit_velocity_avg)) + geom_point(size = 3) +
        geom_smooth(method = "lm") + theme(text= element_text(size = 25))
      #plot(exit_velocity_avg ~ Round, data = d)
      #abline(lm(exit_velocity_avg ~ Round, data = d), col = "blue")
    } else if (input$reg == "Barrel Batted Rate") {
      ggplot(d, aes(x=Round, y=barrel_batted_rate)) + geom_point(size = 3) +
        geom_smooth(method = "lm") +
        theme(text= element_text(size = 25))
      #plot(barrel_batted_rate ~ Round, data = d)
      #abline(lm(barrel_batted_rate ~ Round, data = d), col = "blue")
    }
  })
  
  #text for regression tab
  output$data <- renderPrint({
    if (input$reg == "Batting Average") {
      return(summary(BAlm))
    } else if (input$reg == "Exit Velocity") {
      return(summary(EVlm))
    } else if(input$reg == "Barrel Batted Rate") {
      return(summary(BBRlm))
    }
  })
  
  #code for prediction tab
  output$teamPlot <- renderPlot({
    if (input$team == "Top" && input$stats =="Batting Average") {
      ggplot(d[1:10,]) + geom_histogram(aes(x= BA)) +
        theme(text= element_text(size=25))
    } else if (input$team == "Top" && input$stats =="Exit Velocity") {
      ggplot(d[1:10,]) + geom_histogram(aes(x= exit_velocity_avg)) +
        theme(text= element_text(size=25))
    } else if (input$team == "Top" && input$stats =="Barrel Batted") {
      ggplot(d[1:10,]) + geom_histogram(aes(x= barrel_batted_rate)) +
        theme(text= element_text(size=25))
    }#end of top graphic code
    else if (input$team == "Middle" && input$stats == "Batting Average"){
      ggplot(d[18:28,]) + geom_histogram(aes(x= BA)) +
        theme(text = element_text(size =25))
    } else if (input$team == "Middle" && input$stats == "Exit Velocity"){
      ggplot(d[18:28,]) + geom_histogram(aes(x= exit_velocity_avg)) +
        theme(text = element_text(size =25))
    } else if (input$team == "Middle" && input$stats == "Barrel Batted"){
      ggplot(d[18:28,]) + geom_histogram(aes(x= barrel_batted_rate)) +
        theme(text = element_text(size =25))
    }#end of the middle graphic code
    else if (input$team == "Bottom" && input$stats== "Batting Average") {
      ggplot(d[37:47,]) + geom_histogram(aes(x= BA)) +
        theme(text= element_text(size=25))
    } else if (input$team == "Bottom" && input$stats== "Exit Velocity") {
      ggplot(d[37:47,]) + geom_histogram(aes(x= exit_velocity_avg)) +
        theme(text= element_text(size=25))
    } else if (input$team == "Bottom" && input$stats== "Barrel Batted") {
      ggplot(d[37:47,]) + geom_histogram(aes(x= barrel_batted_rate)) +
        theme(text= element_text(size=25))
    } #end of the bottom graphic code
    else if (input$team == "Random" && input$stats == "Batting Average"){
      ggplot(sample_n(d, 10)) + geom_bar(aes(x= BA)) + 
        theme(text = element_text(size = 25))
    } else if (input$team == "Random" && input$stats == "Exit Velocity"){
      ggplot(sample_n(d, 10)) + geom_bar(aes(x= exit_velocity_avg)) + 
        theme(text = element_text(size = 25))
    }  else if (input$team == "Random" && input$stats == "Barrel Batted"){
      ggplot(sample_n(d, 10)) + geom_bar(aes(x= barrel_batted_rate)) + 
        theme(text = element_text(size = 25))
    }
  }) #end of render plot
  
} #server output end bracket
# Create Shiny object
shinyApp(ui = ui, server = server)