library(shiny)
library(ggplot2)
library(tidyverse)
library(broom)
library(ggstance)
library(shinythemes)



college_type <- c("Public", "private nonprofit","private for-profit")
student_type <- c("Independent", "Dependent")



ui <- fluidPage(
  titlePanel("College Score Card"),
  tabsetPanel(
    tabPanel("Basic Information",
             sidebarLayout(
               sidebarPanel(width = 4,
                            ),
               mainPanel(width = 8)
             )),
    tabPanel("Jack's"),
    tabPanel("Family Income",
             sidebarLayout(
               sidebarPanel(width = 4,
                            numericInput("income", "Family Income", value = 0),
                            radioButtons("college_type", "Which type of college do you prefer to check?",
                                         choices = college_type),
                            radioButtons("Student_type", "Which type of student do you prefer to check?",
                                         choices = student_type)),
               mainPanel(width = 8,
                         plotOutput("boxplot"),
                         tableOutput("test"))
             )),
    tabPanel("Sam's")
  )
  
  
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)