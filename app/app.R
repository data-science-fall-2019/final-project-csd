library(shiny)
library(ggplot2)
library(tidyverse)
library(broom)
library(ggstance)
library(shinythemes)



college_type <- c("1-public", "2-private nonprofit","3-private for-profit")
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
                            checkboxGroupInput("college", "Which type of univeristy do you prefer to check?", 
                                               choices = college_type),
                            checkboxGroupInput("student", "Which type of student do you prefer to check?",
                                           choices = student_type)),
               mainPanel(width = 8,
                         plotOutput("boxplot"),
                         tableOutput("table"))                          
             )),
    tabPanel("Sam's")
  )
  
  
  
)

server <- function(input, output, session) {
  output$boxplot <- renderPlot({
    
  if(input$income > 0 & input$income < 30000){
    whole_income %>% 
      filter(quintiles == "1_LOW") %>% 
      filter(Type %in% input$student,
             CONTROL %in% input$college) %>% 
      ggplot(aes(x = CONTROL, y = pct, fill = CONTROL))+
      geom_boxplot()+
      theme_bw()+
      ggtitle("Low Level")->
      pl
  }else if(input$income > 30000 & input$income < 48000 ){
    whole_income %>% 
      filter(quintiles == "2_Median1") %>% 
      filter(Type %in% input$student,
             CONTROL %in% input$college) %>% 
      ggplot(aes(x = CONTROL, y = pct, fill = CONTROL))+
      geom_boxplot()+
      theme_bw()+
      ggtitle("Median1 Level")->
      pl
  }else if(input$income > 48000 & input$income < 75000 ){
    whole_income %>% 
      filter(quintiles == "3_Median2") %>% 
      filter(Type %in% input$student,
             CONTROL %in% input$college) %>% 
      ggplot(aes(x = CONTROL, y = pct, fill = CONTROL))+
      geom_boxplot()+
      theme_bw()+
      ggtitle("Median2 Level")->
      pl
  }else if(input$income > 75000 & input$income < 110000 ){
    whole_income %>% 
      filter(quintiles == "4_High1") %>% 
      filter(Type %in% input$student,
             CONTROL %in% input$college) %>% 
      ggplot(aes(x = CONTROL, y = pct, fill = CONTROL))+
      geom_boxplot()+
      theme_bw()+
      ggtitle("High1 Level")->
      pl
  }else{
    whole_income %>% 
      filter(quintiles == "5_High2") %>% 
      filter(Type %in% input$student,
             CONTROL %in% input$college) %>% 
      ggplot(aes(x = CONTROL, y = pct, fill = CONTROL))+
      geom_boxplot()+
      theme_bw()+
      ggtitle("High2 Level")->
      pl
  }
  pl
  
  })
  
  output$table <- renderTable({
    if(input$income > 0 &input$income < 30000){
      whole_income %>% 
        ungroup() %>% 
        filter(quintiles == "1_LOW") %>% 
        filter(Type %in% input$student,
               CONTROL %in% input$college) %>% 
        arrange(-desc(pct)) %>% 
        select(INSTNM) %>% 
        head(5)->
        dt
    }else if(input$income > 30000 & input$income < 48000 ){
      whole_income %>% 
        ungroup() %>% 
        filter(quintiles == "2_Median1") %>% 
        filter(Type %in% input$student,
               CONTROL %in% input$college) %>% 
        arrange(-desc(pct)) %>% 
        select(INSTNM) %>% 
        head(5)->
        dt
    }else if(input$income > 48000 & input$income < 75000 ){
      whole_income %>% 
        ungroup() %>% 
        filter(quintiles == "3_Median2") %>% 
        filter(Type %in% input$student,
               CONTROL %in% input$college) %>% 
        arrange(-desc(pct)) %>% 
        select(INSTNM) %>% 
        head(5)->
        dt
    }else if(input$income > 75000 & input$income < 110000 ){
      whole_income %>% 
        ungroup() %>% 
        filter(quintiles == "4_High1") %>% 
        filter(Type %in% input$student,
               CONTROL %in% input$college) %>% 
        arrange(-desc(pct)) %>% 
        select(INSTNM) %>% 
        head(5)->
        dt
    }else{
      whole_income %>% 
        ungroup() %>% 
        filter(quintiles == "5_High2") %>% 
        filter(Type %in% input$student,
               CONTROL %in% input$college) %>% 
        arrange(-desc(pct)) %>% 
        select(INSTNM) %>% 
        head(5)->
        dt
    }
    dt
  })

  
}

shinyApp(ui, server)