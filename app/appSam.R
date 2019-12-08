library(tidyverse)
library(broom)
library(ggplot2)
library(ggthemes)

scoreboard = read_csv("../data/scoreboard.csv")

scoreboard %>%
  dplyr::select(c("INSTNM", "CITY", "STABBR", "ZIP", "INSTURL", "RET_FT4", "RET_FTL4", "RET_PT4", "RET_PTL4", "RET_FT4_POOLED", "RET_FTL4_POOLED", "RET_PT4_POOLED", "RET_PTL4_POOLED", "POOLYRSRET_FT", "POOLYRSRET_PT", "GT_28K_P6", "GT_28K_P8", "GT_28K_P10")) %>%
  rename("Institution" = 1, "City" = 2, "State" = 3, "Zip" = 4, "url" = 5, "Fulltime-Four-Retention" = 6, "Full-Less-Four-Retention" = 7, "Part-Four-Retention" = 8, "Part-Less-Four-Retention" = 9, "Full-Four-Pooled-Variance" = 10, "Full-Less-Four-Pooled-Variance" = 11, "Part-Four-Pooled-Variance" = 12, "Part-Less-Four-Pooled-Variance" = 13, 
         "Full-Cohort" = 14, "Part-Cohort" = 15, "Earnings after 6 years" = 16, "Earnings after 8 years" = 17, "Earnings after 10 years" = 18) ->
  retention_scoreboard


retention_scoreboard %>%
  select("Institution", "Fulltime-Four-Retention", "Part-Four-Retention", "Earnings after 6 years", "Earnings after 8 years", "Earnings after 10 years", "City", "State", "Zip", "url") %>%
  filter(`Fulltime-Four-Retention` != "NULL") %>%
  filter(`Part-Four-Retention` != "NULL") ->
  four_year_institution_retention

four_year_institution_retention %>%
  rename("FullTime" = 2, "PartTime" = 3) %>%
  gather("FullTime", "PartTime", key = "Type", value = "Retention") %>%
  filter(`Earnings after 6 years` != "PrivacySuppressed") %>%
  mutate(Retention = as.numeric(Retention)) %>%
  mutate(`Earnings after 6 years` = as.numeric(`Earnings after 6 years`)) %>%
  mutate(`Earnings after 8 years` = as.numeric(`Earnings after 8 years`)) %>%
  mutate(`Earnings after 10 years` = as.numeric(`Earnings after 10 years`)) ->
  
  four_year_institution

four_year_institution %>%
  select("Earnings after 6 years", "Earnings after 8 years", "Earnings after 10 years", "State", "Type", "Retention") ->
  four_year_data



library(shiny)


ui <- fluidPage(
  titlePanel("Earnings and Retentions in Four-Year Institutions"), 
  sidebarLayout(
    sidebarPanel(
      varSelectInput("var1", "Variable 1", data = four_year_data),
      checkboxInput("log2", "Log"), 
      varSelectInput("var2", "Variable 2", data = four_year_data),
      checkboxInput("log3", "Log"), 
      checkboxInput("ols", "OLS!"), 
      varSelectInput("var3", "Variable 3", data = four_year_data), 
      sliderInput("bins", "Bins", min = 1, max = 100, value = 20),
      checkboxInput("log4", "Log"), 
      textInput("text1", "Institution"), 
      checkboxInput("parttime", "Part Time"), 
      checkboxInput("fulltime", "Full Time")
    ), 
    
    mainPanel(
      fluidRow(
        splitLayout(cellWidths = c("300px", "300px"), plotOutput("scatter"), plotOutput("hist"))), 
      column(12, dataTableOutput("ttest", width = "300px", height = "5px")), 
      column(11, dataTableOutput("table", width = "300px", height = "5px")), 
    )
  ))


server <- function(input, output, session) {
  
  df = reactive({
    four_year_institution %>%
      filter(Institution == input$text1) ->
      k
    return(k)
  })
  
  
  
  
  output$table = renderDataTable({
    if(input$parttime == TRUE & input$fulltime == FALSE) {
      
      return(df() %>%
               filter(Type == "PartTime"))
    }
    
    if(input$fulltime == TRUE & input$parttime == FALSE) {
      return(df() %>%
               filter(Type == "FullTime"))
    }
    
    
    
    else {
      return(df())
    }
  })
  
  
  
  output$scatter = renderPlot({
    if((class(four_year_data[[input$var1]]) == "numeric") & (class(four_year_data[[input$var2]]) == "numeric")){
      
      p = ggplot(four_year_data, aes(x = !!input$var1, y = !!input$var2)) + 
        geom_point() + 
        theme_bw()
      
      if(input$log2 == TRUE) {
        p = p + scale_x_log10()
      }
      
      if(input$log3 == TRUE) {
        p = p + scale_y_log10()
      }
      if(input$ols == TRUE) {
        p = p + geom_smooth(method = "lm", se = F)
      }
      
      return(p)
    }
    
    if((class(four_year_data[[input$var1]]) == "character" & class(four_year_data[[input$var2]]) == "numeric")) {
      p = ggplot(four_year_data, aes(x = !!input$var1, y = !!input$var2)) +     
        geom_boxplot() + 
        theme_bw()
      if(input$log3 == TRUE) {
        p = p + scale_y_log10()
      }
      
      return(p)
    }
    
    
    if((class(four_year_data[[input$var1]]) == "numeric" & class(four_year_data[[input$var2]]) == "character")) {
      p = ggplot(four_year_data, aes(x = !!input$var2, y = !!input$var1)) +     
        geom_boxplot() + 
        theme_bw() + 
        coord_flip()
      
      if(input$log2 == TRUE) {
        p = p + scale_y_log10()
      }
      
      return(p)
    }
    
    if((class(four_year_data[[input$var1]]) == "character" & class(four_year_data[[input$var2]]) == "character")) {
      p = ggplot(four_year_data, aes(x = !!input$var2, y = !!input$var1))+ 
        geom_jitter(stat = "identity") + 
        theme_bw()
      
      return(p)
    }
    
    
    
  })
  
  output$ttest = renderDataTable({
    if(class(four_year_data[[input$var1]]) == "numeric" & class(four_year_data[[input$var2]]) == "numeric") {
      df1 = reactive({
        t.test(four_year_data[[input$var1]], four_year_data[[input$var2]]) %>%
          tidy() %>%
          select(p.value, conf.low, conf.high) %>%
          rename("P.value" = 1, "Lower" = 2, "Upper" = 3) ->
          k
        return(k)
      })
      
      return(df1())
      
    }
    
    if(class(four_year_data[[input$var1]]) == "character" & class(four_year_data[[input$var2]]) == "numeric") {
      df2 = reactive({
        aov(four_year_data[[input$var2]] ~ four_year_data[[input$var1]]) %>%
          tidy() %>%
          
          filter(term == "four_year_data[[input$var1]]") %>%
          select(df, statistic, p.value) %>%
          rename("Degrees of Freedom" = 1, "Test Statistic" = 2, "P.value" = 3) ->
          m
        
        return(m)
      })
      return(df2())
    }
    
    
    
    if(class(four_year_data[[input$var2]]) == "character" & class(four_year_data[[input$var1]]) == "numeric") {
      df3 = reactive({
        aov(four_year_data[[input$var1]] ~ four_year_data[[input$var2]]) %>%
          tidy() %>%
          
          filter(term == "four_year_data[[input$var2]]") %>%
          select(df, statistic, p.value) %>%
          rename("Degrees of Freedom" = 1, "Test Statistic" = 2, "P.value" = 3) ->
          n
        
        return(n)
      })
      return(df3())
    }
    
    
    
    
    
    
  })
  
  output$hist = renderPlot({
    
    if (class(four_year_data[[input$var3]]) == "numeric"){
      if(input$log4 == TRUE) {
        
        ggplot(four_year_data, aes(log(x = four_year_data[[input$var3]]))) + 
          geom_histogram(bins = input$bins, color = "black", fill = "white") + 
          theme_bw() + 
          labs(x = input$var3, y = "count")}
      
      else {
        ggplot(four_year_data, aes(x = four_year_data[[input$var3]])) + 
          geom_histogram(bins = input$bins, color = "black", fill = "white") + 
          theme_bw() + 
          labs(x = input$var3, y = "count")
      }
    }
    
    else {
      if(input$log4 == TRUE) {
        
        
        ggplot(data.frame(table(four_year_data[[input$var3]])), aes(x = Var1, y =          log(Freq))) + 
          geom_bar(stat = "identity", color = "black", fill = "white") + 
          theme_bw() + 
          labs(x = input$var3, y = "frequency")
        
      }
      
      else {
        ggplot(data.frame(table(four_year_data[[input$var3]])), aes(x = Var1, y =           Freq)) + 
          geom_bar(stat = "identity", color = "black", fill = "white") + 
          theme_bw() + 
          labs(x = input$var1, y = "frequency")
      }
      
      
    }
  })
}
shinyApp(ui, server)