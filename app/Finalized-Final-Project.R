library(tidyverse)
library(broom)
library(ggplot2)
library(ggthemes)
library(shinyWidgets)
library(shinythemes)
library(ggstance)

scoreboard = read_csv("../data/scoreboard.csv")


scoreboard %>% 
  select(INSTNM,CONTROL,INC_PCT_LO, INC_PCT_M1,INC_PCT_M2, INC_PCT_H1, INC_PCT_H2, DEP_INC_AVG, DEP_INC_N, DEP_INC_PCT_LO, DEP_INC_PCT_M1, DEP_INC_PCT_M2, DEP_INC_PCT_H1, DEP_INC_PCT_H2, IND_INC_AVG, IND_INC_N, IND_INC_PCT_LO, IND_INC_PCT_M1, IND_INC_PCT_M2, IND_INC_PCT_H1, IND_INC_PCT_H2) %>% 
  mutate(CONTROL = as.character(CONTROL),
         INC_PCT_LO = as.numeric(INC_PCT_LO),
         INC_PCT_M1 = as.numeric(INC_PCT_M1),
         INC_PCT_M2 = as.numeric(INC_PCT_M2),
         INC_PCT_H1 = as.numeric(INC_PCT_H1),
         INC_PCT_H2 = as.numeric(INC_PCT_H2),
         DEP_INC_AVG = as.numeric(DEP_INC_AVG),
         DEP_INC_N = as.numeric(DEP_INC_N),
         DEP_INC_PCT_LO = as.numeric(DEP_INC_PCT_LO),
         DEP_INC_PCT_M1 = as.numeric(DEP_INC_PCT_M1),
         DEP_INC_PCT_M2 = as.numeric(DEP_INC_PCT_M2),
         DEP_INC_PCT_H1 = as.numeric(DEP_INC_PCT_H1),
         DEP_INC_PCT_H2 = as.numeric(DEP_INC_PCT_H2),
         IND_INC_AVG = as.numeric(IND_INC_AVG),
         IND_INC_N = as.numeric(IND_INC_N),
         IND_INC_PCT_LO = as.numeric(IND_INC_PCT_LO),
         IND_INC_PCT_M1 = as.numeric(IND_INC_PCT_M1),
         IND_INC_PCT_M2 = as.numeric(IND_INC_PCT_M2),
         IND_INC_PCT_H1 = as.numeric(IND_INC_PCT_H1),
         IND_INC_PCT_H2 = as.numeric(IND_INC_PCT_H2)) %>% 
  mutate(CONTROL = replace(CONTROL, CONTROL == "1", "1-public"),
         CONTROL = replace(CONTROL, CONTROL == "2", "2-private nonprofit"),
         CONTROL = replace(CONTROL, CONTROL == "3", "3-private for-profit"))->
  scoreboard1


## boxplot of INC_PCT_, group by university type

scoreboard1 %>% 
  select(INSTNM:INC_PCT_H2) %>% 
  filter(!is.na(INC_PCT_LO),
         !is.na(INC_PCT_M1),
         !is.na(INC_PCT_M2),
         !is.na(INC_PCT_H1),
         !is.na(INC_PCT_H2)) %>% 
  group_by(CONTROL) %>% 
  gather(INC_PCT_LO : INC_PCT_H2, key = "quintiles", value = "pct") %>% 
  mutate(quintiles = replace(quintiles, quintiles == "INC_PCT_LO", "1_LOW"),
         quintiles = replace(quintiles, quintiles == "INC_PCT_M1", "2_Median1"),
         quintiles = replace(quintiles, quintiles == "INC_PCT_M2", "3_Median2"),
         quintiles = replace(quintiles, quintiles == "INC_PCT_H1", "4_High1"),
         quintiles = replace(quintiles, quintiles == "INC_PCT_H2", "5_High2")) ->
  scoreboard1_INC

scoreboard1_INC %>% 
  ggplot(aes(x = quintiles, y = pct, fill = CONTROL))+
  geom_boxplot()+
  theme_bw()+
  scale_fill_colorblind()


## boxplot of independent 
scoreboard1 %>% 
  select(INSTNM,CONTROL,IND_INC_AVG, IND_INC_N, IND_INC_PCT_LO, IND_INC_PCT_M1, IND_INC_PCT_M2, IND_INC_PCT_H1, IND_INC_PCT_H2) %>% 
  filter(!is.na(IND_INC_AVG),
         !is.na(IND_INC_N),
         !is.na(IND_INC_PCT_LO),
         !is.na(IND_INC_PCT_M1),
         !is.na(IND_INC_PCT_M2),
         !is.na(IND_INC_PCT_H1),
         !is.na(IND_INC_PCT_H2)) %>% 
  group_by(CONTROL) %>% 
  gather(IND_INC_PCT_LO : IND_INC_PCT_H2, key = "quintiles", value = "pct") %>%
  mutate(quintiles = replace(quintiles, quintiles == "IND_INC_PCT_LO", "1_LOW"),
         quintiles = replace(quintiles, quintiles == "IND_INC_PCT_M1", "2_Median1"),
         quintiles = replace(quintiles, quintiles == "IND_INC_PCT_M2", "3_Median2"),
         quintiles = replace(quintiles, quintiles == "IND_INC_PCT_H1", "4_High1"),
         quintiles = replace(quintiles, quintiles == "IND_INC_PCT_H2", "5_High2")) ->
  scoreboard1_IND

scoreboard1_IND%>% 
  ggplot(aes(x = quintiles, y = pct, fill = CONTROL))+
  geom_boxplot()+
  theme_bw()+
  scale_fill_colorblind()

##boxplot of dependent
scoreboard1 %>% 
  select(INSTNM,CONTROL,DEP_INC_AVG, DEP_INC_N, DEP_INC_PCT_LO, DEP_INC_PCT_M1, DEP_INC_PCT_M2, DEP_INC_PCT_H1, DEP_INC_PCT_H2) %>% 
  filter(!is.na(DEP_INC_AVG),
         !is.na(DEP_INC_N),
         !is.na(DEP_INC_PCT_LO),
         !is.na(DEP_INC_PCT_M1),
         !is.na(DEP_INC_PCT_M2),
         !is.na(DEP_INC_PCT_H1),
         !is.na(DEP_INC_PCT_H2)) %>% 
  group_by(CONTROL) %>% 
  gather(DEP_INC_PCT_LO : DEP_INC_PCT_H2, key = "quintiles", value = "pct") %>%
  mutate(quintiles = replace(quintiles, quintiles == "DEP_INC_PCT_LO", "1_LOW"),
         quintiles = replace(quintiles, quintiles == "DEP_INC_PCT_M1", "2_Median1"),
         quintiles = replace(quintiles, quintiles == "DEP_INC_PCT_M2", "3_Median2"),
         quintiles = replace(quintiles, quintiles == "DEP_INC_PCT_H1", "4_High1"),
         quintiles = replace(quintiles, quintiles == "DEP_INC_PCT_H2", "5_High2"))->
  scoreboard1_DEP

scoreboard1_DEP%>% 
  ggplot(aes(x = quintiles, y = pct, fill = CONTROL))+
  geom_boxplot()+
  theme_bw()+
  scale_fill_colorblind()

## Boxplot of DEP_IND
scoreboard1 %>% 
  select(CONTROL, DEP_INC_AVG, IND_INC_AVG) %>% 
  filter(!is.na(DEP_INC_AVG),
         !is.na(IND_INC_AVG)) %>% 
  gather(DEP_INC_AVG:IND_INC_AVG, key = "DEP_IND", value = "AVG") %>% 
  group_by(CONTROL) %>% 
  ggplot(aes(x = DEP_IND, y = AVG, fill = CONTROL))+
  geom_boxplot()+
  theme_bw()+
  scale_fill_colorblind()

scoreboard1_IND %>% 
  mutate(Type = "Independent") %>% 
  select(INSTNM,CONTROL,quintiles,pct,Type)->
  score_1 

scoreboard1_DEP %>% 
  mutate(Type = "Dependent") %>% 
  select(INSTNM,CONTROL,quintiles,pct,Type)->
  score_2 

whole_income <- rbind(score_1, score_2)


college_type <- c("1-public", "2-private nonprofit","3-private for-profit")
student_type <- c("Independent", "Dependent")

scoreboard %>%
  select(INSTNM, AVGFACSAL, TUITIONFEE_IN, TUITIONFEE_OUT, CONTROL) %>%
  mutate(Ave_Faculty_Salary  = as.numeric(AVGFACSAL),
         TUITIONFEE_IN = as.numeric(TUITIONFEE_IN),
         TUITIONFEE_OUT = as.numeric(TUITIONFEE_OUT)) %>%
  mutate(CONTROL = replace(CONTROL, CONTROL == "1", "1-public"),
         CONTROL = replace(CONTROL, CONTROL == "2", "2-private nonprofit"),
         CONTROL = replace(CONTROL, CONTROL == "3", "3-private for-profit")) %>%
  select(-AVGFACSAL) %>%
  na.omit() -> salary_tuition

scoreboard %>%
  filter(TUITIONFEE_IN > 60000) -> 
  case


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



ui <- fluidPage(
  titlePanel("College Score Card"),
  tabsetPanel(
    tabPanel("Family Income",
             sidebarLayout(
               sidebarPanel(numericInput("income", "Family Income", value = 0),
                            checkboxGroupInput("college", "Which type of univeristy do you prefer to check?", choices = college_type),
                            checkboxGroupInput("student", "Which type of student do you prefer to check?",  choices = student_type)),
               mainPanel(width = 8,
                         plotOutput("boxplot1"),
                         tableOutput("table1"))                          
             )),
    tabPanel("Faculty Salary vs Tuition", 
             sidebarLayout(
               sidebarPanel(
                 varSelectInput("x", "x", data = salary_tuition, 
                                selected = "Ave_Faculty_Salary"),
                 checkboxInput("logx", "Log"),
                 varSelectInput("y", "y", data = salary_tuition, 
                                selected = "TUITIONFEE_IN"),
                 checkboxInput("logy", "Log"),
                 checkboxInput("ols", "OLS?"),
                 checkboxInput("boxplot", "Boxplot"),
                 checkboxInput("filter", "extreme case"),
                 tableOutput("test_results"),
                 tableOutput("name_out")
                 
               ),
               mainPanel(
                 plotOutput("plot"),
                 plotOutput("boxplot")
               )
             )), 
    
    
    
    
    tabPanel("Retention VS Earnings", 
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
  )
  
  
  
)

server <- function(input, output, session) {
  output$boxplot1 <- renderPlot({
    
    if(input$income > 0 & input$income <= 30000){
      whole_income %>% 
        filter(quintiles == "1_LOW") %>% 
        filter(Type %in% input$student,
               CONTROL %in% input$college) %>% 
        ggplot(aes(x = CONTROL, y = pct, fill = CONTROL))+
        geom_boxplot()+
        theme_bw()+
        ggtitle("Low Level")->
        pl
    }else if(input$income > 30000 & input$income <= 48000 ){
      whole_income %>% 
        filter(quintiles == "2_Median1") %>% 
        filter(Type %in% input$student,
               CONTROL %in% input$college) %>% 
        ggplot(aes(x = CONTROL, y = pct, fill = CONTROL))+
        geom_boxplot()+
        theme_bw()+
        ggtitle("Median1 Level")->
        pl
    }else if(input$income > 48000 & input$income <= 75000 ){
      whole_income %>% 
        filter(quintiles == "3_Median2") %>% 
        filter(Type %in% input$student,
               CONTROL %in% input$college) %>% 
        ggplot(aes(x = CONTROL, y = pct, fill = CONTROL))+
        geom_boxplot()+
        theme_bw()+
        ggtitle("Median2 Level")->
        pl
    }else if(input$income > 75000 & input$income <= 110000 ){
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
  
  output$table1 <- renderTable({
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
  
  output$test_results <- renderTable({
    if (!input$logx & !input$logy) {
      lmout <- lm(salary_tuition[[input$y]] ~ salary_tuition[[input$x]])
    } else if (!input$logx & input$logy) {
      lmout <- lm(salary_tuition[[input$y]] ~ log2(salary_tuition[[input$x]]))
    } else if (input$logx & !input$logy) {
      lmout <- lm(log2(salary_tuition[[input$y]]) ~ salary_tuition[[input$x]])
    } else {
      lmout <- lm(log2(salary_tuition[[input$y]]) ~ log2(salary_tuition[[input$x]]))
    }
    
    tidy(lmout, conf.int = TRUE) %>%
      select(Term = term, Estimate = estimate, Lower = conf.low, Upper = conf.high) ->
      tout
    
    tout$Term[[1]] <- "Intercept"
    tout$Term[[2]] <- "Slope"
    tout
  })
  
  output$plot <- renderPlot({
    salary_tuition %>%
      ggplot(aes(x = !!input$x, y = !!input$y, color = CONTROL)) +
      theme_bw() +
      scale_color_colorblind()->
      pl
    
    
    if (is.numeric(salary_tuition[[input$x]]) & is.numeric(salary_tuition[[input$y]])) {
      pl <- pl + geom_point()
    } else if (!is.numeric(salary_tuition[[input$x]]) & is.numeric(salary_tuition[[input$y]])) {
      pl <- pl + geom_boxplot()
    } else if (is.numeric(salary_tuition[[input$x]]) & !is.numeric(salary_tuition[[input$y]])) {
      pl <- pl + geom_boxplot()
    } else {
      pl <- pl + geom_jitter()
    }
    
    
    if (input$logx & is.numeric(salary_tuition[[input$x]])) {
      pl <- pl + scale_x_log10()
    }
    
    if (input$logy & is.numeric(salary_tuition[[input$y]])) {
      pl <- pl + scale_y_log10()
    }
    
    if (input$ols & is.numeric(salary_tuition[[input$x]]) &
        is.numeric(salary_tuition[[input$y]])) {
      pl <- pl + geom_smooth(se = FALSE, method = "lm")
    }
    
    pl
    
    
  })
  
  
  
  output$boxplot <- renderPlot({
    
    salary_tuition %>%
      ggplot(aes(x = !!input$x, y = !!input$y, color = CONTROL)) +
      theme_bw() +
      scale_color_colorblind() -> 
      pl_1
    
    
    if(input$boxplot & is.numeric(salary_tuition[[input$x]]) &
       is.numeric(salary_tuition[[input$y]])) {
      pl_1 <- pl_1 + geom_boxplot()
    }
    
    pl_1
    
  })
  
  output$name_out <- renderPrint({
    if(input$filter){
      print(c("School: Aviator College of Aeronautical Science and Technology",
              "Tuition: 74514",
              "Average Facultys' Salary: 2136 "))
    }
    
  })
  
  
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




