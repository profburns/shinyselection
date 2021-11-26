#Shiny app for Selection Purpose
library(readr)
library(shiny)
library(ggplot2)
library(dplyr)
library(psych)

#Reading PAS data from github
##Need to update so user can upload data
pas<-read_csv("https://raw.githubusercontent.com/profburns/shinyselection/main/pas.csv")
pas$sex<-factor(pas$sex, levels = c("1","2"), labels = c("Male","Female"))
pas$race<-factor(pas$race, levels = c("1","2"), labels = c("White","Not White"))
pas <- pas[!is.na(pas$sex), ]
pas <- pas[!is.na(pas$race), ]

#Setting up UI
ui <- fluidPage(
  titlePanel("Selection Visulation Tool"),
#Sidebar allowing selection of predictor, criterion, grouping variable, and cutscore. 
  sidebarLayout(
    sidebarPanel(
      varSelectInput('predictor', 'Select Predictor', pas, selected = "g"),
      varSelectInput('criterion', 'Select Criterion', pas, selected = "perf_fnl"),
      varSelectInput('group', 'Select grouping variable for adverse impact analysis', pas, selected = "sex"),
#Code for Slider based on selected variable
      uiOutput('cutoff'),
      uiOutput('crit_cut')
      ),
#Mainpanel broken into four tabs 
   mainPanel(
      tabsetPanel(type = "tabs",
#Panel with correlation, mean performance, and AI data
##Might be nice to present summary table of "good" and "bad" hires by cutoff. Or this could be a new tab. 
                  tabPanel("Summary Statistics",
                           br(),
#Displaying correlation between selected variables
                           textOutput('corr'),
                           br(),
#Displaying scatterplot between selected variaables
                          plotOutput('trend'),
#Displaying mean standardized perfoormance for cutoff
                           "The selected predictor cutoff results in the following standardized criterion scores.",
                           tableOutput('scoreSelected'),
                           br(),
#Displaying Cohen's d and adverse impact
                           textOutput('cohend'),
                           br(),
#AI calculations 
                           textOutput('aiRatio')
                           ),
##Would Like to Utilize GPareto to graph solution pareto optimizaton in this tab
                  tabPanel("Reserved" ),
                  tabPanel("Assumptions",
                           br(),
                           "Currently this illustration uses a small incumbent data set. 
                            Future updates will allow users to upload their own data.",
                           br(),
                           br(),
                           "Currently you can explore the different predictors one at a time. 
                             Future updates will allow users to select multiple predictors.",
                           br(),
                           br(),
                          "The only criterion is perf_fnl and 
                           the only grouping variables are sex and race (both dichotomous). 
                           Selection of other variables may lead to unexpected results. 
                           Error testing still in progress.")

                  )

    )
))


#Server Side Functions

server <- function(input, output, session) {

#Scatter plot with verticle line matching selected cut off
  output$trend <- renderPlot({
    ggplot(pas, aes_string(x = input$predictor, 
                           y = input$criterion,
                           color =input$group)) +
      geom_jitter() +
      geom_smooth(method=lm, se=FALSE) + 
      geom_vline(xintercept = input$cutscore) +
      geom_hline(yintercept = input$crit_cut)
  })

#Code for Updating Slider based on selected variable
  output$cutoff <- renderUI({
    predictorData<-pull(pas%>%filter(!is.na(!!input$predictor))%>%select(!!input$predictor))
    medPD<-median(predictorData, na.rm=T)
    sliderInput("cutscore", "Select cutscore of predictor",
                min=pas%>%select(!!input$predictor)%>%min(na.rm=T),
                max=pas%>%select(!!input$predictor)%>%max(na.rm=T),
                value=medPD,
                step=.25,
                width = "90%")
  })

  
#Code for Updating High/Low Criterion on selected variable
  output$crit_cut <- renderUI({
    criterionData<-pull(pas%>%filter(!is.na(!!input$criterion))%>%select(!!input$criterion))
    medCD<-median(criterionData, na.rm=T)
    sliderInput("crit_cut", "Select high/low criterion level",
                min=pas%>%select(!!input$criterion)%>%min(na.rm=T),
                max=pas%>%select(!!input$criterion)%>%max(na.rm=T),
                value=medCD,
                step=.25,
                width = "90%")
  })

  
#Code for Computing Correlation
  output$corr <- renderText({
    paste0("The Pearson correlation between the predictor and the criterion is ",
          round(cor(pas%>%select(!!input$predictor,!!input$criterion),use="complete.obs")[2],2),
    ".")
  })

#Code for Displaying Standardized Performance by Cutoff
  output$scoreSelected <- renderTable({
      pas%>%
           mutate(zperf=scale(!!input$criterion),
                  Score = factor(ifelse(!!input$predictor>=input$cutscore,1,0),
                              levels=c(0,1),
                              labels=c("Below Cutoff","At or Above Cutoff")),
                  HighPerf = ifelse(!!input$criterion>=input$crit_cut,1,0))%>%
           filter(!is.na(Score)) %>%
           group_by(Score)%>%
           summarise(n = n(),
                     '%' = 100*(n()/nrow(pas)),
             Criterion = mean(zperf,na.rm=T),
             '% High Performers' = mean(HighPerf,na.rm=T)*100)
  })
  
#Reporting Cohen's d and other group differences
  output$cohend <- renderText({
    predictorData<-pull(pas%>%filter(!is.na(!!input$predictor))%>%filter(!is.na(!!input$group))%>%select(!!input$predictor))
    groupingData<-pull(pas%>%filter(!is.na(!!input$predictor))%>%filter(!is.na(!!input$group))%>%select(!!input$group))
    d<-(cohen.d(predictorData,groupingData))[[1]][[2]]
    t<-t.test(predictorData~groupingData, var.equal=T)
    paste0("The standardized mean difference between the selected groups is ",round(d,2), " standard deviations. ",
           "The test statistic for this difference is t (",t[[2]],") = ",
           round(t[[1]],2), ", p = ",round(t[[3]],4)," (two-tailed).")
  })

#Calculating Adverse Impact Ratio
  output$aiRatio <- renderText({
    air<-pas%>%
      mutate(Score = factor(ifelse(!!input$predictor>=input$cutscore,1,0),
                            levels=c(0,1),
                            labels=c("Below Cutoff","At or Above Cutoff")))%>%
      filter(!is.na(Score)) %>%
      group_by(Score)%>%
      count(!!input$group,Score)
    r1<-air[3,3]/(air[3,3]+air[1,3])
    r2<-air[4,3]/(air[4,3]+air[2,3])
    airatio<-min(r1,r2)/max(r1,r2)
    paste0("At the selected predictor cutoff, the selection ratio for the two groups are ",
           round(r1,4)," and ",round(r2,4),". The adverse impact ratio is ",
           round(airatio,4),".")
  })
  
##Pareto Optimization with ParetoR.R only works with multiple predictor
  
}


# Run the application 
shinyApp(ui = ui, server = server)

