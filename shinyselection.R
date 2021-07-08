#Shiny app for Selection Purpose
library(readr)
library(shiny)
library(ggplot2)
library(dplyr)

#Reading PAS data from github
##Need to update so user can upload data
pas<-read_csv("https://raw.githubusercontent.com/profburns/shinyselection/main/pas.csv")


#Setting up UI
ui <- fluidPage(
  titlePanel("Selection Visulation Tool"),
#Sidebar allowing selection of predictor, criterion, grouping variable, and cutscore. 
  sidebarLayout(
    sidebarPanel(
      varSelectInput('predictor', 'Select Predictor', pas),
      varSelectInput('criterion', 'Select Criterion', pas),
##Not sure how to change this selected variable to a factor
      varSelectInput('group', 'Select grouping variable for adverse impact analysis', pas),
#Code for Slider based on selected variable
      uiOutput('cutoff')
      ),
#Mainpanel broken into three tabs 
   mainPanel(
      tabsetPanel(type = "tabs",
#Default panel Displayed - Graphing Scatterplot
                  tabPanel("Selected Data",
                           plotOutput('trend')
                           ),
#Panel with correlation, mean performance, and AI data
##Might be nice to present summary table of "good" and "bad" hires by cutoff. Or this could be a new tab. 
##AI Data still needs to be entered
##Would be nice to show standardized performance by group, t-test and cohen's d, and Adverse Impact Ratio
                  tabPanel("Summary Statistics",
                           br(),
#Displaying correlation between selected variables
                           textOutput('corr'),
                           br(),
#Displaying mean standardized perfoormance for cutoff
                           "The selected cutoff results in the following standardized criterion scores.",
                           tableOutput('scoreSelected')
##Start AI calculations here
                           ),
##Would Like to Utilize GPareto to graph solution pareto optimizaton in this tab
                  tabPanel("Reserved" )
                  )

    )
))


#Server Side Functions

server <- function(input, output, session) {

#Scatter plot with verticle line matching selected cut off
  output$trend <- renderPlot({
    ggplot(pas, aes_string(x = input$predictor, 
                           y = input$criterion,
##Errors occur when I attempt to change this to a factor
                           color =input$group)) +
      geom_jitter() +
      geom_smooth(method=lm, se=FALSE) + 
      geom_vline(xintercept = input$cutscore)
  })

#Code for Updating Slider based on selected variable
  output$cutoff <- renderUI({
    sliderInput("cutscore", "Select cutscore of predictor",
                min=pas%>%select(!!input$predictor)%>%min(na.rm=T),
                max=pas%>%select(!!input$predictor)%>%max(na.rm=T),
                value=0,
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
                              labels=c("Below Cutoff","At or Above Cutoff")))%>%
           filter(!is.na(Score)) %>%
           group_by(Score)%>%
           summarise(n = n(),
                     '%' = 100*(n()/nrow(pas)),
             Criterion = mean(zperf,na.rm=T))
  })
 
}


# Run the application 
shinyApp(ui = ui, server = server)

