#Shiny app for Selection Purpose
library(readr)
library(shiny)
library(ggplot2)
library(dplyr)

pas<-read_csv("https://raw.githubusercontent.com/profburns/shinyselection/main/pas.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Selection Slider Tool"),
  sidebarLayout(
    sidebarPanel(
      varSelectInput('predictor', 'Select Predictor', pas),
      varSelectInput('criterion', 'Select Criterion', pas),
      varSelectInput('group', 'Select grouping variable for adverse impact analysis', pas)    ),
    mainPanel(
      plotOutput('trend'),
      uiOutput('cutoff')
    )
))


#Server Side Functions

server <- function(input, output, session) {
  
  output$trend <- renderPlot({
    ggplot(pas, aes_string(x = input$predictor, 
                           y = input$criterion,
                           color =input$group)) +
      geom_point() +
      geom_smooth(method=lm, se=FALSE) + 
      geom_vline(xintercept = input$cutscore)
  })

  output$cutoff <- renderUI({
    sliderInput("cutscore", "Select cutscore of predictor",
                min=pas%>%select(!!input$predictor)%>%min(na.rm=T),
                max=pas%>%select(!!input$predictor)%>%max(na.rm=T),
                value=0,
                width = "90%")
  })


}


# Run the application 
shinyApp(ui = ui, server = server)

