#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Chase Transaction History"),
   
   fluidRow(
     column(8,
            uiOutput('file_selector')
     )
   ),
   
   fluidRow(
     column(8,
            plotOutput('fullplot', width='800px'))
   ),
   
   
   fluidRow(
     column(8,
            uiOutput('category_checkboxes')
     )
   ),
   fluidRow(
     column(8,
            plotOutput('catplot', width='800px'))
   
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
  output$file_selector <- renderUI({
    fileInput('file', 'Choose the Data')
  })
  
  dat <- reactive({
    infile <- input$file
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    readRDS(infile$datapath)
  })
  output$category_checkboxes <- renderUI({
    cats <- unique(as.character(dat()$category))
    checkboxGroupInput("box_input", 
                       "Click the Boxes to Choose Categories to Plot", 
                       cats,
                       inline=T)
  })
  
  output$catplot <- renderPlot({
    if (!is.null(input$file) & length(input$box_input) > 0) {
      dat() %>% 
        subset(category %in% input$box_input) %>%
        group_by(week, category) %>% 
        summarise(cost = sum(-Amount, na.rm=T)) %>% 
        ggplot(aes(x=week, y=cost, group=category, color=category)) + 
        geom_line() + 
        labs(title='Weekly Spending By Category', x='Week', y='Dollars') + 
        theme(legend.position = "bottom", axis.text = element_text(size=16), 
              axis.title=element_text(size=18), title=element_text(size=24),
              legend.title = element_blank(), legend.text=element_text(size=16))
    }
  })
  output$fullplot <- renderPlot({
    if (!is.null(input$file)){
      dat() %>% 
        group_by(week) %>% 
        summarise(cost = sum(-Amount, na.rm=T)) %>% 
        ggplot(aes(x=week, y=cost)) + 
        geom_line() + 
        labs(title='Weekly Spending - All Categories', x='Week', y='Dollars') + 
        theme(legend.position = "bottom", axis.text = element_text(size=16), 
              axis.title=element_text(size=18), title=element_text(size=24),
              legend.title = element_blank(), legend.text=element_text(size=16))
    }
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

