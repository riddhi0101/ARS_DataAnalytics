#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#    Other references: https://www.simplesabermetrics.com/post/developing-an-r-shiny-application-with-statcast-data

## app.R ##

library(readr) 
library(shinydashboard)
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)

## app.R ##

ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Aggie Reuse Store Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("user")),
      menuItem("Items", tabName = "items", icon = icon("store")),
      menuItem("Social Media", tabName = "social", icon = icon("thumbs-up"))
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "overview",
              h2("Overview"),
              fluidPage(
                titlePanel("Total Revenue Per Pop-up Sale"),
                
                sidebarPanel(
                  dateRangeInput(inputId = 'DateRangeInput',
                                 label = 'Date',
                                 start = min(clean_entire$New_Date), 
                                 end = max(clean_entire$New_Date))), #allows date range specification to see revenue for pop up sales within this range
                
                mainPanel(
                  plotOutput("barchart")  #creating a spot for the pop up sales revenue barplot
                )
              )),
      
      # Second tab content
      tabItem(tabName = "items",
              
              h2("Items Sold"),
              
              fluidPage(
                dateRangeInput('dateRange',
                               label = 'Date',
                               start = Sys.Date() - 2, end = Sys.Date() + 2), 
                box(plotOutput("top10", width = 800)),
                mainPanel(
                  plotOutput("items_per_week")  
                ),
                
                DT::dataTableOutput("table"),
                
                h2("Items Sold Per Day of the Week"),
                
                tabPanel("Data Table", fluid = TRUE, 
                         fluidRow(
                           column(4, selectInput(
                             inputId = "DayInput", 
                             label = "Day", 
                             choices = sort(unique(clean_entire$Day)))),
                           column(4, selectInput(
                             inputId = "ItemInput", 
                             label = "Item", 
                             choices = sort(unique(clean_entire$Item)))),
                           column(4, dateRangeInput(inputId = 'DateRangeInput2',
                                                    label = 'Date',
                                                    start = min(clean_entire$New_Date), 
                                                    end = max(clean_entire$New_Date)))
                         ),
                  DT::dataTableOutput('table2')
                ),
              )
              
      ),
      # Third tab content
      tabItem(tabName = "social",
              h2("Social Media")
      )
    )
    
    
  )
)

library(dplyr)

server <- function(input, output) {
  output$table <- DT::renderDataTable(DT::datatable({
    data <- clean_entire
    data
  }))
  
  dataFilter2 <- reactive({
    filtered <- clean_entire %>%
      filter(Day == input$DayInput, Item == input$ItemInput, between(New_Date, input$DateRangeInput2[1],
                                                                     input$DateRangeInput2[2])) %>%
      group_by(New_Date)
    filtered[,c(1:3)]
    
  })
  
  output$table2 <- DT::renderDataTable(DT::datatable({
    data <- dataFilter2()
    data
  }))
  
  #Date range 
  output$barchart <- renderPlot({

    dataFilter <- reactive({
      table_out <- clean_entire %>% 
        filter(between(New_Date, input$DateRangeInput[1],
                       input$DateRangeInput[2])) %>%
        group_by(New_Date) %>%
        summarize(Total = sum(Price))
      table_out$New_Date <- format(c(table_out$New_Date), format="%m/%d/%y")
      table_out
    })
    
    ggplot(dataFilter(), 
           aes(x = as.factor(New_Date), 
               y = Total)) + 
      geom_bar(stat='identity', fill = "#1d6fa6") + #Other possible color: #3498DB 
      labs(x = "Pop-up Sale Date", 
           y = "Total Revenue", 
           title = "Total Revenue of Pop-up Sales by Date") + 
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
  })
    
    output$top10 <- renderPlot({
        data <- clean_entire %>%
            group_by(Item) %>%
            count(Item) %>%
            arrange(desc(n))
        
        plot <- ggplot2::ggplot(data[1:10, ]) +
                ggplot2::geom_bar(ggplot2::aes(x=Item, y=n, fill=Item),stat="identity")
        
        plot
    })

}
shinyApp(ui, server)