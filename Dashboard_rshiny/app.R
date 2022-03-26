#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## app.R ##
library(shinydashboard)

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
                    h2("Overview")
            ),
            
            # Second tab content
            tabItem(tabName = "social",
                    h2("Social Media")
            ),
            
            #Third tab content
            tabItem(tabName = "items",
                    h2("Items Sold"),
                    fluidRow(
                        box(plotOutput("plot1", height = 250)),
                        
                        box(
                            title = "Controls",
                            sliderInput("slider", "Number of observations:", 1, 100, 50)
                        )
                    ),
                    fluidRow(box(plotOutput("top10"), width = 300)),
                    DT::dataTableOutput("table")
            )
            
        )
    )
    

)

library(dplyr)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    clean_entire <- read.csv("../Data/clean_entire.csv", header = TRUE)
    
    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    output$table <- DT::renderDataTable(DT::datatable({
        data <- clean_entire
        data
    }))
    
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