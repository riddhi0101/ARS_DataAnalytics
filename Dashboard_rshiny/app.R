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
                    h2("Overview"),
                    fluidPage(
                        dateRangeInput('dateRange',
                        label = 'Date range: yyyy-mm-dd',
                        start = Sys.Date() - 2, end = Sys.Date() + 2), #allows date range specification to see revenue for pop up sales within this range
                        
                        mainPanel(
                            plotOutput("revenue_per_sale")  #creating a spot for the pop up sales revenue barplot
                        )
            )),
            
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
                    fluidRow(
                        dateRangeInput('dateRangePie',
                                       label = 'Date range: yyyy-mm-dd',
                                       start = Sys.Date() - 2, end = Sys.Date() + 2), 
                        
                        selectInput('cat', 'Category', c('All', levels(clean_entire$Category))),
                        
                        mainPanel(
                            plotOutput("piechart")  
                        ),
                    ),
                    fluidPage(
                        dateRangeInput('dateRange',
                        label = 'Date range: yyyy-mm-dd',
                        start = Sys.Date() - 2, end = Sys.Date() + 2), 
                    DT::dataTableOutput("table")
                    )
                    
            
        )
    )
    

)
)

server <- function(input, output) {
    set.seed(122)
    histdata <- rnorm(500)
    

    output$plot1 <- renderPlot({
        data <- histdata[seq_len(input$slider)]
        hist(data)
    })
    
    output$table <- DT::renderDataTable(DT::datatable({
        data <- clean_entire
        data
    }))
    
    output$piechart <- renderPlot({
        
        time_start = as.Date(input$dateRangePie[1])
        time_end = as.Date(input$dateRangePie[2])
        
        cat = input$cat
        print(cat)
        if (cat == 'All') {
            subset_df = df %>% filter(Date >= time_start, Date <= time_end) %>% count(Category)
            title = 'Category Breakdown of All Items Sold'
            fig <- plot_ly(subset_df, labels = ~Category, values = ~n, type = 'pie')
            fig <- fig %>% layout(title = title,
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
            
            
        }else{
            title = paste('Items sold in ', cat)
            subset_df = df %>% filter(Date >= time_start, Date <= time_end, Category == cat) %>% count(Item)
            fig <- plot_ly(subset_df, labels = ~Item, values = ~n, type = 'pie')
            fig <- fig %>% layout(title = title,
                                  xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                  yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
        }
        fig
    
    }
    
    )
}

shinyApp(ui, server)
