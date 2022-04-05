Videos <- c("Video Interactions", "Video Interactions Delta", "Video Likes", "Video Shares")
video_data <- c(95, "+20.2% vs Sep 27 - Dec 25", 94, 1)
stories <- c("Story Interactions", "Story Interactions Delta", "Story Replies", "Story Shares")
story_data <- c(18, "-72.4% vs Sep 27 - Dec 25", 6, 12)
posts <- c("Post Interactions", "Post Interactions Delta", "Post Likes", "Post Shares")
posts_data <- c(1251, "-8.7% vs Sep 27 - Dec 25", 1040, 88)

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)
sidebar <- dashboardSidebar(
  sidebarMenu(id = "tab",
              menuItem("1", tabName = "1")
  )
)
body <-   ## Body content
  dashboardBody(box(width = 12,fluidRow(
    fluidRow(DT::dataTableOutput("op")))))
ui <-   dashboardPage(dashboardHeader(title = "Summary of Content Interactions"),
                      sidebar,
                      body)
# Define the server code
server <- function(input, output,session) {
  df <- data.frame(structure(list(Videos <- c("Video Interactions", "Video Interactions Delta", "Video Likes", "Video Shares"), video_data <- c(95, "+20.2% vs Sep 27 - Dec 25", 94, 1)
  )
  , class = "data.frame", row.names = c(NA, 4L)))
  output$op <-renderDataTable({ df })
}
shinyApp(ui = ui, server = server)