### Test RShiny Web App
library(shiny)
library(shinythemes)
ui <- fluidPage(
  titlePanel("2023 Pre-Season Elo Ratings"),
  sidebarLayout(
    sidebarPanel(
      selectInput("team", "Team:",
                  choices = c("All Teams", unique(data_with_probabilities$home_team,
                                                  data_with_probabilities$away_team)))
    ),
    mainPanel(
      tableOutput("table")
    )
  )
)
server <- function(input, output){
  output$table <- renderTable({
    if (input$team == "All Teams") {
      data_with_probabilities
    } else {
      data_with_probabilities[data_with_probabilities$home_team == input$home_team]
    }
  })
}
library(shiny)

shinyApp(ui, server)
