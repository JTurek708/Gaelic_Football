## Latest Elo Rating script
library(lubridate)
library(tidyverse)
install.packages("gt")
library(gt)
library(shiny)
install.packages("shinyWidgets")
library(shinyWidgets)
install.packages("gtsummary")
library(gtsummary)
install.packages("gtExtras")
library(gtExtras)

# Develop final elo ratings by competition year
gaa_data_elo <- gaa_data_prob
gaa_data_elo$Competition_Year <- year(gaa_data_elo$Date)
View(gaa_data_elo)
latest_elo_ratings <- gaa_data_elo %>%
  select(Date, Competition_Year, competition, team1, team1_elo_post,
         team2, team2_elo_post) %>%
  mutate(
    team = if_else(!is.na(team1), team1, team2),
    elo = if_else(!is.na(team1), team1_elo_post, team2_elo_post)
  ) %>%
  select(team, elo) %>%
  group_by(team, gaa_data_elo$Competition_Year) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  arrange(desc(Competition_Year))
View(latest_elo_ratings)
latest_elo_ratings <- latest_elo_ratings %>%
  rename("Competition Year" = `gaa_data_elo$Competition_Year`)

# Create gt table
gt_elo_table <- gt(latest_elo_ratings)
gt_elo_table
gt_elo_table <- gt_elo_table %>%
  tab_header(
    title = md("**Senior County GAA Football Elo Ratings**"),
    subtitle = md("*By Competition Year*")) %>%
  tab_source_note(
    source_note = "Source: GAA Fixtures & Results, https://www.gaa.ie/fixtures-results/"
  )
gt_elo_table <- gt_elo_table %>%
  cols_label(
    team = "TEAM",
    elo = "TEAM RATING"
  ) %>%
  gt_theme_538()
  
gt_elo_table


### Shiny App ###
ui <- fluidPage(
  titlePanel("GAA Football Team Ratings"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "comp_year",
        label = "Select Competition Year:",
        choices = unique(latest_elo_ratings$`Competition Year`),
        multiple = TRUE
      ),
      selectInput(
        inputId = "team",
        label = "Select Team:",
        choices = unique(latest_elo_ratings$team),
        multiple = TRUE
      )
    ),
    mainPanel(
      gt_output(outputId = "elo_table")
    )
  )
)
server <- function(input, output) {
  # Filter data based on user inputs
  filtered_data <- reactive({
    data <- latest_elo_ratings
    if (!is.null(input$comp_year)) {
      data <- data %>% filter(`Competition Year` %in% input$comp_year)
    }
    if (!is.null(input$team)){
      data <- data %>% filter(team %in% input$team)
    }
    data
  })
  
  # Render gt table
  output$elo_table <- render_gt({
    gt(filtered_data()) %>%
      tab_header(
        title = md("**Senior County GAA Football Team Ratings**"),
        subtitle = md("*By Competition Year*")
      ) %>%
      tab_source_note(
        source_note = "Source: GAA Fixtures % Results, https://www.gaa.ie/fixtures-results/"
      ) %>%
      cols_label(
        team = "TEAM",
        elo = "TEAM RATING"
      ) %>%
      gt_theme_538()
  })
}
shinyApp(ui, server)
