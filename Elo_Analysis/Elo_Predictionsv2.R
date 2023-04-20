### Elo Predictions v2
library(tidyverse)
library(gt)
library(gtExtras)
head(future_matches)
View(future_matches)

# Create function for win_prob
win_prob <- function(team1_rating, team2_rating) {
  return(1 / (1+10^((team2_rating - team1_rating) / 400)))
}

# Functino to calculate expected margin
margin_pred <- function(team1_rating, team2_rating) {
  return((team1_rating - team2_rating) / 28.5)
}

# Match Ratings df
match_ratings <- data.frame(
  team1 = c("Sligo", "Kerry", "Limerick", "Cavan", "Roscommon",
            "Westmeath", "Kildare", "Laois", "Offaly", "Down",
            "Derry"),
  team1_elo = c(1541, 1543, 1467, 1536, 1518, 1519, 1495, 1506,
                 1499, 1489, 1561),
  team2 = c("New York", "Tipperary", "Clare", "Armagh", "Galway",
            "Louth", "Wicklow", "Dublin", "Meath", "Donegal",
            "Monaghan"),
  team2_elo = c(1492, 1464, 1488, 1502, 1544, 1524, 1503,
                 1552, 1486, 1486, 1485)
)

# Calculate expected win probability
match_predictions <- match_ratings %>%
  rowwise() %>%
  mutate(
    team1_win_prob = win_prob(team1_elo, team2_elo),
    team2_win_prob = abs(1 - team1_win_prob),
    margin_pred = margin_pred(team1_elo, team2_elo)
  )
match_predictions$team1_win_prob <- round((
  match_predictions$team1_win_prob *100
), 2)
match_predictions
match_predictions$team2_win_prob <- round((
  match_predictions$team2_win_prob * 100
), 2)

match_predictions <- match_predictions %>%
  select(-c(margin_pred))
match_predictions$Competition <- future_matches$competition
View(match_predictions)

# Table
match_predictions %>% gt(rowname_col = "row", groupname_col = "Competition") %>%
  cols_label(team1 = "Team 1",
             team1_elo = "Team 1 Rating",
             team2 = "Team 2",
             team2_elo = "Team 2 Rating",
             team1_win_prob = "Team 1 Win Probability",
             team2_win_prob = "Team 2 Win Probability") %>%
  cols_align(
    align = c("center")
  ) %>%
  tab_header(
    title = md("**GAA Match Weekend Predictions**")
  ) %>%
  tab_source_note(md("Fixture schedule from: https://www.gaa.ie/fixtures-results/"))
