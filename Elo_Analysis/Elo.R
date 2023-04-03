### GAA Analysis - ELO 
install.packages("elo")
library(elo)
library(MLmetrics)
library(lubridate)
library(formattable)

# Drop columns
historic_match_results <- subset(historic_match_results, select = -c(home_win, draw))

# Create column total with amount of home/way points
historic_match_results <- historic_match_results %>%
  mutate(total_home_points = home_points + (3*home_goals),
         total_away_points = away_points + (3*away_goals))

# Show match result from home team / away team & margin
historic_match_results <- historic_match_results %>%
  mutate(home_result = case_when(total_home_points > total_away_points ~ 1,
                                 total_home_points < total_away_points ~ 0,
                                 total_home_points == total_away_points ~ 0.5),
         away_result = case_when(total_home_points < total_away_points ~ 1,
                                 total_home_points > total_away_points ~ 0,
                                 total_home_points == total_away_points ~ 0.5),
         margin = abs(total_home_points - total_away_points))

# Adjust date column
historic_match_results$Date <- mdy(historic_match_results$Date)

# First elo model run
elo_model_1 <- elo.run(data = historic_match_results,
                       formula = home_result ~ home_team + away_team + k(30 + 2*margin))
elo_model_1_results <- elo_model_1 %>% as.data.frame()
elo_model_1_results %>% tail(n=10)
final_elos_model_1 <- final.elos(elo_model_1)
final_elos_model_1 %>%sort(decreasing = TRUE) %>% head(n=35)



# Elo Model accounting for draws
draw_rates <- data.frame(win_prob = elo_model_1$elos[,3],
                         win_loss_draw = elo_model_1$elos[,4]) %>%
  mutate(prob_bucket = abs(round((win_prob)*20)) / 20) %>%
  group_by(prob_bucket) %>%
  summarise(draw_prob = sum(ifelse(win_loss_draw == 0.5, 1, 0))/n())

draw_rates %>% head(n=20)

# Merge with existing data
data_with_probabilities <- historic_match_results %>%
  select(competition, Date, home_team, away_team, home_points, home_goals,
         away_points, away_goals, home_result, away_result) %>%
  mutate(home_elo = elo_model_1_results$elo.A - elo_model_1_results$update.A,
         away_elo = elo_model_1_results$elo.B - elo_model_1_results$update.B,
         home_prob = elo_model_1_results$p.A,
         away_prob = 1-home_prob) %>%
  mutate(prob_bucket = round(20*home_prob)/20) %>%
  left_join(draw_rates, by = "prob_bucket") %>%
  relocate(draw_prob, .after=home_prob) %>%
  select(-prob_bucket)

# redist win/loss probs to sum to 1
data_with_probabilities <- data_with_probabilities %>%
  mutate(home_prob = home_prob - home_prob * draw_prob,
         away_prob = away_prob - away_prob * draw_prob)
data_with_probabilities %>%
  select(home_team, away_team, home_prob, draw_prob, away_prob) %>%
  tail(n=10)
View(data_with_probabilities)
# Create new Year column
data_with_probabilities$Competition_Year <- year(data_with_probabilities$Date)
data_with_probabilities$home_team <- str_to_title(data_with_probabilities$home_team)
data_with_probabilities$away_team <- str_to_title(data_with_probabilities$away_team)

# Create unique team names vector
team_names <- unique(c(data_with_probabilities$home_team, data_with_probabilities$away_team))
team_names
