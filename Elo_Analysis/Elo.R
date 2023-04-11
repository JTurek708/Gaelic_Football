### GAA Analysis - ELO 
install.packages("elo")
library(elo)
library(MLmetrics)
library(lubridate)
library(formattable)

# Read in new data frame
gaa_elo <- read_csv("GAA_Elo.csv")
View(gaa_elo)

# Drop columns
historic_match_results <- subset(historic_match_results, select = -c(home_win, draw))

# Create column total with amount of home/way points
historic_match_results <- historic_match_results %>%
  mutate(total_home_points = home_points + (3*home_goals),
         total_away_points = away_points + (3*away_goals))

# Show match result from home team / away team & margin
gaa_elo <- gaa_elo %>%
  mutate(home_result = case_when(total_home_score > total_away_score ~ 1,
                                 total_home_score < total_away_score ~ 0,
                                 total_home_score == total_away_score ~ 0.5),
         away_result = case_when(total_home_score < total_away_score ~ 1,
                                 total_home_score > total_away_score ~ 0,
                                 total_home_score == total_away_score ~ 0.5),
         margin = abs(total_home_score - total_away_score))

# Adjust date column
gaa_elo$Date <- mdy(gaa_elo$Date)

# First elo model run
elo_model_1 <- elo.run(data = gaa_elo,
                       formula = home_result ~ home_team + away_team + k(15 + 2*margin))
elo_model_1_results <- elo_model_1 %>% as.data.frame()
View(elo_model_1_results)
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
data_with_probabilities <- gaa_elo %>%
  select(competition, Date, home_team, away_team, home_points, home_goals,
         away_points, away_goals, home_result, away_result, total_home_score,
         total_away_score, margin) %>%
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
  mutate(home_win_prob = home_prob - home_prob * draw_prob,
         away_win_prob = away_prob - away_prob * draw_prob)
gaa_data_prob <- data_with_probabilities %>%
  select(Date, competition, home_team, home_goals, home_points, away_team, 
         away_goals, away_points,total_home_score, total_away_score, home_prob, draw_prob, away_prob, home_elo, away_elo)
View(gaa_data_prob)
gaa_data_prob$home_elo <- round(gaa_data_prob$home_elo, 0)
gaa_data_prob$away_elo <- round(gaa_data_prob$away_elo, 0)
write_csv(gaa_data_prob, "GAA_Elo.csv")
# Create new Year column
gaa_data_prob$Competition_Year <- year(data_with_probabilities$Date)
gaa_data_prob$home_team <- str_to_title(gaa_data_prob$home_team)
gaa_data_prob$away_team <- str_to_title(gaa_data_prob$away_team)


# Create post-match Elos
elo_model_1_results <- elo_model_1_results %>%
  rename("home_elo_post" = elo.A,
         "away_elo_post" = elo.B)
df2$home_elo_post <- elo_model_1_results$home_elo_post
df2$away_elo_post <- elo_model_1_results$away_elo_post
gaa_data_prob <- df2
View(gaa_data_prob)
gaa_data_prob$home_elo_post <- 0
gaa_data_prob$away_elo_post <- 0
gaa_data_prob$home_elo_post <- round(elo_model_1_results$home_elo_post, 0)
gaa_data_prob$away_elo_post <- round(elo_model_1_results$away_elo_post,0)

# Round probabilities to 100
gaa_data_prob$home_prob <- round(100*(gaa_data_prob$home_prob), 0)
gaa_data_prob$draw_prob <- round(100*gaa_data_prob$draw_prob, 0)
gaa_data_prob$away_prob <- round(100*gaa_data_prob$away_prob,0)

# Rename home and away columns
gaa_data_prob <- gaa_data_prob %>%
  rename("team1" = home_team,
         "team1_goals" = home_goals,
         "team1_points" = home_points,
         "team2" = away_team,
         "team2_goals" = away_goals,
         "team2_points" = away_points,
         "team1_score" = total_home_score,
         "team2_score" = total_away_score,
         "team1_win_prob" = home_prob,
         "team2_win_prob" = away_prob,
         "team1_elo" = home_elo,
         "team2_elo" = away_elo,
         "team1_elo_post" = home_elo_post,
         "team2_elo_post" = away_elo_post)


# Write gaa_data_prob to csv
write_csv(gaa_data_prob, "gaa_data_prob.csv")
