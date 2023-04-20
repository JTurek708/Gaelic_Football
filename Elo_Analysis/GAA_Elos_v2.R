### GAA Elos v2
library(elo)
library(MLmetrics)
library(lubridate)

library(formattable)
library(scales)

# Read in new data frame
gaa_elo <- read.csv("GAA_Elo.csv", header = TRUE, stringsAsFactors = FALSE)
View(gaa_elo)

# Adjust date column
gaa_elo$Date <- mdy(gaa_elo$Date)
# Create new Competition Year column
gaa_elo$Competition_Year <- year(gaa_elo$Date)

# Adjust column names
gaa_elo <- gaa_elo %>%
  rename("team2_result" = away_result)

# Add margin column
gaa_elo <- gaa_elo %>%
  mutate(margin = abs(team1_score - team2_score))
# Add results
gaa_elo <- gaa_elo %>%
  mutate(team1_result = case_when(team1_score > team2_score ~ 1,
                                 team1_score < team2_score ~ 0,
                                 team1_score == team2_score ~ 0.5),
         away_result = case_when(team1_score < team2_score ~ 1,
                                 team1_score > team2_score ~ 0,
                                 team1_score == team2_score ~ 0.5))

# Separate into results and future fixtures dfs
results <- gaa_elo %>%
  filter(Date <= "2023-04-16")
View(results)
future_matches <- gaa_elo %>%
  filter(Date > "2023-04-16")
head(future_matches)

# Set Elo paramters
carryOver <- 0.5 # season elo rating carry over
k_val <- 15

# Map margin function
map_margin_to_outcome <- function(margin, marg.max = 80, marg.min=-80){
  norm <- (margin - marg.min)/(marg.max - marg.min)
  norm %>% pmin(1) %>% pmax(0)
}

# Calculate Elo
elo_rating_1 <- elo.run(score(team1_score, team2_score) ~ team1 + team2 +
                          regress(Competition_Year, 1500, carryOver),
                        data = results, k = 15)
                        
as.data.frame(elo_rating_1) %>% tail()
as.matrix(elo_rating_1) %>% tail()
elo_ratingsv2 <- elo_rating_1 %>% as.data.frame()
View(elo_ratingsv2)
final_elos_model_1 <- final.elos(elo_model_1)
View(final_elos_model_1)

# Elo Model accounting for draws
draw_rates <- data.frame(win_prob = elo_rating_1$elos[,3],
                         win_loss_draw = elo_rating_1$elos[,4]) %>%
  mutate(prob_bucket = abs(round((win_prob)*20)) / 20) %>%
  group_by(prob_bucket) %>%
  summarise(draw_prob = sum(ifelse(win_loss_draw == 0.5, 1, 0))/n())

# Merge with results
data_with_probabilities <- results %>%
  select(competition, Date, team1, team2, team1_points, team1_goals,
         team2_points, team2_goals, team1_result, team2_result, team1_score,
         team2_score, margin) %>%
  mutate(team1_elo = elo_ratingsv2$elo.A - elo_ratingsv2$update.A,
         team2_elo = elo_ratingsv2$elo.B - elo_ratingsv2$update.B,
         team1_prob = elo_ratingsv2$p.A,
         team2_prob = 1-team1_prob) %>%
  mutate(prob_bucket = round(20*team1_prob)/20) %>%
  left_join(draw_rates, by = "prob_bucket") %>%
  relocate(draw_prob, .after=team1_prob) %>%
  select(-prob_bucket)

# redist win/loss probs to sum to 1
data_with_probabilities <- data_with_probabilities %>%
  mutate(team1_win_prob = team1_prob - team1_prob * draw_prob,
         team2_win_prob = team2_prob - team2_prob * draw_prob)

gaa_data_prob <- data_with_probabilities %>%
  select(Date, competition, team1, team1_goals, team1_points, team2, 
         team2_goals, team2_points,team1_score, team2_score, team1_prob, draw_prob, 
         team2_prob, team1_elo, team2_elo)
gaa_data_prob$team1_elo <- round(gaa_data_prob$team1_elo, 0)
gaa_data_prob$team2_elo <- round(gaa_data_prob$team2_elo, 0)
write_csv(gaa_data_prob, "gaa_data_prob.csv")
View(gaa_data_prob)

# Create post-match Elos
elo_ratingsv2 <- elo_ratingsv2 %>%
  rename("team1_elo_post" = elo.A,
         "team2_elo_post" = elo.B)
gaa_data_prob$team1_elo_post <- 0
gaa_data_prob$team2_elo_post <- 0
gaa_data_prob$team1_elo_post <- round(elo_ratingsv2$team1_elo_post, 0)
gaa_data_prob$team2_elo_post <- round(elo_ratingsv2$team2_elo_post,0)

# Round probabilities to 100
gaa_data_prob$team1_prob <- round(100*(gaa_data_prob$team1_prob), 1)
gaa_data_prob$draw_prob <- round(100*gaa_data_prob$draw_prob, 1)
gaa_data_prob$team2_prob <- round(100*gaa_data_prob$team2_prob,1)

# Write to csv
write_csv(gaa_data_prob, "gaa_data_prob.csv")
