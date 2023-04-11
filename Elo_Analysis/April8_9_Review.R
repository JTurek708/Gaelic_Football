### April 8-9 Review ###
april_wknd <- gaa_data_prob %>%
  filter(Date >= "2023-04-08")
april_wknd
table <- april_wknd %>%
  mutate(margin = abs(team1_score - team2_score)) %>%
  select(Date, competition, team1, team2, team1_win_prob,
         team2_win_prob, team1_score, team2_score, margin)
table <- table %>%
  rename("Margin" = margin,
         "Team 1" = team1,
         "Team 2" = team2,
         "Team 1 Win Probability" = team1_win_prob,
         "Team 2 Win Probability" = team2_win_prob,
         "Team 1 Score" = team1_score,
         "Team 2 Score" = team2_score,
         "Competition" = competition)
  
library(formattable)
formattable(table,
            align = c("l", "c", "c", "c", "c",
                      "c", "c", "c", "c", "r"))

table2 <- april_wknd %>%
  select(Date, competition, team1, team2, team1_elo, team2_elo, team1_elo_post, team2_elo_post)
table2 <- table2 %>%
  rename("Competition" = competition, "Team 1" = team1,
         "Team 2" = team2, "Team 1 Elo" = team1_elo,
         "Team 2 Elo" = team2_elo, "Team 1 Post-Game Elo" = team1_elo_post,
         "Team 2 Post-Game Elo" = team2_elo_post)
formattable(table2,
            align = c("l", "c", "c", "c", "c", "c", "c", "c"))
