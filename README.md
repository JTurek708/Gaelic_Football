# Gaelic Football & Hurling
Data, code, visuals for GAA-related analysis

## Workflow
- Add completed matches to GAA_Elo.csv file (re-add to GH once complete)
- Run GAA_Elo through the ratings script
  - This will generate new post-match Elo ratings which are then used for the next match
- Create win/draw/loss probabilities and write to gaa_data_prob.csv
- Run gaa_data_prob.csv through the "Latest Elo Ratings" script
  - This pulls each team's most recent Elo rating for a rack & stack

