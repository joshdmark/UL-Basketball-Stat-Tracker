# install.packages("devtools")
# devtools::install_github("lbenz730/ncaahoopR")
library(tidyverse)
library(data.table)
library(sqldf)
library(ncaahoopR)

## get louisville schedule
lou_schedule <- ncaahoopR::get_schedule(team = 'Louisville')

## lou games: game ids for games played
lou_games <- lou_schedule %>% 
  filter(!is.na(team_score)) %>% 
  distinct(game_id)

## get PBP data
lou_pbp <- get_pbp(team = 'Louisville') %>% 
  data.frame() %>% 
  mutate(team = 'Louisville', 
         opponent = ifelse(home == 'Louisville', away, home), 
         team_score = ifelse(home == 'Louisville', home_score, away_score),
         opp_score = ifelse(home == 'Louisville', away_score, home_score), 
         diff = team_score - opp_score) 

## tv timeouts
tv_timeouts <- lou_pbp %>% 
  filter(description == 'Official TV Timeout' | 
           (game_id == 401269680 & play_id == 66) | ## missing Wisconsin timeout
           (game_id == 401257671 & play_id == 310) | ## missing WKU end of game
           (description == 'PLAY' & half == 2) | 
           (description == 'End of Game')) %>% 
  arrange(date, play_id) %>% 
  distinct() %>% 
  group_by(game_id) %>% 
  mutate(timeout_nbr = 1:n()) %>% 
  ungroup() %>% 
  data.frame() %>% 
  data.table() %>% 
  .[game_id == 401257660 & timeout_nbr == 5, 
    `:=` (home_score = 36, 
          away_score = 16, 
          score_diff = 20)] %>% ## Evansville
  .[game_id == 401257663 & timeout_nbr == 5, 
    `:=` (home_score = 38, 
          away_score = 38, 
          score_diff = 0)] %>% ## Seton Hall
  .[game_id == 401257667 & timeout_nbr == 5, 
    `:=` (home_score = 41, 
          away_score = 31, 
          score_diff = 10)] %>% ## PVAMU
  .[game_id == 401257671 & timeout_nbr == 5, 
    `:=` (home_score = 34, 
          away_score = 29, 
          score_diff = 5)] %>% ## WKU
  .[game_id == 401269680 & timeout_nbr == 5, 
    `:=` (home_score = 44, 
          away_score = 18, 
          score_diff = -26)] %>% ## Wisconsin
  data.frame()

## convert tv_timeout to wide format 
timeouts_wide <- tv_timeouts %>% 
  distinct(game_id, date, play_id, description, timeout_nbr) %>% 
  mutate(timeout_nbr = paste('timeout_', timeout_nbr, sep = '')) %>%
  select(-description, -date) %>% 
  spread(key = timeout_nbr, value = play_id)

## add timeouts to PBP data
lou_pbp <- merge(lou_pbp, timeouts_wide, by = 'game_id', all.x = TRUE) %>% 
  data.frame() %>% 
  mutate(segment_nbr = case_when(
    play_id <= timeout_1 ~ 1, 
    play_id > timeout_1 & play_id <= timeout_2 ~ 2,
    play_id > timeout_2 & play_id <= timeout_3 ~ 3,
    play_id > timeout_3 & play_id <= timeout_4 ~ 4,
    play_id > timeout_4 & play_id <= timeout_5 ~ 5,
    play_id > timeout_5 & play_id <= timeout_6 ~ 6,
    play_id > timeout_6 & play_id <= timeout_7 ~ 7,
    play_id > timeout_7 & play_id <= timeout_8 ~ 8, 
    play_id > timeout_8 & play_id <= timeout_9 ~ 9, 
    play_id > timeout_9 ~ 10,
    TRUE ~ 0
  ))

## segment diffs 
segment_diffs <- tv_timeouts %>% 
  distinct(game_id, timeout_nbr, diff, team_score, opp_score) %>% 
  mutate(prev_segment_diff = lag(diff), 
         prev_segment_diff = ifelse(timeout_nbr == 1, 0, prev_segment_diff),
         segment_diff = diff - prev_segment_diff) %>% 
  mutate(half_nbr = ifelse(timeout_nbr <= 5, 1, 2), 
         segment_nbr = timeout_nbr)

## add opponent to segment_diffs 
segment_diffs <- sqldf("select l1.*, l2.opponent, l2.date
             from segment_diffs l1 
             left join lou_schedule l2 on l1.game_id = l2.game_id")

## add game_dt column to segment_diffs
segment_diffs <- segment_diffs %>% 
  mutate(game_dt = format(date, '%m/%d'))
  
## get shot chart locations
lou_shot_locs <- ncaahoopR::get_shot_locs(lou_games$game_id) %>% 
  data.frame() %>% 
  filter(team_name == 'Louisville') %>% 
  mutate(player_last_name = sub('.*\\.', '', shooter))

## add opponent to lou_shot_locs 
lou_shot_locs <- sqldf("select l1.*, l2.opponent
             from lou_shot_locs l1 
             left join lou_schedule l2 on l1.game_id = l2.game_id")

## add game_dt column to lou_shot_locs
lou_shot_locs <- lou_shot_locs %>% 
  mutate(game_dt = format(date, '%m/%d'))

## get box scores 
lou_box_scores <- data.frame()
for (g in lou_games$game_id){
  # print(g)
  ## get single game box score
  tmp_box_score <- ncaahoopR::get_boxscore(game_id = g)
  tmp_box_score <- tmp_box_score$Louisville
  tmp_box_score <- tmp_box_score %>% mutate(game_id = g)
  
  ## combine single game with all box scores 
  lou_box_scores <- rbind(lou_box_scores, tmp_box_score)
}

## convert starter to binary
lou_box_scores <- lou_box_scores %>% 
  mutate(game_started_ind = ifelse(starter == TRUE, 1, 0)) %>% 
  mutate(player_last_name = sub('.*\\.', '', player))

## add opponent to lou_box_scores 
lou_box_scores <- sqldf("select l1.*, l2.opponent, l2.date
             from lou_box_scores l1 
             left join lou_schedule l2 on l1.game_id = l2.game_id")

## add game_dt column to lou_box_scores
lou_box_scores <- lou_box_scores %>% 
  mutate(game_dt = format(date, '%m/%d'))

## add game number to lou_schedule
lou_schedule <- lou_schedule %>% 
  arrange(date) %>% 
  mutate(game_nbr = 1:n()) 

## files for tableau
fwrite(lou_box_scores, "Documents/Sports/UL Basketball/lou_box_scores.csv")
fwrite(lou_schedule, "Documents/Sports/UL Basketball/lou_schedule.csv")
fwrite(lou_shot_locs, "Documents/Sports/UL Basketball/lou_shot_locs.csv")
fwrite(segment_diffs, "Documents/Sports/UL Basketball/segment_diffs.csv")

