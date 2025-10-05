library(cfbfastR)
library(espnscrapeR)
library(tidyverse)
seasons = 2024


pbp <- load_cfb_pbp(seasons = 2025)

rosters <- load_cfb_rosters()


#Sameer's NFL Rush Model Fit
qbrun_fit <-
  lmer(epa ~ 1 + (1 | rusher_player_id) + (1 | defteam) +
         shotgun + no_huddle + posteam_type + pass_strength,
       data = qb_runs)
nonqb_run_fit <-
  lmer(epa ~ 1 + (1 | rusher_player_id) + (1 | defteam) +  
         shotgun + no_huddle + posteam_type + rusher_position + run_context + pass_strength,
       data = nonqb_runs)

#Variables to compute - pass strength, rush strength, conference adjustment (FBS and FCS)
#Consider using variables like defensive stuffed run rate, redzone play or not, rush player's success rate

colnames(pbp)




# Points to win
# Uses only games where both teams are fbs and where both teams don't have a score of zero (canceled)
games = load_cfb_schedules(seasons) %>% 
  filter(season_type == "regular", 
         home_division == 'fbs', 
         away_division == 'fbs',
         completed == TRUE,
         (home_points != 0 | away_points != 0))

conf = games %>% 
  select(season,home_team,home_conference) %>%
  rename(team = home_team, conf = home_conference) %>% 
  distinct(season,team,conf)

games = games %>% 
  mutate(result = home_points-away_points,
         win_t = ifelse(result > 0, home_team, away_team),
         lose_t = ifelse(result < 0, home_team, away_team),
         win_by = ifelse(win_t == home_team, result, -1*result),
         lose_by = ifelse(lose_t == home_team, result, -1*result) ) %>% 
  select(season,win_t,win_by,lose_t,lose_by,result)

win_diff =
  games  %>% 
  dplyr::group_by(season, win_t) %>%
  dplyr::summarise(wins = dplyr::n(), win_diff = sum(win_by), .groups = 'drop') %>%
  dplyr::rename(team = win_t)

loss_diff =
  games %>%
  dplyr::group_by(season, lose_t) %>%
  dplyr::summarise(loss = dplyr::n(), loss_diff = sum(lose_by), .groups = 'drop') %>%
  dplyr::rename(team = lose_t)

records =
  win_diff %>%
  dplyr::full_join(y = loss_diff, by = c("season", "team")) %>%
  dplyr::mutate(across(everything(), ~replace_na(.x,0)),
                scoring_diff = win_diff + loss_diff) %>% 
  left_join(conf, by = c("season","team"))

win_score_fit = lm(wins~scoring_diff, data = records)
points_to_win = coefficients(win_score_fit)[2] %>% unname()
