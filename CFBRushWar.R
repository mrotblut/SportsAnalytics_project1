library(cfbfastR)
library(espnscrapeR)
library(dplyr)
library(tidyverse)
library(lme4)
rosters <- load_cfb_rosters(seasons = 2024)
conferences <- cfbd_conferences()
teams <- cfbd_team_info()



pbp <- load_cfb_pbp(seasons = 2024)

rushes <- pbp %>%
  filter(rush == 1)

rushes <- rushes %>%
  left_join(teams, by = c("def_pos_team" = "school"))

rushes <- rushes %>%
  rename(oppConf = conference.y)

milroecheck <- rushes %>%
  filter(rusher_player_name == "Jalen Milroe") %>%
  pull(rush_player_id)

rosterids <- rosters %>%
  mutate(
    name = paste(first_name, last_name, sep = " ")
  ) %>%
  select(name, team, position, athlete_id)

rushes$rush_player_id <- as.character(rushes$rush_player_id)

rushes <- rushes %>%
  left_join(rosterids, by = c("rush_player_id" = "athlete_id"))

rushes <- rushes %>%
  mutate(
    qbRun = ifelse(position == "QB", 1, 0)
  )

rushes <- rushes %>%
  mutate(
    homeTeam = ifelse(pos_team == home, 1, 0)
    )

pass_strength <-
  pbp |>
  filter(pass == 1) %>%
  dplyr::group_by(pos_team) |>
  dplyr::summarise(pass_strength = mean(EPA, na.rm = TRUE))

rushes <-
  rushes |>
  dplyr::inner_join(pass_strength, by = "pos_team") 


qbRuns <- rushes %>%
  filter(qbRun == 1)

rushesfilter <- rushes %>%
  filter(qbRun == 0)

rushesfilter <- rushesfilter %>%
  select(game_id, id_play, rush_player_id, qbRun, passer_player_name, pos_team, oppConf, def_pos_team, homeTeam, rz_play, epa_success,
         EPA, conference_game, stuffed_run, pass_strength)

qbRuns <- qbRuns %>%
  select(game_id, id_play, rush_player_id, qbRun, passer_player_name, pos_team, oppConf, def_pos_team, homeTeam, rz_play, epa_success,
         EPA, conference_game, stuffed_run, pass_strength)

rushStuffRate <- rushesfilter %>%
  group_by(def_pos_team) %>%
  summarise(
    defStuffRate = mean(stuffed_run, na.rm = TRUE)
  )
qbStuffRate <- qbRuns %>%
  group_by(def_pos_team) %>%
  summarise(
    defStuffRate = mean(stuffed_run, na.rm = TRUE)
  )


rushesfilter <-
  rushesfilter |>
  dplyr::inner_join(rushStuffRate, by = "def_pos_team") 

qbRuns <-
  qbRuns |>
  dplyr::inner_join(qbStuffRate, by = "def_pos_team") 


#Sameer's NFL Rush Model Fit
qbrun_fit <-
  lmer(EPA ~ 1 + (1 | rush_player_id) + (1 | oppConf) + (1 | def_pos_team) +
         homeTeam + pass_strength + rz_play + defStuffRate,
       data = qbRuns)
nonqb_run_fit <-
  lmer(EPA ~ 1 + (1 | rush_player_id) + (1 | oppConf) + (1 | def_pos_team) +
         homeTeam + pass_strength + rz_play + defStuffRate,
       data = rushesfilter)

#Variables to compute - pass strength, rush strength, conference adjustment (FBS and FCS)
#Consider using variables like defensive stuffed run rate, redzone play or not, rush player's success rate

tmp_qbrun <- ranef(qbrun_fit)
qbrun_effects <-
  data.frame(
    athlete_id = rownames(tmp_qbrun[["rush_player_id"]]), 
    ipa_qbrun = tmp_qbrun[["rush_player_id"]][,1])

tmp_run <- ranef(nonqb_run_fit) 
run_effects <-
  data.frame(
    athlete_id= rownames(tmp_run[["rush_player_id"]]),
    ipa_run = tmp_run[["rush_player_id"]][,1])


run_effects |>
  dplyr::left_join(y = rosterids |> dplyr::select(athlete_id, name), by = "athlete_id") |>
  dplyr::select(name, ipa_run) |>
  dplyr::arrange(dplyr::desc(ipa_run)) |>
  dplyr::slice_head(n = 10)




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
