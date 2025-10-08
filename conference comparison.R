library(cfbfastR)
library(dplyr)
library(ggplot2)
library(ggimage)
library(tidyr)

logos = data.frame(
  team = c("ACC","American Athletic", "Big 12","Big Ten", "Conference USA", "FBS Independents", "Mid-American", "Mountain West", "Pac-12", "SEC", "Sun Belt"),
  logo = c("https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/1.png&transparent=true&w=30&h=30",
           "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/151.png&transparent=true&w=30&h=30",
           "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/4.png&transparent=true&w=30&h=30",
           "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/5.png&transparent=true&w=30&h=30",
           "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/12.png&transparent=true&w=30&h=30",
           "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/18.png&transparent=true&w=30&h=30",
           "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/15.png&transparent=true&w=30&h=30",
           "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/17.png&transparent=true&w=30&h=30",
           "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/9.png&transparent=true&w=30&h=30",
           "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/8.png&transparent=true&w=30&h=30",
           "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa_conf/500/37.png&transparent=true&w=30&h=30")
)
games = load_cfb_schedules(2024) %>% 
  filter(season_type == "regular", 
         home_division == 'fbs', 
         away_division == 'fbs',
         completed == TRUE,
         (home_points != 0 | away_points != 0),
         home_conference != away_conference)

conf = games %>% 
  select(season,home_team,home_conference) %>%
  rename(team = home_team, conf = home_conference) %>% 
  distinct(conf)

games = games %>% 
  mutate(result = home_points-away_points,
         win_t = ifelse(result > 0, home_conference, away_conference),
         lose_t = ifelse(result < 0, home_conference, away_conference),
         win_by = ifelse(win_t == home_conference, result, -1*result),
         lose_by = ifelse(lose_t == home_conference, result, -1*result) ) %>% 
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
  dplyr::full_join(y = loss_diff) %>%
  dplyr::mutate(scoring_diff = win_diff + loss_diff,
                record = wins-loss) %>% 
  left_join(logos)


ggplot(records,aes(scoring_diff,wins,image = logo))+
  geom_image()+
  labs(x = "Total scoring margin",
       y = "Wins",
       title = "Wins vs Score Differential")+
  theme_bw()

ggplot(records,aes(scoring_diff,loss,image = logo))+
  geom_image()+
  labs(x = "Total scoring margin",
       y = "Loses",
       title = "Loses vs Score Differential")+
  theme_bw()

ggplot(records,aes(scoring_diff,record,image = logo))+
  geom_image(size=.1)+
  labs(x = "Total scoring margin",
       y = "Record",
       title = "Record vs Score Differential")+
  theme_bw()


##########################


games = load_cfb_schedules(2024) %>% 
  filter(season_type == "regular", 
         home_division == 'fbs', 
         away_division == 'fbs',
         completed == TRUE,
         (home_points != 0 | away_points != 0),
         home_conference != away_conference)

conf_games = games %>%
  mutate(point_diff = home_points - away_points) %>% 
  mutate(
    conf_pair = ifelse(home_conference < away_conference,
                       paste(home_conference, away_conference, sep = "_"),
                       paste(away_conference, home_conference, sep = "_")),
    # Flip sign if we swapped the order so that point_diff always corresponds to the first conf in the pair
    point_diff_aligned = ifelse(home_conference < away_conference, point_diff, -point_diff),
    conf1 = ifelse(home_conference < away_conference, home_conference, away_conference),
    conf2 = ifelse(home_conference < away_conference, away_conference, home_conference)
  ) %>% 
  select(conf1,conf2,point_diff_aligned)

avg_diff <- conf_games %>%
  group_by(conf1, conf2) %>%
  summarize(avg_point_diff = mean(point_diff_aligned), .groups = "drop")




logos = rename(logos, conference = team)


plot2_data_a = avg_diff %>% 
  rename(conf_1 = conf2,
         conf_2 = conf1) %>% 
  rename(conf1 = conf_1,
         conf2 = conf_2) %>% 
  mutate(avg_point_diff = -avg_point_diff)

plot2_data = avg_diff %>% 
  full_join(plot2_data_a) %>% 
  left_join(logos, by=c("conf2"="conference")) %>% 
  mutate(avg_point_diff = -avg_point_diff)

ggplot(plot2_data, aes(x=avg_point_diff,y=conf1,image=logo))+
  geom_image(size = 0.04)+
  theme_bw()+
  labs(
    title = "Average Point Differences Between FBS Conferences",
    subtitle = "When the conferences play each other",
    x = "Relative Average Point Differential",
    y = "Conference"
  )

#######

rosters <- load_cfb_rosters(seasons = 2024)
conferences <- cfbd_conferences()
teams <- cfbd_team_info()

pbp <- load_cfb_pbp(seasons = 2024)

rushes <- pbp %>%
  filter(rush == 1) %>%
  filter(season_type == "regular")
rushes$rush_player_id <- as.character(rushes$rush_player_id)


rosterids <- rosters %>%
  mutate(
    name = paste(first_name, last_name, sep = " ")
  ) %>%
  select(name, team, position, athlete_id)


rushes <- rushes %>%
  left_join(rosterids, by = c("rush_player_id" = "athlete_id"))

teaminfo <- rosterids %>%
  left_join(teams, by = c("team" = "school"))

#Replacement player calculation
rbs <- rushes %>%
  filter(position %in% c("RB")) %>%
  group_by(pos_team, position, conference, rusher_player_name) %>%
  summarize(
    rush_attempts = n(),
    total_rush_epa = sum(EPA, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  group_by(conference) %>% 
  summarize(
    avg_epa_conf = mean(total_rush_epa, na.rm = TRUE, .groups = "drop")
  ) %>% 
  filter(conference %in% c("American Athletic", "ACC", "Big 12", "Big Ten", "Conference USA",
                           "FBS Independents", "Mid-American", "Mountain West", "Pac-12",
                           "SEC", "Sun Belt")) %>% 
  left_join(logos)

ggplot(rbs)+
  geom_image(aes(x = conference, y=avg_epa_conf, image = logo), position = position_jitter(height = .2))+
  geom_point(aes(y=-.5,x=0), alpha = 0)+
  geom_point(aes(y=.5,x=0), alpha = 0)+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())+
  labs(
    title = "Average EPA Per Conference",
    x = "Conference",
    y = "Average Player EPA"
  )
