library(cfbfastR)
library(espnscrapeR)
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