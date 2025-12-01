library(arrow)
library(ggplot2)
library(dplyr)


targetdata <- read_parquet("sumer_coverages_player_play.parquet")

write.csv(targetdata, "SumerSupplementData.csv", row.names = FALSE)

playerinfo <- data %>%
  distinct(game_id, play_id, nfl_id, .keep_all = TRUE) %>%
  select(game_id, play_id, nfl_id, player_name, player_position, player_to_predict, player_side)

outputs <- rbind(week1out,week2out, week3out, week4out, week5out, week6out, week7out, week8out,
                 week9out, week10out, week11out, week12out, week13out, week14out, week15out, week16out,
                 week17out, week18out)



outputs <- outputs %>%
  left_join(playerinfo, by = c("game_id", "play_id", "nfl_id"))

outputsinfo <- outputs %>%
  group_by(game_id,play_id, frame_id) %>%
  mutate(
    defense = (ifelse(player_side == "Defense", 1, 0))
  ) %>%
  summarise(
    totalDef = sum(defense)
  )

summary(outputsinfo$totalDef)

week1targs <- week1 %>%
  left_join(targetdata, by = c("game_id", "play_id", "nfl_id"))

week1targs <- week1targs %>%
  filter(player_role == "Targeted Receiver" |  targeted_defender == TRUE)


targs <- data %>%
  left_join(targetdata, by = c("game_id", "play_id", "nfl_id"))

receivers479 <- targs %>%
  filter(player_role == "Targeted Receiver") %>%
  select(game_id, play_id, frame_id, nfl_id, player_name, x, y, player_position, ball_land_x, ball_land_y)

# Get defenders at the same frame
defenders479 <- targs %>%
  filter(targeted_defender == TRUE) %>%
  select(game_id, play_id, frame_id, player_name, defenderId = nfl_id, defenderY = y, defenderX = x, player_position)

joined479 <- inner_join(receivers479, defenders479, by = c("game_id", "play_id", "frame_id"))


outputs479 <- outputs %>%
  semi_join(joined479, by = c("game_id", "play_id"))

receiversoutputs479 <- outputs479 %>%
  filter(player_position %in% c("RB", "WR", "TE")) %>%
  select(game_id, play_id, frame_id, nfl_id, player_name, x, y, player_position)

# Get defenders at the same frame
defendersoutputs479 <- outputs479 %>%
  filter(player_position %in% c("CB", "SS","FS", "DB", "ILB", "LB", "MLB")) %>%
  select(game_id, play_id, frame_id, player_name, defenderId = nfl_id, defenderY = y, defenderX = x, player_position)

joinedoutputs479 <- inner_join(receiversoutputs479, defendersoutputs479, by = c("game_id", "play_id", "frame_id"))


joinedoutputs479 <- joinedoutputs479 %>%
  left_join(targetdata, by = c("game_id", "play_id", "defenderId" = "nfl_id"))

joinedoutputs479 <- joinedoutputs479 %>%
  filter(targeted_defender == "TRUE")


final_frame_df <- joinedoutputs479 %>%
  group_by(game_id, play_id) %>%
  slice_max(order_by = frame_id, n = 1, with_ties = FALSE) %>%
  ungroup()

distances <- final_frame_df %>%
  left_join(supdata, by = c("game_id", "play_id"))

colnames(supdata)

distances <- distances %>%
  filter(!grepl("fumble", play_description, ignore.case = TRUE)) %>%
  filter(pass_result == "C", is.na(penalty_yards))%>%
  mutate(
    yac = yards_gained - pass_length,
    final_distance = sqrt((x - defenderX)^2 +
                            (y - defenderY)^2)
  ) %>%
  filter(yac > 0, final_distance < 40, pass_length <= 10)
  

observe <- distances %>%
  select(player_name.x, player_name.y, x, y, defenderX, defenderY, yac, final_distance, )

model <- lm(yac ~ final_distance, data = distances)
summary(model)

library(ggplot2)

ggplot(distances, aes(final_distance, yac)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Relationship Between Defender Distance at Final Frame and YAC",
    x = "Final Defender–Receiver Distance",
    y = "Yards After Catch"
  )


