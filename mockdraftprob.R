library(PlackettLuce)
library(dplyr)
library(tidyverse)
library(mclust)
library(readxl)

raw_data <- read.csv("three_round_mocks.csv")

consensus <- read.csv("479 NFL Team Needs - Sheet2 (1).csv")


consensus$Player[consensus$Player == "Quenten Nelson"] <- "Quenton Nelson"
consensus$Player[consensus$Player == "Lamar Jackson"] <- "Lamar Jackson (LOU)"
consensus$Player[consensus$Player == "Josh Allen"] <- "Josh Allen (WYO)"
consensus$Player[consensus$Player == "Mo Hurst"] <- "Maurice Hurst"
consensus$Player[consensus$Player == "DJ Moore"] <- "D.J. Moore"
consensus$Player[consensus$Player == "Lamar Jackson"] <- "Lamar Jackson (LOU)"

raw_data <- raw_data %>%
  left_join(consensus, by = c("name" = "Player"))

raw_data <- raw_data %>%
  mutate(
    premium = ifelse(position %in% c("QB", "WR", "OT", "DE", "CB"), 1, 0),
    qb = ifelse(position == "QB", 1, 0)
  )

expert_data <- raw_data %>%
  filter(type == "Expert", !is.na(Consensus.Rank))

ranking_matrix <- expert_data %>%
  select(site, date, name, pick, url) %>%
  pivot_wider(names_from = name, values_from = pick) %>%
  select(-all_of(c("date", "url", "site"))) %>%
  mutate_all(~replace(., lengths(.)==0, NA)) %>%
  as.matrix()

item_names <- colnames(ranking_matrix)
features_item <- expert_data %>%
  distinct(name, position, qb, Consensus.Rank) %>%   
  filter(name %in% item_names) %>%      
  right_join(tibble(name = item_names), by = "name") %>%
  filter(!is.na(Consensus.Rank))


mock_rankings <- as.rankings(x = ranking_matrix)
#Base Plackett Luce with no covariates
fit <- PlackettLuce(rankings = mock_rankings)
lambda_hat <- coef(fit)
print(lambda_hat)

#Plackett Luce with binary variable checking whether or not player is QB, and their rank on the consensus big board

standardPL <- pladmm(mock_rankings, ~qb + Consensus.Rank, data = features_item)


lambda_hat <- itempar(standardPL)

print(lambda_hat)

summary(standardPL)

players <- names(lambda_hat)

players  

n_sims <- 1e5
results <- array(dim = c(n_sims, 32))

for(r in 1:n_sims){
  set.seed(479 + 3*r)
  
  selected_players <- rep(NA, times = 32)
  available_players <- players
  
  for(i in 1:32){
    available_players <- players[!players %in% selected_players]
    probs <- (x = lambda_hat[available_players])
    names(probs) <- available_players
    pick <- sample(x = available_players, size = 1, prob = probs)
    selected_players[i] <- pick
  }
  results[r,] <- selected_players
}

results_df <- as.data.frame(results)
colnames(results_df) <- paste0("Pick_", 1:32)

long_results <- results_df |>
  mutate(Simulation = 1:n()) |>
  pivot_longer(cols = starts_with("Pick_"),
               names_to = "Pick",
               values_to = "Player") |>
  mutate(Pick = as.integer(gsub("Pick_", "", Pick)))

mean_pick_df <- long_results |>
  group_by(Player) |>
  summarise(
    Mean_Pick = mean(Pick, na.rm = TRUE),
    Median_Pick = median(Pick, na.rm = TRUE),
    SD_Pick = sd(Pick, na.rm = TRUE)
  ) |>
  arrange(Mean_Pick)

resultscheck <- mean_pick_df %>%
  left_join(consensus, by = "Player")



player_picked <- function(x, player, pick){
  return(player %in% x[1:pick])
}

mult_players_picked <- function(x, players, pick){
  return(any(players %in% x[1:pick]))
}

players_all_picked <- function(x, players, pick){
  return(all(players %in% x[1:pick]))
}
players_all_picked(x = results[1,], player = c("Sam Darnold", "Josh Rosen", "Baker Mayfield"), pick = 5)

allqbs <- apply(X = results, MARGIN = 1, FUN = players_all_picked, player = c("Sam Darnold", "Josh Rosen", "Baker Mayfield"), pick = 5)
mean(allqbs)

saquon_pick10 <- apply(X = results, MARGIN = 1, FUN = player_picked, player = "Saquon Barkley", pick = 10)
N <- sum(!saquon_pick10)
D <- sum(!saquon_pick3)

cat("N = ", N, " D = ", D, "\n")

cat("P(Saquon available at pick 11 | Saquon not taken in top-3) = ", round(100*N/D, digits = 3), "%\n")








