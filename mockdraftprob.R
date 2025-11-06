library(PlackettLuce)
library(dplyr)
library(tidyverse)
library(mclust)
library(readxl)
library(ggthemes)
library(gtExtras)
library(gt)
library(nflreadr)

headshots <- load_player_stats(seasons = 2019, stat_type = "defen")

headshots <- headshots %>%
  filter(player_display_name %in% c("Josh Rosen", "Sam Darnold", "Baker Mayfield",
                            "Saquon Barkley", "Quenton Nelson", "Bradley Chubb",
                            "Roquan Smith", "Lamar Jackson", "Derwin James",
                            "Minkah Fitzpatrick"))
headshots$player_display_name[headshots$player_display_name == "Lamar Jackson"] <- "Lamar Jackson (WYO)"


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
probs <- exp(lambda_hat[players])/sum(exp(lambda_hat[players]))
print(probs)
rawLambdas <- data.frame(
  Name = names(lambda_hat),
  Probs = (probs)
)
#Plackett Luce with binary variable checking whether or not player is QB, and their rank on the consensus big board

standardPL <- pladmm(mock_rankings, ~qb + Consensus.Rank, data = features_item)


lambda_hat <- predict(standardPL)

print(lambda_hat)
sum(lambda_hat)

summary(standardPL)

players <- names(lambda_hat)

players  

covprobs <- exp(lambda_hat[players])/sum(exp(lambda_hat[players]))
print(covprobs)

finallambdas <- data.frame(
  Name = names(lambda_hat),
  Probs = (covprobs)
)

rownames(finallambdas) <- NULL

finallambdas <- finallambdas %>%
  arrange(desc(Probs))

displaylambdas <- finallambdas %>%
  slice_head(n = 10)



n_sims <- 1e5
results <- array(dim = c(n_sims, 32))

for(r in 1:n_sims){
  set.seed(479 + 3*r)
  
  selected_players <- rep(NA, times = 32)
  available_players <- players
  
  for(i in 1:32){
    available_players <- players[!players %in% selected_players]
    probs <- mclust::softmax(x = lambda_hat[available_players])
    names(covprobs) <- available_players
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

resultscheck <- resultscheck %>%
  left_join(finallambdas, by = c("Player" = "Name"))



player_picked <- function(x, player, pick){
  return(player %in% x[1:pick])
}

mult_players_picked <- function(x, players, pick){
  return(any(players %in% x[1:pick]))
}

players_all_picked <- function(x, players, pick){
  return(all(players %in% x[1:pick]))
}

players_all_available <- function(x, players, pick){
  return(all(!players %in% x[1:pick]))
}

player_picked_exact <- function(x, player, pick) {
  return(x[pick] == player)
}



Darnold <- apply(X = results, MARGIN = 1, FUN = player_picked, player = c("Sam Darnold"), pick = 5)
Rosen <- apply(X = results, MARGIN = 1, FUN = player_picked, player = c("Josh Rosen"), pick = 5)
Baker <- apply(X = results, MARGIN = 1, FUN = player_picked, player = c("Baker Mayfield"), pick = 5)

Pick1Qb <- apply(X = results, MARGIN = 1, FUN = mult_players_picked, player = c("Sam Darnold", "Josh Rosen", "Baker Mayfield", "Josh Allen (WYO)", "Lamar Jackson (LOU)"), pick = 1)
mean(Pick1Qb)

baker1 <- apply(X = results, MARGIN = 1, FUN = player_picked_exact, player = c("Baker Mayfield"), pick = 1)
darnold1 <- apply(X = results, MARGIN = 1, FUN = player_picked_exact, player = c("Sam Darnold"), pick = 1)

AllQBs <- apply(X = results, MARGIN = 1, FUN = players_all_available, player = c("Sam Darnold", "Josh Rosen", "Baker Mayfield"), pick = 5)
NoQBs <- apply(X = results, MARGIN = 1, FUN = players_all_picked, player = c("Sam Darnold", "Josh Rosen", "Baker Mayfield"), pick = 5)

mean(AllQBs)
mean(NoQBs)


graphDF <- data.frame(
  names = c("Baker Mayfield", "Sam Darnold", "Josh Rosen"),
  probs = c(mean(!Baker), mean(!Darnold), mean(!Rosen))
) 


totalDF <- data.frame(
  types = c("At Least 1 QB Available","All QB's Available",  "None of the QB's Available"),
  probs = c( mean(!AllQBs), mean(AllQBs), mean(NoQBs))
)



picks <- 2:5


bakerprobs <- sapply(picks, function(p) {
  subset_results <- results[darnold1, ]
  mean(apply(subset_results, 1, players_all_available, player = "Baker Mayfield", pick = p))
})

samprobs <- sapply(picks, function(p) {
  subset_results <- results[baker1, ]
  mean(apply(subset_results, 1, players_all_available, player = "Sam Darnold", pick = p))
})

bakerdf <- data.frame(
  pick = picks,
  probability = bakerprobs
)

samdf <- data.frame(
  pick = picks,
  probability = samprobs
)

library(ggplot2)
ggplot(graphDF, aes(x = reorder(names, -probs), y = probs, fill = names)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_text(aes(label = scales::percent(probs, accuracy = 0.1)),
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title = paste("Probability of QB's being available at Pick 6")
  ) +
  theme_fivethirtyeight(base_size = 14) +
  theme(axis.title = element_text()) + ylab('Probability') + xlab("QB")
  theme(legend.position = "none")


ggplot(totalDF, aes(x = reorder(types,-probs), y = probs, fill = types)) +
  geom_col(width = 0.6, alpha = 0.8) +
  geom_text(aes(label = scales::percent(probs, accuracy = 0.1)),
            vjust = -0.5, size = 4) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 1)) +
  labs(
    title = paste("Joint Probability Events for Mayfield, Darnold, Rosen")
  ) +
  theme_fivethirtyeight(base_size = 14) +
  theme(axis.title = element_text()) + ylab('Probability') + xlab("Scenario")
  theme(legend.position = "none")


  
  ggplot(samdf, aes(x = factor(pick), y = probability)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = scales::percent(probability, accuracy = 1)),
              vjust = -0.5, size = 4) +
    labs(
      title = "Probability of Sam Darnold Being Available after Picks 2-5",
      subtitle = "Conditional on Baker Mayfield being the 1st overall pick"
    ) +
    theme_fivethirtyeight() + 
    theme(axis.title = element_text()) + ylab('Probability') + xlab("Draft Pick") 
  
  
  ggplot(bakerdf, aes(x = factor(pick), y = probability)) +
    geom_col(fill = "darkgreen") +
    geom_text(aes(label = scales::percent(probability, accuracy = 1)),
              vjust = -0.5, size = 4) +
    labs(
      title = "Probability of Baker Mayfield Being Available after Picks 2-5",
      subtitle = "Conditional on Sam Darnold being the 1st overall pick"
    ) +
    theme_fivethirtyeight() +
    theme(axis.title = element_text()) + ylab('Probability') + xlab("Draft Pick")

  
  
displaylambdas <- displaylambdas %>%
  left_join(headshots, by = c("Name" = "player_display_name")) %>%
  select(Name, headshot_url, Probs) 
displaylambdas <- displaylambdas %>%
  distinct(Name, .keep_all = TRUE)
#Latent Ability Rankings
Rankings <- displaylambdas %>%
    mutate(
      Probs = 100 * round(Probs, 2)
    ) %>%
    arrange(desc(Probs)) %>%
    gt() %>%
    cols_align(align = "center") %>%
    cols_label(
      Name = "Name",
      headshot_url = "",
      Probs = "Probabilities (%)"
    ) %>%
    tab_header(
      title = "Player Rankings Using Expert Mock Drafts",
      subtitle = "Rank - Probability of Player Being Selected 1st Overall"
    ) %>%
    gtExtras::gt_theme_538()
  Rankings <- gt_hulk_col_numeric(Rankings, column = "Probs")
  Rankings <- gt_img_rows(Rankings, column = "headshot_url")
  

  Rankings
  
# Draft order chart 
final_order <- c()

for (pick in names(results_df)) {
  pick_counts <- sort(table(results_df[[pick]]), decreasing = TRUE)
  
  for (player in names(pick_counts)) {
    if (!(player %in% final_order)) {
      final_order <- c(final_order, player)
      break
    }
  }
}


draft_order_df <- data.frame(
  Pick = seq_along(final_order),
  Player = final_order,
  stringsAsFactors = FALSE
)
plot_df <- draft_order_df %>% 
  slice_head(n=10) %>% 
  left_join(mean_pick_df) %>% 
  select(Pick,Player,Mean_Pick)
plot_df$Mean_Pick = round(plot_df$Mean_Pick, 2)
plot_df %>% 
  gt() %>%
  cols_align(align = "center") %>%
  cols_label(
    Pick = "Pick",
    Player = "Player",
    Mean_Pick = "Mean Pick"
  ) %>%
  tab_header(
    title = "Top 10 picks",
  subtitle = "Based on who was picked the most at each pick"
  ) %>% 
  gtExtras::gt_theme_538()


