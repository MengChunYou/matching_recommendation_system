# Import packages
library(tidyverse)
library(igraph)
library(magrittr)

# Load data
load("./data/raw/picked_heros.Rdata")
team_points <- read.csv("./data/raw/team_points.csv")
n_matches <- length(picked_heros)

# Picked heroes in winning teams
heros_in_winning_teams <- vector("list", n_matches)
query_win <- function(records) {
  for (i in 1:length(records)) {
    heros_in_winning_teams[[i]] <- records[[i]][which(records[[i]]$victory == 1), ]
  }
  return(heros_in_winning_teams)
}
heros_in_winning_teams <- query_win(picked_heros)

# Picked heroes in losing teams
heros_in_losing_teams <- vector("list", n_matches)
query_lose <- function(records) {
  for (i in 1:length(records)) {
    heros_in_losing_teams[[i]] <- records[[i]][which(records[[i]]$victory == 0), ]
  }
  return(heros_in_losing_teams)
}
heros_in_losing_teams <- query_lose(picked_heros)

# Names of winning teams (Character)
win_teams <- c(1:n_matches)
WT <- function(WT) {
  for (i in 1:length(WT)) {
    for (u in 1:nrow(WT[[i]])) {
      win_teams[i] <- WT[[i]][u, ]$team_name
    }
  }
  return(win_teams)
}
win_teams <- WT(heros_in_winning_teams)

# Names of losing teams (Character)
lose_teams <- c(1:n_matches)
LT <- function(LT) {
  for (i in 1:length(LT)) {
    for (u in 1:nrow(LT[[i]])) {
      lose_teams[i] <- LT[[i]][u, ]$team_name
    }
  }
  return(lose_teams)
}
lose_teams <- WT(heros_in_losing_teams)

# Winning Team Points
win_team_points <- c(1:length(win_teams))
WR <- function(W) {
  for (i in 1:length(W)) {
    win_team_points[i] <- team_points$rating[which(team_points$Teams == W[i])]
  }
  return(win_team_points)
}
win_team_points <- WR(win_teams)

# Losing Team Weight
lose_team_points <- c(1:length(lose_teams))
LR <- function(L) {
  for (i in 1:length(L)) {
    lose_team_points[i] <- team_points$rating[which(team_points$Teams == L[i])]
  }
  return(lose_team_points)
}
lose_team_points <- LR(lose_teams)

# team_strength_weight
Weight1 <- win_team_points - lose_team_points
Weight2 <- Weight1 - min(Weight1)
Weight3 <- Weight2/max(Weight2)
team_strength_weight <- 1/(Weight3+1)
save(team_strength_weight, file = "./data/weight/team_strength_weight.Rdata")

# KDA Weight
KDA_WIN <- c(1:n_matches)
for (i in 1:n_matches) {
  KDA_WIN[i] <- sum(heros_in_winning_teams[[i]]$kda)
}
KDA_LOSE <- c(1:n_matches)
for (i in 1:n_matches) {
  KDA_LOSE[i] <- 1 / sum(heros_in_losing_teams[[i]]$kda)
}

KDA <- KDA_WIN - KDA_LOSE
KDA_weight <- KDA/max(KDA)
save(KDA_weight, file = "./data/weight/KDA_weight.Rdata")

# Paired Win Rate
M_Win <-
  matrix(0, length(hero$id), n_matches) %>% as.data.frame()
row.names(M_Win) <- hero$id

for (i in 1:n_matches) {
  colnames(M_Win)[i] <- Win_list[[i]]$match_id[1] %>% as.character()
  M_Win[c(as.character(Win_list[[i]]$hero_id)), Win_list[[i]]$match_id[1] %>% as.character()] = 1
}

M_Win2 <- M_Win
for (i in 1:n_matches) {
  M_Win2[, i] <-
    M_Win[, i] * team_strength_weight[i] * 0.4 + M_Win[, i] * KDA_weight[i] * 0.2
}
M_Win <- M_Win %>% as.matrix()
M_Win2 <- M_Win2 %>% as.matrix()

input_win_matrix = M_Win2 %*% t(M_Win)
for (i in hero$id) {
  input_win_matrix[i, i] = 0
}

M_Win_WW <- M_Win
for (i in 1:n_matches) {
  M_Win_WW[, i] <- M_Win[, i]
}
M_Win <- M_Win %>% as.matrix()
M_Win_WW <- M_Win %>% as.matrix()

input_win_ww_matrix = M_Win_WW %*% t(M_Win)
for (i in hero$id) {
  input_win_ww_matrix[i, i] = 0
}

M_Lose <- matrix(0,length(hero$id),n_matches) %>% as.data.frame()
row.names(M_Lose) <- hero$id

for(i in 1:n_matches){
  colnames(M_Lose)[i] <- Lose_list[[i]]$match_id[1] %>% as.character()
  M_Lose[c(as.character(Lose_list[[i]]$hero_id)),Lose_list[[i]]$match_id[1] %>% as.character()] = 1
}

M_Lose_WW <- M_Lose
for (i in 1:n_matches) {
  M_Lose_WW[, i] <- M_Lose[, i]
}
M_Lose <- M_Lose %>% as.matrix()
M_Lose_WW <- M_Lose_WW %>% as.matrix()

input_lose_ww_matrix <- M_Lose_WW %*% t(M_Lose)
for (i in hero$id) {
  input_lose_ww_matrix[i, i] = 0
}

# Calculate integrated matching scores for paired heroes
HWN <- graph.adjacency(adjmatrix = input_win_matrix,
                       mode = "upper",
                       weighted = TRUE)

V(HWN)$name <- hero_name
win_edgelist <- as_edgelist(HWN) %>% as.data.frame()
win_edgelist$weight <- E(HWN)$weight

input_total_matrix <- input_win_ww_matrix/(input_lose_ww_matrix + input_win_ww_matrix)
input_total_matrix[is.nan(input_total_matrix)] <- 0
HTN <- graph.adjacency(adjmatrix = input_total_matrix,
                       mode = "upper",
                       weighted = TRUE)
V(HTN)$name <- hero_name
paired_matching_scores <- as_edgelist(HTN) %>% as.data.frame()
paired_matching_scores$win_rate <- E(HTN)$weight

paired_matching_scores$score <- paired_matching_scores$win_rate*0.4 + win_edgelist$weight
save(paired_matching_scores, "./app/paired_matching_scores.Rdata")