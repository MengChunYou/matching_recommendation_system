# Author：Wei-Jye Goy

# import packages
library(tidyverse)
library(httr)
library(jsonlite)
library(igraph)
library(magrittr)

# THE ANIMAJOR ID
TAM1_url <-
  "https://api.opendota.com/api/proMatches?less_than_match_id=6040802788"
TAM1. <- fromJSON(TAM1_url)
TAM1 <-
  TAM1.[which(TAM1.$league_name == c("WePlay Kyiv Major 2021")), ]

TAM2_url <-
  "https://api.opendota.com/api/proMatches?less_than_match_id=6036563229"
TAM2. <- fromJSON(TAM2_url)
TAM2 <-
  TAM2.[which(TAM2.$league_name == c("WePlay Kyiv Major 2021")), ]

TAM3_url <-
  "https://api.opendota.com/api/proMatches?less_than_match_id=6029392023"
TAM3. <- fromJSON(TAM3_url)
TAM3 <-
  TAM3.[which(TAM3.$league_name == c("WePlay Kyiv Major 2021")), ]

TAM4_url <-
  "https://api.opendota.com/api/proMatches?less_than_match_id=6024546902"
TAM4. <- fromJSON(TAM4_url)
TAM4 <-
  TAM4.[which(TAM4.$league_name == c("WePlay Kyiv Major 2021")), ]

TAM_ID <- c(TAM1$match_id, TAM2$match_id, TAM3$match_id, TAM4$match_id)

## NULL pickbans Data
TAM_ID <- TAM_ID[-c(41, 42, 136, 139)]
TAM_ID <- TAM_ID[-c(108, 134)]
TAM_ID <- TAM_ID[-c(131)]

## CN DPC UPPER 
DPC_CN <- read.csv("./data/raw/matches/DPC_CN.csv")
DPC_CN <- DPC_CN$match_id
DPC_CN <- DPC_CN[-c(28)]

## SA DPC 
DPC_SA <- read.csv("./data/raw/matches/DPC_SA.csv")
DPC_SA <- DPC_SA$match_id
DPC_SA <- DPC_SA[-c(6, 64)]

## NA DPC
DPC_NA <- read.csv("./data/raw/matches/DPC_NA.csv")
DPC_NA <- DPC_NA$match_id

## EU DPC
DPC_EU <- read.csv("./data/raw/matches/DPC_EU.csv")
DPC_EU <- DPC_EU$match_id
DPC_EU <- DPC_EU[-c(45)]

## SEA DPC
DPC_SEA <- read.csv("./data/raw/matches/DPC_SEA.csv")
DPC_SEA <- DPC_SEA$match_id
DPC_SEA <- DPC_SEA[-c(73)]

## WE DPC
DPC_WE <- read.csv("./data/raw/matches/DPC_WE.csv")
DPC_WE <- DPC_WE$match_id
DPC_WE <- DPC_WE[-c(37, 42, 47)]

## DPC SEASON2
DPC_S2 <- c(DPC_CN, DPC_NA, DPC_EU, DPC_SA, DPC_SEA, DPC_WE, TAM_ID)

# GET TOURNAMENT DATA
Dota_url <- c()
DOTA <- function(ID) {
  Dota <- vector("list", length(ID))
  BP <- vector("list", length(ID))
  Win <- vector("list", length(ID))
  Result <- vector("list", length(ID))
  Radiant_ID <- vector("list", length(ID))
  Dire_ID <- vector("list", length(ID))
  Radiant_Name <- vector("list", length(ID))
  Dire_Name <- vector("list", length(ID))
  BP_list <- vector("list", length(ID))
  kda <- vector("list", length(ID))
  order <- vector("list", length(ID))
  D <- vector("list", length(ID))
  K <- vector("list", length(ID))
  A <- vector("list", length(ID))
  draft_timing <- vector("list", length(ID))
  
  for (i in 1:length(ID)) {
    Dota_url[i] <-
      paste("https://api.opendota.com/api/matches/",
            ID[i],
            "?api_key=YOUR-API-KEY",
            sep = "")
    Dota[[i]] <- fromJSON(Dota_url[i])
    BP[[i]] <- Dota[[i]]$picks_bans
    Win[[i]] <- Dota[[i]]$radiant_win
    Radiant_ID[[i]] <- Dota[[i]]$radiant_team_id
    Dire_ID[[i]] <- Dota[[i]]$dire_team_id
    Radiant_Name[[i]] <- Dota[[i]]$radiant_team$name
    Dire_Name[[i]] <- Dota[[i]]$dire_team$name
    D[[i]] <- Dota[[i]]$players$deaths %>% as.numeric()
    K[[i]] <- Dota[[i]]$players$kills %>% as.numeric()
    A[[i]] <- Dota[[i]]$players$assists %>% as.numeric()
    draft_timing[[i]] <- Dota[[i]]$draft_timings
    BP[[i]]$victory <- c(1:nrow(BP[[i]]))
    BP[[i]]$team_id <- c(1:nrow(BP[[i]]))
    BP[[i]]$team_name <- c(1:nrow(BP[[i]]))
    BP[[i]]$kda <- c(1:nrow(BP[[i]]))
    
    order[[i]] <-
      draft_timing[[i]][which(draft_timing[[i]]$pick == TRUE),]
    
    for (u in 1:nrow(BP[[i]])) {
      if (Win[[i]] == TRUE && BP[[i]][u, ]$team == 0) {
        BP[[i]][u, ]$victory <- 1
      } else if (Win[[i]] == FALSE && BP[[i]][u, ]$team == 1) {
        BP[[i]][u, ]$victory <- 1
      } else if (Win[[i]] == FALSE && BP[[i]][u, ]$team == 0) {
        BP[[i]][u, ]$victory <- 0
      } else if (Win[[i]] == TRUE && BP[[i]][u, ]$team == 1) {
        BP[[i]][u, ]$victory <- 0
      }
      if (BP[[i]][u, ]$team == 0) {
        BP[[i]][u, ]$team_id <- Radiant_ID[[i]]
        BP[[i]][u, ]$team_name <- Radiant_Name[[i]]
      } else if (BP[[i]][u, ]$team == 1) {
        BP[[i]][u, ]$team_id <- Dire_ID[[i]]
        BP[[i]][u, ]$team_name <- Dire_Name[[i]]
      }
    }
    for (m in 1:10) {
      if (D[[i]][m] > 0) {
        kda[[i]][m] <- (K[[i]][m] + A[[i]][m]) / D[[i]][m]
      } else{
        kda[[i]][m] <- (K[[i]][m] + A[[i]][m])
      }
    }
    kda[[i]] <- kda[[i]][order[[i]]$player_slot + 1]
    BP_list[[i]] <- BP[[i]][which(BP[[i]]$is_pick == TRUE),]
    BP_list[[i]]$kda <- kda[[i]]
  }
  return(BP_list)
}
# BP_list <- DOTA(DPC_S2)
# save(BP_list, file = "./data/raw/picked_heros/BP_list.Rdata")
load("./data/raw/picked_heros/BP_list.Rdata")


# TEAMS WEIGHT & HEROES
pts <- read.csv("./data/raw/team_points/Points.csv")
colnames(pts) <- c("Teams", "Points")
pts$rating <- pts$Points
pts$Teams <- as.character(pts$Teams)
pts$Teams[27] <- c("Infamous Gaming")
pts$Teams[36] <- c("sadboys")
pts$Teams[50] <- c("Lilgun ")
pts$Teams[52] <- c("Ωmega Esports")
pts$Teams[60] <- c("Winstrike Team")

hero_url <- "https://api.opendota.com/api/heroes"
hero <- fromJSON(hero_url)
hero <-
  data.frame(
    "id" = as.character(hero$id),
    "name" = as.character(hero$localized_name),
    "Attr" = hero$primary_attr
  )
hero_name <- as.character(hero$name)

# Weight (Winning)

## Only Winning Team (LIST)
Win_list <- vector("list", length(DPC_S2))
WIN <- function(WIN) {
  for (i in 1:length(WIN)) {
    Win_list[[i]] <- WIN[[i]][which(WIN[[i]]$victory == 1), ]
  }
  return(Win_list)
}
Win_list <- WIN(BP_list)

## Only Winning Team Name (Character)
Win_team <- c(1:length(DPC_S2))
WT <- function(WT) {
  for (i in 1:length(WT)) {
    for (u in 1:nrow(WT[[i]])) {
      Win_team[i] <- WT[[i]][u, ]$team_name
    }
  }
  return(Win_team)
}
Win_team <- WT(Win_list)


## Winning Team Weight
Win_Rating <- c(1:length(Win_team))
WR <- function(W) {
  for (i in 1:length(W)) {
    Win_Rating[i] <- pts$rating[which(pts$Teams == W[i])]
  }
  return(Win_Rating)
}
Win_Rating <- WR(Win_team)

# Weight (Losing)

## Only Losing Team (LIST)
Lose_list <- vector("list", length(DPC_S2))
Lose <- function(LOSE) {
  for (i in 1:length(LOSE)) {
    Lose_list[[i]] <- LOSE[[i]][which(LOSE[[i]]$victory == 0), ]
  }
  return(Lose_list)
}
Lose_list <- Lose(BP_list)

## Only Winning Team Name (Character)
Lose_team <- c(1:length(DPC_S2))
LT <- function(LT) {
  for (i in 1:length(LT)) {
    for (u in 1:nrow(LT[[i]])) {
      Lose_team[i] <- LT[[i]][u, ]$team_name
    }
  }
  return(Lose_team)
}
Lose_team <- WT(Lose_list)

## Winning Team Weight
Lose_Rating <- c(1:length(Lose_team))
LR <- function(L) {
  for (i in 1:length(L)) {
    Lose_Rating[i] <- pts$rating[which(pts$Teams == L[i])]
  }
  return(Lose_Rating)
}
Lose_Rating <- LR(Lose_team)

Weight1 <- Win_Rating - Lose_Rating
Weight2 <- Weight1 - min(Weight1)
Weight3 <- Weight2/max(Weight2)
Weight <- 1/(Weight3+1)

# KDA Weight
KDA_WIN <- c(1:length(DPC_S2))
for (i in 1:length(DPC_S2)) {
  KDA_WIN[i] <- sum(Win_list[[i]]$kda)
}
KDA_LOSE <- c(1:length(DPC_S2))
for (i in 1:length(DPC_S2)) {
  KDA_LOSE[i] <- 1 / sum(Lose_list[[i]]$kda)
}

KDA <- KDA_WIN - KDA_LOSE
KDA <- KDA/max(KDA)

# Tournament Winning Matrix

## Tournament Matrix
M_Win <-
  matrix(0, length(hero$id), length(DPC_S2)) %>% as.data.frame()
row.names(M_Win) <- hero$id

for (i in 1:length(DPC_S2)) {
  colnames(M_Win)[i] <- Win_list[[i]]$match_id[1] %>% as.character()
  M_Win[c(as.character(Win_list[[i]]$hero_id)), Win_list[[i]]$match_id[1] %>% as.character()] = 1
}

M_Win2 <- M_Win
for (i in 1:length(DPC_S2)) {
  M_Win2[, i] <-
    M_Win[, i] * Weight[i] * 0.4 + M_Win[, i] * KDA[i] * 0.2
}
M_Win <- M_Win %>% as.matrix()
M_Win2 <- M_Win2 %>% as.matrix()

input_win_matrix = M_Win2 %*% t(M_Win)
for (i in hero$id) {
  input_win_matrix[i, i] = 0
}

# Tournament Total
M_Win_WW <- M_Win
for (i in 1:length(DPC_S2)) {
  M_Win_WW[, i] <- M_Win[, i]
}
M_Win <- M_Win %>% as.matrix()
M_Win_WW <- M_Win %>% as.matrix()

input_win_ww_matrix = M_Win_WW %*% t(M_Win)
for (i in hero$id) {
  input_win_ww_matrix[i, i] = 0
}

M_Lose <- matrix(0,length(hero$id),length(DPC_S2)) %>% as.data.frame()
row.names(M_Lose) <- hero$id

for(i in 1:length(DPC_S2)){
  colnames(M_Lose)[i] <- Lose_list[[i]]$match_id[1] %>% as.character()
  M_Lose[c(as.character(Lose_list[[i]]$hero_id)),Lose_list[[i]]$match_id[1] %>% as.character()] = 1
}

M_Lose_WW <- M_Lose
for (i in 1:length(DPC_S2)) {
  M_Lose_WW[, i] <- M_Lose[, i]
}
M_Lose <- M_Lose %>% as.matrix()
M_Lose_WW <- M_Lose_WW %>% as.matrix()

input_lose_ww_matrix = M_Lose_WW %*% t(M_Lose)
for (i in hero$id) {
  input_lose_ww_matrix[i, i] = 0
}

# Tournament IGRAPH & Edgelist
HWN <- graph.adjacency(adjmatrix = input_win_matrix,
                       mode = "upper",
                       weighted = TRUE)

V(HWN)$name <- hero_name
win_edgelist <- as_edgelist(HWN) %>% as.data.frame()
win_edgelist$weight <- E(HWN)$weight 

input_total_matrix <- input_win_ww_matrix/( input_lose_ww_matrix + input_win_ww_matrix)
input_total_matrix[is.nan(input_total_matrix)] <- 0
HTN <- graph.adjacency(adjmatrix = input_total_matrix,
                       mode = "upper",
                       weighted = TRUE)
V(HTN)$name <- hero_name
total_edgelist <- as_edgelist(HTN) %>% as.data.frame()
total_edgelist$win_rate <- E(HTN)$weight

total_edgelist$score <- total_edgelist$win_rate*0.4 + win_edgelist$weight