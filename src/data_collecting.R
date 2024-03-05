# Import packages
library(httr)
library(jsonlite)

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
# picked_heroes <- DOTA(DPC_S2)
# save(picked_heroes, file = "./data/raw/picked_heroes.Rdata")

# heroes
hero_url <- "https://api.opendota.com/api/heroes"
hero <- fromJSON(hero_url)
heroes <-
  data.frame(
    "id" = as.character(hero$id),
    "name" = as.character(hero$localized_name),
    "Attr" = hero$primary_attr
  )
hero_name <- as.character(hero$name)
# save(heroes, file = "./app/heroes.Rdata")