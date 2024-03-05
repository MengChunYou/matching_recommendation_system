# matching_recommendation_system

This repository contains the final project for the [Network Data Analysis and Models](https://wenlab501.github.io/teaching.html#_8) course instructed by [Tzai-Hung WEN](https://homepage.ntu.edu.tw/~wenthung/index.htm), which took place from March to June 2021. The project focuses  on using network analysis to understand the compatibility of hero pairs and establish a hero selection recommendation system.

## Table of Contents

- [Project Abstract](#project-abstract)
- [Repository Structure](#repository-structure)
- [Required Packages](#required-packages)

## Project Abstract

Dota 2 is a multiplayer online battle arena game, and its market value is reflected in player count, e-sports competitions, and the secondary market for in-game hero cosmetics. In the game, players often struggle to pick heroes for matches, and previous research on hero recommendations or popularity metrics has not considered the compatibility of heroes within the same matches. This study employs network analysis to understand the compatibility of hero pairs, considering factors such as win rate, team strength differentials, and team performance differentials. Additionally, Betweenness Centrality is used to measure the popularity of heroes within the hero network. Our findings have practical implications for professional players, game companies, and non-players. Particularly, we establish a hero selection recommendation system using R Shiny Dashboard, which enables professional players to quickly find suitable heroes to play together during matches.

## Repository Structure

```plaintext
root/
 ├── data/                       
 │    ├── raw/
 │    │    ├── team_strength_weight
 │    │    └── KDA_weight
 │    └── processed/
 │         ├── unweighted/
 │         │    ├── win_pair.csv
 │         │    ├── lose_pair.csv
 │         │    ├── total_pair.csv
 │         │    └── win_rate_pair.csv
 │         ├── team_strength_weighted/
 │         │    └── ...
 │         ├── KDA_weighted/
 │         │    └── ...
 │         └── integrated/
 │              └── paired_matching_scores.csv
 │
 ├── src/ 
 │    ├── common_functions.R 
 │    ├── data_collecting.R
 │    ├── data_processing.R
 │    └── visualization.R
 │
 ├── app/
 │    ├── hero.Rdata
 │    ├── edgelist.Rdata
 │    ├── server.R
 │    └── ui.R
 │
 ├── outputs/
 │    └── r_shiny_dashboard_link.txt
 │
 ├── reports/                    
 │    ├── report.pdf
 │    └── slides.pdf
 │
 ├── main_script.R
 ├── README.md               
 └── .gitignore   
```

- `reports/`: This directory contains presentation slides and report written in Traditional Chinese.
- `outputs/r_shiny_dashboard_link.txt`: This directory contains a link to the hero selection recommendation system built with R Shiny Dashboard.
- 
## Required Packages

- igraph
- dplyr
- shiny
- egor

Download the required packages by running the following command in R:

``` plaintext
install.packages(c("igraph", "dplyr", "shiny", "egor"))
```
