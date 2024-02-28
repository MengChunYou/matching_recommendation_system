# matching_recommendation_system

## Table of Contents
- [Repository Structure](#repository-structure)

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
