# main_script.R

# Set working directory to source file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# 1: Collect heroes used in matches along with win-loss outcomes.
source("src/data_collecting.R")

# 2: Calculate matching scores for paired heroes based on raw data.
source("src/network_analysis.R")

# end of main_script.R