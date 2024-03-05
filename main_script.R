# main_script.R

# Set working directory to source file location
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

source("src/common_functions.R")

# 1: data collecting
source("src/data_collecting.R")

# 2: data processing
source("src/data_processing.R")

# 3: data visualization
source("src/visualization.R")

# end of main_script.R