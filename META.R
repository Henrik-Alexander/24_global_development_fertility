##
# Project: Development and fertility at the global scale
# Purpose: Meta
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 15.07.2025
######

# Structure
# 1. Create the folder structure
# 2. Run the code files


# Set the seed
set.seed(1789)

# 0. Install the packages --------------------------------

# Packages to install
packages <- c("traj", "clustra", "tidyverse", "data.table", "segmented", "readxl", "zoo", "svglite")
install.packages(packages)

# 1. Create the folder structure -------------------------

# Folders
folders <- c("data", "raw", "code", "figures", "tables")
lapply(folders, FUN = function(x) {
  if (!dir.exists(x)) dir.create(x)
})


# 2. Run data preparation ============================

source("code/01_data_preparation.R")
source("code/02_descriptives.R")
source("code/03_clustering.R")

### END ###############################################