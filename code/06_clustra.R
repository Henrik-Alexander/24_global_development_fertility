#####
# Project: Development and fertility at the global scale
# Purpose: Clustering
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 16.07.2025
######

# Structure:
# 1. Breakpoint analysis
# 1. Clustra
# 2. Traj
# 3. Singular Value Decomposition

library(clustra)
library(traj)


## 2. Cluster with clustra ------------------

if (analysis) {
  
  # Create a separate dataset for clustra
  df_clustra <- analysis
  
  # Rename columns for clustra
  setnames(df_clustra,
           old=c("region", "Year", "TFR"),
           new=c("id", "time", "response"))
  
  # Perhaps needs a year 0 -> using the minimum TFR for that
  df_clustra[, min_tfr := min(tfr), by = .(id)]
  
  # Run the clustering algorithm
  clustra(data=df_clustra, k=4, maxdf=30, conv=c(10, 0), mccores=1, verbose=TRUE)
  
}

## 2. Cluster with traj -------------------

# 


## 3. SVD ---------------------------------

# Create TFR matrices
acast(analysis[, c("TFR", "region", "Year")], TFR ~ Year ~ region, unique)

# SVD for TFRs


### END ###################################