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
library(reshape2)
library(segmented)

analysis <- FALSE

# Load the data
load("data/analysis.Rda")

# Filter the country observations
analysis <- analysis[analysis$LocTypeName=="Country/Area", ]

## 1. Breakpoint analysis -------------------


if (analysis) {
  
  # Create a list that contains data for each country
  analysis_list <- split(analysis, by="region")
  
  # Make the breakpoint analysis
  breakpoint_results <- lapply(analysis_list, breakpoint_analysis)
  
  # Save the breakpoint results
  save(breakpoint_results, file="data/breakpoint_results_list.Rda")
  
  
  # Create the columns
  analysis[, c("nr_breakpoints", paste("breakpoints", 1:4, sep="_"), paste("slopes", 1:5, sep="_"))] <- NA
  
  # Analysis
  analysis <- as.data.frame(analysis)
  
  ## Extract the results
  for (country in unique(analysis$region)) {
    
    print(country)
    
    # Breakpoint list
    bp_list <- breakpoint_results[[country]]
    
    if (is.list(bp_list)) {
    
    # Extract the number of breakpoints
    bps <- bp_list$best_model
    
    # If BPs > 4, set to 4
    if (bps>4)  bps <- 4
    
    # Assign the number of break points in the best model
    analysis$nr_breakpoints[analysis$region==country] <- bps
    
    # Extract the breakpoints and slopes
    breakpoints <- bp_list$breakpoints[[bps]]
    slopes <- bp_list$slopes[[bps]]$HLI
    
    # Assign the results
    for (i in 1:bps) {
    analysis[analysis$region==country, paste("breakpoints", i, sep="_")] <- breakpoints[i, 1]
    analysis[analysis$region==country, paste("slopes", i, sep="_")] <- slopes[i, 1]
    }
    analysis[analysis$region==country, paste("slopes", i+1, sep="_")] <- slopes[i+1, 1]
    
    
    }
  }
  
  
  # Extract the number of break points
  nr_breakpoints <- do.call(rbind, lapply(breakpoint_results, FUN = function(bp) if(is.list(bp)){ bp$best_model} else {NA}))
  
  # Obtain the breakpoints
  breakpoints <- lapply(breakpoint_results, FUN = function(bp) if(is.list(bp)){ bp$breakpoints } else {})
  slopes <- lapply(breakpoint_results, FUN = function(bp) if(is.list(bp)){ bp$slopes } else {})
  
  # Save the individual results
  save(nr_breakpoints, breakpoints, slopes, file="data/breakpoint_results.Rda")
  
  # Save the analysis data
  save(analysis, file="data/analysis_results.Rda")

} else {
  
  load("data/analysis_results.Rda")
  load("data/breakpoint_results.Rda")
}

# Create a decade column
analysis$decade <- create_decade(analysis$Year)

# Plot and cluster by number of breakpoints
plot_breakpoints <- analysis |> 
  group_by(region, decade, SDGRegName, nr_breakpoints) |> 
  summarise(TFR=mean(TFR), HLI=mean(HLI), .groups = "drop") |> 
  ggplot(aes(x=HLI, y=TFR, group=region, colour=decade)) +
  #geom_hline(yintercept=2.1, linetype="dashed") +
  #geom_vline(xintercept=76.82, linetype="dashed") +
  geom_point(alpha=0.8, shape=16, size=0.9) +
  geom_line(alpha=1) + 
  facet_wrap(~ nr_breakpoints) +
  scale_x_continuous(expand = c(0, 0), n.breaks=10) +
  scale_y_continuous("TFR (decade average)", n.breaks=8) +
  scale_colour_brewer("Decade", palette = "YlGnBu") #+
  #annotate(geom="text", x=76.82, y=8, hjust="right", label="Inversion point:\n HLI=76.82", family="serif") +
  #annotate(geom="text", x=10, y=2.1, vjust="bottom", hjust="left", label="Replacement TFR=2.1", family="serif") 

## Plot the break points
breakpoints <- analysis |> 
  dplyr::select(region, tidyselect::starts_with("breakpoints_"), nr_breakpoints) |> 
  pivot_longer(cols=starts_with("breakpoints_"), names_to="breakpoints", names_prefix="breakpoints_", values_to="HLI") |> 
  filter(!is.na(HLI)) |> 
  unique()

# Add the breakpoints to the plot
plot_breakpoints +
  geom_linerange(data=breakpoints, aes(x=HLI, linetype=breakpoints), ymin=0, ymax=2.1, alpha=0.5)

slopes <- analysis |> 
  dplyr::select(region, starts_with("slopes_"), tidyselect::starts_with("breakpoints_"), nr_breakpoints) |> 
  pivot_longer(cols=starts_with("slopes_"), names_to="slopes", names_prefix="slopes_", values_to="slope") |> 
  filter(!is.na(slope)) |> 
  unique()

stop()

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