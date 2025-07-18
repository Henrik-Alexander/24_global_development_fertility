##
# Project: Development and fertility at the global scale
# Purpose: Functions
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 15.07.2025
######


# Structure:
# 1. HLI


# 1. Human Life Indicator =======================


# Function to estimate the HLI
estimate_hli <- function(ax, dx, age=1:100, radix = 100000) {
  prod((age+ax)^(dx/radix))
}

# 2. Creaete decade column ======================

create_decade <- function(x) {
  cut_years <- seq(1950, 2030, by=10)
  cut(x, breaks=cut_years, include.lowest=T, labels = paste(cut_years[1:length(cut_years)-1], cut_years[1:length(cut_years)-1]+9, sep="-"))
}

# 3. Breakpoint analysis ========================

# Function to perform a breakpoint analysis
breakpoint_analysis <- function(df) {
  
  print(unique(df$region))
  
  # Estimate the linear model
  lm_model <- lm(TFR ~ HLI, data=df)
  
  # Find the best number of breakpoints
  nr_breakpoints <- selgmented(lm(TFR ~ HLI, data=df), Kmax=4, type="bic")
  nr_breakpoints <- nr_breakpoints$selection.psi$npsi
  
  if (nr_breakpoints>0) { 
    
    # Perform the breakpoint analysis for 0 to 4 breakpoints
    bp_result <- lapply(1:4, FUN= function(nr_breakpoints) {
      try(segmented(lm_model, npsi = nr_breakpoints))
    })
    
    # Remove those that are type lm
    bp_result <- bp_result[!(lapply(bp_result, class) == "lm")]
    
    # Estimate the slopes with confidence intervals
    slopes <- lapply(bp_result, slope)
    
    # Get the confidence intervals for the breakpoints
    breakpoints <- lapply(bp_result, confint.segmented)
    
    # Plot the result for 4 different break points
    svg(file=paste0("figures/breakpoint_analysis_", unique(df$region), ".svg"))
    par(mfrow=c(2, 2))
    lapply(bp_result, FUN = function(x) {
      plot(x, main="Breakpoint result")
      points(df$HLI, df$TFR)
    })
    dev.off()
    
    # Collect the results in a list
    bp_result_list <- list(models = bp_result,
                           best_model = nr_breakpoints,
                           slopes = slopes, 
                           breakpoints = breakpoints)
    
    return(bp_result_list)
    
  } else {
    
    # Plot the relationship
    par(mfrow=c(1, 1))
    plot(df$HLI, df$TFR,
         main = paste("Weird pattern in", unique(df$region)))
    
    return(NA)
  }
}

# 4. Create the data for the breakpoint plot --------------------

# Create the data
sim_seg <- function (model) {
  
  # Get the turning points
  I <- confint.segmented(model)
  n_tp <- nrow(I)
  
  # Get the slopes
  slopes <- slope(model)
  
  # Get the range in the data
  min_val <- min(model$model$lag_HLI_female)
  max_val <- max(model$model$lag_HLI_female)
  
  # Simulate the data
  cont <- tibble(lag_HLI_female = c(as.numeric(I[, 1]), min_val, max_val),
                 names = c(rownames(I), "min", "max"),
                 TFR_female = NA,
                 slope = NA)
  
  # Estimate the change in the predictor
  cont <- cont |> 
    arrange(lag_HLI_female) |> 
    mutate(change = dplyr::lead(lag_HLI_female) - lag_HLI_female )
  
  
  # Estimate the values
  cont[!is.na(cont$change), ]$slope <- slopes$lag_HLI_female[, 1]
  
  # Iterate
  for (i in 1:nrow(cont)) {
    if (i == 1){
      cont[1, ]$TFR_female <- 0
    } else {
      cont[i, ]$TFR_female <- cont[i-1, ]$TFR_female + cont[i-1, ]$slope * cont[i-1, ]$change
    }
  }
  
  # Select the row
  row <- which.min(abs(0 - cont$lag_HLI_female))
  
  # Adjust the height
  x1 <- cont[row, ]$lag_HLI_female
  y1 <- cont[row, ]$TFR_female
  
  # TFR 0
  if (x1 < 0) diff <-  y1 + (0 - x1) * cont[row, ]$slope
  if (x1 > 0) diff <-  y1 - (x1 - 0) * cont[row-1, ]$slope
  
  # Add to every TFR value
  cont$TFR_female <- cont$TFR_female + (0 - diff)
  
  return(cont)
}
