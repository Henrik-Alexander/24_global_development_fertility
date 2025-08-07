#####
# Project: Development and fertility at the global scale
# Purpose: Regression models
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 16.07.2025
######

library(segmented)
library(reshape2)
library(latex2exp)
library(tidyverse)

# Load the data
load("data/analysis.Rda")

# Load the graphics scheme
source("code/graphics.R")



### Functions =======================================

# Function to select a case
select_case <- function() {
}

df <- select_case("France")


# Estimate the fit
estimate_R2 <- function(prediction, y) {
  ss_res <- sum((y-prediction)^2)
  ss_tot <- sum((y-mean(y))^2)
  R2 <- 1 - ss_res/ss_tot
  return(R2)
}


# Compare the different regression models
regression_models <- function(country="Germany", df=analysis) {

  print(country)
  
  # Select the case
  df <- df[df$region==country, ]
  
  # Assign the values
  x <- df$HLI; y <- df$TFR
  
  # Linear regression
  mod1 <- lm(y~x)
  
  # Quadratic regression
  mod2 <- lm(y~poly(x, 2))
  
  # Cubic model
  mod3 <- lm(y~poly(x, 3))
  
  # Make a breakpoint analysis
  breakpoint_res <- breakpoint_analysis(df)
  mod4 <- segmented(mod1, npsi = ifelse(!is.list(breakpoint_res), 2, breakpoint_res$best_model))
  
  # Get the models
  models <- mget(ls(pattern="mod[1-9]"))
  
  # Get the predictions
  predictions <- lapply(models, predict, newdata=as.data.frame(x))
  
  # Create the degrees of freedom
  df_res <- sapply(models, function(model) model[["df.residual"]])
  
  # Get the fits
  fits <- sapply(predictions, estimate_R2, y=y)
  
  # Adjust the fits by the number of parameters
  fits_adjusted <- 1 - (1-fits) * (n-1) / df_res
  
  
  # Create the prediction plot
  pdf(file = paste0("figures/parametric_fits_", country, ".pdf"), height=12, width=20)
  model_names <- c("Linear", "Quadratic", "Cubic", "Breakpoint")
  plot(x, y, 
       main= paste("Model fit for", country),
       xlab="Human Life Indictor (HLI)",
       ylab="Total fertility rate (TFR)",
       pch=16)
  cols <- viridisLite::plasma(4)
  for (i in seq_along(models)) {

    lines(x=seq(min(x), max(x), length.out=200), 
          y=predict(models[[i]], newdata=data.frame(x=seq(min(x), max(x), length.out=200))),
          col=cols[i], lwd=2)
  }
  legend("topright", 
         legend=TeX(paste(model_names, ": $R^2_{adjusted}$=", round(fits_adjusted, 2))),
         lty=1, 
         col=cols,
         lwd=2)
  
  dev.off()
  
  # Bundle the results
  results <- list(fits=fits,
                  fits_adjusted = fits_adjusted,
                  models=models,
                  data=data.frame(x, y, bind_cols(predictions)),
                  best_model=which.max(fits_adjusted))
  
  return(results)

}


### Run the calculations ========================

# Store the results
regression_results <- lapply(unique(analysis$region), regression_models)
names(regression_results) <- unique(analysis$region)

# Save the results
save(regression_results, file="data/regression_results.Rda")


### END #############################################