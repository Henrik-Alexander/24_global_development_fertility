#####
# Project: Development and fertility at the global scale
# Purpose: Descriptives
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 16.07.2025
######

library(tidyverse)
library(data.table)
library(zoo)

options(scipen=999)

# Load the data
load("data/analysis.Rda")

# Load the graphics
source("code/graphics.R")

# Load the helper functions
source("code/functions.R")

## Functions ------------------------------

descr_univariate <- function(variable="TFR", data=analysis) {
  
  summary <- list(
       mean = mean(data[[variable]], na.rm=T),
       sd = sd(data[[variable]], na.rm=T),
       min = min(data[[variable]], na.rm=T),
       Q1 = quantile(data[[variable]], probs=0.25, na.rm=T),
       Q3 = quantile(data[[variable]], probs=0.75, na.rm=T),
       max = max(data[[variable]]),
       NAs = sum(is.na(data[[variable]]))
       )
  
  print(summary)
  
  
  # Plot a histogram
  summary_plot <- ggplot(data=data, aes_string(x=variable)) +
    geom_histogram(fill="white", colour="black") +
    geom_vline(aes(xintercept=summary$mean)) +
    geom_vline(aes(xintercept=summary$Q1), linetype="dotted") +
    geom_vline(aes(xintercept=summary$Q3), linetype="dotted")
  
  print(summary_plot)
  
}


# Univariate ------------------------------

# Describe the TFR
descr_univariate()

# Describe the HLI
descr_univariate(variable="HLI")

# Check the TFR results
ggplot(subset(analysis, LocTypeName=="Country/Area"), aes(x=Year, y=TFR, group=region)) + 
  geom_line(alpha=0.3) + 
  facet_wrap(~SDGRegName) +
  scale_x_continuous(expand = c(0, 0))

# Check the TFR results
ggplot(subset(analysis, LocTypeName=="Country/Area"), aes(x=HLI, y=TFR, group=region)) + 
  geom_line(alpha=0.3) + 
  facet_wrap(~SDGRegName) +
  scale_x_continuous(expand = c(0, 0))

# Create a decade column
analysis$decade <- create_decade(analysis$Year)

# Plot the decade averages
analysis[, .(HLI=mean(HLI), TFR=mean(TFR)), by=.(region, decade, SDGRegName)] |> 
  ggplot(aes(x=HLI, y=TFR, group=region,colour=decade)) + 
  geom_hline(yintercept=2.1, linetype="dashed") +
  geom_vline(xintercept=76.82, linetype="dashed") +
  geom_point(alpha=0.8, shape=16, size=0.9) +
  geom_line(alpha=1) + 
  facet_wrap(~SDGRegName) +
  scale_x_continuous(expand = c(0, 0), n.breaks=10) +
  scale_y_continuous("TFR (decade average)", n.breaks=8) +
  scale_colour_brewer("Decade", palette = "YlGnBu") +
  annotate(geom="text", x=76.82, y=8, hjust="right", label="Inversion point:\n HLI=76.82", family="serif") +
  annotate(geom="text", x=10, y=2.1, vjust="bottom", hjust="left", label="Replacement TFR=2.1", family="serif")
ggsave(filename="figures/relationship_HLI_TFR_decades.pdf", height=17, width=25, unit="cm")

# Create the main plot
k <- 10
window <- rep(k, length(unique(analysis$Year)))
window[1:k] <- 1:k
window[length(window):(length(window)-k+1)] <- 1:k

## Plot smoothed rates =======================================
analysis[order(region, Year), c("HLI_smooth", "TFR_smooth") := lapply(.SD, frollmean, n=5, fill=NA, align="center", adaptive=FALSE), .SDcols=c("HLI", "TFR"), by=.(region)]
ggplot(subset(analysis, LocTypeName=="Country/Area"), aes(x=HLI, y=TFR, group=region)) +
  geom_point(alpha=0.3, size=0.5) +
  geom_smooth(method="loess", se=F, linewidth=0.5, colour="black") +
  facet_wrap(~SDGRegName) +
  scale_y_continuous("Total fertility rate (smoothed)", n.breaks=10) +
  scale_x_continuous("Human life indicator (smoothed)", n.breaks=10)
ggsave(file="figures/smoothed_hli_tfr_panel.pdf", height=20, width=25, unit="cm")

# Save the plot for Europe and north america
ggplot(subset(analysis, SDGRegName=="Europe and Northern America"&region!="Holy See"), aes(x=HLI, y=TFR, group=region, colour=region)) +
  geom_point(alpha=0.5, size=0.75) +
  geom_smooth(method="loess", se=F, linewidth=0.8) +
  #geom_text(data=subset(analysis, SDGRegName=="Europe and Northern America" & Year==2023&HLI>75&region!="Holy See"), aes(label=region)) +
  facet_wrap(~region) +
  scale_y_continuous("Total fertility rate (smoothed)", n.breaks=8) +
  scale_x_continuous("Human life indicator (smoothed)", n.breaks=10, expand=c(0, 0)) +
  coord_cartesian(ylim=c(1, 3.2), xlim=c(50, 85)) +
  guides(colour="none")
ggsave(file="figures/smoothed_hli_tfr_europe.pdf", height=20, width=25, unit="cm")

### END ####################################