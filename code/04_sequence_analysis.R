#####
# Project: Development and fertility at the global scale
# Purpose: Sequence-analysis
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 16.07.2025
######

library(TraMineR)
library(cluster)

# Load the data
load("data/analysis.Rda")

# Load the graphics scheme
source("code/graphics.R")


### Functions ===================================

# 1. Create the states ==========================

# States for fertility: lowest low, low, replacement, high, very high fertility
analysis$state_fertility <- cut(analysis$TFR,
                                breaks = c(0, 1.2, 1.5, 2.1, 3, 10), 
                                labels = c("lowest low", "low", "replacement", "high", "very high"),
                                include.lowest = T)


# States for development
analysis$state_development <- cut(analysis$HLI,
                                  breaks = quantile(analysis$HLI, probs = seq(0, 1, by=0.25)),
                                  labels = seq(1:4),
                                  include.lowest = T)

# States for change in TFR
analysis <- analysis %>% 
  arrange(region, Year) %>% 
  group_by(region) %>% 
  mutate(tfr_change = TFR - lag(TFR),
         hli_change = HLI - lag(HLI))

# Plot the change
ggplot(analysis, aes(x=tfr_change, y=hli_change, group=region, colour=SDGRegName)) +
  geom_path(alpha=0.1, aes(group=region)) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  geom_point(alpha=0.2) +
  facet_wrap(~SDGRegName, scales="free")

# Create states for slopes
analysis$state_slope <- ifelse(analysis$tfr_change*analysis$hli_change>0, "positive", "negative")

# Plot the slops
ggplot(data=analysis, aes(x=HLI, y=TFR, colour=state_slope)) +
  geom_point(alpha=0.2) +
  facet_wrap(~SDGRegName)

# Plot the states
analysis %>% 
  group_by(state_development, state_slope) %>% 
  count() %>% 
  ggplot(aes(x=state_development, y=n, fill=state_slope)) +
  geom_col()

# 2. Create the sequences =======================

# Paste the data
analysis$state <- paste(analysis$state_development, analysis$state_slope)

# PIvot wider
df_seq <- pivot_wider(analysis[!is.na(analysis$state_slope), ], names_from=Year, values_from=state)

# Define the sequence data
seq_obj <- seqdef(data=df_seq, var=match(colnames(df_seq), paste(1950:2025)))

# 3. Descriptives ===============================

par(mfrow=c(2, 2))

# Get the plot legend
seqlegend(seq_obj, cex=1.3)

# Plot the first 10 sequences
seqiplot(seq_obj, main="Index plot (10 first sequences)", withlegend=F, border=NA)

# Plot the most frequent sequences
seqfplot(seq_obj, main = "Sequence frequency plot", withlegend=F, border=NA)

# Plot the state distribution by time points
seqdplot(seq_obj, main = "State distribution plot", border=NA, withlegend=F)

# Plot the entropy of the state distribution
seqHtplot(seq_obj, title = "Entropy index")


# 4. Summarize the sequences ====================

# Compute, summarize and plot the histogram of the sequence turbulences
seq_turbulance <- seqST(seq_obj)
summary(seq_turbulance)
hist(seq_turbulance, col="white", main="Sequence turbulance")

# Compute the optimal matching distances using substitatuion costs based on transition rates oberseved int he data
seq_submat <- seqsubm(seq_obj, method = "TRATE")
dist_om1 <- seqdist(seq_obj, method = "OM", indel = 1, sm= seq_submat)


# 4. Cluster the sequences ======================

# build a Ward hierarchical clustering of the sequences from the optimal matching distances
# and retrieve for each individual sequence the cluster membership of the 4 class solution
cluster_ward1 <- agnes(dist_om1, diss = TRUE, method = "ward")

# Plot the clusters
par(mfrow = c(1, 2))
plot(cluster_ward1)

# Create the trees
n_cluster <- 6
df_seq$cluster <- factor(cutree(cluster_ward1, k = n_cluster), labels = paste("Cluster", 1:n_cluster))
seqdplot(seq_obj, group = df_seq$cluster, border=NA)

# 6. Explore the clusters ========================

# Plot the state distribution at each time point within each cluster
seqdplot(seq_obj, group = df_seq$cluster, boder = NA)

# Plot the sequence frequencies within each cluster
seqfplot(seq_obj, group=df_seq$cluster, border=NA)

### END #########################################