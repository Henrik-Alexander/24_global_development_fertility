#####
# Project: Development and fertility at the global scale
# Purpose: Data preparation
# Author: Henrik-Alexander Schubert
# E-mail: schubert@demogr.mpg.de
# Date: 15.07.2025
######

library(data.table)
library(tidyverse)
library(readxl)

# Load the functions
source("code/functions.R")


# Structure
# 1. Load the life tables from WPP 2024
# 2. Estimate the Human Life Indicator

# 1. Load the life tables from WPP 2024 ------------

# World Population Prospects Life tables for both sexes (downloaded: 15.07.2025)
# https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/4_Mortality/WPP2024_MORT_F06_1_SINGLE_AGE_LIFE_TABLE_ESTIMATES_BOTH_SEXES.xlsx

# The loading is pretty slow, because data is massive (22728000 observations)
first_run <- TRUE

path_wpp24_ltb <- "data/cleaned_life_table_b.Rda"

# Load, clean and save the WPP 2025 life tables
if (first_run) {
  # Find the file for the WPP life tables
  path_wpp24_ltb_raw <- list.files("raw", pattern = "LIFE_TABLE_ESTIMATES_BOTH", full.names=TRUE)
  if (is.null(path_wpp24_ltb_raw)) {
    stop("WPP life table is not saved under ./raw/! Plrease download from https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/4_Mortality/WPP2024_MORT_F06_1_SINGLE_AGE_LIFE_TABLE_ESTIMATES_BOTH_SEXES.xlsx!")
  }

  # Load the WPP life tables saved in raw
  lt_b <- lapply(1:7, FUN = function(sheet) {
    data.table(read_excel(path=path_wpp24_ltb_raw, sheet = sheet, col_names = TRUE, col_types = NULL, na = "", skip = 16))
  })
  
  # Bind the life tables together
  lt_b <- rbindlist(lt_b)
  
  # Rename variables
  setnames(lt_b, 
           old = c("Region, subregion, country or area *", "Age (x)", "Age interval (n)", "Central death rate m(x,n)", "Probability of dying q(x,n)" ,
                      "Probability of surviving p(x,n)", "Number of survivors l(x)", "Number of deaths d(x,n)",
                     "Number of person-years lived L(x,n)", "Survival ratio S(x,n)", "Person-years lived T(x)", "Expectation of life e(x)",
                     "Average number of years lived a(x,n)"), 
           new = c("region", "age", "n", "mx", "qx", "px", "lx", "dx", "Lx", "Sx", "Tx", "ex", "ax"))
  
  # Select the important variables:
  lt_b <- lt_b[, .(Variant, region, Year, age, n, mx, qx, px, lx, dx, Lx, Sx, Tx, ex, ax)]
  
  # Make life table columns numeric
  numeric_columns <- c("age", "n", "mx", "qx", "px", "lx", "dx", "Lx", "Sx", "Tx", "ex", "ax")
  lt_b[, (numeric_columns) := lapply(.SD, as.numeric), .SDcols=numeric_columns]
  
  # Save the data
  save(lt_b, file = path_wpp24_ltb)

} else {
  
  if (!file.exists(path_wpp24_ltb)) {
    stop("You must load and clean the original file first: Change first_run to TRUE!")
  } else {
    load(path_wpp24_ltb)
  }
}


# 2. Estimate the Human Life Indicator ----

# Estimate the HLI
hli_b <- lt_b[, .(HLI = estimate_hli(ax=ax, dx=dx, age=age)), by = .(region, Year)]

# Remove weard estimates
hli_b <- hli_b[region != "Australia/New Zealand" & region != "Wallis and Futuna Islands" & region != "Latin America and the Caribbean", ]

ggplot(hli_b, aes(x=Year, y=HLI, group=region)) + 
  geom_line()

ggplot(subset(lt_b, region=='Australia/New Zealand'), aes(x=age, y=log(mx), colour=Year, group=Year)) +
  geom_line()


# Save the HLI information
save(hli_b, file = "data/hli_both_wpp2024.Rda")
write.csv(hli_b, file="data/Hli_both_wpp2025.csv")

# 3. Estimate the total fertility rate TFR ----


# World Population Prospects ASFRs (downloaded: 15.07.2025)
# https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/3_Fertility/WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx

# The loading is pretty slow, because obs=22728
first_run <- TRUE

path_wpp24_ltb <- "data/cleaned_life_table_b.Rda"


# Find the file for the WPP life tables
path_wpp24_asfr_raw <- list.files("raw", pattern = "FERTILITY", full.names=TRUE)


if (is.null(path_wpp24_asfr_raw)) {
  stop("WPP ASFRs are not saved under ./raw/! Plrease download from https://population.un.org/wpp/assets/Excel%20Files/1_Indicator%20(Standard)/EXCEL_FILES/3_Fertility/WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx")
}

# Load the WPP ASFRs saved in raw
asfr <- data.table(read_excel(path=path_wpp24_asfr_raw, sheet = 1, col_names = TRUE, col_types = NULL, na = "", skip = 16))

# Rename
setnames(asfr, old="Region, subregion, country or area *", new="region")

# Estimate the TFRs
asfr_columns <- paste(15:49)
asfr[, (asfr_columns) := lapply(.SD, as.numeric), .SDcols=asfr_columns]
asfr[, TFR := rowSums(.SD/1000), .SDcols=asfr_columns]

# Select the tFR data
tfr <- asfr[, .(TFR, Year, region)]

# Save the data
save(tfr, file="data/tfr_wpp2024.Rda")

## Merge the data ----------------------------------

# Create the analysis data
analysis <- merge(tfr, hli_b, by=c("Year", "region"))

# Filter the non-missing observations
analsis <- na.omit(analysis)

## Create indicator for countries -----------------

# Load the locations
locations <- read.csv(list.files("raw", pattern="LOCATIONS", full.names=T))
locations <- locations[, c("Location", "LocTypeName", "GeoRegName", "SDGRegName", "SDGSubRegName")]

# Merge analysis with locations
analysis <- merge(analysis, locations, by.x="region", by.y="Location", all.x=TRUE, all.y=FALSE)

# Remove empty rows
analysis <- analysis[!is.na(TFR)]

# Save the analysis data
save(analysis, file="data/analysis.Rda")
write.csv(analysis, file="data/wpp2024_hli_tfr.csv")

### END ###################################