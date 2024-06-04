#####################################################
#  Script to transform and standardised variables   #
#####################################################

### Script elaborated by Israel Moreno-Contreras (Ornithology, UNAM)

### Call libraries
library(maptools) # Allow read shapefiles
library(ggplot2)
library(Rmisc) # Merge plots
library(moments) # To quantify Kurtosis and Skewness
library(GGally) # Nice scatter plots
library(fuzzySim) # Evaluate multicollinearty between independent variables
library("tidyverse") # For filtering of PD metrics of urban areas
library("ggpubr") ### For export boxplots
library("dplyr")

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspace
load("../data/output_data/Workspaces/Preparation_variables.Rdata")

### Read csv files containing environmental variables
city_variables <- read.csv("..\\data\\raw_data\\Variables\\environ_variables.csv", header = TRUE, row.names = 1)

### Read csv files containing Phylogenetic Diversity metrics using Prum backbone
PD_metrics.results_prum <- read.csv("..\\data\\output_data\\Diversity_metrics\\PD_metrics.results_prum.csv", header = TRUE, row.names = 1)

### Read csv files containing Phylogenetic Diversity metrics using Hackett backbone
PD_metrics.results_hack <- read.csv("..\\data\\output_data\\Diversity_metrics\\PD_metrics.results_hack.csv", header = TRUE, row.names = 1)

### Read csv files containing Functional Diversity metrics using Gower distance
FD_metrics.results <- read.csv("..\\data\\output_data\\Diversity_metrics\\FD_metrics.results.csv", header = TRUE, row.names = 1)

### Make a text pattern to keep
urban_key_words <- c("urban")
regional_key_words <- c("_reg")

### Make a filter using regional_key_words
PD_metrics.results_prum_regional <- filter(PD_metrics.results_prum, grepl(paste(regional_key_words, collapse="|"), PD_metrics.results_prum$City)) ### 81 records

PD_metrics.results_hack_regional <- filter(PD_metrics.results_hack, grepl(paste(regional_key_words, collapse="|"), PD_metrics.results_hack$City)) ### 81 records

FD_metrics.results_regional <- filter(FD_metrics.results, grepl(paste(regional_key_words, collapse="|"), City)) ### 81 records

### Make a filter using key_words
PD_metrics.results_prum_urban <- filter(PD_metrics.results_prum, grepl(paste(urban_key_words, collapse="|"), City)) ### 81 records

PD_metrics.results_hack_urban <- filter(PD_metrics.results_hack, grepl(paste(urban_key_words, collapse="|"), City)) ### 81 records

FD_metrics.results_urban <- filter(FD_metrics.results, grepl(paste(urban_key_words, collapse="|"), City)) ### 81 records

### Reorder data frame of urban areas containing Phylogenetic and Functional metrics according City column
PD_metrics.results_prum_urban <- PD_metrics.results_prum_urban[order(PD_metrics.results_prum_urban$City),]

PD_metrics.results_hack_urban <- PD_metrics.results_hack_urban[order(PD_metrics.results_hack_urban$City),]

FD_metrics.results_urban <- FD_metrics.results_urban[order(FD_metrics.results_urban$City),]

### Reorder data frame of regional species pools containing Phylogenetic and Functional metrics according City column
PD_metrics.results_prum_regional <- PD_metrics.results_prum_regional[order(PD_metrics.results_prum_regional$City),]

PD_metrics.results_hack_regional <- PD_metrics.results_hack_regional[order(PD_metrics.results_hack_regional$City),]

FD_metrics.results_regional <- FD_metrics.results_regional[order(FD_metrics.results_regional$City),]

### Make a new column indicating the species pool category: https://stackoverflow.com/questions/34429001/adding-a-column-to-data-frame-and-fill-that-column-with-a-particular-string-usin
PD_metrics.results_prum_regional$Species_pool <- 'Regional'

PD_metrics.results_prum_regional$City_code <- c(1:81)

PD_metrics.results_prum_urban$Species_pool <- 'Urban'

PD_metrics.results_prum_urban$City_code <- c(1:81)

FD_metrics.results_regional$Species_pool <- 'Regional'

FD_metrics.results_regional$City_code <- c(1:81)

FD_metrics.results_urban$Species_pool <- 'Urban'

FD_metrics.results_urban$City_code <- c(1:81)

### Export data frame containing PD_metrics.results_prum_regional for Spatial_GLMM_winter analyses
write.csv(PD_metrics.results_prum_regional, file = "..\\data\\output_data\\Diversity_metrics\\PD_metrics.results_prum_regional.csv")

### Export data frame containing PD_metrics.results_prum_urban for Spatial_GLMM_winter analyses
write.csv(PD_metrics.results_prum_urban, file = "..\\data\\output_data\\Diversity_metrics\\PD_metrics.results_prum_urban.csv")

### Export data frame containing FD_metrics.results_regional for Spatial_GLMM_winter analyses
write.csv(FD_metrics.results_regional, file = "..\\data\\output_data\\Diversity_metrics\\FD_metrics.results_regional.csv")

### Export data frame containing FD_metrics.results_urban for Spatial_GLMM_winter analyses
write.csv(FD_metrics.results_urban, file = "..\\data\\output_data\\Diversity_metrics\\FD_metrics.results_urban.csv")

### Merge Phylogenetic diversity metrics of regional and urban species pools based on Prum backbone
PD_metrics_prum_merge <- rbind.data.frame(PD_metrics.results_prum_urban, PD_metrics.results_prum_regional)

### Export Merged Phylogenetic diversity metrics of regional and urban species pools based on Prum backbone
write.csv(PD_metrics_prum_merge, file = "..\\data\\output_data\\Diversity_metrics\\PD_metrics_prum_merge.csv")

### Merge Functional diversity metrics of regional and urban species pools
FD_metrics_merge <- rbind.data.frame(FD_metrics.results_urban, FD_metrics.results_regional)

### Export Merged Functional diversity metrics of regional and urban species pools
write.csv(FD_metrics_merge, file = "..\\data\\output_data\\Diversity_metrics\\FD_metrics_merge.csv")

### Merge all response and predictor variables
all_variables <- cbind.data.frame(city_variables[,1:10], PD_metrics.results_prum_regional$Richness, PD_metrics.results_prum_urban$Richness, 
                                  PD_metrics.results_prum_urban$MPD_norm_prum, PD_metrics.results_prum_urban$ses.MPD_prum, 
                                  PD_metrics.results_hack_urban$MPD_norm_hack, PD_metrics.results_hack_urban$ses.MPD_hack,
                                  FD_metrics.results_urban$MFD_gow_vec, FD_metrics.results_urban$MFD_estand_gow_vec,
                                  city_variables[,11:22])

### Change name columns
colnames(all_variables)[which(names(all_variables) == "PD_metrics.results_prum_regional$Richness")] <- "Regional_TD"
colnames(all_variables)[which(names(all_variables) == "PD_metrics.results_prum_urban$Richness")] <- "Richness"
colnames(all_variables)[which(names(all_variables) == "PD_metrics.results_prum_urban$MPD_norm_prum")] <- "MPD_prum"
colnames(all_variables)[which(names(all_variables) == "PD_metrics.results_prum_urban$ses.MPD_prum")] <- "sesMPD_prum"
colnames(all_variables)[which(names(all_variables) == "PD_metrics.results_hack_urban$ses.MPD_hack")] <- "sesMPD_hack"
colnames(all_variables)[which(names(all_variables) == "PD_metrics.results_hack_urban$MPD_norm_hack")] <- "MPD_hack"
colnames(all_variables)[which(names(all_variables) == "FD_metrics.results_urban$MFD_gow_vec")] <- "FMPD"
colnames(all_variables)[which(names(all_variables) == "FD_metrics.results_urban$MFD_estand_gow_vec")] <- "sesFMPD"

### Change name of all_variables into raw_variables
raw_variables <-  all_variables
  
### Export data frame containing variables
write.csv(raw_variables, file = "..\\data\\output_data\\Variables\\Raw_var\\raw_variables.csv")

### Log transformation of the response variable (species richness)
all_variables$Regional_TD <- log(all_variables$Regional_TD + 1)
all_variables$Richness <- log(all_variables$Richness + 1)

### Scale all predictors variables to mean zero and standard deviation of 1 
### to allow coeficient comparison
all_variables$Richness <- scale(all_variables$Richness, center = T)
all_variables$Prec <- scale(all_variables$Prec, center = T)
all_variables$Tmin <- scale(all_variables$Tmin, center = T)
all_variables$NDVI <- scale(all_variables$NDVI, center = T)
all_variables$Elev <- scale(all_variables$Elev, center = T)
all_variables$road_density <- scale(all_variables$road_density, center = T)
all_variables$city_age <- scale(all_variables$city_age, center = T)
all_variables$gdp <- scale(all_variables$gdp, center = T)
all_variables$area <- scale(all_variables$area, center = T)
all_variables$shape_ind <- scale(all_variables$shape_ind, center = T)
all_variables$dist_rivers <- scale(all_variables$dist_rivers, center = T)
all_variables$dist_coasts <- scale(all_variables$dist_coasts, center = T)
all_variables$dist_mounta <- scale(all_variables$dist_mounta, center = T)

### Convert data frame
all_variables <- as.data.frame(all_variables)

### Export data frame containing transformed and standardised variables
write.csv(all_variables, file = "..\\data\\output_data\\Variables\\Standardised_var\\all_variables.csv")

### Read csv files containing transformed and standardised variables
all_variables <- read.csv("..\\data\\output_data\\Variables\\Standardised_var\\all_variables.csv", header = TRUE, row.names = 1)

### Save Workspace
save.image("../data/output_data/Workspaces/Preparation_variables.Rdata")

#################################################################################################
