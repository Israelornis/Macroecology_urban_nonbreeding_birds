###########################
#   Spatial correlations  #
###########################

### Script elaborated by Israel Moreno-Contreras (Ornithology, UNAM)

### Call libraries
library("SpatialPack")

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspace
load("../data/output_data/Workspaces/Spatial_correlations.Rdata")

### Read csv files containing transformed and standardized variables
all_variables <- read.csv("..\\data\\output_data\\Variables\\Standardised_var\\all_variables.csv", header = TRUE, row.names = 1)

### Extract coordinates of all_variables
city_coords <- all_variables[c("X_Behrmann", "Y_Behrmann")]

#### https://stats.stackexchange.com/questions/579921/na-p-value-in-r-cor-test(NaN)

### Perform Modified t test for Richness
modified.ttest(all_variables$Richness, all_variables$sesMPD_prum, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$sesMPD_hack, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$sesFMPD, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$Prec, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$Tmin, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$NDVI, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$Elev, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$road_density, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$city_age, city_coords, nclass = 14)
modified.ttest(all_variables$Richness, all_variables$gdp, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$area, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$shape_ind, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$dist_rivers, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$dist_coasts, city_coords, nclass = NULL)
modified.ttest(all_variables$Richness, all_variables$dist_mounta, city_coords, nclass = NULL)

### Perform Modified t test for sesMPD_prum
modified.ttest(all_variables$sesMPD_prum, all_variables$sesMPD_hack, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$sesFMPD, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$Prec, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$Tmin, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$NDVI, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$Elev, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$road_density, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$city_age, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$gdp, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$area, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$shape_ind, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$dist_rivers, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$dist_coasts, city_coords, nclass = NULL)
modified.ttest(all_variables$sesMPD_prum, all_variables$dist_mounta, city_coords, nclass = NULL)

### Perform Modified t test for sesFMPD
modified.ttest(all_variables$sesFMPD, all_variables$sesMPD_hack, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$sesFMPD, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$Prec, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$Tmin, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$NDVI, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$Elev, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$road_density, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$city_age, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$gdp, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$area, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$shape_ind, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$dist_rivers, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$dist_coasts, city_coords, nclass = NULL)
modified.ttest(all_variables$sesFMPD, all_variables$dist_mounta, city_coords, nclass = NULL)

### Perform Modified t test for Tmin
modified.ttest(all_variables$Tmin, all_variables$Prec, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$Tmin, all_variables$NDVI, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$Tmin, all_variables$Elev, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Tmin, all_variables$road_density, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Tmin, all_variables$city_age, city_coords, nclass = 14) # NaN
modified.ttest(all_variables$Tmin, all_variables$gdp, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Tmin, all_variables$area, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Tmin, all_variables$shape_ind, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Tmin, all_variables$dist_rivers, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Tmin, all_variables$dist_coasts, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Tmin, all_variables$dist_mounta, city_coords, nclass = NULL) # NS

### Perform Modified t test for Precipitation
modified.ttest(all_variables$Prec, all_variables$NDVI, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$Prec, all_variables$Elev, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$Prec, all_variables$road_density, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$Prec, all_variables$city_age, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Prec, all_variables$gdp, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Prec, all_variables$area, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Prec, all_variables$shape_ind, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Prec, all_variables$dist_rivers, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Prec, all_variables$dist_coasts, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$Prec, all_variables$dist_mounta, city_coords, nclass = NULL) # NS

### Perform Modified t test for NDVI
modified.ttest(all_variables$NDVI, all_variables$Elev, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$NDVI, all_variables$road_density, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$NDVI, all_variables$city_age, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$NDVI, all_variables$gdp, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$NDVI, all_variables$area, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$NDVI, all_variables$shape_ind, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$NDVI, all_variables$dist_rivers, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$NDVI, all_variables$dist_coasts, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$NDVI, all_variables$dist_mounta, city_coords, nclass = NULL) # sig

### Perform Modified t test for Elevation
modified.ttest(all_variables$Elev, all_variables$road_density, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$Elev, all_variables$city_age, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Elev, all_variables$gdp, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Elev, all_variables$area, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Elev, all_variables$shape_ind, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Elev, all_variables$dist_rivers, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Elev, all_variables$dist_coasts, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$Elev, all_variables$dist_mounta, city_coords, nclass = NULL) # sig

### Perform Modified t test for road_density
modified.ttest(all_variables$road_density, all_variables$city_age, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$road_density, all_variables$gdp, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$road_density, all_variables$area, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$road_density, all_variables$shape_ind, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$road_density, all_variables$dist_rivers, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$road_density, all_variables$dist_coasts, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$road_density, all_variables$dist_mounta, city_coords, nclass = NULL) # sig

### Perform Modified t test for city_age
modified.ttest(all_variables$city_age, all_variables$gdp, city_coords, nclass = 14) # NaN NS
modified.ttest(all_variables$city_age, all_variables$area, city_coords, nclass = 14) # NaN
modified.ttest(all_variables$city_age, all_variables$shape_ind, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$city_age, all_variables$dist_rivers, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$city_age, all_variables$dist_coasts, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$city_age, all_variables$dist_mounta, city_coords, nclass = NULL) # sig

### Perform Modified t test for gdp
modified.ttest(all_variables$gdp, all_variables$area, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$gdp, all_variables$shape_ind, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$gdp, all_variables$dist_rivers, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$gdp, all_variables$dist_coasts, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$gdp, all_variables$dist_mounta, city_coords, nclass = NULL) # sig

### Perform Modified t test for area
modified.ttest(all_variables$area, all_variables$shape_ind, city_coords, nclass = NULL) # sig
modified.ttest(all_variables$area, all_variables$dist_rivers, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$area, all_variables$dist_coasts, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$area, all_variables$dist_mounta, city_coords, nclass = NULL) # sig

### Perform Modified t test for shape_ind
modified.ttest(all_variables$shape_ind, all_variables$dist_rivers, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$shape_ind, all_variables$dist_coasts, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$shape_ind, all_variables$dist_mounta, city_coords, nclass = NULL) # sig

### Perform Modified t test for dist_rivers
modified.ttest(all_variables$dist_rivers, all_variables$dist_coasts, city_coords, nclass = NULL) # NS
modified.ttest(all_variables$dist_rivers, all_variables$dist_mounta, city_coords, nclass = NULL) # NS

### Perform Modified t test for dist_coasts
modified.ttest(all_variables$dist_coasts, all_variables$dist_mounta, city_coords, nclass = NULL) # sig

### Save Workspace
save.image("../data/output_data/Workspaces/Spatial_correlations.Rdata")

#################################################################################################
