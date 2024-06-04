#########################
##    Spatial GLMMs    ##
#########################

### Call libraries
library("rgdal") ### Read spatial data
library("fastDummies") ### For converting categorical into dummy variables
library("MASS") ### For running spatial GLMMs
library("nlme") ### Correlation structure

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspace
load("../data/output_data/Workspaces/spatGLMM_bivariate.Rdata")

### Read csv archive of the community-level data for each City
city_data <- read.csv("..\\data\\raw_data\\Survey_info\\Surveys_info_final.csv",  header = TRUE)

### Read csv file containing transformed and standardized variables
all_variables <- read.csv("..\\data\\output_data\\Variables\\Standardised_var\\all_variables.csv", header = TRUE, row.names = 1)

### Read csv file containing raw variables
raw_variables <- read.csv("..\\data\\output_data\\Variables\\Raw_var\\raw_variables.csv", header = TRUE, row.names = 1)

### Read csv file containing PD_metrics.results_prum_regional for Spatial_GLMM_winter analyses
PD_metrics.results_prum_regional <- read.csv(file = "..\\data\\output_data\\Diversity_metrics\\PD_metrics.results_prum_regional.csv", header = TRUE)

### Read shapefile containing Ecoregional information
points_ecoregion <- readOGR(dsn = "..\\data\\raw_data\\Shapefiles\\Ecoregions_Spatial_join", layer = "Ecoregions2017_SpatialJoin")

### Read shapefile containing Ecoregional information
points_regions_zoogeo <- readOGR(dsn = "..\\data\\raw_data\\Shapefiles\\Zoogeo_realms\\Projection", layer = "regions_spatial_join")

### Add column of Ecoregion names
city_data <- cbind.data.frame(city_data, points_ecoregion$ECO_NAME)

### Add column of Zoogeographic regions names
city_data <- cbind.data.frame(city_data, points_regions_zoogeo$Regions)

### Change column name for variables
colnames(city_data)[which(names(city_data) == "points_ecoregion$ECO_NAME")] <- "Ecoregion"

colnames(city_data)[which(names(city_data) == "points_regions_zoogeo$Regions")] <- "Zoo_region"

colnames(city_data)[which(names(city_data) == "points_regions_zoogeo$Regions")] <- "Zoo_region"

colnames(city_data)[which(names(city_data) == "Survey_method")] <- "SM"

colnames(city_data)[which(names(city_data) == "Number_visits_categories")] <- "NV"

colnames(city_data)[which(names(city_data) == "Duration_years_categories")] <- "ET"

### Log transformation for the response variable (Species richness)
city_data$Richness_log <- log(city_data$Richness + 1)

### Log transformation for the Study area plot (in km2)
city_data$Study_plot_area_km2 <- log(city_data$Study_plot_area_km2 + 1)

### Convert variables into categories
city_data$NV <- as.factor(city_data$NV) 

city_data$SM <- as.factor(city_data$SM)

city_data$ET <- as.factor(city_data$ET)

city_data$Ecoregion <- as.factor(city_data$Ecoregion)

city_data$Zoo_region <- as.factor(city_data$Zoo_region)

city_data$Continent <- as.factor(city_data$Continent)

### Add variables
city_data$sesMPD_prum <- raw_variables$sesMPD_prum

city_data$sesFMPD <- raw_variables$sesFMPD

city_data$Richness_regional <- PD_metrics.results_prum_regional$Richness

city_data$NDVI <- raw_variables$NDVI

city_data$Tmin <- raw_variables$Tmin

city_data$Prec <- raw_variables$Prec

city_data$Elevation <- raw_variables$Elev

city_data$road_density <- raw_variables$road_density

city_data$gdp <- raw_variables$gdp

city_data$area <- raw_variables$area

city_data$drivers <- raw_variables$dist_rivers

city_data$dcoasts <- raw_variables$dist_coasts

### Scale predictors variables to mean zero and standard deviation of 1 
### to allow coeficient comparison
city_data$Study_plot_area_km2 <- scale(city_data$Study_plot_area_km2, center = T)

### Transform categorical variable into dummy variables
city_data <- dummy_cols(city_data, select_columns = c("NV", "ET", "SM"), remove_first_dummy  = TRUE)

### Get data frame containing variables used in the spatial GLMMs
survey_data<- data.frame(TD_log = city_data$Richness_log, 
                         SPA_km2 = city_data$Study_plot_area_km2,
                         SM_Points_counts = city_data$SM_Point_counts, 
                         SM_Study_plot_mapping = city_data$SM_Study_plot_mapping, 
                         SM_Transects =city_data$SM_Transects,
                         NV_SV = city_data$NV_single_visit,
                         ET_OY = city_data$ET_One_year,
                         ET_TY = city_data$ET_Two_years,
                         Latitude = city_data$Latitude, 
                         Longitude = city_data$Longitude,
                         Zoo_region = city_data$Zoo_region)

### Spatial GLMM fits 
model_GLMM <- glmmPQL(TD_log ~ SPA_km2 +
                        SM_Points_counts + 
                        SM_Study_plot_mapping +  
                        SM_Transects +
                        NV_SV + ET_OY + ET_TY, 
                      random = ~1|Zoo_region,
                      data = survey_data,
                      family = gaussian(link = "identity"))

### See GLMM results
summary(model_GLMM)

### Plot variogram
plot(Variogram(model_GLMM), main = "No Correlation Structure")

### Exponential correlation structure
model_GLMM_ecs <- glmmPQL(TD_log ~ SPA_km2 + 
                            SM_Points_counts + SM_Study_plot_mapping +  SM_Transects +
                            NV_SV + ET_OY + ET_TY,
                   random = ~1|Zoo_region,
                   data = survey_data,
                   correlation = corExp(form = ~Longitude + Latitude, nugget = T),
                   family = gaussian(link = "identity"))

### Gaussian correlation structure
model_GLMM_gcs <- glmmPQL(TD_log ~ SPA_km2 + 
                            SM_Points_counts + SM_Study_plot_mapping +  SM_Transects +
                            NV_SV + ET_OY + ET_TY,
                          random = ~1|Zoo_region,
                          data = survey_data,
                          correlation = corGaus(form = ~Longitude + Latitude, nugget = T),
                          family = gaussian(link = "identity"))

### Spherical correlation structure
model_GLMM_scs <- glmmPQL(TD_log ~ SPA_km2 + 
                            SM_Points_counts + SM_Study_plot_mapping +  SM_Transects +
                            NV_SV + ET_OY + ET_TY,
                          random = ~1|Zoo_region,
                          data = survey_data,
                          correlation = corSpher(form = ~Longitude + Latitude, nugget = T),
                          family = gaussian(link = "identity"))

### See GLMM results
summary(model_GLMM_ecs)
summary(model_GLMM_gcs)
summary(model_GLMM_scs)

### Plot variogram
plot(Variogram(model_GLMM_ecs), main = "Exponential Correlation")

plot(Variogram(model_GLMM_scs), main = "Spherical Correlation")

### R-squared for GLMMs 
r.squaredGLMM(model_GLMM)
r.squaredGLMM(model_GLMM_ecs)
r.squaredGLMM(model_GLMM_gcs)
r.squaredGLMM(model_GLMM_scs)

### Save Workspace
save.image("../data/output_data/Workspaces/spatGLMM_bivariate.Rdata")

###########################################################################################

###########################
##    Bivariate plots    ##
###########################

### Call libraries
library("ggplot2")
library("ggpubr")
library("cowplot")

### Define breaks for city age legend in bivariate plots
bb <- c(0, 500, 1000, 1500, 2000, 2500) # define breaks. 

### Bivariate plots for latitude 

# Taxonomic diversity
species_urban_lat_plot <- ggplot(data = city_data, aes(x = Latitude, y = Richness)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
    geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Latitude (°)") + ylab("Urban species richness") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-23, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_x_continuous(limits = c(-38, 67), breaks = c(-40, -20, 0, 20, 40, 60)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Phylogenetic diversity
sesMPD_urban_lat_plot <- ggplot(data = city_data, aes(x = Latitude, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Latitude (°)") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-5.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-5.0, -4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) + 
  scale_x_continuous(limits = c(-38, 67), breaks = c(-40, -20, 0, 20, 40, 60)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Functional diversity
sesFMPD_urban_lat_plot <- ggplot(data = city_data, aes(x = Latitude, y = sesFMPD)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Latitude (°)") + ylab("sesFMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-3.0, 1.7), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-3.0, -2.0, -1.0, 0, 1.0)) + 
  scale_x_continuous(limits = c(-38, 67), breaks = c(-40, -20, 0, 20, 40, 60)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

### Merge latitude plots for Taxonomic, Phylogenetic, and Functional diversity
scatter_latitude_plots <- ggarrange(species_urban_lat_plot, sesMPD_urban_lat_plot, sesFMPD_urban_lat_plot,
                                    ncol = 3, nrow = 1, common.legend = TRUE, legend = "right")

### Export latitude plots for Taxonomic, Phylogenetic, and Functional diversity
png(filename="..\\data\\output_data\\Figures\\Scatters\\scatter_latitude_plots.png", units="mm", width=350, height=100, pointsize=15, res=2000)
ggdraw(scatter_latitude_plots) +
  draw_label("(a)", x = 0.01, y = 0.98, fontface = "bold", size = 14) +
  draw_label("(b)", x = 0.291, y = 0.98, fontface = "bold", size = 14) +
  draw_label("(c)", x = 0.576, y = 0.98, fontface = "bold", size = 14)
dev.off()


#############
#### Bivariate plots for Taxonomic diversity

# Regional species pool
species_urban_regional_plot <- ggplot(data = city_data, aes(x = Richness_regional, y = Richness)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Regional species richness") + ylab("Urban species richness") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-23, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_x_continuous(limits = c(0, 450), breaks = c(0, 100, 200, 300, 400)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14),
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Minimum temperature
species_urban_tmin_plot <- ggplot(data = city_data, aes(x = Tmin, y = Richness)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Minimum temperature (°C)") + ylab("Urban species richness") + 
  scale_y_continuous(limits = c(-23, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Elevation
species_urban_elev_plot <- ggplot(data = city_data, aes(x = Elevation, y = Richness)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Elevation (m)") + ylab("Urban species richness") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-23, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_x_continuous(limits = c(0, 2450), breaks = c(0, 600, 1200, 1800, 2400)) +  
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Urban area
#expression(bold(Area~(km^{2})))
species_urban_area_plot <- ggplot(data = city_data, aes(x = area, y = Richness)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Area (square km)") + ylab("Urban species richness") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) + 
  scale_y_continuous(limits = c(-23, 90), breaks = c(0, 20, 40, 60, 80)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# City age
species_urban_year_plot <- ggplot(data = city_data, aes(x = City_age, y = Richness)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("City age (yrs)") + ylab("Urban species richness") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_x_continuous(limits = c(0, 2800), breaks = c(0, 500, 1000, 1500, 2000, 2500)) +
  scale_y_continuous(limits = c(-23, 90), breaks = c(0, 20, 40, 60, 80)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Precipitation
species_urban_prec_plot <- ggplot(data = city_data, aes(x = Prec, y = Richness)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Precipitation (mm)") + ylab("Urban species richness") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_x_continuous(limits = c(0, 150), breaks = c(0, 30, 60, 90, 120, 150)) +
  scale_y_continuous(limits = c(-23, 90), breaks = c(0, 20, 40, 60, 80)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# GDP
species_urban_gdp_plot <- ggplot(data = city_data, aes(x = gdp, y = Richness)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("GDP (millions of 2017 US dollars)") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-23, 90), breaks = c(0, 20, 40, 60, 80)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Distance to coasts
species_urban_dcoa_plot <- ggplot(data = city_data, aes(x = dcoasts, y = Richness)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Distance to coasts (km)") + ylab("Urban species richness") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) + 
  scale_y_continuous(limits = c(-23, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_x_continuous(limits = c(0, 1100), breaks = c(0, 250, 500, 750, 1000)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# NDVI
species_urban_ndvi_plot <- ggplot(data = city_data, aes(x = NDVI, y = Richness)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("NDVI") + ylab("Urban species richness") + 
  scale_y_continuous(limits = c(-20, 85), breaks = c(0, 20, 40, 60, 80)) +
  scale_x_continuous(limits = c(-0.1, 0.44), breaks = c(-0.1, 0, 0.1, 0.2, 0.3, 0.4)) +
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

### Merge bivariate plots of the Taxonomic diversity and variables
scatter_richness_plots <- ggarrange(species_urban_tmin_plot, species_urban_elev_plot, species_urban_area_plot,
                                    species_urban_year_plot, species_urban_prec_plot, species_urban_gdp_plot,
                                    species_urban_dcoa_plot, species_urban_ndvi_plot, species_urban_regional_plot,
                                    ncol = 3, nrow = 3, common.legend = TRUE, legend = "right")

### Export bivariate plots of the Taxonomic diversity and variables
png(filename="..\\data\\output_data\\Figures\\Scatters\\scatter_richness_plots3.png", units="mm", width=350, height=300, pointsize=15, res=1500)
ggdraw(scatter_richness_plots) +
  draw_label("(a)", x = 0.01, y = 0.99, fontface = "bold", size = 14) +
  draw_label("(b)", x = 0.29, y = 0.99, fontface = "bold", size = 14) +
  draw_label("(c)", x = 0.58, y = 0.99, fontface = "bold", size = 14) +
  draw_label("(d)", x = 0.01, y = 0.655, fontface = "bold", size = 14) +
  draw_label("(e)", x = 0.29, y = 0.655, fontface = "bold", size = 14) +
  draw_label("(f)", x = 0.58, y = 0.655, fontface = "bold", size = 14) +
  draw_label("(g)", x = 0.01, y = 0.321, fontface = "bold", size = 14) +
  draw_label("(h)", x = 0.29, y = 0.321, fontface = "bold", size = 14) +
  draw_label("(i)", x = 0.58, y = 0.321, fontface = "bold", size = 14) 
dev.off()

############################################################################

#### Bivariate plots for Phylogenetic diversity

# Elevation
sesMPD_urban_elev_plot <- ggplot(data = city_data, aes(x = Elevation, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Elevation (m)") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +   
  scale_y_continuous(limits = c(-5.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-5.0, -4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) + 
  scale_x_continuous(limits = c(0, 2450), breaks = c(0, 600, 1200, 1800, 2400)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Precipitation
sesMPD_urban_prec_plot <- ggplot(data = city_data, aes(x = Prec, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Precipitation (mm)") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-5.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-5.0, -4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) + 
  scale_x_continuous(limits = c(0, 150), breaks = c(0, 30, 60, 90, 120, 150)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Taxonomic diversity
sesMPD_urban_rich_plot <- ggplot(data = city_data, aes(x = Richness, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Urban species richness") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-5.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-5.0, -4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) + 
  scale_x_continuous(limits = c(0, 85), breaks = c(0, 20, 40, 60, 80)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Distance to coasts
sesMPD_urban_dcoa_plot <- ggplot(data = city_data, aes(x = dcoasts, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Distance to coasts (km)") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) + 
  scale_y_continuous(limits = c(-5.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-5.0, -4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) + 
  scale_x_continuous(limits = c(0, 1100), breaks = c(0, 250, 500, 750, 1000)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Minimum temperature
sesMPD_urban_tmin_plot <- ggplot(data = city_data, aes(x = Tmin, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Minimum temperature (°C)") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-5.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-5.0, -4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Distance to rivers
sesMPD_urban_driv_plot <- ggplot(data = city_data, aes(x = drivers, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Distance to rivers (km)") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-5.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-5.0, -4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) + 
  scale_x_continuous(limits = c(0, 800), breaks = c(0, 200, 400, 600, 800)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# NDVI
sesMPD_urban_ndvi_plot <- ggplot(data = city_data, aes(x = NDVI, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("NDVI") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-5.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-5.0, -4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) + 
    scale_x_continuous(limits = c(-0.1, 0.44), breaks = c(-0.1, 0, 0.1, 0.2, 0.3, 0.4)) +
    theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))  

# Urban area
sesMPD_urban_area_plot <- ggplot(data = city_data, aes(x = area, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Area (square km)") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-5.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-5.0, -4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))  

# GDP
sesMPD_urban_gdp_plot <- ggplot(data = city_data, aes(x = gdp, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("GDP (millions of 2017 US dollars)") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) + 
  scale_y_continuous(limits = c(-5.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-5.0, -4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))  

# Road density
sesMPD_urban_rden_plot <- ggplot(data = city_data, aes(x = road_density, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Road density") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +   
  scale_y_continuous(limits = c(-5.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-5.0, -4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))  

# City age
sesMPD_urban_year_plot <- ggplot(data = city_data, aes(x = City_age, y = sesMPD_prum)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("City age (yrs)") + ylab("sesMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +   
  scale_y_continuous(limits = c(-4.0, 2.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-4.0, -3.0, -2.0, -1.0, 0, 1.0, 2.0)) +
  scale_x_continuous(limits = c(0, 2800), breaks = c(0, 500, 1000, 1500, 2000, 2500)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

### Merge bivariate plots of the Phylogenetic diversity and variables
scatter_sesMPD_plots <- ggarrange(sesMPD_urban_rich_plot, sesMPD_urban_elev_plot, sesMPD_urban_prec_plot,
                                  sesMPD_urban_dcoa_plot, sesMPD_urban_tmin_plot, sesMPD_urban_gdp_plot,
                                  sesMPD_urban_driv_plot, sesMPD_urban_rden_plot, sesMPD_urban_year_plot,
                                    ncol = 3, nrow = 3, common.legend = TRUE, legend = "right")

### Export bivariate plots of the Phylogenetic diversity and variables
png(filename="..\\data\\output_data\\Figures\\Scatters\\scatter_sesMPD_plots3.png", units="mm", width=350, height=300, pointsize=15, res=1500)
ggdraw(scatter_sesMPD_plots) +
  draw_label("(a)", x = 0.01, y = 0.99, fontface = "bold", size = 14) +
  draw_label("(b)", x = 0.29, y = 0.99, fontface = "bold", size = 14) +
  draw_label("(c)", x = 0.58, y = 0.99, fontface = "bold", size = 14) +
  draw_label("(d)", x = 0.01, y = 0.655, fontface = "bold", size = 14) +
  draw_label("(e)", x = 0.29, y = 0.655, fontface = "bold", size = 14) +
  draw_label("(f)", x = 0.58, y = 0.655, fontface = "bold", size = 14) +
  draw_label("(g)", x = 0.01, y = 0.321, fontface = "bold", size = 14) +
  draw_label("(h)", x = 0.29, y = 0.321, fontface = "bold", size = 14) +
  draw_label("(i)", x = 0.58, y = 0.321, fontface = "bold", size = 14) 
dev.off()

################################################################


#### Bivariate plots for Functional diversity

# sesMPD
sesFMPD_urban_smpd_plot <- ggplot(data = city_data, aes(x = sesMPD_prum, y = sesFMPD)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("sesMPD") + ylab("sesFMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +   
  scale_y_continuous(limits = c(-3.0, 1.7), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-3.0, -2.0, -1.0, 0, 1.0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Road density
sesFMPD_urban_rden_plot <- ggplot(data = city_data, aes(x = road_density, y = sesFMPD)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Road density") + ylab("sesFMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-3.0, 1.7), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-3.0, -2.0, -1.0, 0, 1.0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# NDVI
sesFMPD_urban_ndvi_plot <- ggplot(data = city_data, aes(x = NDVI, y = sesFMPD)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("NDVI") + ylab("sesFMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) + 
  scale_y_continuous(limits = c(-3.0, 1.7), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-3.0, -2.0, -1.0, 0, 1.0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Elevation
sesFMPD_urban_elev_plot <- ggplot(data = city_data, aes(x = Elevation, y = sesFMPD)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Elevation (m)") + ylab("sesFMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +   
  scale_y_continuous(limits = c(-3.0, 1.7), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-3.0, -2.0, -1.0, 0, 1.0)) + 
  scale_x_continuous(limits = c(0, 2450), breaks = c(0, 600, 1200, 1800, 2400)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# GDP
sesFMPD_urban_gdp_plot <- ggplot(data = city_data, aes(x = gdp, y = sesFMPD)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("GDP (millions of 2017 US dollars)") + ylab("sesFMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +   
  scale_y_continuous(limits = c(-3.0, 1.7), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-3.0, -2.0, -1.0, 0, 1.0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))  

# Minimum temperature
sesFMPD_urban_tmin_plot <- ggplot(data = city_data, aes(x = Tmin, y = sesFMPD)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Minimum temperature (°C)") + ylab("sesFMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-3.0, 1.7), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-3.0, -2.0, -1.0, 0, 1.0)) + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Taxonomic diversity
sesMPD_urban_rich_plot <- ggplot(data = city_data, aes(x = Richness, y = sesFMPD)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Urban species richness") + ylab("sesFMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-3.0, 1.7), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-3.0, -2.0, -1.0, 0, 1.0)) + 
  scale_x_continuous(limits = c(0, 85), breaks = c(0, 20, 40, 60, 80)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# Distance to rivers
sesFMPD_urban_driv_plot <- ggplot(data = city_data, aes(x = drivers, y = sesFMPD)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Distance to rivers (km)") + ylab("sesFMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-3.0, 1.7), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-3.0, -2.0, -1.0, 0, 1.0)) +
  scale_x_continuous(limits = c(0, 800), breaks = c(0, 200, 400, 600, 800)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

# City age
sesFMPD_urban_year_plot <- ggplot(data = city_data, aes(x = City_age, y = sesFMPD)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "gam") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("City age (yrs)") + ylab("sesFMPD") + 
  scale_size(expression(bold("City age (yrs)")), range = c(0,15), limits = c(0,3000), breaks = bb, labels=c( '< 500', '500-999', '1000-1499', '1500-1999', '2000-2499', "\u2265 2500")) +  
  scale_y_continuous(limits = c(-3.0, 1.7), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-3.0, -2.0, -1.0, 0, 1.0)) +
  scale_x_continuous(limits = c(0, 2800), breaks = c(0, 500, 1000, 1500, 2000, 2500)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), 
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.border = element_blank(), axis.line.x = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_blank(),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))

### Merge bivariate plots of the Functional diversity and variables
scatter_sesFMPD_plots <- ggarrange(sesFMPD_urban_smpd_plot, sesFMPD_urban_rden_plot, sesFMPD_urban_ndvi_plot,
                                   sesFMPD_urban_tmin_plot, sesFMPD_urban_elev_plot, sesFMPD_urban_gdp_plot,
                                  sesMPD_urban_rich_plot, sesFMPD_urban_driv_plot, sesFMPD_urban_year_plot,
                                  ncol = 3, nrow = 3, common.legend = TRUE, legend = "right")

### Merge bivariate plots of the Functional diversity and variables
png(filename="..\\data\\output_data\\Figures\\Scatters\\scatter_sesFMPD_plots3.png", units="mm", width=350, height=300, pointsize=15, res=1500)
ggdraw(scatter_sesFMPD_plots) +
  draw_label("(a)", x = 0.01, y = 0.99, fontface = "bold", size = 14) +
  draw_label("(b)", x = 0.29, y = 0.99, fontface = "bold", size = 14) +
  draw_label("(c)", x = 0.58, y = 0.99, fontface = "bold", size = 14) +
  draw_label("(d)", x = 0.01, y = 0.655, fontface = "bold", size = 14) +
  draw_label("(e)", x = 0.29, y = 0.655, fontface = "bold", size = 14) +
  draw_label("(f)", x = 0.58, y = 0.655, fontface = "bold", size = 14) +
  draw_label("(g)", x = 0.01, y = 0.321, fontface = "bold", size = 14) +
  draw_label("(h)", x = 0.29, y = 0.321, fontface = "bold", size = 14) +
  draw_label("(i)", x = 0.58, y = 0.321, fontface = "bold", size = 14) 
dev.off()





#### Bivariate plots for Asian cities

### Make a text pattern to keep
man_screening_asia <- as.vector(c("Gujranwala", "Gwangju", "Kathmandu", "Layyah", "Nanning", "Quezon_City")) 

### Remove species based on filter function and Code Column: 
urban_comm_asia <- filter(city_data, grepl(paste(man_screening_asia, collapse="|"), City_1))



asia_species_urban_lat_plot <- ggplot(data = urban_comm_asia, aes(x = Latitude, y = Richness)) +
  geom_smooth(color = "chocolate1", fill = "#acd3bf", alpha = 0.6, method = "lm") +
  geom_point(colour="#404d33",
             aes(size = City_age),
             alpha=0.2) +
  xlab("Latitude (°)") + ylab("Urban species richness") + 
  scale_size_continuous(expression(bold("City age (yrs)")), range=c(0,15), breaks = bb) +
  scale_y_continuous(limits = c(-23, 90), breaks = c(0, 20, 40, 60, 80)) +
  scale_x_continuous(limits = c(-38, 67), breaks = c(-40, -20, 0, 20, 40, 60)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), 
        legend.text = element_text(size = 14))


Optimal Bins = log(81, 2) + 1



# Calculating the Sturges bins
breaks <- pretty(range(city_data$City_age),
                 n = nclass.Sturges(city_data$City_age),
                 min.n = 1)
city_data$breaks <- breaks