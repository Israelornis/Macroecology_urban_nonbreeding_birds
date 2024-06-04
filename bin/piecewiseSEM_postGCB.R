#----------------------------------------------------------------------------------------------#
#### 6. Piecewise Structural Equation modelling using Spatial Autoregressive Error models  #####
#----------------------------------------------------------------------------------------------#
# This code is based on Skeels et al. 2020 Global Ecology and Biogeography ()

### Load Workspace
load("H:/Passport/Macroecology_urban_birds/data/output_data/Workspaces/piecewiseSEM_postGCB.Rdata")

### Load packages
library(raster)
library(rgdal)
library(ncf) ### Correlog function
library(spdep)
library(spatialreg) # version 1.1-3
library(dplyr)
library(piecewiseSEM) # version 2.0.2
library("DiagrammeR")

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Make a directory work
dir_work <- ("H:/Passport/Macroecology_urban_birds/data/output_data/Results_pSEM_postGCB") 

### Read csv files containing transformed and standardized variables
all_variables <- read.csv("..\\data\\output_data\\Variables\\Standardised_var\\all_variables.csv", header = TRUE, row.names = 1)

### Model selection for SAR neighbourhood distances and weighting schemes ###
# This code fits a series of spatial autoregressive models,
# using different neighbour distances and weights scheme to find the best fit for each path 

### Matrix weighting schemes to prove
scheme <- c("W", "C", "S")

### Make a matrix of spatial coordinates (X and Y coordinates)
sp <- SpatialPoints(data.frame(x=all_variables$X_Behrmann, y=all_variables$Y_Behrmann))
crs(sp) <- "+proj=cea +lat_ts=30"
coords <- coordinates(sp)

### Find min and maximum nearest neighbour
k1 = knn2nb(knearneigh(coords, k=1)) 
dist <- unlist(nbdists(k1, coords))
max1 <- max(dist) ### Lavras and Pretoria
min1 <- min(dist) ### Raisio and Turku

### create a series of neighbour matrices based on different distances (distances are in m)
d1 <- dnearneigh(sp, longlat = T, d1=0, d2=min1)
d2 <- dnearneigh(sp, longlat = T, d1=0, d2=max1) 

#d3 <- dnearneigh(sp, longlat = T, d1=0, d2=3537870)
#d4 <- dnearneigh(sp, longlat = T, d1=0, d2=5306805)

d3 <- dnearneigh(sp, longlat = T, d1=0, d2=7500000)
d4 <- dnearneigh(sp, longlat = T, d1=0, d2=15000000)

### Create a data frame with path number and equations that will be tested in the pSEM
path_equation <- data.frame(path_number = paste0("path",1:5))
path_equation$equation <-c(
  "Richness ~ Prec + Tmin + NDVI + Elev + road_density + city_age + gdp + area + shape_ind + dist_rivers + dist_coasts + dist_mounta",
  "sesMPD_prum ~ Richness + Prec + Tmin + NDVI + Elev + road_density + city_age + gdp + area + shape_ind + dist_rivers + dist_coasts + dist_mounta",
  "sesFMPD ~ sesMPD_prum + Prec + Tmin + NDVI + Elev + road_density + city_age + gdp + area + shape_ind + dist_rivers + dist_coasts + dist_mounta",
  "Tmin ~ Prec + road_density + city_age + gdp + area + dist_rivers + dist_coasts + dist_mounta",
  "NDVI ~ Prec + Tmin + Elev + city_age + dist_coasts")

### Create models for each path and weighting scheme ###
# We tested three schemes ("W", "C", "S"), two distance (d1, d2) and two models (OLS and SAR)
# To find the best combination that minimize the autocorrelation and increase the fit for each path equation

for (i in 1:length(scheme)){
  # Create a directory by scheme
  setwd(dir_work)
  dir_scheme <- scheme[i]
  if(!dir.exists(dir_scheme)) dir.create(dir_scheme)
  setwd(dir_scheme)
  # Create weighting scheme distance matrix
  spatial_weights_d1 <- nb2listw(d1, zero.policy=TRUE, style=scheme[i])
  spatial_weights_d2 <- nb2listw(d2, zero.policy=TRUE, style=scheme[i])
  spatial_weights_d3 <- nb2listw(d3, zero.policy=TRUE, style=scheme[i])
  spatial_weights_d4 <- nb2listw(d4, zero.policy=TRUE, style=scheme[i])
  for (p in 1:nrow(path_equation)) {
    schemes <- rep(c(paste(scheme[i])), times=5)
    paths <- rep(c(paste(path_equation[p,1])), times=5)
    models <- c("lm_mod", "error_d1", "error_d2", "error_d3", "error_d4")
    results_path <- data.frame(scheme=schemes, path=paths, model=models)
    # OLS model 
    lm_mod <- lm(paste(path_equation[p,2]),
                 data = all_variables)
    lm_mod_s <- summary(lm_mod)
    R2_lm <- lm_mod_s[["adj.r.squared"]]
    # SAR error models 
    # Minimum distance (d1)
    error_d1 <- spatialreg::errorsarlm(paste(path_equation[p,2]),
                                       data = all_variables,listw = spatial_weights_d1,tol=1e-12,zero.policy=T)
    error_d1_s <-summary(error_d1, Nagelkerke=TRUE)
    R2_error_d1 <- error_d1_s$NK
    # Maximum distance (d2)
    error_d2 <- spatialreg::errorsarlm(paste(path_equation[p,2]),
                                       data = all_variables,listw = spatial_weights_d2, tol=1e-12,zero.policy=T)
    error_d2_s <-summary(error_d2, Nagelkerke=TRUE)
    R2_error_d2 <- error_d2_s$NK
    # Minimum distance (d3)
    error_d3 <- spatialreg::errorsarlm(paste(path_equation[p,2]),
                                       data = all_variables,listw = spatial_weights_d3,tol=1e-12,zero.policy=T)
    error_d3_s <-summary(error_d3, Nagelkerke=TRUE)
    R2_error_d3 <- error_d3_s$NK
    # Maximum distance (d4)
    error_d4 <- spatialreg::errorsarlm(paste(path_equation[p,2]),
                                       data = all_variables,listw = spatial_weights_d4, tol=1e-12,zero.policy=T)
    error_d4_s <-summary(error_d4, Nagelkerke=TRUE)
    R2_error_d4 <- error_d4_s$NK
    # Save R2 (pseudo R2 for errorsar) and AIC
    results_path$R2_models <- c(R2_lm, R2_error_d1, R2_error_d2, R2_error_d3, R2_error_d4)
    results_path$AIC <- AIC(lm_mod, error_d1, error_d2, error_d3, error_d4)
    write.csv(results_path, file=paste(path_equation[p,1], ".csv"),
              row.names=F)
    # Make correlograms of residual autocorrelation
    cor.ols1.res<-correlog(all_variables$X_Behrmann, all_variables$Y_Behrmann, z=residuals(lm_mod), na.rm=TRUE, increment=1, resamp=1)
    cor.sar1.res<-correlog(all_variables$X_Behrmann, all_variables$Y_Behrmann, z=residuals(error_d1),na.rm=TRUE, increment=1, resamp=1)
    cor.sar2.res<-correlog(all_variables$X_Behrmann, all_variables$Y_Behrmann, z=residuals(error_d2), na.rm=TRUE, increment=1, resamp=1)
    cor.sar3.res<-correlog(all_variables$X_Behrmann, all_variables$Y_Behrmann, z=residuals(error_d3), na.rm=TRUE, increment=1, resamp=1)
    cor.sar4.res<-correlog(all_variables$X_Behrmann, all_variables$Y_Behrmann, z=residuals(error_d4), na.rm=TRUE, increment=1, resamp=1)
    # Save correlograms in a jpeg file
    jpeg(filename = paste(path_equation[p,1],".jpg"),
         width = 215, height = 279, units = "mm",res = 600)
    par(mfrow = c(5, 1))
    plot(cor.ols1.res, xlab = "Distance (m)", ylab = "Moran's I", ylim=c(-1,1), type = "l",
         lwd= 2, main = paste(models[1]), cex.main=2, cex.lab=1.8, cex.axis=1.5)
    abline(h=0, lty=5)
    plot(cor.sar1.res, xlab = "Distance (m)", ylab = "Moran's I", ylim=c(-1,1), type = "l",
         lwd= 2, main =paste(models[2]), cex.main=2, cex.lab=1.8, cex.axis=1.5)
    abline(h=0, lty=5)
    plot(cor.sar2.res, xlab = "Distance (m)", ylab = "Moran's I", ylim=c(-1,1), type = "l",
         lwd= 2, main = paste(models[3]), cex.main=2, cex.lab=1.8, cex.axis=1.5)
    abline(h=0, lty=5)
    plot(cor.sar3.res, xlab = "Distance (m)", ylab = "Moran's I", ylim=c(-1,1), type = "l",
         lwd= 2, main = paste(models[4]), cex.main=2, cex.lab=1.8, cex.axis=1.5)
    abline(h=0, lty=5)
    plot(cor.sar4.res, xlab = "Distance (m)", ylab = "Moran's I", ylim=c(-1,1), type = "l",
         lwd= 2, main = paste(models[5]), cex.main=2, cex.lab=1.8, cex.axis=1.5)
    abline(h=0, lty=5)
    dev.off()
    # Save path results
    save.image(file=paste(path_equation[p,1], ".RData"))
  }
}

### Summarize the results in a data frame easily accessible ###
results <- data.frame()
for (i in 1:length(scheme)){
  setwd(dir_work)
  # Extract results from each scheme directory
  dir_scheme <- scheme[i]
  setwd(dir_scheme)
  scheme_results_list <- list.files(path = ".", pattern= '.csv$')
  # Join all results
  scheme_results <- lapply(scheme_results_list, read.csv)
  scheme_results2 <- do.call("rbind", scheme_results)
  results <- rbind(results, scheme_results2)
}

### Back to your working directory
setwd("H:/Passport/Macroecology_urban_birds/data/output_data/Results_pSEM_postGCB")

### Save the model selection results in a csv 
write.csv(results, file="results_model_selection.csv", row.names=F)

### Check results and select the better fitted scheme and model
# by the lower AIC and the higher pseudo-R for each path

### Make a text pattern to keep
path1_words <- c("path1")
path2_words <- c("path2")
path3_words <- c("path3")
path4_words <- c("path4")
path5_words <- c("path5")

### Make a filter for path1
path1_pSEM <- filter(results, grepl(paste(path1_words, collapse="|"), path)) ### 15 records

path2_pSEM <- filter(results, grepl(paste(path2_words, collapse="|"), path)) ### 15 records

path3_pSEM <- filter(results, grepl(paste(path3_words, collapse="|"), path)) ### 15 records

path4_pSEM <- filter(results, grepl(paste(path4_words, collapse="|"), path)) ### 15 records

path5_pSEM <- filter(results, grepl(paste(path5_words, collapse="|"), path)) ### 15 records

### Piecewise Structural Equation modelling evaluation ###
# After the model selection we choose the "W" spatial weighting matrix with d2 distance (maximum distance) for all paths 

### Create spatial weighting matrix
spatial_weights_d2_S <- nb2listw(d2, zero.policy=TRUE, style="S")
spatial_weights_d2_W <- nb2listw(d2, zero.policy=TRUE, style="W")
spatial_weights_d2_S <- nb2listw(d2, zero.policy=TRUE, style="S")
spatial_weights_d2_C <- nb2listw(d2, zero.policy=TRUE, style="C")
spatial_weights_d4_S <- nb2listw(d4, zero.policy=TRUE, style="S")
                                 
### AIC = 178.119, 
### Testing the theoretical pSEM model: Fisher's C = 46.119 with P-value = 0 and on 18 degrees of freedom
sem_sar_model_theoretical <- piecewiseSEM::psem(
  # 1 # Equation 1: Species richness as response
  spatialreg::errorsarlm(Richness ~ Prec + Tmin + NDVI + Elev + road_density + city_age + gdp + area + shape_ind + dist_rivers + dist_coasts + dist_mounta,
                         data = all_variables,
                         listw = spatial_weights_d2_S,
                         tol=1e-12,zero.policy=T),
  # 2 # Equation 2: sesMPD_prum as response
  spatialreg::errorsarlm(sesMPD_prum ~ Richness + Prec + Tmin + NDVI + Elev + road_density + city_age + gdp + area + shape_ind + dist_rivers + dist_coasts + dist_mounta,
                         data = all_variables,
                         listw = spatial_weights_d2_W,
                         tol=1e-12,zero.policy=T),
  # 3 # Equation 3: sesFMPD as response  
  spatialreg::errorsarlm(sesFMPD ~ sesMPD_prum + Prec + Tmin + NDVI + Elev + road_density + city_age + gdp + area + shape_ind + dist_rivers + dist_coasts + dist_mounta,
                         data = all_variables,
                         listw = spatial_weights_d2_S,
                         tol=1e-12,zero.policy=T),
  # 4 # Equation 4: Tmin as response
  spatialreg::errorsarlm(Tmin ~ Prec + road_density + city_age + gdp + area + dist_rivers + dist_coasts + dist_mounta,
                         data = all_variables,
                         listw = spatial_weights_d2_C,
                         tol=1e-12,zero.policy=T),
  # 5 # Equation 5: NDVI as response
  spatialreg::errorsarlm(NDVI ~ Prec + Tmin + Elev + city_age + dist_coasts,
                         data = all_variables,
                         listw = spatial_weights_d4_S,
                         tol=1e-12,zero.policy=T),
  data=all_variables)

### Run summary model 
summary_sar_model_theoretical <- summary(sem_sar_model_theoretical)

### Save RData with the pSEM model and summary model
save(sem_sar_model_theoretical, summary_sar_model_theoretical, file="H:/Passport/Macroecology_urban_birds/data/output_data/Results_pSEM_postGCB/psem_sar_theoretical_.RData")

### Extract path coefficients
coefs_sar_model_theoretical <- coefs(sem_sar_model_theoretical)

# Save coefficients
write.csv(coefs_sar_model_theoretical, file = "H:/Passport/Macroecology_urban_birds/data/output_data/Results_pSEM_postGCB/coefs_psem_sar_model_theoretical.csv")

### AIC = 141.893, Fisher's C = 69.893 with P-value = 0.136 and on 58 degrees of freedom
#### SI SALIO!!!!!!
sem_sar_model_final <- piecewiseSEM::psem(
  # 1 # Equation 1: Species richness as response
  spatialreg::errorsarlm(Richness ~ Tmin + Elev + city_age + area,
                         data=all_variables,
                         listw = spatial_weights_d2_S,
                         tol=1e-12,zero.policy=T),
  # 2 # Equation 2: sesMPD_prum as response
  spatialreg::errorsarlm(sesMPD_prum ~ Richness + Prec + Tmin + Elev + road_density + gdp + dist_rivers + dist_coasts,
                         data=all_variables,
                         listw = spatial_weights_d2_W,
                         tol=1e-12,zero.policy=T),
  # 3 # Equation 3: sesFMPD as response  
  spatialreg::errorsarlm(sesFMPD ~ sesMPD_prum + NDVI + road_density,
                         data=all_variables,
                         listw = spatial_weights_d2_S,
                         tol=1e-12,zero.policy=T),
  # 4 # Equation 4: Tmin as response
  spatialreg::errorsarlm(Tmin ~ Prec + gdp + dist_coasts,
                         data = all_variables,
                         listw = spatial_weights_d2_C,
                         tol=1e-12,zero.policy=T),
  # 4 # Equation 5: NDVI as response
  spatialreg::errorsarlm(NDVI ~ Tmin + city_age + dist_coasts,
                         data = all_variables,
                         listw = spatial_weights_d4_S,
                         tol=1e-12,zero.policy=T),
  data=all_variables)

### Run summary model 
summary_sar_model_final <- summary(sem_sar_model_final)

### Save pSEM model and summary model
save(sem_sar_model_final, summary_sar_model_final, file = "H:/Passport/Macroecology_urban_birds/data/output_data/Results_pSEM_postGCB/psem_sar_final.RData")

### Extract path coefficients
coefs_sar_model_final<- coefs(sem_sar_model_final)

### Save coefficients
write.csv(coefs_sar_model_final, file = "H:/Passport/Macroecology_urban_birds/data/output_data/Results_pSEM_postGCB/coefs_psem_sar_modelo_final.csv")

### Set working directory
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Save Workspace
save.image("../data/output_data/Workspaces/piecewiseSEM_postGCB.Rdata")



##########################################################################


#### MACROECOLOGICAL HYPOTHESIS TESTING ####

### Generate best model for multifaceted avian diversity according pSEM parameters ###

## BEST MODEL - Richness: AIC = 139.0639
best_model_richness <- spatialreg::errorsarlm(Richness ~ Tmin + Elev + city_age + area,
                                              data=all_variables,
                                              listw = spatial_weights_d2_S,
                                              tol=1e-12,zero.policy=T)

summary(best_model_richness, Nagelkerke = TRUE)
AIC(best_model_richness)

### BEST MODEL - sesMPD: AIC = 227.8501
best_model_sesMPD_prum <- spatialreg::errorsarlm(sesMPD_prum ~ Richness + Prec + Tmin + Elev + dist_rivers + dist_coasts,
                                                 data=all_variables,
                                                 listw = spatial_weights_d2_W,
                                                 tol=1e-12,zero.policy=T) 

summary(best_model_sesMPD_prum, Nagelkerke = TRUE)
AIC(best_model_sesMPD_prum)

### BEST MODEL - sesFMPD: AIC = 146.5209
best_model_sesFMPD <- spatialreg::errorsarlm(sesFMPD ~ sesMPD_prum + NDVI + road_density,
                                             data=all_variables,
                                             listw = spatial_weights_d2_S,
                                             tol=1e-12,zero.policy=T) 

summary(best_model_sesFMPD, Nagelkerke = TRUE)
AIC(best_model_sesFMPD)

### Generate models explaining the Latitudinal gradient diversity for H1 ###
### Richness: AIC = 182.9327
H1_model_richness <- spatialreg::errorsarlm(Richness ~ NDVI,
                                            data=all_variables,
                                            listw = spatial_weights_d2_S,
                                            tol=1e-12,zero.policy=T)

summary(H1_model_richness, Nagelkerke = TRUE)
AIC(H1_model_richness)

### sesMPD: AIC = 244.9901
H1_model_sesMPD_prum <- spatialreg::errorsarlm(sesMPD_prum ~ NDVI,
                                               data=all_variables,
                                               listw = spatial_weights_d2_W,
                                               tol=1e-12,zero.policy=T) 

summary(H1_model_sesMPD_prum, Nagelkerke = TRUE)
AIC(H1_model_sesMPD_prum)

### sesFMPD: AIC = 192.2402
H1_model_sesFMPD <- spatialreg::errorsarlm(sesFMPD ~ NDVI,
                                           data=all_variables,
                                           listw = spatial_weights_d2_S,
                                           tol=1e-12,zero.policy=T) 

summary(H1_model_sesFMPD, Nagelkerke = TRUE)
AIC(H1_model_sesFMPD)

### Generate models explaining the Latitudinal gradient diversity for H2 ###
### Richness: AIC = 179.2991
H2_model_richness <- spatialreg::errorsarlm(Richness ~ NDVI + Prec + dist_rivers + dist_coasts,
                                            data=all_variables,
                                            listw = spatial_weights_d2_S,
                                            tol=1e-12,zero.policy=T)

summary(H2_model_richness, Nagelkerke = TRUE)
AIC(H2_model_richness)

### sesMPD: AIC = 241.8871
H2_model_sesMPD_prum <- spatialreg::errorsarlm(sesMPD_prum ~ NDVI + Prec + dist_rivers + dist_coasts,
                                               data=all_variables,
                                               listw = spatial_weights_d2_W,
                                               tol=1e-12,zero.policy=T) 

summary(H2_model_sesMPD_prum, Nagelkerke = TRUE)
AIC(H2_model_sesMPD_prum)

### sesFMPD: AIC = 197.9867
H2_model_sesFMPD <- spatialreg::errorsarlm(sesFMPD ~ NDVI + Prec + dist_rivers + dist_coasts,
                                           data=all_variables,
                                           listw = spatial_weights_d2_S,
                                           tol=1e-12,zero.policy=T) 

summary(H2_model_sesFMPD, Nagelkerke = TRUE)
AIC(H2_model_sesFMPD)

### Generate models explaining the Latitudinal gradient diversity for H3 ###
### Richness: AIC = 170.9802
H3_model_richness <- spatialreg::errorsarlm(Richness ~ Tmin + Prec,
                                            data=all_variables,
                                            listw = spatial_weights_d2_S,
                                            tol=1e-12,zero.policy=T)

summary(H3_model_richness, Nagelkerke = TRUE)
AIC(H3_model_richness)

### sesMPD: AIC = 241.3735
H3_model_sesMPD_prum <- spatialreg::errorsarlm(sesMPD_prum ~ Tmin + Prec,
                                               data=all_variables,
                                               listw = spatial_weights_d2_W,
                                               tol=1e-12,zero.policy=T) 

summary(H3_model_sesMPD_prum, Nagelkerke = TRUE)
AIC(H3_model_sesMPD_prum)

### sesFMPD: AIC = 198.624
H3_model_sesFMPD <- spatialreg::errorsarlm(sesFMPD ~ Tmin + Prec,
                                           data=all_variables,
                                           listw = spatial_weights_d2_S,
                                           tol=1e-12,zero.policy=T) 

summary(H3_model_sesFMPD, Nagelkerke = TRUE)
AIC(H3_model_sesFMPD)

### Generate models explaining the Latitudinal gradient diversity for H4 ###
### Richness: AIC = 181.5426
H4_model_richness <- spatialreg::errorsarlm(Richness ~ Elev + dist_mounta,
                                            data=all_variables,
                                            listw = spatial_weights_d2_S,
                                            tol=1e-12,zero.policy=T) 

summary(H4_model_richness, Nagelkerke = TRUE)
AIC(H4_model_richness)

### sesMPD: AIC = 242.8621
H4_model_sesMPD_prum <- spatialreg::errorsarlm(sesMPD_prum ~ Elev + dist_mounta,
                                               data=all_variables,
                                               listw = spatial_weights_d2_W,
                                               tol=1e-12,zero.policy=T) 

summary(H4_model_sesMPD_prum, Nagelkerke = TRUE)
AIC(H4_model_sesMPD_prum)

### sesFMPD: AIC = 197.5074
H4_model_sesFMPD <- spatialreg::errorsarlm(sesFMPD ~ Elev + dist_mounta,
                                           data=all_variables,
                                           listw = spatial_weights_d2_S,
                                           tol=1e-12,zero.policy=T)

summary(H4_model_sesFMPD, Nagelkerke = TRUE)
AIC(H4_model_sesFMPD)

### Generate models explaining the Latitudinal gradient diversity for H5 ###
### Richness: AIC = 173.1775
H5_model_richness <- spatialreg::errorsarlm(Richness ~ NDVI + road_density + area + shape_ind + city_age + dist_rivers + dist_coasts,
                                            data=all_variables,
                                            listw = spatial_weights_d2_S,
                                            tol=1e-12,zero.policy=T)

summary(H5_model_richness, Nagelkerke = TRUE)
AIC(H5_model_richness)

### sesMPD: AIC = 246.0158
H5_model_sesMPD_prum <- spatialreg::errorsarlm(sesMPD_prum ~ NDVI + road_density + area + shape_ind + city_age + dist_rivers + dist_coasts,
                                               data=all_variables,
                                               listw = spatial_weights_d2_W,
                                               tol=1e-12,zero.policy=T)

summary(H5_model_sesMPD_prum, Nagelkerke = TRUE)
AIC(H5_model_sesMPD_prum)

### sesFMPD: AIC = 200.3076
H5_model_sesFMPD <- spatialreg::errorsarlm(sesFMPD ~ NDVI + road_density + area + shape_ind + city_age + dist_rivers + dist_coasts,
                                           data=all_variables,
                                           listw = spatial_weights_d2_S,
                                           tol=1e-12,zero.policy=T) 

summary(H5_model_sesFMPD, Nagelkerke = TRUE)
AIC(H5_model_sesFMPD)


### Generate models explaining the Latitudinal gradient diversity for H6 ###
### Richness: AIC = 175.9213
H6_model_richness <- spatialreg::errorsarlm(Richness ~ gdp,
                                            data=all_variables,
                                            listw = spatial_weights_d2_S,
                                            tol=1e-12,zero.policy=T)

summary(H6_model_richness, Nagelkerke = TRUE)
AIC(H6_model_richness)

### sesMPD: AIC = 240.6761
H6_model_sesMPD_prum <- spatialreg::errorsarlm(sesMPD_prum ~ gdp,
                                               data=all_variables,
                                               listw = spatial_weights_d2_W,
                                               tol=1e-12,zero.policy=T)

summary(H6_model_sesMPD_prum, Nagelkerke = TRUE)
AIC(H6_model_sesMPD_prum)

### sesFMPD: AIC = 196.7367
H6_model_sesFMPD <- spatialreg::errorsarlm(sesFMPD ~ gdp,
                                           data=all_variables,
                                           listw = spatial_weights_d2_S,
                                           tol=1e-12,zero.policy=T) 

summary(H6_model_sesFMPD, Nagelkerke = TRUE)
AIC(H6_model_sesFMPD)
