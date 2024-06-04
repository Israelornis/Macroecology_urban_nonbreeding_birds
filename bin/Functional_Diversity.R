#####################################################
# Script to quantify functional diversity metrics   #
#####################################################
### Helping web: https://pedrohbraga.github.io/CommunityPhylogenetics-Workshop/CommunityPhylogenetics-Workshop.html

### Script elaborated by Israel Moreno-Contreras (Ornithology, UNAM)

### Install libraries
library(ape)
library(picante)
library(FD) ### To obtain Gower distances
library(vegan) ### To obtain Average Functional Distinctness
library(tidyverse)
library(ggrepel)
library(adephylo)
library("plyr") ### Read https://stackoverflow.com/questions/11562656/calculate-the-mean-by-group
library("factoextra")
library("PhyloMeasures") ### Fast computaion of phylogenetic metrics
library("dplyr") # For functional distinctness plot

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspaces
load("../data/output_data/Workspaces/Functional_Diversity.Rdata")

### Read CSV files in Urban_cells folder: Data provided upon reasonable request!
setwd("H:/Passport/Macroecology_urban_birds/data/raw_data/Bird_distributions/Urban_cells")

### Get data frame of all csv files of the urban species pool
range_urban <- ldply(list.files(), read.csv, header=TRUE)

### Add Category
range_urban$Species_pool <- 'Urban'

### Read CSV files in Regional_cells folder: Data provided upon reasonable request!
setwd("H:/Passport/Macroecology_urban_birds/data/raw_data/Bird_distributions/Regional_cells")

### Get data frame of all csv files of the regional species pool
range_regional <- ldply(list.files(), read.csv, header=TRUE)

### Add Category
range_regional$Species_pool <- 'Regional'

#### Merge range size
range_all_species <- rbind.data.frame(range_urban, range_regional)

### Remove species (including three aquatic species)
range_all_spe_filt <- filter(range_all_species, Species != "Calothorax_lucifer", Species != "Icterus_spurius",
                             Species != "Muscicapa_striata", Species != "Petrochelidon_pyrrhonota",
                             Species != "Selasphorus_platycercus", Species != "Streptopelia_turtur",
                             Species != "Rostratula_benghalensis", 
                             Species != "Rostratula_semicollaris",
                             Species != "Amaurornis_phoenicurus")

### Order alphabetically of range_all_spe_filt
range_all_spe_filt <- range_all_spe_filt[order(range_all_spe_filt$Species),]
range_all_spe_filt

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Read csv archive of functional traits of Tobias et al.
Tobias_Pigot_traits <- read.csv("..\\data\\raw_data\\Functional_traits\\Tobias\\Tobias_Pigot_traits.csv", header = TRUE)

### Make a text pattern to keep
man_screening <- as.vector(range_all_spe_filt$Species) 

# Remove species based on filter function and Code Column: 
Tobias_Pigot_traits_filt <- filter(Tobias_Pigot_traits, grepl(paste(man_screening, collapse="|"), Jetz_species_modified))

### Remove species of the Tobias et al. database
Tobias_Pigot_traits_filt <- filter(Tobias_Pigot_traits_filt, Jetz_species_modified != "Haliaeetus_vociferoides", 
                             Jetz_species_modified != "Lanius_excubitoroides")

### Order alphabetically of Tobias et al. database
Tobias_Pigot_traits_filt <- Tobias_Pigot_traits_filt[order(Tobias_Pigot_traits_filt$Jetz_species_modified),]
Tobias_Pigot_traits_filt

### Read csv archive of functional traits of Wilman et al.
Wilman_2014_traits <- read.csv("..\\data\\raw_data\\Functional_traits\\Wilman\\Wilman_2014.csv", header = TRUE)

# Remove species based on filter function and Code Column: 
Wilman_2014_traits_filt <- filter(Wilman_2014_traits, grepl(paste(man_screening, collapse="|"), Species))

### Remove species of the Wilman et al. database
Wilman_2014_traits_filt <- filter(Wilman_2014_traits_filt, Species != "Haliaeetus_vociferoides", 
                                   Species != "Lanius_excubitoroides")

### Order alphabetically of Wilman_2014_traits_filt
Wilman_2014_traits_filt <- Wilman_2014_traits_filt[order(Wilman_2014_traits_filt$Species),]
Wilman_2014_traits_filt


### Merge Functional traits database
functional_traits <- cbind.data.frame(range_all_spe_filt[,2:3], Tobias_Pigot_traits_filt[,8:11],
                                      Tobias_Pigot_traits_filt[,13:17], Wilman_2014_traits_filt[,11:20], 
                                      Wilman_2014_traits_filt[,25:31])

### Change name of Species distribution area column
colnames(functional_traits)[which(names(functional_traits) == "Area")] <- "Range_size_km"

### Get raw functional traits for comparison
functional_traits_raw <- functional_traits

### Export Raw functional traits for comparison
write.csv(functional_traits_raw, file = "..\\data\\raw_data\\Functional_traits\\Pruned_functional_traits\\Raw\\functional_traits_raw.csv")

### Log transformation for all morphological traits
log_functional_traits <- log(functional_traits[, 2:11] + 1)

### Merge transformed log
functional_traits <- cbind.data.frame(functional_traits[,1], log_functional_traits, functional_traits[, 12:28])

##################################################################################################
##### Trisos et al. (2014): two-step PCA #####
###  Specifically, following Miles et al. (1987), we classified
### (1) beak dimensions as trophic traits related to prey item selection and (2) wing, tail, and tarsus lengths as locomotory
### traits related to foraging substrate and maneuver. We performed PCAs on trophic and locomotory traits separately and
### then performed a further PCA using the first components from both the trophic and locomotory PCAs to derive an index
### of body size (fig. A3). 

### First step: PCA of trophic traits (Beak length [nares], Beak width, and Beak depth) https://www.datacamp.com/community/tutorials/pca-analysis-r
trophic_pca <- prcomp(functional_traits[,c(3,4,5)], center = TRUE,scale. = TRUE)

### Get data frame of trophic traits
trophic_pca_df <- as.data.frame(trophic_pca$x)

### First step: PCA of locomotory traits (tarsus length, secondary 1 wing, wing chord, hand wing and tail length) https://www.datacamp.com/community/tutorials/pca-analysis-r
locomotory_pca <- prcomp(functional_traits[,c(6:10)], center = TRUE,scale. = TRUE)

### Get data frame of locomotory traits
locomotory_pca_df <- as.data.frame(locomotory_pca$x)

### PC1 from both trophic and locomotory trait analyses were combined in a secondary PCA to create an axis
### representing overall body size

### Merge vectors of PC1s for trophic and locomotory traits, and body size
pc1_tro_loc <- cbind.data.frame(trophic_pca_df[,1], locomotory_pca_df[,1], 
                                functional_traits[, 11])

### Change name of PC1s from First PCA 
colnames(pc1_tro_loc)[which(names(pc1_tro_loc) == "trophic_pca_df[, 1]")] <- "PC1_trophic"
colnames(pc1_tro_loc)[which(names(pc1_tro_loc) == "locomotory_pca_df[, 1]")] <- "PC1_locomotory"
colnames(pc1_tro_loc)[which(names(pc1_tro_loc) == "functional_traits[, 11]")] <- "Mass"

### Second step: PCA containing log-transformed Range size, PC1 of trophic traits,
### PC1 of trophic and locomotory traits, and body size
second_pca <- prcomp(pc1_tro_loc, center = TRUE, scale. = TRUE)
  
### Get data frame of Second PCA
second_pca_df <- as.data.frame(second_pca$x)
  
### Get data frame of new transformed functional traits and dietary traits
new_funciontal_traits <- cbind.data.frame(functional_traits$`functional_traits[, 1]`, second_pca_df, 
                                          functional_traits[, 2], functional_traits[, 12:28])

### Change column name of Species name
colnames(new_funciontal_traits)[which(names(new_funciontal_traits) == "functional_traits$`functional_traits[, 1]`")] <- "Species"
colnames(new_funciontal_traits)[which(names(new_funciontal_traits) == "functional_traits[, 2]")] <- "Range_size"

### Export data frame of new functional traits without row names
write.csv(new_funciontal_traits, file = "..\\data\\raw_data\\Functional_traits\\Pruned_functional_traits\\Transformed\\transformed_funciontal_traits.csv", row.names = FALSE)

# Hereafter running this script. If it is necessary to have data on the avian geographic ranges, write a request. 
# Thank you!
### Read transformed traits file without row names
traits_transformed <- read.csv("..\\data\\raw_data\\Functional_traits\\Pruned_functional_traits\\Transformed\\transformed_funciontal_traits.csv", header = TRUE, row.names = 1)

### Convert percentages into ordinal variables as in https://www.nature.com/articles/s41597-020-00788-5/tables/4
traits_transformed <- dplyr::mutate(traits_transformed,  
              Diet.Inv = 
                dplyr::recode_factor(Diet.Inv,   
                                     `0`= 0, `10`= 0, `20`= 1,  
                                     `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                     `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                     .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    Diet.Vend = 
                                      dplyr::recode_factor(Diet.Vend,   
                                                           `0`= 0, `10`= 0, `20`= 1,  
                                                           `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    Diet.Vect = 
                                      dplyr::recode_factor(Diet.Vect,   
                                                           `0`= 0, `10`= 0, `20`= 1,  
                                                           `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    Diet.Vfish = 
                                      dplyr::recode_factor(Diet.Vfish,   
                                                           `0`= 0, `10`= 0, `20`= 1,  
                                                           `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    Diet.Vunk = 
                                      dplyr::recode_factor(Diet.Vunk,   
                                                           `0`= 0, `10`= 0, `20`= 1,  
                                                           `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    Diet.Scav = 
                                      dplyr::recode_factor(Diet.Scav,   
                                                           `0`= 0, `10`= 0, `20`= 1,  
                                                           `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    Diet.Fruit = 
                                      dplyr::recode_factor(Diet.Fruit,   
                                                           `0`= 0, `10`= 0, `20`= 1,  
                                                           `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    Diet.Nect = 
                                      dplyr::recode_factor(Diet.Nect,   
                                                           `0`= 0, `10`= 0, `20`= 1,  
                                                           `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    Diet.Seed = 
                                      dplyr::recode_factor(Diet.Seed,   
                                                           `0`= 0, `10`= 0, `20`= 1,  
                                                           `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    Diet.PlantO = 
                                      dplyr::recode_factor(Diet.PlantO,   
                                                           `0`= 0, `10`= 0, `20`= 1,  
                                                           `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    ForStrat.watbelowsurf = 
                                      dplyr::recode_factor(ForStrat.watbelowsurf,   
                                                           `0`= 0, `10`= 0, `20`= 1,  
                                                           `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    ForStrat.wataroundsurf = 
                                      dplyr::recode_factor(ForStrat.wataroundsurf,   
                                                           `0`= 0, `10`= 0, `20`= 1,  
                                                           `30`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    ForStrat.ground = 
                                      dplyr::recode_factor(ForStrat.ground,   
                                                           `0`= 0, `10`= 0, `20`= 1, `25`= 2, 
                                                           `30`= 2, `33`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    ForStrat.understory = 
                                      dplyr::recode_factor(ForStrat.understory,   
                                                           `0`= 0, `10`= 0, `20`= 1, `25`= 2, 
                                                           `30`= 2, `33`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    ForStrat.midhigh = 
                                      dplyr::recode_factor(ForStrat.midhigh,   
                                                           `0`= 0, `10`= 0, `20`= 1, `25`= 2, 
                                                           `30`= 2, `33`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    ForStrat.canopy = 
                                      dplyr::recode_factor(ForStrat.canopy,   
                                                           `0`= 0, `10`= 0, `20`= 1, `25`= 2, 
                                                           `30`= 2, `33`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))

traits_transformed <- dplyr::mutate(traits_transformed,  
                                    ForStrat.aerial = 
                                      dplyr::recode_factor(ForStrat.aerial,   
                                                           `0`= 0, `10`= 0, `20`= 1, `25`= 2, 
                                                           `30`= 2, `33`= 2, `40`= 2, `50`= 2, `60` = 3,
                                                           `70` = 3, `80` = 3, `90` = 3, `100` = 3,
                                                           .ordered = TRUE))
              
### Read csv archive of the community-level data
comm <- read.csv("..\\data\\raw_data\\Community_level_data\\bird_sample_urban_regional_2.csv", header = TRUE, row.names = 1)

comm_2 <- read.csv("..\\data\\raw_data\\Community_level_data\\bird_sample_urban_regional_2.csv", header = TRUE)

### Convert data frame containing bird communities into matrix
comm_matrix <- as.matrix(comm)

### Create a Gower Dissimilarity matrix from traits data
gower_distance <- gowdis(traits_transformed, ord = "podani")

### Obtain functional dendogram using a UPGMA algorithm
func_den <- hclust(gower_distance, method = "average")

### Plot a cluster (dendogram) using a UPGMA algorithm
plot(func_den)

### Convert dendrogram into "phylo" format
functional_tree <- as.phylo(func_den) 

### Export Write tree file in parenthetic Format
write.tree(functional_tree, file = "..\\data\\raw_data\\Functional_traits\\Functional_tree\\functional_tree_scaled.tre") 

### Read functional tree 
functional_tree <- read.tree("..\\data\\raw_data\\Functional_traits\\Functional_tree\\functional_tree_scaled.tre")

#######################################################################################################
###### Functional Diversity, ses FD, MFD, 

### Calculates the Functional diversity using PhyloMeasures 
FD_gow <- pd.query(functional_tree, comm_matrix, standardize = FALSE, 
                    null.model = "uniform", reps = 1000, seed)

### Calculates the Functional diversity using PhyloMeasures 
FD_estand_gow <- pd.query(functional_tree, comm_matrix, standardize = TRUE, 
                           null.model = "uniform", reps= 1000, seed)

### Calculates the P-values of Functional diversity using PhyloMeasures 
FD_pvalue_gow <- pd.pvalues(functional_tree, comm_matrix, null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures 
MFD_gow <- mpd.query(functional_tree, comm_matrix, standardize = FALSE, 
                      null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures 
MFD_estand_gow <- mpd.query(functional_tree, comm_matrix, standardize = TRUE, 
                             null.model = "uniform", reps = 1000, seed)

### Calculates the P-values of Mean Pairwise Distance using PhyloMeasures 
FMPD_pvalue_gow <- mpd.pvalues(functional_tree, comm_matrix, null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures 
FMNTD_gow <- mntd.query(functional_tree, comm_matrix, standardize = FALSE, 
                        null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures
FMNTD_estand_gow <- mntd.query(functional_tree, comm_matrix, standardize = TRUE, 
                               null.model = "uniform", reps = 1000, seed)

### Calculates the P-values of Mean Pairwise Distance using PhyloMeasures 
FMNTD_pvalue_gow <- mntd.pvalues(functional_tree, comm_matrix, null.model = "uniform", reps = 1000, seed)

### Transpose community-level data
comm_t <- as.data.frame(t(comm))

##################################################################################
### Export data for Functional diversity metrics

### Obtain a vector for City, Richness, FD, SES.FD, FMPD, NTI, FMNTD, and NTI
City <- as.vector(comm_2$X)
Richness <- as.vector(colSums(comm_t[,1:162]))
FD_gow_vec <- as.vector(FD_gow)
FD_estand_gow_vec <- as.vector(FD_estand_gow)
MFD_gow_vec <- as.vector(MFD_gow)
MFD_estand_gow_vec <- as.vector(MFD_estand_gow)
FMNTD_gow_vec <- as.vector(FMNTD_gow)
FMNTD_estand_gow_vec <- as.vector(FMNTD_estand_gow)

### Obtain a data frame containing City, Richnnes, PD, SES.PD, MPD, NTI, MNTD, and NTI
FD_metrics.results <- cbind.data.frame(City, Richness, FD_gow_vec, FD_estand_gow_vec, 
                                            MFD_gow_vec, MFD_estand_gow_vec, FMNTD_gow_vec, 
                                            FMNTD_estand_gow_vec)

### Export data frame containing FD metrics (Gower distance)
write.csv(FD_metrics.results, file = "..\\data\\output_data\\Diversity_metrics\\FD_metrics.results.csv")

### Save Workspace
save.image("../data/output_data/Workspaces/Functional_Diversity.Rdata")

