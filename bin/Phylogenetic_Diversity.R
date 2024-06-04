#####################################################
# Script to quantify phylogenetic diversity metrics #
#####################################################

### Script elaborated by Israel Moreno-Contreras (Ornithology, UNAM)

### Install libraries
library(ape)
library(picante)
library(vegan)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(adephylo)
library("PhyloMeasures") ### Fast computaion of phylogenetic metrics
library("phytools") ### Force ultrametric tree

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspaces
load("..//data//output_data//Workspaces//Phylogenetic_Diversity.Rdata")

### Read Prum prhylogenetic tree including 9993 bird species 
Prum_tree <- read.tree("../data/raw_data/Phylogenetic_information/Prum_merge_hackett_stage2_1K_mcc.tre")

### Force Prum ultrametric tree
Prum_tree_ultra <- force.ultrametric(Prum_tree, method = "extend")

### Read Hackett phylogenetic tree including 9993 bird species 
Hackett_tree <- read.nexus("../data/raw_data/Phylogenetic_information/Hackett_Stage2_1K_MCCtree_CommonAncestor.nex")

### Read csv archive of the community-level data
comm <- read.csv("..\\data\\raw_data\\Community_level_data\\bird_sample_urban_regional_2.csv", header = TRUE, row.names = 1)

comm_2 <- read.csv("..\\data\\raw_data\\Community_level_data\\bird_sample_urban_regional_2.csv", header = TRUE)

### Convert data frame containing bird communities into matrix
comm_matrix <- as.matrix(comm)

### Prune the Prum phylogenetic tree tree before running pd
pruned_prum_tree <- prune.sample(comm, Prum_tree_ultra)

### Prune Hackett phylogenetic tree before running pd
pruned_hack_tree <- prune.sample(comm, Hackett_tree)

### Check philogenetic tips and community data
Birds_picCleanTree <- match.phylo.comm(phy = pruned_prum_tree, comm = comm)$phy

### Export pruned Prum phylogenetic tree file in parenthetic Format
write.tree(pruned_prum_tree, file = "..\\data\\raw_data\\Phylogenetic_information\\Prum_prunned_tree.tre")

### Export pruned Hackett phylogenetic tree file in parenthetic Format
write.tree(pruned_hack_tree, file = "..\\data\\raw_data\\Phylogenetic_information\\Hackett_prunned_tree.tre")

### Read pruned Phylogenetic tree (Prum et al. 2015 backbone)
pruned_prum_tree <- read.tree("..\\data\\raw_data\\Phylogenetic_information\\Prum_prunned_tree.tre")


#######################################################################################################
###### Prum backbone

### Calculates the Phylogenetic diversity using PhyloMeasures and Prum backbone
PD_prum <- pd.query(pruned_prum_tree, comm_matrix, standardize = FALSE, 
         null.model = "uniform", reps = 1000, seed)

### Calculates the Phylogenetic diversity using PhyloMeasures and Prum backbone
PD_estand_prum <- pd.query(pruned_prum_tree, comm_matrix, standardize = TRUE, 
         null.model = "uniform", reps= 1000, seed)

### Calculates the P-values of Phylogenetic diversity using PhyloMeasures and Prum backbone
PD_pvalue_prum <- pd.pvalues(pruned_prum_tree, comm_matrix, null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures and Prum backbone
MPD_prum <- mpd.query(pruned_prum_tree, comm_matrix, standardize = FALSE, 
                    null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures and Prum backbone
MPD_estand_prum <- mpd.query(pruned_prum_tree, comm_matrix, standardize = TRUE, 
                           null.model = "uniform", reps = 1000, seed)

### Calculates the P-values of Mean Pairwise Distance using PhyloMeasures and Prum backbone
MPD_pvalue_prum <- mpd.pvalues(pruned_prum_tree, comm_matrix, null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures and Prum backbone
MNTD_prum <- mntd.query(pruned_prum_tree, comm_matrix, standardize = FALSE, 
                      null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures and Prum backbone
MNTD_estand_prum <- mntd.query(pruned_prum_tree, comm_matrix, standardize = TRUE, 
                             null.model = "uniform", reps = 1000, seed)

### Calculates the P-values of Mean Pairwise Distance using PhyloMeasures and Prum backbone
MNTD_pvalue_prum <- mntd.pvalues(pruned_prum_tree, comm_matrix, null.model = "uniform", reps = 1000, seed)


#######################################################################################################
###### Hackett backbone

### Calculates the Phylogenetic diversity using PhyloMeasures and Hackett backbone
PD_hack <- pd.query(pruned_hack_tree, comm_matrix, standardize = FALSE, 
                    null.model = "uniform", reps = 1000, seed)

### Calculates the Phylogenetic diversity using PhyloMeasures and Hackett backbone
PD_estand_hack <- pd.query(pruned_hack_tree, comm_matrix, standardize = TRUE, 
                           null.model = "uniform", reps = 1000, seed)

### Calculates the P-values of Phylogenetic diversity using PhyloMeasures and Hackett backbone
PD_pvalue_hack <- pd.pvalues(pruned_hack_tree, comm_matrix, null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures and Prum backbone
MPD_hack <- mpd.query(pruned_hack_tree, comm_matrix, standardize = FALSE, 
                      null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures and Hackett backbone
MPD_estand_hack <- mpd.query(pruned_hack_tree, comm_matrix, standardize = TRUE, 
                             null.model = "uniform", reps = 1000, seed)

### Calculates the P-values of Mean Pairwise Distance using PhyloMeasures and Hackett backbone
MPD_pvalue_hack <- mpd.pvalues(pruned_hack_tree, comm_matrix, null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures and Hackett backbone
MNTD_hack <- mntd.query(pruned_hack_tree, comm_matrix, standardize = FALSE, 
                        null.model = "uniform", reps = 1000, seed)

### Calculates the Mean Pairwise Distance using PhyloMeasures and Hackett backbone
MNTD_estand_hack <- mntd.query(pruned_hack_tree, comm_matrix, standardize = TRUE, 
                               null.model = "uniform", reps = 1000, seed)

### Calculates the P-values of Mean Pairwise Distance using PhyloMeasures and Hackett backbone
MNTD_pvalue_hack <- mntd.pvalues(pruned_hack_tree, comm_matrix, null.model = "uniform", reps = 1000, seed)


### Transpose community-level data
comm_t <- as.data.frame(t(comm))

##################################################################################
### Export data for Prum backbone

### Obtain a vector for City, Richnnes, PD, SES.PD, MPD, NTI, MNTD, and NTI using Prum backbone
City <- as.vector(comm_2$X)
Richness <- as.vector(colSums(comm_t[,1:162]))
PD_faith_prum <- as.vector(PD_prum)
ses.PD_prum <- as.vector(PD_estand_prum)
MPD_norm_prum <- as.vector(MPD_prum)
ses.MPD_prum <- as.vector(MPD_estand_prum)
MNTD_norm_prum <- as.vector(MNTD_prum)
ses.MNTD_prum <- as.vector(MNTD_estand_prum)

### Obtain a data frame containing City, Richnnes, PD, SES.PD, MPD, NTI, MNTD, and NTI
PD_metrics.results_prum <- cbind.data.frame(City, Richness, PD_faith_prum, ses.PD_prum, 
                                       MPD_norm_prum, ses.MPD_prum, MNTD_norm_prum, 
                                       ses.MNTD_prum)

### Export data frame containing PD metrics (Prum backbone)
write.csv(PD_metrics.results_prum, file = "..\\data\\output_data\\Diversity_metrics\\PD_metrics.results_prum.csv")

##################################################################################
### Export data for Hackett backbone

### Obtain a vector for City, Richnnes, PD, SES.PD, MPD, NTI, MNTD, and NTI using Hackett backbone
City <- as.vector(comm_2$X)
Richness <- as.vector(colSums(comm_t[,1:162]))
PD_faith_hack <- as.vector(PD_hack)
ses.PD_hack <- as.vector(PD_estand_hack)
MPD_norm_hack <- as.vector(MPD_hack)
ses.MPD_hack <- as.vector(MPD_estand_hack)
MNTD_norm_hack <- as.vector(MNTD_hack)
ses.MNTD_hack <- as.vector(MNTD_estand_hack)

### Obtain a data frame containing City, Richnnes, PD, SES.PD, MPD, NTI, MNTD, and NTI
PD_metrics.results_hack <- cbind.data.frame(City, Richness, PD_faith_hack, ses.PD_hack, 
                                            MPD_norm_hack, ses.MPD_hack, MNTD_norm_hack, 
                                            ses.MNTD_hack)

### Export data frame containing PD metrics (Hackett backbone)
write.csv(PD_metrics.results_hack, file = "..\\data\\output_data\\Diversity_metrics\\PD_metrics.results_hack.csv")

### Save Workspace
save.image("../data/output_data/Workspaces/Phylogenetic_Diversity.Rdata")


#################################################################################################
### Make a text pattern to keep
urban_key_words <- c("urban")
regional_key_words <- c("_reg")

### Make a filter using regional_key_words
PD_metrics.results_prum_regional <- filter(PD_metrics.results_prum, grepl(paste(regional_key_words, collapse="|"), City)) ### 81 records

PD_metrics.results_hack_regional <- filter(PD_metrics.results_hack, grepl(paste(regional_key_words, collapse="|"), City)) ### 81 records

### Make a filter using urban_key_words
PD_metrics.results_prum_urban <- filter(PD_metrics.results_prum, grepl(paste(urban_key_words, collapse="|"), City)) ### 81 records

PD_metrics.results_hack_urban <- filter(PD_metrics.results_hack, grepl(paste(urban_key_words, collapse="|"), City)) ### 81 records


boxplot(PD_metrics.results_prum_regional$ses.MPD_prum, PD_metrics.results_prum_urban$ses.MPD_prum)

boxplot(PD_metrics.results_hack_regional$ses.MPD_hack, PD_metrics.results_hack_urban$ses.MPD_hack)

boxplot(PD_metrics.results_hack_urban$ses.MPD_hack, PD_metrics.results_prum_urban$ses.MPD_prum)








