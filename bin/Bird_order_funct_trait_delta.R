#####################################################
# Script to calculate Bird taxonomy categories      #
#####################################################
### Helping web: https://pedrohbraga.github.io/CommunityPhylogenetics-Workshop/CommunityPhylogenetics-Workshop.html

### Script elaborated by Israel Moreno-Contreras (Ornithology, UNAM)

### Call libraries
library("plyr") ### Read https://stackoverflow.com/questions/11562656/calculate-the-mean-by-group
library("dplyr") # For functional distinctness plot
library("WRS2") # Percentile t bootstrap method
library("ggpubr") ## For boxplots
library("cowplot") ## For merge images

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspace
load("../data/output_data/Workspaces/Bird_order_funct_trait_delta.Rdata")

### Read CSV files of Bird taxonomy
urban_species_pool <- read.csv("..\\data\\raw_data\\Community_level_data\\Winter_birds_urban_list_species_genera.csv", header = TRUE)

regional_species_pool <- read.csv("..\\data\\raw_data\\Community_level_data\\Winter_birds_regional_list_species_genera.csv", header = TRUE)

### Make a text pattern to keep
man_screening <- as.vector(urban_species_pool$BirdTree) 

man_screening_regional <- as.vector(regional_species_pool$BirdTree) 

### Read CSV files of Bird taxonomy
bird_taxonomy <- read.csv("..\\data\\raw_data\\Taxonomic_information\\BLIOCPhyloMasterTax.csv", header = TRUE, row.names = 1)

### Remove species based on filter function and Code Column: 
## For urban species pools
bird_taxonomy_urban <- filter(bird_taxonomy, grepl(paste(man_screening, collapse="|"), TipLabel))

## For regional species pools
bird_taxonomy_regional <- filter(bird_taxonomy, grepl(paste(man_screening_regional, collapse="|"), TipLabel))

### Remove two species from the regional species pool
bird_taxonomy_regional <- filter(bird_taxonomy_regional, TipLabel != "Haliaeetus_vociferoides", 
                                 TipLabel != "Lanius_excubitoroides")

### Convert columns of Species, family, and order in factors (URBAN)
bird_taxonomy_urban$TipLabel <- as.factor(bird_taxonomy_urban$TipLabel)

bird_taxonomy_urban$BLFamilyLatin <- as.factor(bird_taxonomy_urban$BLFamilyLatin)

bird_taxonomy_urban$IOCOrder <- as.factor(bird_taxonomy_urban$IOCOrder)

### Merge bird taxonomy data
taxonomy_urban_categories <- cbind.data.frame(urban_species_pool[1:2], bird_taxonomy_urban[10], bird_taxonomy_urban[13])

### Convert columns of Species, genera, family, and order in factors
taxonomy_urban_categories$BirdTree <- as.factor(taxonomy_urban_categories$BirdTree)

taxonomy_urban_categories$Genera <- as.factor(taxonomy_urban_categories$Genera)

### List all factor levels of a data.frame using library dplyr: https://stackoverflow.com/questions/27676404/list-all-factor-levels-of-a-data-frame
taxonomy_urban_categories %>% 
  sapply(levels)

# Urban species pool for 81 cities
### 548 species, 303 genera, 70 families, and 14 orders 

### Count the number of times that a factor (taxonomic category) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_gen_urban <- taxonomy_urban_categories %>% count(Genera)
number_species_fam_urban <- taxonomy_urban_categories %>% count(BLFamilyLatin)
taxonomy_urban_categories %>% count(BirdTree)
number_species_order_urban <- taxonomy_urban_categories %>% count(IOCOrder)


##############################################################################


### Convert columns of Species, family, and order in factors (REGIONAL)
bird_taxonomy_regional$TipLabel <- as.factor(bird_taxonomy_regional$TipLabel)

bird_taxonomy_regional$BLFamilyLatin <- as.factor(bird_taxonomy_regional$BLFamilyLatin)

bird_taxonomy_regional$IOCOrder <- as.factor(bird_taxonomy_regional$IOCOrder)

### Merge bird taxonomy data for regional species pools
taxonomy_regional_categories <- cbind.data.frame(regional_species_pool[1:2], bird_taxonomy_regional[10], bird_taxonomy_regional[13])

### Convert columns of Species, genera, family, and order in factors
taxonomy_regional_categories$BirdTree <- as.factor(taxonomy_regional_categories$BirdTree)

taxonomy_regional_categories$Genera <- as.factor(taxonomy_regional_categories$Genera)

### List all factor levels of a data.frame using library(dplyr): https://stackoverflow.com/questions/27676404/list-all-factor-levels-of-a-data-frame
taxonomy_regional_categories %>% 
  sapply(levels)

# Regional species pool for 81 cities
### 2178 species, 849 genera, 106 families, and 25 orders 

### Count the number of times that a factor (taxonomic category) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_gen_regional <- taxonomy_regional_categories %>% count(Genera)
number_species_fam_regional <- taxonomy_regional_categories %>% count(BLFamilyLatin)
taxonomy_regional_categories %>% count(BirdTree)
number_species_order_regional <- taxonomy_regional_categories %>% count(IOCOrder)

##############################################################################

### Read Raw functional traits for comparison
functional_traits_raw <- read.csv(file = "..\\data\\raw_data\\Functional_traits\\Pruned_functional_traits\\Raw\\functional_traits_raw.csv", header = TRUE)

### Remove species based on filter function and Code Column: 
## For urban species pools
functional_traits_urban <- filter(functional_traits_raw, grepl(paste(man_screening, collapse="|"), Species))

## For regional species pools
functional_traits_regional <- filter(functional_traits_raw, grepl(paste(man_screening_regional, collapse="|"), Species))

### Mean values for each functional trait considering only urban species o regional species
mean(functional_traits_urban$Range_size_km)

mean(functional_traits_regional$Range_size_km)

mean(functional_traits_urban$Bill_Nares)

mean(functional_traits_regional$Bill_Nares)

mean(functional_traits_urban$Bill_Width)

mean(functional_traits_regional$Bill_Width)

mean(functional_traits_urban$Bill_Depth)

mean(functional_traits_regional$Bill_Depth)

mean(functional_traits_urban$Tarsus_Length)

mean(functional_traits_regional$Tarsus_Length)

mean(functional_traits_urban$Secondary1)

mean(functional_traits_regional$Secondary1)

mean(functional_traits_urban$Wing_Chord)

mean(functional_traits_regional$Wing_Chord)

mean(functional_traits_urban$Hand.Wing.Index..Claramunt.2011.)

mean(functional_traits_regional$Hand.Wing.Index..Claramunt.2011.)

mean(functional_traits_urban$Tail_Length)

mean(functional_traits_regional$Tail_Length)


mean(functional_traits_urban$Mass)

mean(functional_traits_regional$Mass)


mean(functional_traits_urban$Diet.Inv)

mean(functional_traits_regional$Diet.Inv)


### t-test for functional traits
t.test(functional_traits_urban$Range_size_km, functional_traits_regional$Range_size_km)
t.test(functional_traits_urban$Bill_Nares, functional_traits_regional$Bill_Nares)
t.test(functional_traits_urban$Bill_Width, functional_traits_regional$Bill_Width)
t.test(functional_traits_urban$Bill_Depth, functional_traits_regional$Bill_Depth)
t.test(functional_traits_urban$Tarsus_Length, functional_traits_regional$Tarsus_Length)
t.test(functional_traits_urban$Secondary1, functional_traits_regional$Secondary1)
t.test(functional_traits_urban$Wing_Chord, functional_traits_regional$Wing_Chord)
t.test(functional_traits_urban$Hand.Wing.Index..Claramunt.2011., functional_traits_regional$Hand.Wing.Index..Claramunt.2011.)
t.test(functional_traits_urban$Tail_Length, functional_traits_regional$Tail_Length)
t.test(functional_traits_urban$Mass, functional_traits_regional$Mass)


t.test(functional_traits_urban$Mass, mu = 0, alternative = "two.sided")


##############

### Read community-level data
bird_sample_urban_regional <- read.csv(file = "..\\data\\raw_data\\Community_level_data\\bird_sample_urban_regional_2.csv", header = TRUE, row.names = 1)

### Get only regional community-level data
bird_sample_urban_regional <- bird_sample_urban_regional[1:81,]

### Transpose community-level data
bird_sample_urban_regional <- as.data.frame(t(bird_sample_urban_regional))

### Extract rownames for each regional and species pool: https://stackoverflow.com/questions/24265649/extract-specific-row-names-from-data-frame

# Aanekoski
Aanekoski_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Aanekoski_urban == 1,])
Aanekoski_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Aanekoski_regional == 1,])

functional_Aanekoski_urban <- filter(functional_traits_raw, grepl(paste(Aanekoski_urban_filtering, collapse="|"), Species))
functional_Aanekoski_regional <- filter(functional_traits_raw, grepl(paste(Aanekoski_regional_filtering, collapse="|"), Species))

mean_Aanekoski_subs_cont <- as.data.frame(colMeans(functional_Aanekoski_urban[3:12]) - colMeans(functional_Aanekoski_regional[3:12]))

mean_Aanekoski_subs_cate <- as.data.frame(colMeans(functional_Aanekoski_urban[13:29]) - colMeans(functional_Aanekoski_regional[13:29]))

colnames(mean_Aanekoski_subs_cont) <- c('Aanekoski')
colnames(mean_Aanekoski_subs_cate) <- c('Aanekoski')


# Ain_Beida
Ain_Beida_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Ain_Beida_urban == 1,])
Ain_Beida_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Ain_Beida_regional == 1,])

functional_Ain_Beida_urban <- filter(functional_traits_raw, grepl(paste(Ain_Beida_urban_filtering, collapse="|"), Species))
functional_Ain_Beida_regional <- filter(functional_traits_raw, grepl(paste(Ain_Beida_regional_filtering, collapse="|"), Species))

mean_Ain_Beida_subs_cont <- as.data.frame(colMeans(functional_Ain_Beida_urban[3:12]) - colMeans(functional_Ain_Beida_regional[3:12]))

mean_Ain_Beida_subs_cate <- as.data.frame(colMeans(functional_Ain_Beida_urban[13:29]) - colMeans(functional_Ain_Beida_regional[13:29]))

colnames(mean_Ain_Beida_subs_cont) <- c('Ain_Beida')
colnames(mean_Ain_Beida_subs_cate) <- c('Ain_Beida')


# Asuncion
Asuncion_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Asuncion_urban == 1,])
Asuncion_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Asuncion_regional == 1,])

functional_Asuncion_urban <- filter(functional_traits_raw, grepl(paste(Asuncion_urban_filtering, collapse="|"), Species))
functional_Asuncion_regional <- filter(functional_traits_raw, grepl(paste(Asuncion_regional_filtering, collapse="|"), Species))

mean_Asuncion_subs_cont <- as.data.frame(colMeans(functional_Asuncion_urban[3:12]) - colMeans(functional_Asuncion_regional[3:12]))

mean_Asuncion_subs_cate <- as.data.frame(colMeans(functional_Asuncion_urban[13:29]) - colMeans(functional_Asuncion_regional[13:29]))

colnames(mean_Asuncion_subs_cont) <- c('Asuncion')
colnames(mean_Asuncion_subs_cate) <- c('Asuncion')


# Belem
Belem_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Belem_urban == 1,])
Belem_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Belem_regional == 1,])

functional_Belem_urban <- filter(functional_traits_raw, grepl(paste(Belem_urban_filtering, collapse="|"), Species))
functional_Belem_regional <- filter(functional_traits_raw, grepl(paste(Belem_regional_filtering, collapse="|"), Species))

mean_Belem_subs_cont <- as.data.frame(colMeans(functional_Belem_urban[3:12]) - colMeans(functional_Belem_regional[3:12]))

mean_Belem_subs_cate <- as.data.frame(colMeans(functional_Belem_urban[13:29]) - colMeans(functional_Belem_regional[13:29]))

colnames(mean_Belem_subs_cont) <- c('Belem')
colnames(mean_Belem_subs_cate) <- c('Belem')


# Bernal
Bernal_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Bernal_urban == 1,])
Bernal_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Bernal_regional == 1,])

functional_Bernal_urban <- filter(functional_traits_raw, grepl(paste(Bernal_urban_filtering, collapse="|"), Species))
functional_Bernal_regional <- filter(functional_traits_raw, grepl(paste(Bernal_regional_filtering, collapse="|"), Species))

mean_Bernal_subs_cont <- as.data.frame(colMeans(functional_Bernal_urban[3:12]) - colMeans(functional_Bernal_regional[3:12]))

mean_Bernal_subs_cate <- as.data.frame(colMeans(functional_Bernal_urban[13:29]) - colMeans(functional_Bernal_regional[13:29]))

colnames(mean_Bernal_subs_cont) <- c('Bernal')
colnames(mean_Bernal_subs_cate) <- c('Bernal')


# Biala_Podlaska
Biala_Podlaska_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Biala_Podlaska_urban == 1,])
Biala_Podlaska_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Biala_Podlaska_regional == 1,])

functional_Biala_Podlaska_urban <- filter(functional_traits_raw, grepl(paste(Biala_Podlaska_urban_filtering, collapse="|"), Species))
functional_Biala_Podlaska_regional <- filter(functional_traits_raw, grepl(paste(Biala_Podlaska_regional_filtering, collapse="|"), Species))

mean_Biala_Podlaska_subs_cont <- as.data.frame(colMeans(functional_Biala_Podlaska_urban[3:12]) - colMeans(functional_Biala_Podlaska_regional[3:12]))

mean_Biala_Podlaska_subs_cate <- as.data.frame(colMeans(functional_Biala_Podlaska_urban[13:29]) - colMeans(functional_Biala_Podlaska_regional[13:29]))

colnames(mean_Biala_Podlaska_subs_cont) <- c('Biala_Podlaska')
colnames(mean_Biala_Podlaska_subs_cate) <- c('Biala_Podlaska')


# Bialystok
Bialystok_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Bialystok_urban == 1,])
Bialystok_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Bialystok_regional == 1,])

functional_Bialystok_urban <- filter(functional_traits_raw, grepl(paste(Bialystok_urban_filtering, collapse="|"), Species))
functional_Bialystok_regional <- filter(functional_traits_raw, grepl(paste(Bialystok_regional_filtering, collapse="|"), Species))

mean_Bialystok_subs_cont <- as.data.frame(colMeans(functional_Bialystok_urban[3:12]) - colMeans(functional_Bialystok_regional[3:12]))

mean_Bialystok_subs_cate <- as.data.frame(colMeans(functional_Bialystok_urban[13:29]) - colMeans(functional_Bialystok_regional[13:29]))

colnames(mean_Bialystok_subs_cont) <- c('Bialystok')
colnames(mean_Bialystok_subs_cate) <- c('Bialystok')


# Bydgoszcz
Bydgoszcz_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Bydgoszcz_urban == 1,])
Bydgoszcz_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Bydgoszcz_regional == 1,])

functional_Bydgoszcz_urban <- filter(functional_traits_raw, grepl(paste(Bydgoszcz_urban_filtering, collapse="|"), Species))
functional_Bydgoszcz_regional <- filter(functional_traits_raw, grepl(paste(Bydgoszcz_regional_filtering, collapse="|"), Species))

mean_Bydgoszcz_subs_cont <- as.data.frame(colMeans(functional_Bydgoszcz_urban[3:12]) - colMeans(functional_Bydgoszcz_regional[3:12]))

mean_Bydgoszcz_subs_cate <- as.data.frame(colMeans(functional_Bydgoszcz_urban[13:29]) - colMeans(functional_Bydgoszcz_regional[13:29]))

colnames(mean_Bydgoszcz_subs_cont) <- c('Bydgoszcz')
colnames(mean_Bydgoszcz_subs_cate) <- c('Bydgoszcz')


# Ciudad_Juarez
Ciudad_Juarez_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Ciudad_Juarez_urban == 1,])
Ciudad_Juarez_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Ciudad_Juarez_regional == 1,])

functional_Ciudad_Juarez_urban <- filter(functional_traits_raw, grepl(paste(Ciudad_Juarez_urban_filtering, collapse="|"), Species))
functional_Ciudad_Juarez_regional <- filter(functional_traits_raw, grepl(paste(Ciudad_Juarez_regional_filtering, collapse="|"), Species))

mean_Ciudad_Juarez_subs_cont <- as.data.frame(colMeans(functional_Ciudad_Juarez_urban[3:12]) - colMeans(functional_Ciudad_Juarez_regional[3:12]))

mean_Ciudad_Juarez_subs_cate <- as.data.frame(colMeans(functional_Ciudad_Juarez_urban[13:29]) - colMeans(functional_Ciudad_Juarez_regional[13:29]))

colnames(mean_Ciudad_Juarez_subs_cont) <- c('Ciudad_Juarez')
colnames(mean_Ciudad_Juarez_subs_cate) <- c('Ciudad_Juarez')


# Czestochowa
Czestochowa_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Czestochowa_urban == 1,])
Czestochowa_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Czestochowa_regional == 1,])

functional_Czestochowa_urban <- filter(functional_traits_raw, grepl(paste(Czestochowa_urban_filtering, collapse="|"), Species))
functional_Czestochowa_regional <- filter(functional_traits_raw, grepl(paste(Czestochowa_regional_filtering, collapse="|"), Species))

mean_Czestochowa_subs_cont <- as.data.frame(colMeans(functional_Czestochowa_urban[3:12]) - colMeans(functional_Czestochowa_regional[3:12]))

mean_Czestochowa_subs_cate <- as.data.frame(colMeans(functional_Czestochowa_urban[13:29]) - colMeans(functional_Czestochowa_regional[13:29]))

colnames(mean_Czestochowa_subs_cont) <- c('Czestochowa')
colnames(mean_Czestochowa_subs_cate) <- c('Czestochowa')


# Durango
Durango_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Durango_urban == 1,])
Durango_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Durango_regional == 1,])

functional_Durango_urban <- filter(functional_traits_raw, grepl(paste(Durango_urban_filtering, collapse="|"), Species))
functional_Durango_regional <- filter(functional_traits_raw, grepl(paste(Durango_regional_filtering, collapse="|"), Species))

mean_Durango_subs_cont <- as.data.frame(colMeans(functional_Durango_urban[3:12]) - colMeans(functional_Durango_regional[3:12]))

mean_Durango_subs_cate <- as.data.frame(colMeans(functional_Durango_urban[13:29]) - colMeans(functional_Durango_regional[13:29]))

colnames(mean_Durango_subs_cont) <- c('Durango')
colnames(mean_Durango_subs_cate) <- c('Durango')


# Fresno
Fresno_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Fresno_urban == 1,])
Fresno_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Fresno_regional == 1,])

functional_Fresno_urban <- filter(functional_traits_raw, grepl(paste(Fresno_urban_filtering, collapse="|"), Species))
functional_Fresno_regional <- filter(functional_traits_raw, grepl(paste(Fresno_regional_filtering, collapse="|"), Species))

mean_Fresno_subs_cont <- as.data.frame(colMeans(functional_Fresno_urban[3:12]) - colMeans(functional_Fresno_regional[3:12]))

mean_Fresno_subs_cate <- as.data.frame(colMeans(functional_Fresno_urban[13:29]) - colMeans(functional_Fresno_regional[13:29]))

colnames(mean_Fresno_subs_cont) <- c('Fresno')
colnames(mean_Fresno_subs_cate) <- c('Fresno')


# Gdansk
Gdansk_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Gdansk_urban == 1,])
Gdansk_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Gdansk_regional == 1,])

functional_Gdansk_urban <- filter(functional_traits_raw, grepl(paste(Gdansk_urban_filtering, collapse="|"), Species))
functional_Gdansk_regional <- filter(functional_traits_raw, grepl(paste(Gdansk_regional_filtering, collapse="|"), Species))

mean_Gdansk_subs_cont <- as.data.frame(colMeans(functional_Gdansk_urban[3:12]) - colMeans(functional_Gdansk_regional[3:12]))

mean_Gdansk_subs_cate <- as.data.frame(colMeans(functional_Gdansk_urban[13:29]) - colMeans(functional_Gdansk_regional[13:29]))

colnames(mean_Gdansk_subs_cont) <- c('Gdansk')
colnames(mean_Gdansk_subs_cate) <- c('Gdansk')


# Gdynia
Gdynia_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Gdynia_urban == 1,])
Gdynia_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Gdynia_regional == 1,])

functional_Gdynia_urban <- filter(functional_traits_raw, grepl(paste(Gdynia_urban_filtering, collapse="|"), Species))
functional_Gdynia_regional <- filter(functional_traits_raw, grepl(paste(Gdynia_regional_filtering, collapse="|"), Species))

mean_Gdynia_subs_cont <- as.data.frame(colMeans(functional_Gdynia_urban[3:12]) - colMeans(functional_Gdynia_regional[3:12]))

mean_Gdynia_subs_cate <- as.data.frame(colMeans(functional_Gdynia_urban[13:29]) - colMeans(functional_Gdynia_regional[13:29]))

colnames(mean_Gdynia_subs_cont) <- c('Gdynia')
colnames(mean_Gdynia_subs_cate) <- c('Gdynia')


# Gorzow_Wielkopolski
Gorzow_Wielkopolski_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Gorzow_Wielkopolski_urban == 1,])
Gorzow_Wielkopolski_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Gorzow_Wielkopolski_regional == 1,])

functional_Gorzow_Wielkopolski_urban <- filter(functional_traits_raw, grepl(paste(Gorzow_Wielkopolski_urban_filtering, collapse="|"), Species))
functional_Gorzow_Wielkopolski_regional <- filter(functional_traits_raw, grepl(paste(Gorzow_Wielkopolski_regional_filtering, collapse="|"), Species))

mean_Gorzow_Wielkopolski_subs_cont <- as.data.frame(colMeans(functional_Gorzow_Wielkopolski_urban[3:12]) - colMeans(functional_Gorzow_Wielkopolski_regional[3:12]))

mean_Gorzow_Wielkopolski_subs_cate <- as.data.frame(colMeans(functional_Gorzow_Wielkopolski_urban[13:29]) - colMeans(functional_Gorzow_Wielkopolski_regional[13:29]))

colnames(mean_Gorzow_Wielkopolski_subs_cont) <- c('Gorzow_Wielkopolski')
colnames(mean_Gorzow_Wielkopolski_subs_cate) <- c('Gorzow_Wielkopolski')


# Gujranwala
Gujranwala_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Gujranwala_urban == 1,])
Gujranwala_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Gujranwala_regional == 1,])

functional_Gujranwala_urban <- filter(functional_traits_raw, grepl(paste(Gujranwala_urban_filtering, collapse="|"), Species))
functional_Gujranwala_regional <- filter(functional_traits_raw, grepl(paste(Gujranwala_regional_filtering, collapse="|"), Species))

mean_Gujranwala_subs_cont <- as.data.frame(colMeans(functional_Gujranwala_urban[3:12]) - colMeans(functional_Gujranwala_regional[3:12]))

mean_Gujranwala_subs_cate <- as.data.frame(colMeans(functional_Gujranwala_urban[13:29]) - colMeans(functional_Gujranwala_regional[13:29]))

colnames(mean_Gujranwala_subs_cont) <- c('Gujranwala')
colnames(mean_Gujranwala_subs_cate) <- c('Gujranwala')

# Gwangju
Gwangju_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Gwangju_urban == 1,])
Gwangju_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Gwangju_regional == 1,])

functional_Gwangju_urban <- filter(functional_traits_raw, grepl(paste(Gwangju_urban_filtering, collapse="|"), Species))
functional_Gwangju_regional <- filter(functional_traits_raw, grepl(paste(Gwangju_regional_filtering, collapse="|"), Species))

mean_Gwangju_subs_cont <- as.data.frame(colMeans(functional_Gwangju_urban[3:12]) - colMeans(functional_Gwangju_regional[3:12]))

mean_Gwangju_subs_cate <- as.data.frame(colMeans(functional_Gwangju_urban[13:29]) - colMeans(functional_Gwangju_regional[13:29]))

colnames(mean_Gwangju_subs_cont) <- c('Gwangju')
colnames(mean_Gwangju_subs_cate) <- c('Gwangju')


# Hamina
Hamina_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Hamina_urban == 1,])
Hamina_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Hamina_regional == 1,])

functional_Hamina_urban <- filter(functional_traits_raw, grepl(paste(Hamina_urban_filtering, collapse="|"), Species))
functional_Hamina_regional <- filter(functional_traits_raw, grepl(paste(Hamina_regional_filtering, collapse="|"), Species))

mean_Hamina_subs_cont <- as.data.frame(colMeans(functional_Hamina_urban[3:12]) - colMeans(functional_Hamina_regional[3:12]))

mean_Hamina_subs_cate <- as.data.frame(colMeans(functional_Hamina_urban[13:29]) - colMeans(functional_Hamina_regional[13:29]))

colnames(mean_Hamina_subs_cont) <- c('Hamina')
colnames(mean_Hamina_subs_cate) <- c('Hamina')


# Helsinki
Helsinki_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Helsinki_urban == 1,])
Helsinki_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Helsinki_regional == 1,])

functional_Helsinki_urban <- filter(functional_traits_raw, grepl(paste(Helsinki_urban_filtering, collapse="|"), Species))
functional_Helsinki_regional <- filter(functional_traits_raw, grepl(paste(Helsinki_regional_filtering, collapse="|"), Species))

mean_Helsinki_subs_cont <- as.data.frame(colMeans(functional_Helsinki_urban[3:12]) - colMeans(functional_Helsinki_regional[3:12]))

mean_Helsinki_subs_cate <- as.data.frame(colMeans(functional_Helsinki_urban[13:29]) - colMeans(functional_Helsinki_regional[13:29]))

colnames(mean_Helsinki_subs_cont) <- c('Helsinki')
colnames(mean_Helsinki_subs_cate) <- c('Helsinki')


# Inowroclaw
Inowroclaw_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Inowroclaw_urban == 1,])
Inowroclaw_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Inowroclaw_regional == 1,])

functional_Inowroclaw_urban <- filter(functional_traits_raw, grepl(paste(Inowroclaw_urban_filtering, collapse="|"), Species))
functional_Inowroclaw_regional <- filter(functional_traits_raw, grepl(paste(Inowroclaw_regional_filtering, collapse="|"), Species))

mean_Inowroclaw_subs_cont <- as.data.frame(colMeans(functional_Inowroclaw_urban[3:12]) - colMeans(functional_Inowroclaw_regional[3:12]))

mean_Inowroclaw_subs_cate <- as.data.frame(colMeans(functional_Inowroclaw_urban[13:29]) - colMeans(functional_Inowroclaw_regional[13:29]))

colnames(mean_Inowroclaw_subs_cont) <- c('Inowroclaw')
colnames(mean_Inowroclaw_subs_cate) <- c('Inowroclaw')


# Joensuu
Joensuu_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Joensuu_urban == 1,])
Joensuu_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Joensuu_regional == 1,])

functional_Joensuu_urban <- filter(functional_traits_raw, grepl(paste(Joensuu_urban_filtering, collapse="|"), Species))
functional_Joensuu_regional <- filter(functional_traits_raw, grepl(paste(Joensuu_regional_filtering, collapse="|"), Species))

mean_Joensuu_subs_cont <- as.data.frame(colMeans(functional_Joensuu_urban[3:12]) - colMeans(functional_Joensuu_regional[3:12]))

mean_Joensuu_subs_cate <- as.data.frame(colMeans(functional_Joensuu_urban[13:29]) - colMeans(functional_Joensuu_regional[13:29]))

colnames(mean_Joensuu_subs_cont) <- c('Joensuu')
colnames(mean_Joensuu_subs_cate) <- c('Joensuu')


# Jyvaskyla
Jyvaskyla_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Jyvaskyla_urban == 1,])
Jyvaskyla_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Jyvaskyla_regional == 1,])

functional_Jyvaskyla_urban <- filter(functional_traits_raw, grepl(paste(Jyvaskyla_urban_filtering, collapse="|"), Species))
functional_Jyvaskyla_regional <- filter(functional_traits_raw, grepl(paste(Jyvaskyla_regional_filtering, collapse="|"), Species))

mean_Jyvaskyla_subs_cont <- as.data.frame(colMeans(functional_Jyvaskyla_urban[3:12]) - colMeans(functional_Jyvaskyla_regional[3:12]))

mean_Jyvaskyla_subs_cate <- as.data.frame(colMeans(functional_Jyvaskyla_urban[13:29]) - colMeans(functional_Jyvaskyla_regional[13:29]))

colnames(mean_Jyvaskyla_subs_cont) <- c('Jyvaskyla')
colnames(mean_Jyvaskyla_subs_cate) <- c('Jyvaskyla')


# Kajaani
Kajaani_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kajaani_urban == 1,])
Kajaani_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kajaani_regional == 1,])

functional_Kajaani_urban <- filter(functional_traits_raw, grepl(paste(Kajaani_urban_filtering, collapse="|"), Species))
functional_Kajaani_regional <- filter(functional_traits_raw, grepl(paste(Kajaani_regional_filtering, collapse="|"), Species))

mean_Kajaani_subs_cont <- as.data.frame(colMeans(functional_Kajaani_urban[3:12]) - colMeans(functional_Kajaani_regional[3:12]))

mean_Kajaani_subs_cate <- as.data.frame(colMeans(functional_Kajaani_urban[13:29]) - colMeans(functional_Kajaani_regional[13:29]))

colnames(mean_Kajaani_subs_cont) <- c('Kajaani')
colnames(mean_Kajaani_subs_cate) <- c('Kajaani')


# Kathmandu
Kathmandu_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kathmandu_urban == 1,])
Kathmandu_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kathmandu_regional == 1,])

functional_Kathmandu_urban <- filter(functional_traits_raw, grepl(paste(Kathmandu_urban_filtering, collapse="|"), Species))
functional_Kathmandu_regional <- filter(functional_traits_raw, grepl(paste(Kathmandu_regional_filtering, collapse="|"), Species))

mean_Kathmandu_subs_cont <- as.data.frame(colMeans(functional_Kathmandu_urban[3:12]) - colMeans(functional_Kathmandu_regional[3:12]))

mean_Kathmandu_subs_cate <- as.data.frame(colMeans(functional_Kathmandu_urban[13:29]) - colMeans(functional_Kathmandu_regional[13:29]))

colnames(mean_Kathmandu_subs_cont) <- c('Kathmandu')
colnames(mean_Kathmandu_subs_cate) <- c('Kathmandu')


# Kauhava
Kauhava_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kauhava_urban == 1,])
Kauhava_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kauhava_regional == 1,])

functional_Kauhava_urban <- filter(functional_traits_raw, grepl(paste(Kauhava_urban_filtering, collapse="|"), Species))
functional_Kauhava_regional <- filter(functional_traits_raw, grepl(paste(Kauhava_regional_filtering, collapse="|"), Species))

mean_Kauhava_subs_cont <- as.data.frame(colMeans(functional_Kauhava_urban[3:12]) - colMeans(functional_Kauhava_regional[3:12]))

mean_Kauhava_subs_cate <- as.data.frame(colMeans(functional_Kauhava_urban[13:29]) - colMeans(functional_Kauhava_regional[13:29]))

colnames(mean_Kauhava_subs_cont) <- c('Kauhava')
colnames(mean_Kauhava_subs_cate) <- c('Kauhava')


# Kemi
Kemi_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kemi_urban == 1,])
Kemi_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kemi_regional == 1,])

functional_Kemi_urban <- filter(functional_traits_raw, grepl(paste(Kemi_urban_filtering, collapse="|"), Species))
functional_Kemi_regional <- filter(functional_traits_raw, grepl(paste(Kemi_regional_filtering, collapse="|"), Species))

mean_Kemi_subs_cont <- as.data.frame(colMeans(functional_Kemi_urban[3:12]) - colMeans(functional_Kemi_regional[3:12]))

mean_Kemi_subs_cate <- as.data.frame(colMeans(functional_Kemi_urban[13:29]) - colMeans(functional_Kemi_regional[13:29]))

colnames(mean_Kemi_subs_cont) <- c('Kemi')
colnames(mean_Kemi_subs_cate) <- c('Kemi')


# Kemijarvi
Kemijarvi_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kemijarvi_urban == 1,])
Kemijarvi_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kemijarvi_regional == 1,])

functional_Kemijarvi_urban <- filter(functional_traits_raw, grepl(paste(Kemijarvi_urban_filtering, collapse="|"), Species))
functional_Kemijarvi_regional <- filter(functional_traits_raw, grepl(paste(Kemijarvi_regional_filtering, collapse="|"), Species))

mean_Kemijarvi_subs_cont <- as.data.frame(colMeans(functional_Kemijarvi_urban[3:12]) - colMeans(functional_Kemijarvi_regional[3:12]))

mean_Kemijarvi_subs_cate <- as.data.frame(colMeans(functional_Kemijarvi_urban[13:29]) - colMeans(functional_Kemijarvi_regional[13:29]))

colnames(mean_Kemijarvi_subs_cont) <- c('Kemijarvi')
colnames(mean_Kemijarvi_subs_cate) <- c('Kemijarvi')


# Kotka
Kotka_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kotka_urban == 1,])
Kotka_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kotka_regional == 1,])

functional_Kotka_urban <- filter(functional_traits_raw, grepl(paste(Kotka_urban_filtering, collapse="|"), Species))
functional_Kotka_regional <- filter(functional_traits_raw, grepl(paste(Kotka_regional_filtering, collapse="|"), Species))

mean_Kotka_subs_cont <- as.data.frame(colMeans(functional_Kotka_urban[3:12]) - colMeans(functional_Kotka_regional[3:12]))

mean_Kotka_subs_cate <- as.data.frame(colMeans(functional_Kotka_urban[13:29]) - colMeans(functional_Kotka_regional[13:29]))

colnames(mean_Kotka_subs_cont) <- c('Kotka')
colnames(mean_Kotka_subs_cate) <- c('Kotka')


# Kouvola
Kouvola_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kouvola_urban == 1,])
Kouvola_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Kouvola_regional == 1,])

functional_Kouvola_urban <- filter(functional_traits_raw, grepl(paste(Kouvola_urban_filtering, collapse="|"), Species))
functional_Kouvola_regional <- filter(functional_traits_raw, grepl(paste(Kouvola_regional_filtering, collapse="|"), Species))

mean_Kouvola_subs_cont <- as.data.frame(colMeans(functional_Kouvola_urban[3:12]) - colMeans(functional_Kouvola_regional[3:12]))

mean_Kouvola_subs_cate <- as.data.frame(colMeans(functional_Kouvola_urban[13:29]) - colMeans(functional_Kouvola_regional[13:29]))

colnames(mean_Kouvola_subs_cont) <- c('Kouvola')
colnames(mean_Kouvola_subs_cate) <- c('Kouvola')


# Krakow
Krakow_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Krakow_urban == 1,])
Krakow_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Krakow_regional == 1,])

functional_Krakow_urban <- filter(functional_traits_raw, grepl(paste(Krakow_urban_filtering, collapse="|"), Species))
functional_Krakow_regional <- filter(functional_traits_raw, grepl(paste(Krakow_regional_filtering, collapse="|"), Species))

mean_Krakow_subs_cont <- as.data.frame(colMeans(functional_Krakow_urban[3:12]) - colMeans(functional_Krakow_regional[3:12]))

mean_Krakow_subs_cate <- as.data.frame(colMeans(functional_Krakow_urban[13:29]) - colMeans(functional_Krakow_regional[13:29]))

colnames(mean_Krakow_subs_cont) <- c('Krakow')
colnames(mean_Krakow_subs_cate) <- c('Krakow')


# Laitila
Laitila_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Laitila_urban == 1,])
Laitila_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Laitila_regional == 1,])

functional_Laitila_urban <- filter(functional_traits_raw, grepl(paste(Laitila_urban_filtering, collapse="|"), Species))
functional_Laitila_regional <- filter(functional_traits_raw, grepl(paste(Laitila_regional_filtering, collapse="|"), Species))

mean_Laitila_subs_cont <- as.data.frame(colMeans(functional_Laitila_urban[3:12]) - colMeans(functional_Laitila_regional[3:12]))

mean_Laitila_subs_cate <- as.data.frame(colMeans(functional_Laitila_urban[13:29]) - colMeans(functional_Laitila_regional[13:29]))

colnames(mean_Laitila_subs_cont) <- c('Laitila')
colnames(mean_Laitila_subs_cate) <- c('Laitila')


# Lappeenranta
Lappeenranta_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Lappeenranta_urban == 1,])
Lappeenranta_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Lappeenranta_regional == 1,])

functional_Lappeenranta_urban <- filter(functional_traits_raw, grepl(paste(Lappeenranta_urban_filtering, collapse="|"), Species))
functional_Lappeenranta_regional <- filter(functional_traits_raw, grepl(paste(Lappeenranta_regional_filtering, collapse="|"), Species))

mean_Lappeenranta_subs_cont <- as.data.frame(colMeans(functional_Lappeenranta_urban[3:12]) - colMeans(functional_Lappeenranta_regional[3:12]))

mean_Lappeenranta_subs_cate <- as.data.frame(colMeans(functional_Lappeenranta_urban[13:29]) - colMeans(functional_Lappeenranta_regional[13:29]))

colnames(mean_Lappeenranta_subs_cont) <- c('Lappeenranta')
colnames(mean_Lappeenranta_subs_cate) <- c('Lappeenranta')


# Lavras
Lavras_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Lavras_urban == 1,])
Lavras_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Lavras_regional == 1,])

functional_Lavras_urban <- filter(functional_traits_raw, grepl(paste(Lavras_urban_filtering, collapse="|"), Species))
functional_Lavras_regional <- filter(functional_traits_raw, grepl(paste(Lavras_regional_filtering, collapse="|"), Species))

mean_Lavras_subs_cont <- as.data.frame(colMeans(functional_Lavras_urban[3:12]) - colMeans(functional_Lavras_regional[3:12]))

mean_Lavras_subs_cate <- as.data.frame(colMeans(functional_Lavras_urban[13:29]) - colMeans(functional_Lavras_regional[13:29]))

colnames(mean_Lavras_subs_cont) <- c('Lavras')
colnames(mean_Lavras_subs_cate) <- c('Lavras')


# Layyah
Layyah_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Layyah_urban == 1,])
Layyah_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Layyah_regional == 1,])

functional_Layyah_urban <- filter(functional_traits_raw, grepl(paste(Layyah_urban_filtering, collapse="|"), Species))
functional_Layyah_regional <- filter(functional_traits_raw, grepl(paste(Layyah_regional_filtering, collapse="|"), Species))

mean_Layyah_subs_cont <- as.data.frame(colMeans(functional_Layyah_urban[3:12]) - colMeans(functional_Layyah_regional[3:12]))

mean_Layyah_subs_cate <- as.data.frame(colMeans(functional_Layyah_urban[13:29]) - colMeans(functional_Layyah_regional[13:29]))

colnames(mean_Layyah_subs_cont) <- c('Layyah')
colnames(mean_Layyah_subs_cate) <- c('Layyah')


# Lodz
Lodz_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Lodz_urban == 1,])
Lodz_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Lodz_regional == 1,])

functional_Lodz_urban <- filter(functional_traits_raw, grepl(paste(Lodz_urban_filtering, collapse="|"), Species))
functional_Lodz_regional <- filter(functional_traits_raw, grepl(paste(Lodz_regional_filtering, collapse="|"), Species))

mean_Lodz_subs_cont <- as.data.frame(colMeans(functional_Lodz_urban[3:12]) - colMeans(functional_Lodz_regional[3:12]))

mean_Lodz_subs_cate <- as.data.frame(colMeans(functional_Lodz_urban[13:29]) - colMeans(functional_Lodz_regional[13:29]))

colnames(mean_Lodz_subs_cont) <- c('Lodz')
colnames(mean_Lodz_subs_cate) <- c('Lodz')


# Loimaa
Loimaa_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Loimaa_urban == 1,])
Loimaa_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Loimaa_regional == 1,])

functional_Loimaa_urban <- filter(functional_traits_raw, grepl(paste(Loimaa_urban_filtering, collapse="|"), Species))
functional_Loimaa_regional <- filter(functional_traits_raw, grepl(paste(Loimaa_regional_filtering, collapse="|"), Species))

mean_Loimaa_subs_cont <- as.data.frame(colMeans(functional_Loimaa_urban[3:12]) - colMeans(functional_Loimaa_regional[3:12]))

mean_Loimaa_subs_cate <- as.data.frame(colMeans(functional_Loimaa_urban[13:29]) - colMeans(functional_Loimaa_regional[13:29]))

colnames(mean_Loimaa_subs_cont) <- c('Loimaa')
colnames(mean_Loimaa_subs_cate) <- c('Loimaa')


# Lublin
Lublin_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Lublin_urban == 1,])
Lublin_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Lublin_regional == 1,])

functional_Lublin_urban <- filter(functional_traits_raw, grepl(paste(Lublin_urban_filtering, collapse="|"), Species))
functional_Lublin_regional <- filter(functional_traits_raw, grepl(paste(Lublin_regional_filtering, collapse="|"), Species))

mean_Lublin_subs_cont <- as.data.frame(colMeans(functional_Lublin_urban[3:12]) - colMeans(functional_Lublin_regional[3:12]))

mean_Lublin_subs_cate <- as.data.frame(colMeans(functional_Lublin_urban[13:29]) - colMeans(functional_Lublin_regional[13:29]))

colnames(mean_Lublin_subs_cont) <- c('Lublin')
colnames(mean_Lublin_subs_cate) <- c('Lublin')


# Mar_del_Plata
Mar_del_Plata_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Mar_del_Plata_urban == 1,])
Mar_del_Plata_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Mar_del_Plata_regional == 1,])

functional_Mar_del_Plata_urban <- filter(functional_traits_raw, grepl(paste(Mar_del_Plata_urban_filtering, collapse="|"), Species))
functional_Mar_del_Plata_regional <- filter(functional_traits_raw, grepl(paste(Mar_del_Plata_regional_filtering, collapse="|"), Species))

mean_Mar_del_Plata_subs_cont <- as.data.frame(colMeans(functional_Mar_del_Plata_urban[3:12]) - colMeans(functional_Mar_del_Plata_regional[3:12]))

mean_Mar_del_Plata_subs_cate <- as.data.frame(colMeans(functional_Mar_del_Plata_urban[13:29]) - colMeans(functional_Mar_del_Plata_regional[13:29]))

colnames(mean_Mar_del_Plata_subs_cont) <- c('Mar_del_Plata')
colnames(mean_Mar_del_Plata_subs_cate) <- c('Mar_del_Plata')


# Montpellier
Montpellier_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Montpellier_urban == 1,])
Montpellier_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Montpellier_regional == 1,])

functional_Montpellier_urban <- filter(functional_traits_raw, grepl(paste(Montpellier_urban_filtering, collapse="|"), Species))
functional_Montpellier_regional <- filter(functional_traits_raw, grepl(paste(Montpellier_regional_filtering, collapse="|"), Species))

mean_Montpellier_subs_cont <- as.data.frame(colMeans(functional_Montpellier_urban[3:12]) - colMeans(functional_Montpellier_regional[3:12]))

mean_Montpellier_subs_cate <- as.data.frame(colMeans(functional_Montpellier_urban[13:29]) - colMeans(functional_Montpellier_regional[13:29]))

colnames(mean_Montpellier_subs_cont) <- c('Montpellier')
colnames(mean_Montpellier_subs_cate) <- c('Montpellier')


# Naantali
Naantali_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Naantali_urban == 1,])
Naantali_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Naantali_regional == 1,])

functional_Naantali_urban <- filter(functional_traits_raw, grepl(paste(Naantali_urban_filtering, collapse="|"), Species))
functional_Naantali_regional <- filter(functional_traits_raw, grepl(paste(Naantali_regional_filtering, collapse="|"), Species))

mean_Naantali_subs_cont <- as.data.frame(colMeans(functional_Naantali_urban[3:12]) - colMeans(functional_Naantali_regional[3:12]))

mean_Naantali_subs_cate <- as.data.frame(colMeans(functional_Naantali_urban[13:29]) - colMeans(functional_Naantali_regional[13:29]))

colnames(mean_Naantali_subs_cont) <- c('Naantali')
colnames(mean_Naantali_subs_cate) <- c('Naantali')


# Nanning
Nanning_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Nanning_urban == 1,])
Nanning_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Nanning_regional == 1,])

functional_Nanning_urban <- filter(functional_traits_raw, grepl(paste(Nanning_urban_filtering, collapse="|"), Species))
functional_Nanning_regional <- filter(functional_traits_raw, grepl(paste(Nanning_regional_filtering, collapse="|"), Species))

mean_Nanning_subs_cont <- as.data.frame(colMeans(functional_Nanning_urban[3:12]) - colMeans(functional_Nanning_regional[3:12]))

mean_Nanning_subs_cate <- as.data.frame(colMeans(functional_Nanning_urban[13:29]) - colMeans(functional_Nanning_regional[13:29]))

colnames(mean_Nanning_subs_cont) <- c('Nanning')
colnames(mean_Nanning_subs_cate) <- c('Nanning')


# Olsztyn
Olsztyn_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Olsztyn_urban == 1,])
Olsztyn_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Olsztyn_regional == 1,])

functional_Olsztyn_urban <- filter(functional_traits_raw, grepl(paste(Olsztyn_urban_filtering, collapse="|"), Species))
functional_Olsztyn_regional <- filter(functional_traits_raw, grepl(paste(Olsztyn_regional_filtering, collapse="|"), Species))

mean_Olsztyn_subs_cont <- as.data.frame(colMeans(functional_Olsztyn_urban[3:12]) - colMeans(functional_Olsztyn_regional[3:12]))

mean_Olsztyn_subs_cate <- as.data.frame(colMeans(functional_Olsztyn_urban[13:29]) - colMeans(functional_Olsztyn_regional[13:29]))

colnames(mean_Olsztyn_subs_cont) <- c('Olsztyn')
colnames(mean_Olsztyn_subs_cate) <- c('Olsztyn')


# Ostrow_Wielkopolski
Ostrow_Wielkopolski_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Ostrow_Wielkopolski_urban == 1,])
Ostrow_Wielkopolski_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Ostrow_Wielkopolski_regional == 1,])

functional_Ostrow_Wielkopolski_urban <- filter(functional_traits_raw, grepl(paste(Ostrow_Wielkopolski_urban_filtering, collapse="|"), Species))
functional_Ostrow_Wielkopolski_regional <- filter(functional_traits_raw, grepl(paste(Ostrow_Wielkopolski_regional_filtering, collapse="|"), Species))

mean_Ostrow_Wielkopolski_subs_cont <- as.data.frame(colMeans(functional_Ostrow_Wielkopolski_urban[3:12]) - colMeans(functional_Ostrow_Wielkopolski_regional[3:12]))

mean_Ostrow_Wielkopolski_subs_cate <- as.data.frame(colMeans(functional_Ostrow_Wielkopolski_urban[13:29]) - colMeans(functional_Ostrow_Wielkopolski_regional[13:29]))

colnames(mean_Ostrow_Wielkopolski_subs_cont) <- c('Ostrow_Wielkopolski')
colnames(mean_Ostrow_Wielkopolski_subs_cate) <- c('Ostrow_Wielkopolski')


# Oulu
Oulu_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Oulu_urban == 1,])
Oulu_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Oulu_regional == 1,])

functional_Oulu_urban <- filter(functional_traits_raw, grepl(paste(Oulu_urban_filtering, collapse="|"), Species))
functional_Oulu_regional <- filter(functional_traits_raw, grepl(paste(Oulu_regional_filtering, collapse="|"), Species))

mean_Oulu_subs_cont <- as.data.frame(colMeans(functional_Oulu_urban[3:12]) - colMeans(functional_Oulu_regional[3:12]))

mean_Oulu_subs_cate <- as.data.frame(colMeans(functional_Oulu_urban[13:29]) - colMeans(functional_Oulu_regional[13:29]))

colnames(mean_Oulu_subs_cont) <- c('Oulu')
colnames(mean_Oulu_subs_cate) <- c('Oulu')


# Pachuca
Pachuca_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pachuca_urban == 1,])
Pachuca_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pachuca_regional == 1,])

functional_Pachuca_urban <- filter(functional_traits_raw, grepl(paste(Pachuca_urban_filtering, collapse="|"), Species))
functional_Pachuca_regional <- filter(functional_traits_raw, grepl(paste(Pachuca_regional_filtering, collapse="|"), Species))

mean_Pachuca_subs_cont <- as.data.frame(colMeans(functional_Pachuca_urban[3:12]) - colMeans(functional_Pachuca_regional[3:12]))

mean_Pachuca_subs_cate <- as.data.frame(colMeans(functional_Pachuca_urban[13:29]) - colMeans(functional_Pachuca_regional[13:29]))

colnames(mean_Pachuca_subs_cont) <- c('Pachuca')
colnames(mean_Pachuca_subs_cate) <- c('Pachuca')


# Paimio
Paimio_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Paimio_urban == 1,])
Paimio_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Paimio_regional == 1,])

functional_Paimio_urban <- filter(functional_traits_raw, grepl(paste(Paimio_urban_filtering, collapse="|"), Species))
functional_Paimio_regional <- filter(functional_traits_raw, grepl(paste(Paimio_regional_filtering, collapse="|"), Species))

mean_Paimio_subs_cont <- as.data.frame(colMeans(functional_Paimio_urban[3:12]) - colMeans(functional_Paimio_regional[3:12]))

mean_Paimio_subs_cate <- as.data.frame(colMeans(functional_Paimio_urban[13:29]) - colMeans(functional_Paimio_regional[13:29]))

colnames(mean_Paimio_subs_cont) <- c('Paimio')
colnames(mean_Paimio_subs_cate) <- c('Paimio')


# Pargas
Pargas_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pargas_urban == 1,])
Pargas_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pargas_regional == 1,])

functional_Pargas_urban <- filter(functional_traits_raw, grepl(paste(Pargas_urban_filtering, collapse="|"), Species))
functional_Pargas_regional <- filter(functional_traits_raw, grepl(paste(Pargas_regional_filtering, collapse="|"), Species))

mean_Pargas_subs_cont <- as.data.frame(colMeans(functional_Pargas_urban[3:12]) - colMeans(functional_Pargas_regional[3:12]))

mean_Pargas_subs_cate <- as.data.frame(colMeans(functional_Pargas_urban[13:29]) - colMeans(functional_Pargas_regional[13:29]))

colnames(mean_Pargas_subs_cont) <- c('Pargas')
colnames(mean_Pargas_subs_cate) <- c('Pargas')


# Patras
Patras_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Patras_urban == 1,])
Patras_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Patras_regional == 1,])

functional_Patras_urban <- filter(functional_traits_raw, grepl(paste(Patras_urban_filtering, collapse="|"), Species))
functional_Patras_regional <- filter(functional_traits_raw, grepl(paste(Patras_regional_filtering, collapse="|"), Species))

mean_Patras_subs_cont <- as.data.frame(colMeans(functional_Patras_urban[3:12]) - colMeans(functional_Patras_regional[3:12]))

mean_Patras_subs_cate <- as.data.frame(colMeans(functional_Patras_urban[13:29]) - colMeans(functional_Patras_regional[13:29]))

colnames(mean_Patras_subs_cont) <- c('Patras')
colnames(mean_Patras_subs_cate) <- c('Patras')


# Pelotas
Pelotas_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pelotas_urban == 1,])
Pelotas_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pelotas_regional == 1,])

functional_Pelotas_urban <- filter(functional_traits_raw, grepl(paste(Pelotas_urban_filtering, collapse="|"), Species))
functional_Pelotas_regional <- filter(functional_traits_raw, grepl(paste(Pelotas_regional_filtering, collapse="|"), Species))

mean_Pelotas_subs_cont <- as.data.frame(colMeans(functional_Pelotas_urban[3:12]) - colMeans(functional_Pelotas_regional[3:12]))

mean_Pelotas_subs_cate <- as.data.frame(colMeans(functional_Pelotas_urban[13:29]) - colMeans(functional_Pelotas_regional[13:29]))

colnames(mean_Pelotas_subs_cont) <- c('Pelotas')
colnames(mean_Pelotas_subs_cate) <- c('Pelotas')


# Phoenix
Phoenix_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Phoenix_urban == 1,])
Phoenix_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Phoenix_regional == 1,])

functional_Phoenix_urban <- filter(functional_traits_raw, grepl(paste(Phoenix_urban_filtering, collapse="|"), Species))
functional_Phoenix_regional <- filter(functional_traits_raw, grepl(paste(Phoenix_regional_filtering, collapse="|"), Species))

mean_Phoenix_subs_cont <- as.data.frame(colMeans(functional_Phoenix_urban[3:12]) - colMeans(functional_Phoenix_regional[3:12]))

mean_Phoenix_subs_cate <- as.data.frame(colMeans(functional_Phoenix_urban[13:29]) - colMeans(functional_Phoenix_regional[13:29]))

colnames(mean_Phoenix_subs_cont) <- c('Phoenix')
colnames(mean_Phoenix_subs_cate) <- c('Phoenix')


# Pila
Pila_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pila_urban == 1,])
Pila_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pila_regional == 1,])

functional_Pila_urban <- filter(functional_traits_raw, grepl(paste(Pila_urban_filtering, collapse="|"), Species))
functional_Pila_regional <- filter(functional_traits_raw, grepl(paste(Pila_regional_filtering, collapse="|"), Species))

mean_Pila_subs_cont <- as.data.frame(colMeans(functional_Pila_urban[3:12]) - colMeans(functional_Pila_regional[3:12]))

mean_Pila_subs_cate <- as.data.frame(colMeans(functional_Pila_urban[13:29]) - colMeans(functional_Pila_regional[13:29]))

colnames(mean_Pila_subs_cont) <- c('Pila')
colnames(mean_Pila_subs_cate) <- c('Pila')


# Piotrkow_Trybunalski
Piotrkow_Trybunalski_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Piotrkow_Trybunalski_urban == 1,])
Piotrkow_Trybunalski_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Piotrkow_Trybunalski_regional == 1,])

functional_Piotrkow_Trybunalski_urban <- filter(functional_traits_raw, grepl(paste(Piotrkow_Trybunalski_urban_filtering, collapse="|"), Species))
functional_Piotrkow_Trybunalski_regional <- filter(functional_traits_raw, grepl(paste(Piotrkow_Trybunalski_regional_filtering, collapse="|"), Species))

mean_Piotrkow_Trybunalski_subs_cont <- as.data.frame(colMeans(functional_Piotrkow_Trybunalski_urban[3:12]) - colMeans(functional_Piotrkow_Trybunalski_regional[3:12]))

mean_Piotrkow_Trybunalski_subs_cate <- as.data.frame(colMeans(functional_Piotrkow_Trybunalski_urban[13:29]) - colMeans(functional_Piotrkow_Trybunalski_regional[13:29]))

colnames(mean_Piotrkow_Trybunalski_subs_cont) <- c('Piotrkow_Trybunalski')
colnames(mean_Piotrkow_Trybunalski_subs_cate) <- c('Piotrkow_Trybunalski')


# Poznan
Poznan_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Poznan_urban == 1,])
Poznan_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Poznan_regional == 1,])

functional_Poznan_urban <- filter(functional_traits_raw, grepl(paste(Poznan_urban_filtering, collapse="|"), Species))
functional_Poznan_regional <- filter(functional_traits_raw, grepl(paste(Poznan_regional_filtering, collapse="|"), Species))

mean_Poznan_subs_cont <- as.data.frame(colMeans(functional_Poznan_urban[3:12]) - colMeans(functional_Poznan_regional[3:12]))

mean_Poznan_subs_cate <- as.data.frame(colMeans(functional_Poznan_urban[13:29]) - colMeans(functional_Poznan_regional[13:29]))

colnames(mean_Poznan_subs_cont) <- c('Poznan')
colnames(mean_Poznan_subs_cate) <- c('Poznan')


# Pretoria
Pretoria_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pretoria_urban == 1,])
Pretoria_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pretoria_regional == 1,])

functional_Pretoria_urban <- filter(functional_traits_raw, grepl(paste(Pretoria_urban_filtering, collapse="|"), Species))
functional_Pretoria_regional <- filter(functional_traits_raw, grepl(paste(Pretoria_regional_filtering, collapse="|"), Species))

mean_Pretoria_subs_cont <- as.data.frame(colMeans(functional_Pretoria_urban[3:12]) - colMeans(functional_Pretoria_regional[3:12]))

mean_Pretoria_subs_cate <- as.data.frame(colMeans(functional_Pretoria_urban[13:29]) - colMeans(functional_Pretoria_regional[13:29]))

colnames(mean_Pretoria_subs_cont) <- c('Pretoria')
colnames(mean_Pretoria_subs_cate) <- c('Pretoria')


# Przemysl
Przemysl_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Przemysl_urban == 1,])
Przemysl_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Przemysl_regional == 1,])

functional_Przemysl_urban <- filter(functional_traits_raw, grepl(paste(Przemysl_urban_filtering, collapse="|"), Species))
functional_Przemysl_regional <- filter(functional_traits_raw, grepl(paste(Przemysl_regional_filtering, collapse="|"), Species))

mean_Przemysl_subs_cont <- as.data.frame(colMeans(functional_Przemysl_urban[3:12]) - colMeans(functional_Przemysl_regional[3:12]))

mean_Przemysl_subs_cate <- as.data.frame(colMeans(functional_Przemysl_urban[13:29]) - colMeans(functional_Przemysl_regional[13:29]))

colnames(mean_Przemysl_subs_cont) <- c('Przemysl')
colnames(mean_Przemysl_subs_cate) <- c('Przemysl')


# Pyhasalmi
Pyhasalmi_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pyhasalmi_urban == 1,])
Pyhasalmi_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Pyhasalmi_regional == 1,])

functional_Pyhasalmi_urban <- filter(functional_traits_raw, grepl(paste(Pyhasalmi_urban_filtering, collapse="|"), Species))
functional_Pyhasalmi_regional <- filter(functional_traits_raw, grepl(paste(Pyhasalmi_regional_filtering, collapse="|"), Species))

mean_Pyhasalmi_subs_cont <- as.data.frame(colMeans(functional_Pyhasalmi_urban[3:12]) - colMeans(functional_Pyhasalmi_regional[3:12]))

mean_Pyhasalmi_subs_cate <- as.data.frame(colMeans(functional_Pyhasalmi_urban[13:29]) - colMeans(functional_Pyhasalmi_regional[13:29]))

colnames(mean_Pyhasalmi_subs_cont) <- c('Pyhasalmi')
colnames(mean_Pyhasalmi_subs_cate) <- c('Pyhasalmi')


# Quezon_City
Quezon_City_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Quezon_City_urban == 1,])
Quezon_City_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Quezon_City_regional == 1,])

functional_Quezon_City_urban <- filter(functional_traits_raw, grepl(paste(Quezon_City_urban_filtering, collapse="|"), Species))
functional_Quezon_City_regional <- filter(functional_traits_raw, grepl(paste(Quezon_City_regional_filtering, collapse="|"), Species))

mean_Quezon_City_subs_cont <- as.data.frame(colMeans(functional_Quezon_City_urban[3:12]) - colMeans(functional_Quezon_City_regional[3:12]))

mean_Quezon_City_subs_cate <- as.data.frame(colMeans(functional_Quezon_City_urban[13:29]) - colMeans(functional_Quezon_City_regional[13:29]))

colnames(mean_Quezon_City_subs_cont) <- c('Quezon_City')
colnames(mean_Quezon_City_subs_cate) <- c('Quezon_City')


# Raisio
Raisio_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Raisio_urban == 1,])
Raisio_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Raisio_regional == 1,])

functional_Raisio_urban <- filter(functional_traits_raw, grepl(paste(Raisio_urban_filtering, collapse="|"), Species))
functional_Raisio_regional <- filter(functional_traits_raw, grepl(paste(Raisio_regional_filtering, collapse="|"), Species))

mean_Raisio_subs_cont <- as.data.frame(colMeans(functional_Raisio_urban[3:12]) - colMeans(functional_Raisio_regional[3:12]))

mean_Raisio_subs_cate <- as.data.frame(colMeans(functional_Raisio_urban[13:29]) - colMeans(functional_Raisio_regional[13:29]))

colnames(mean_Raisio_subs_cont) <- c('Raisio')
colnames(mean_Raisio_subs_cate) <- c('Raisio')


# Rovaniemi
Rovaniemi_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Rovaniemi_urban == 1,])
Rovaniemi_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Rovaniemi_regional == 1,])

functional_Rovaniemi_urban <- filter(functional_traits_raw, grepl(paste(Rovaniemi_urban_filtering, collapse="|"), Species))
functional_Rovaniemi_regional <- filter(functional_traits_raw, grepl(paste(Rovaniemi_regional_filtering, collapse="|"), Species))

mean_Rovaniemi_subs_cont <- as.data.frame(colMeans(functional_Rovaniemi_urban[3:12]) - colMeans(functional_Rovaniemi_regional[3:12]))

mean_Rovaniemi_subs_cate <- as.data.frame(colMeans(functional_Rovaniemi_urban[13:29]) - colMeans(functional_Rovaniemi_regional[13:29]))

colnames(mean_Rovaniemi_subs_cont) <- c('Rovaniemi')
colnames(mean_Rovaniemi_subs_cate) <- c('Rovaniemi')


# Rzeszow
Rzeszow_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Rzeszow_urban == 1,])
Rzeszow_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Rzeszow_regional == 1,])

functional_Rzeszow_urban <- filter(functional_traits_raw, grepl(paste(Rzeszow_urban_filtering, collapse="|"), Species))
functional_Rzeszow_regional <- filter(functional_traits_raw, grepl(paste(Rzeszow_regional_filtering, collapse="|"), Species))

mean_Rzeszow_subs_cont <- as.data.frame(colMeans(functional_Rzeszow_urban[3:12]) - colMeans(functional_Rzeszow_regional[3:12]))

mean_Rzeszow_subs_cate <- as.data.frame(colMeans(functional_Rzeszow_urban[13:29]) - colMeans(functional_Rzeszow_regional[13:29]))

colnames(mean_Rzeszow_subs_cont) <- c('Rzeszow')
colnames(mean_Rzeszow_subs_cate) <- c('Rzeszow')


# Salo
Salo_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Salo_urban == 1,])
Salo_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Salo_regional == 1,])

functional_Salo_urban <- filter(functional_traits_raw, grepl(paste(Salo_urban_filtering, collapse="|"), Species))
functional_Salo_regional <- filter(functional_traits_raw, grepl(paste(Salo_regional_filtering, collapse="|"), Species))

mean_Salo_subs_cont <- as.data.frame(colMeans(functional_Salo_urban[3:12]) - colMeans(functional_Salo_regional[3:12]))

mean_Salo_subs_cate <- as.data.frame(colMeans(functional_Salo_urban[13:29]) - colMeans(functional_Salo_regional[13:29]))

colnames(mean_Salo_subs_cont) <- c('Salo')
colnames(mean_Salo_subs_cate) <- c('Salo')


# Santiago
Santiago_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Santiago_urban == 1,])
Santiago_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Santiago_regional == 1,])

functional_Santiago_urban <- filter(functional_traits_raw, grepl(paste(Santiago_urban_filtering, collapse="|"), Species))
functional_Santiago_regional <- filter(functional_traits_raw, grepl(paste(Santiago_regional_filtering, collapse="|"), Species))

mean_Santiago_subs_cont <- as.data.frame(colMeans(functional_Santiago_urban[3:12]) - colMeans(functional_Santiago_regional[3:12]))

mean_Santiago_subs_cate <- as.data.frame(colMeans(functional_Santiago_urban[13:29]) - colMeans(functional_Santiago_regional[13:29]))

colnames(mean_Santiago_subs_cont) <- c('Santiago')
colnames(mean_Santiago_subs_cate) <- c('Santiago')


# Siedlce
Siedlce_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Siedlce_urban == 1,])
Siedlce_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Siedlce_regional == 1,])

functional_Siedlce_urban <- filter(functional_traits_raw, grepl(paste(Siedlce_urban_filtering, collapse="|"), Species))
functional_Siedlce_regional <- filter(functional_traits_raw, grepl(paste(Siedlce_regional_filtering, collapse="|"), Species))

mean_Siedlce_subs_cont <- as.data.frame(colMeans(functional_Siedlce_urban[3:12]) - colMeans(functional_Siedlce_regional[3:12]))

mean_Siedlce_subs_cate <- as.data.frame(colMeans(functional_Siedlce_urban[13:29]) - colMeans(functional_Siedlce_regional[13:29]))

colnames(mean_Siedlce_subs_cont) <- c('Siedlce')
colnames(mean_Siedlce_subs_cate) <- c('Siedlce')


# Slupsk
Slupsk_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Slupsk_urban == 1,])
Slupsk_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Slupsk_regional == 1,])

functional_Slupsk_urban <- filter(functional_traits_raw, grepl(paste(Slupsk_urban_filtering, collapse="|"), Species))
functional_Slupsk_regional <- filter(functional_traits_raw, grepl(paste(Slupsk_regional_filtering, collapse="|"), Species))

mean_Slupsk_subs_cont <- as.data.frame(colMeans(functional_Slupsk_urban[3:12]) - colMeans(functional_Slupsk_regional[3:12]))

mean_Slupsk_subs_cate <- as.data.frame(colMeans(functional_Slupsk_urban[13:29]) - colMeans(functional_Slupsk_regional[13:29]))

colnames(mean_Slupsk_subs_cont) <- c('Slupsk')
colnames(mean_Slupsk_subs_cate) <- c('Slupsk')


# Sofia
Sofia_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Sofia_urban == 1,])
Sofia_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Sofia_regional == 1,])

functional_Sofia_urban <- filter(functional_traits_raw, grepl(paste(Sofia_urban_filtering, collapse="|"), Species))
functional_Sofia_regional <- filter(functional_traits_raw, grepl(paste(Sofia_regional_filtering, collapse="|"), Species))

mean_Sofia_subs_cont <- as.data.frame(colMeans(functional_Sofia_urban[3:12]) - colMeans(functional_Sofia_regional[3:12]))

mean_Sofia_subs_cate <- as.data.frame(colMeans(functional_Sofia_urban[13:29]) - colMeans(functional_Sofia_regional[13:29]))

colnames(mean_Sofia_subs_cont) <- c('Sofia')
colnames(mean_Sofia_subs_cate) <- c('Sofia')


# Suolahti
Suolahti_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Suolahti_urban == 1,])
Suolahti_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Suolahti_regional == 1,])

functional_Suolahti_urban <- filter(functional_traits_raw, grepl(paste(Suolahti_urban_filtering, collapse="|"), Species))
functional_Suolahti_regional <- filter(functional_traits_raw, grepl(paste(Suolahti_regional_filtering, collapse="|"), Species))

mean_Suolahti_subs_cont <- as.data.frame(colMeans(functional_Suolahti_urban[3:12]) - colMeans(functional_Suolahti_regional[3:12]))

mean_Suolahti_subs_cate <- as.data.frame(colMeans(functional_Suolahti_urban[13:29]) - colMeans(functional_Suolahti_regional[13:29]))

colnames(mean_Suolahti_subs_cont) <- c('Suolahti')
colnames(mean_Suolahti_subs_cate) <- c('Suolahti')


# Swidnica
Swidnica_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Swidnica_urban == 1,])
Swidnica_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Swidnica_regional == 1,])

functional_Swidnica_urban <- filter(functional_traits_raw, grepl(paste(Swidnica_urban_filtering, collapse="|"), Species))
functional_Swidnica_regional <- filter(functional_traits_raw, grepl(paste(Swidnica_regional_filtering, collapse="|"), Species))

mean_Swidnica_subs_cont <- as.data.frame(colMeans(functional_Swidnica_urban[3:12]) - colMeans(functional_Swidnica_regional[3:12]))

mean_Swidnica_subs_cate <- as.data.frame(colMeans(functional_Swidnica_urban[13:29]) - colMeans(functional_Swidnica_regional[13:29]))

colnames(mean_Swidnica_subs_cont) <- c('Swidnica')
colnames(mean_Swidnica_subs_cate) <- c('Swidnica')


# Szczecin
Szczecin_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Szczecin_urban == 1,])
Szczecin_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Szczecin_regional == 1,])

functional_Szczecin_urban <- filter(functional_traits_raw, grepl(paste(Szczecin_urban_filtering, collapse="|"), Species))
functional_Szczecin_regional <- filter(functional_traits_raw, grepl(paste(Szczecin_regional_filtering, collapse="|"), Species))

mean_Szczecin_subs_cont <- as.data.frame(colMeans(functional_Szczecin_urban[3:12]) - colMeans(functional_Szczecin_regional[3:12]))

mean_Szczecin_subs_cate <- as.data.frame(colMeans(functional_Szczecin_urban[13:29]) - colMeans(functional_Szczecin_regional[13:29]))

colnames(mean_Szczecin_subs_cont) <- c('Szczecin')
colnames(mean_Szczecin_subs_cate) <- c('Szczecin')


# Taubate
Taubate_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Taubate_urban == 1,])
Taubate_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Taubate_regional == 1,])

functional_Taubate_urban <- filter(functional_traits_raw, grepl(paste(Taubate_urban_filtering, collapse="|"), Species))
functional_Taubate_regional <- filter(functional_traits_raw, grepl(paste(Taubate_regional_filtering, collapse="|"), Species))

mean_Taubate_subs_cont <- as.data.frame(colMeans(functional_Taubate_urban[3:12]) - colMeans(functional_Taubate_regional[3:12]))

mean_Taubate_subs_cate <- as.data.frame(colMeans(functional_Taubate_urban[13:29]) - colMeans(functional_Taubate_regional[13:29]))

colnames(mean_Taubate_subs_cont) <- c('Taubate')
colnames(mean_Taubate_subs_cate) <- c('Taubate')


# Tornio
Tornio_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Tornio_urban == 1,])
Tornio_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Tornio_regional == 1,])

functional_Tornio_urban <- filter(functional_traits_raw, grepl(paste(Tornio_urban_filtering, collapse="|"), Species))
functional_Tornio_regional <- filter(functional_traits_raw, grepl(paste(Tornio_regional_filtering, collapse="|"), Species))

mean_Tornio_subs_cont <- as.data.frame(colMeans(functional_Tornio_urban[3:12]) - colMeans(functional_Tornio_regional[3:12]))

mean_Tornio_subs_cate <- as.data.frame(colMeans(functional_Tornio_urban[13:29]) - colMeans(functional_Tornio_regional[13:29]))

colnames(mean_Tornio_subs_cont) <- c('Tornio')
colnames(mean_Tornio_subs_cate) <- c('Tornio')


# Torun
Torun_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Torun_urban == 1,])
Torun_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Torun_regional == 1,])

functional_Torun_urban <- filter(functional_traits_raw, grepl(paste(Torun_urban_filtering, collapse="|"), Species))
functional_Torun_regional <- filter(functional_traits_raw, grepl(paste(Torun_regional_filtering, collapse="|"), Species))

mean_Torun_subs_cont <- as.data.frame(colMeans(functional_Torun_urban[3:12]) - colMeans(functional_Torun_regional[3:12]))

mean_Torun_subs_cate <- as.data.frame(colMeans(functional_Torun_urban[13:29]) - colMeans(functional_Torun_regional[13:29]))

colnames(mean_Torun_subs_cont) <- c('Torun')
colnames(mean_Torun_subs_cate) <- c('Torun')


# Tucson
Tucson_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Tucson_urban == 1,])
Tucson_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Tucson_regional == 1,])

functional_Tucson_urban <- filter(functional_traits_raw, grepl(paste(Tucson_urban_filtering, collapse="|"), Species))
functional_Tucson_regional <- filter(functional_traits_raw, grepl(paste(Tucson_regional_filtering, collapse="|"), Species))

mean_Tucson_subs_cont <- as.data.frame(colMeans(functional_Tucson_urban[3:12]) - colMeans(functional_Tucson_regional[3:12]))

mean_Tucson_subs_cate <- as.data.frame(colMeans(functional_Tucson_urban[13:29]) - colMeans(functional_Tucson_regional[13:29]))

colnames(mean_Tucson_subs_cont) <- c('Tucson')
colnames(mean_Tucson_subs_cate) <- c('Tucson')


# Turku
Turku_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Turku_urban == 1,])
Turku_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Turku_regional == 1,])

functional_Turku_urban <- filter(functional_traits_raw, grepl(paste(Turku_urban_filtering, collapse="|"), Species))
functional_Turku_regional <- filter(functional_traits_raw, grepl(paste(Turku_regional_filtering, collapse="|"), Species))

mean_Turku_subs_cont <- as.data.frame(colMeans(functional_Turku_urban[3:12]) - colMeans(functional_Turku_regional[3:12]))

mean_Turku_subs_cate <- as.data.frame(colMeans(functional_Turku_urban[13:29]) - colMeans(functional_Turku_regional[13:29]))

colnames(mean_Turku_subs_cont) <- c('Turku')
colnames(mean_Turku_subs_cate) <- c('Turku')


# Uusikaupunki
Uusikaupunki_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Uusikaupunki_urban == 1,])
Uusikaupunki_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Uusikaupunki_regional == 1,])

functional_Uusikaupunki_urban <- filter(functional_traits_raw, grepl(paste(Uusikaupunki_urban_filtering, collapse="|"), Species))
functional_Uusikaupunki_regional <- filter(functional_traits_raw, grepl(paste(Uusikaupunki_regional_filtering, collapse="|"), Species))

mean_Uusikaupunki_subs_cont <- as.data.frame(colMeans(functional_Uusikaupunki_urban[3:12]) - colMeans(functional_Uusikaupunki_regional[3:12]))

mean_Uusikaupunki_subs_cate <- as.data.frame(colMeans(functional_Uusikaupunki_urban[13:29]) - colMeans(functional_Uusikaupunki_regional[13:29]))

colnames(mean_Uusikaupunki_subs_cont) <- c('Uusikaupunki')
colnames(mean_Uusikaupunki_subs_cate) <- c('Uusikaupunki')


# Vantaa
Vantaa_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Vantaa_urban == 1,])
Vantaa_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Vantaa_regional == 1,])

functional_Vantaa_urban <- filter(functional_traits_raw, grepl(paste(Vantaa_urban_filtering, collapse="|"), Species))
functional_Vantaa_regional <- filter(functional_traits_raw, grepl(paste(Vantaa_regional_filtering, collapse="|"), Species))

mean_Vantaa_subs_cont <- as.data.frame(colMeans(functional_Vantaa_urban[3:12]) - colMeans(functional_Vantaa_regional[3:12]))

mean_Vantaa_subs_cate <- as.data.frame(colMeans(functional_Vantaa_urban[13:29]) - colMeans(functional_Vantaa_regional[13:29]))

colnames(mean_Vantaa_subs_cont) <- c('Vantaa')
colnames(mean_Vantaa_subs_cate) <- c('Vantaa')


# Vienna
Vienna_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Vienna_urban == 1,])
Vienna_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Vienna_regional == 1,])

functional_Vienna_urban <- filter(functional_traits_raw, grepl(paste(Vienna_urban_filtering, collapse="|"), Species))
functional_Vienna_regional <- filter(functional_traits_raw, grepl(paste(Vienna_regional_filtering, collapse="|"), Species))

mean_Vienna_subs_cont <- as.data.frame(colMeans(functional_Vienna_urban[3:12]) - colMeans(functional_Vienna_regional[3:12]))

mean_Vienna_subs_cate <- as.data.frame(colMeans(functional_Vienna_urban[13:29]) - colMeans(functional_Vienna_regional[13:29]))

colnames(mean_Vienna_subs_cont) <- c('Vienna')
colnames(mean_Vienna_subs_cate) <- c('Vienna')


# Vitoria_Gasteiz
Vitoria_Gasteiz_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Vitoria_Gasteiz_urban == 1,])
Vitoria_Gasteiz_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Vitoria_Gasteiz_regional == 1,])

functional_Vitoria_Gasteiz_urban <- filter(functional_traits_raw, grepl(paste(Vitoria_Gasteiz_urban_filtering, collapse="|"), Species))
functional_Vitoria_Gasteiz_regional <- filter(functional_traits_raw, grepl(paste(Vitoria_Gasteiz_regional_filtering, collapse="|"), Species))

mean_Vitoria_Gasteiz_subs_cont <- as.data.frame(colMeans(functional_Vitoria_Gasteiz_urban[3:12]) - colMeans(functional_Vitoria_Gasteiz_regional[3:12]))

mean_Vitoria_Gasteiz_subs_cate <- as.data.frame(colMeans(functional_Vitoria_Gasteiz_urban[13:29]) - colMeans(functional_Vitoria_Gasteiz_regional[13:29]))

colnames(mean_Vitoria_Gasteiz_subs_cont) <- c('Vitoria_Gasteiz')
colnames(mean_Vitoria_Gasteiz_subs_cate) <- c('Vitoria_Gasteiz')


# Warsaw
Warsaw_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Warsaw_urban == 1,])
Warsaw_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Warsaw_regional == 1,])

functional_Warsaw_urban <- filter(functional_traits_raw, grepl(paste(Warsaw_urban_filtering, collapse="|"), Species))
functional_Warsaw_regional <- filter(functional_traits_raw, grepl(paste(Warsaw_regional_filtering, collapse="|"), Species))

mean_Warsaw_subs_cont <- as.data.frame(colMeans(functional_Warsaw_urban[3:12]) - colMeans(functional_Warsaw_regional[3:12]))

mean_Warsaw_subs_cate <- as.data.frame(colMeans(functional_Warsaw_urban[13:29]) - colMeans(functional_Warsaw_regional[13:29]))

colnames(mean_Warsaw_subs_cont) <- c('Warsaw')
colnames(mean_Warsaw_subs_cate) <- c('Warsaw')


# Wroclaw
Wroclaw_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Wroclaw_urban == 1,])
Wroclaw_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Wroclaw_regional == 1,])

functional_Wroclaw_urban <- filter(functional_traits_raw, grepl(paste(Wroclaw_urban_filtering, collapse="|"), Species))
functional_Wroclaw_regional <- filter(functional_traits_raw, grepl(paste(Wroclaw_regional_filtering, collapse="|"), Species))

mean_Wroclaw_subs_cont <- as.data.frame(colMeans(functional_Wroclaw_urban[3:12]) - colMeans(functional_Wroclaw_regional[3:12]))

mean_Wroclaw_subs_cate <- as.data.frame(colMeans(functional_Wroclaw_urban[13:29]) - colMeans(functional_Wroclaw_regional[13:29]))

colnames(mean_Wroclaw_subs_cont) <- c('Wroclaw')
colnames(mean_Wroclaw_subs_cate) <- c('Wroclaw')


# Xalapa
Xalapa_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Xalapa_urban == 1,])
Xalapa_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Xalapa_regional == 1,])

functional_Xalapa_urban <- filter(functional_traits_raw, grepl(paste(Xalapa_urban_filtering, collapse="|"), Species))
functional_Xalapa_regional <- filter(functional_traits_raw, grepl(paste(Xalapa_regional_filtering, collapse="|"), Species))

mean_Xalapa_subs_cont <- as.data.frame(colMeans(functional_Xalapa_urban[3:12]) - colMeans(functional_Xalapa_regional[3:12]))

mean_Xalapa_subs_cate <- as.data.frame(colMeans(functional_Xalapa_urban[13:29]) - colMeans(functional_Xalapa_regional[13:29]))

colnames(mean_Xalapa_subs_cont) <- c('Xalapa')
colnames(mean_Xalapa_subs_cate) <- c('Xalapa')

# Zielona_Gora
Zielona_Gora_urban_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Zielona_Gora_urban == 1,])
Zielona_Gora_regional_filtering <- rownames(bird_sample_urban_regional[bird_sample_urban_regional$Zielona_Gora_regional == 1,])

functional_Zielona_Gora_urban <- filter(functional_traits_raw, grepl(paste(Zielona_Gora_urban_filtering, collapse="|"), Species))
functional_Zielona_Gora_regional <- filter(functional_traits_raw, grepl(paste(Zielona_Gora_regional_filtering, collapse="|"), Species))

mean_Zielona_Gora_subs_cont <- as.data.frame(colMeans(functional_Zielona_Gora_urban[3:12]) - colMeans(functional_Zielona_Gora_regional[3:12]))

mean_Zielona_Gora_subs_cate <- as.data.frame(colMeans(functional_Zielona_Gora_urban[13:29]) - colMeans(functional_Zielona_Gora_regional[13:29]))

colnames(mean_Zielona_Gora_subs_cont) <- c('Zielona_Gora')
colnames(mean_Zielona_Gora_subs_cate) <- c('Zielona_Gora')

### Merge all continuous and categorical traits for each urban and region species pools comparison
mean_subs_cate <- cbind.data.frame(mean_Aanekoski_subs_cate,
                                   mean_Ain_Beida_subs_cate,
                                   mean_Asuncion_subs_cate,
                                   mean_Belem_subs_cate,
                                   mean_Bernal_subs_cate,
                                   mean_Biala_Podlaska_subs_cate,
                                   mean_Bialystok_subs_cate,
                                   mean_Bydgoszcz_subs_cate,
                                   mean_Ciudad_Juarez_subs_cate,
                                   mean_Czestochowa_subs_cate,
                                   mean_Durango_subs_cate,
                                   mean_Fresno_subs_cate,
                                   mean_Gdansk_subs_cate,
                                   mean_Gdynia_subs_cate,
                                   mean_Gorzow_Wielkopolski_subs_cate,
                                   mean_Gujranwala_subs_cate,
                                   mean_Gwangju_subs_cate,
                                   mean_Hamina_subs_cate,
                                   mean_Helsinki_subs_cate,
                                   mean_Inowroclaw_subs_cate,
                                   mean_Joensuu_subs_cate,
                                   mean_Jyvaskyla_subs_cate,
                                   mean_Kajaani_subs_cate,
                                   mean_Kathmandu_subs_cate,
                                   mean_Kauhava_subs_cate,
                                   mean_Kemi_subs_cate,
                                   mean_Kemijarvi_subs_cate,
                                   mean_Kotka_subs_cate,
                                   mean_Kouvola_subs_cate,
                                   mean_Krakow_subs_cate,
                                   mean_Laitila_subs_cate,
                                   mean_Lappeenranta_subs_cate,
                                   mean_Lavras_subs_cate,
                                   mean_Layyah_subs_cate,
                                   mean_Lodz_subs_cate,
                                   mean_Loimaa_subs_cate,
                                   mean_Lublin_subs_cate,
                                   mean_Mar_del_Plata_subs_cate,
                                   mean_Montpellier_subs_cate,
                                   mean_Naantali_subs_cate,
                                   mean_Nanning_subs_cate,
                                   mean_Olsztyn_subs_cate,
                                   mean_Ostrow_Wielkopolski_subs_cate,
                                   mean_Oulu_subs_cate,
                                   mean_Pachuca_subs_cate,
                                   mean_Paimio_subs_cate,
                                   mean_Pargas_subs_cate,
                                   mean_Patras_subs_cate,
                                   mean_Pelotas_subs_cate,
                                   mean_Phoenix_subs_cate,
                                   mean_Pila_subs_cate,
                                   mean_Piotrkow_Trybunalski_subs_cate,
                                   mean_Poznan_subs_cate,
                                   mean_Pretoria_subs_cate,
                                   mean_Przemysl_subs_cate,
                                   mean_Pyhasalmi_subs_cate,
                                   mean_Quezon_City_subs_cate,
                                   mean_Raisio_subs_cate,
                                   mean_Rovaniemi_subs_cate,
                                   mean_Rzeszow_subs_cate,
                                   mean_Salo_subs_cate,
                                   mean_Santiago_subs_cate,
                                   mean_Siedlce_subs_cate,
                                   mean_Slupsk_subs_cate,
                                   mean_Sofia_subs_cate,
                                   mean_Suolahti_subs_cate,
                                   mean_Swidnica_subs_cate,
                                   mean_Szczecin_subs_cate,
                                   mean_Taubate_subs_cate,
                                   mean_Tornio_subs_cate,
                                   mean_Torun_subs_cate,
                                   mean_Tucson_subs_cate,
                                   mean_Turku_subs_cate,
                                   mean_Uusikaupunki_subs_cate,
                                   mean_Vantaa_subs_cate,
                                   mean_Vienna_subs_cate,
                                   mean_Vitoria_Gasteiz_subs_cate,
                                   mean_Warsaw_subs_cate,
                                   mean_Wroclaw_subs_cate,
                                   mean_Xalapa_subs_cate,
                                   mean_Zielona_Gora_subs_cate)


mean_subs_cont <- cbind.data.frame(mean_Aanekoski_subs_cont,
                                   mean_Ain_Beida_subs_cont,
                                   mean_Asuncion_subs_cont,
                                   mean_Belem_subs_cont,
                                   mean_Bernal_subs_cont,
                                   mean_Biala_Podlaska_subs_cont,
                                   mean_Bialystok_subs_cont,
                                   mean_Bydgoszcz_subs_cont,
                                   mean_Ciudad_Juarez_subs_cont,
                                   mean_Czestochowa_subs_cont,
                                   mean_Durango_subs_cont,
                                   mean_Fresno_subs_cont,
                                   mean_Gdansk_subs_cont,
                                   mean_Gdynia_subs_cont,
                                   mean_Gorzow_Wielkopolski_subs_cont,
                                   mean_Gujranwala_subs_cont,
                                   mean_Gwangju_subs_cont,
                                   mean_Hamina_subs_cont,
                                   mean_Helsinki_subs_cont,
                                   mean_Inowroclaw_subs_cont,
                                   mean_Joensuu_subs_cont,
                                   mean_Jyvaskyla_subs_cont,
                                   mean_Kajaani_subs_cont,
                                   mean_Kathmandu_subs_cont,
                                   mean_Kauhava_subs_cont,
                                   mean_Kemi_subs_cont,
                                   mean_Kemijarvi_subs_cont,
                                   mean_Kotka_subs_cont,
                                   mean_Kouvola_subs_cont,
                                   mean_Krakow_subs_cont,
                                   mean_Laitila_subs_cont,
                                   mean_Lappeenranta_subs_cont,
                                   mean_Lavras_subs_cont,
                                   mean_Layyah_subs_cont,
                                   mean_Lodz_subs_cont,
                                   mean_Loimaa_subs_cont,
                                   mean_Lublin_subs_cont,
                                   mean_Mar_del_Plata_subs_cont,
                                   mean_Montpellier_subs_cont,
                                   mean_Naantali_subs_cont,
                                   mean_Nanning_subs_cont,
                                   mean_Olsztyn_subs_cont,
                                   mean_Ostrow_Wielkopolski_subs_cont,
                                   mean_Oulu_subs_cont,
                                   mean_Pachuca_subs_cont,
                                   mean_Paimio_subs_cont,
                                   mean_Pargas_subs_cont,
                                   mean_Patras_subs_cont,
                                   mean_Pelotas_subs_cont,
                                   mean_Phoenix_subs_cont,
                                   mean_Pila_subs_cont,
                                   mean_Piotrkow_Trybunalski_subs_cont,
                                   mean_Poznan_subs_cont,
                                   mean_Pretoria_subs_cont,
                                   mean_Przemysl_subs_cont,
                                   mean_Pyhasalmi_subs_cont,
                                   mean_Quezon_City_subs_cont,
                                   mean_Raisio_subs_cont,
                                   mean_Rovaniemi_subs_cont,
                                   mean_Rzeszow_subs_cont,
                                   mean_Salo_subs_cont,
                                   mean_Santiago_subs_cont,
                                   mean_Siedlce_subs_cont,
                                   mean_Slupsk_subs_cont,
                                   mean_Sofia_subs_cont,
                                   mean_Suolahti_subs_cont,
                                   mean_Swidnica_subs_cont,
                                   mean_Szczecin_subs_cont,
                                   mean_Taubate_subs_cont,
                                   mean_Tornio_subs_cont,
                                   mean_Torun_subs_cont,
                                   mean_Tucson_subs_cont,
                                   mean_Turku_subs_cont,
                                   mean_Uusikaupunki_subs_cont,
                                   mean_Vantaa_subs_cont,
                                   mean_Vienna_subs_cont,
                                   mean_Vitoria_Gasteiz_subs_cont,
                                   mean_Warsaw_subs_cont,
                                   mean_Wroclaw_subs_cont,
                                   mean_Xalapa_subs_cont,
                                   mean_Zielona_Gora_subs_cont)

### Transpose merged data frames for continuous functional traits
mean_subs_cont <- as.data.frame(t(mean_subs_cont))

### Transpose merged data frames for categorical functional traits
mean_subs_cate <- as.data.frame(t(mean_subs_cate))

### One sample t-test for each continuous traits
ttest_Range_size_km <- t.test(mean_subs_cont$Range_size_km, mu = 0, alternative = "two.sided")
ttest_Bill_Nares <- t.test(mean_subs_cont$Bill_Nares, mu = 0, alternative = "two.sided")
ttest_Bill_Width <- t.test(mean_subs_cont$Bill_Width, mu = 0, alternative = "two.sided")
ttest_Bill_Depth <- t.test(mean_subs_cont$Bill_Depth, mu = 0, alternative = "two.sided")
ttest_Tarsus_Length <- t.test(mean_subs_cont$Tarsus_Length, mu = 0, alternative = "two.sided")
ttest_Secondary1 <- t.test(mean_subs_cont$Secondary1, mu = 0, alternative = "two.sided")
ttest_Wing_Chord <- t.test(mean_subs_cont$Wing_Chord, mu = 0, alternative = "two.sided")
ttest_Hand.Wing.Index..Claramunt.2011. <- t.test(mean_subs_cont$Hand.Wing.Index..Claramunt.2011., mu = 0, alternative = "two.sided")
ttest_Tail_Length <- t.test(mean_subs_cont$Tail_Length, mu = 0, alternative = "two.sided")
ttest_Mass <- t.test(mean_subs_cont$Mass, mu = 0, alternative = "two.sided")

### Make a vector containing Morphology traits names
morpho_name <- c("Bill_Nares", "Bill_Width", "Bill_Depth", "Tarsus_Length", 
               "Secondary1", "Wing_Chord", "Hand.Wing.Index.Claramunt", 
               "Tail_Length")

### Make a vector containing mean values for Morphology traits
morpho_mean_value <- c(ttest_Bill_Nares$estimate, ttest_Bill_Width$estimate, 
                     ttest_Bill_Depth$estimate, ttest_Tarsus_Length$estimate, 
                     ttest_Secondary1$estimate, ttest_Wing_Chord$estimate,
                     ttest_Hand.Wing.Index..Claramunt.2011.$estimate, 
                     ttest_Tail_Length$estimate)

### Make a vector containing t-values for Morphology traits
morpho_tvalue <- c(ttest_Bill_Nares$statistic, ttest_Bill_Width$statistic, 
                   ttest_Bill_Depth$statistic, ttest_Tarsus_Length$statistic, 
                   ttest_Secondary1$statistic, ttest_Wing_Chord$statistic,
                   ttest_Hand.Wing.Index..Claramunt.2011.$statistic, 
                   ttest_Tail_Length$statistic)

### Make a vector containing p-values for Morphology traits
morpho_pvalue <- c(ttest_Bill_Nares$p.value, ttest_Bill_Width$p.value, 
                   ttest_Bill_Depth$p.value, ttest_Tarsus_Length$p.value, 
                   ttest_Secondary1$p.value, ttest_Wing_Chord$p.value,
                   ttest_Hand.Wing.Index..Claramunt.2011.$p.value, 
                   ttest_Tail_Length$p.value)

### Merge vectors containing for Morphology traits
morpho_table <- cbind.data.frame(morpho_name, morpho_mean_value, morpho_tvalue, morpho_pvalue)

### Calculate Holm correction for multiple comparisons (Morphology traits)
morpho_table$Holm =
  p.adjust(morpho_table$morpho_pvalue,
           method = "holm")

### Get data frame containing only Morphology traits
mean_subs_morpho_cont <- as.data.frame(mean_subs_cont[2:9])

### Get data frames as input for percentile t bootstrap method 
mean_subs_morpho_cont_input <- reshape2::melt(mean_subs_morpho_cont, 
                                            measure.vars = c("Bill_Nares", "Bill_Width", "Bill_Depth", "Tarsus_Length", 
                                                             "Secondary1", "Wing_Chord", "Hand.Wing.Index..Claramunt.2011.", 
                                                             "Tail_Length"),
                                            variable.name = 'Morphology_trait')

### Set seed
set.seed(777)

### Perform ANOVA with the percentile t bootstrap method for morphological traits
anova_boot_morpho_notrim <- t1waybt(value ~ Morphology_trait, data = mean_subs_morpho_cont_input, tr = 0, alpha = 0.05, nboot = 99999)


#####################################################

### ttest for Diet categories
ttest_Diet.Inv <- t.test(mean_subs_cate$Diet.Inv, mu = 0, alternative = "two.sided")
ttest_Diet.Vend <- t.test(mean_subs_cate$Diet.Vend, mu = 0, alternative = "two.sided")
ttest_Diet.Vect <- t.test(mean_subs_cate$Diet.Vect, mu = 0, alternative = "two.sided")
ttest_Diet.Vfish <- t.test(mean_subs_cate$Diet.Vfish, mu = 0, alternative = "two.sided")
ttest_Diet.Vunk <- t.test(mean_subs_cate$Diet.Vunk, mu = 0, alternative = "two.sided")
ttest_Diet.Scav <- t.test(mean_subs_cate$Diet.Scav, mu = 0, alternative = "two.sided")
ttest_Diet.Fruit <- t.test(mean_subs_cate$Diet.Fruit, mu = 0, alternative = "two.sided")
ttest_Diet.Nect <- t.test(mean_subs_cate$Diet.Nect, mu = 0, alternative = "two.sided")
ttest_Diet.Seed <- t.test(mean_subs_cate$Diet.Seed, mu = 0, alternative = "two.sided")
ttest_Diet.PlantO <- t.test(mean_subs_cate$Diet.PlantO, mu = 0, alternative = "two.sided")

### Make a vector containing Diet category names
diet_name <- c("Diet.Inv", "Diet.Vend", "Diet.Vect", "Diet.Vfish", "Diet.Vunk", 
               "Diet.Scav", "Diet.Fruit", "Diet.Nect", "Diet.Seed", "Diet.PlantO")

### Make a vector containing mean values for Diet categories
diet_mean_value <- c(ttest_Diet.Inv$estimate, ttest_Diet.Vend$estimate, ttest_Diet.Vect$estimate, 
                 ttest_Diet.Vfish$estimate, ttest_Diet.Vunk$estimate, ttest_Diet.Scav$estimate,
                 ttest_Diet.Fruit$estimate, ttest_Diet.Nect$estimate, ttest_Diet.Seed$estimate,
                 ttest_Diet.PlantO$estimate)

### Make a vector containing t-values for Diet categories
diet_tvalue <- c(ttest_Diet.Inv$statistic, ttest_Diet.Vend$statistic, ttest_Diet.Vect$statistic, 
                     ttest_Diet.Vfish$statistic, ttest_Diet.Vunk$statistic, ttest_Diet.Scav$statistic,
                     ttest_Diet.Fruit$statistic, ttest_Diet.Nect$statistic, ttest_Diet.Seed$statistic,
                     ttest_Diet.PlantO$statistic)

### Make a vector containing P-values for Diet categories
diet_pvalue <- c(ttest_Diet.Inv$p.value, ttest_Diet.Vend$p.value, ttest_Diet.Vect$p.value, 
ttest_Diet.Vfish$p.value, ttest_Diet.Vunk$p.value, ttest_Diet.Scav$p.value,
ttest_Diet.Fruit$p.value, ttest_Diet.Nect$p.value, ttest_Diet.Seed$p.value,
ttest_Diet.PlantO$p.value)

### Merge vectors containing Diet categories
diet_table <- cbind.data.frame(diet_name, diet_mean_value, diet_tvalue, diet_pvalue)

### Calculate Holm correction for multiple comparisons (Diet categories)
diet_table$Holm =
  p.adjust(diet_table$diet_pvalue,
           method = "holm")


### ttest for Strata categories
ttest_ForStrat.watbelowsurf <- t.test(mean_subs_cate$ForStrat.watbelowsurf, mu = 0, alternative = "two.sided")
ttest_ForStrat.wataroundsurf <- t.test(mean_subs_cate$ForStrat.wataroundsurf, mu = 0, alternative = "two.sided")
ttest_ForStrat.ground <- t.test(mean_subs_cate$ForStrat.ground, mu = 0, alternative = "two.sided")
ttest_ForStrat.understory <- t.test(mean_subs_cate$ForStrat.understory, mu = 0, alternative = "two.sided")
ttest_ForStrat.midhigh <- t.test(mean_subs_cate$ForStrat.midhigh, mu = 0, alternative = "two.sided")
ttest_ForStrat.canopy <- t.test(mean_subs_cate$ForStrat.canopy, mu = 0, alternative = "two.sided")
ttest_ForStrat.aerial <- t.test(mean_subs_cate$ForStrat.aerial, mu = 0, alternative = "two.sided")

### Make a vector containing Strata category names
strata_name <- c("ForStrat.watbelowsurf", "ForStrat.wataroundsurf", "ForStrat.ground", 
               "ForStrat.understory", "ForStrat.midhigh", 
               "ForStrat.canopy", "ForStrat.aerial")

### Make a vector containing mean values for Strata categories
strata_mean_value <- c(ttest_ForStrat.watbelowsurf$estimate, ttest_ForStrat.wataroundsurf$estimate, 
                       ttest_ForStrat.ground$estimate, ttest_ForStrat.understory$estimate,
                       ttest_ForStrat.midhigh$estimate, ttest_ForStrat.canopy$estimate,
                       ttest_ForStrat.aerial$estimate)

### Make a vector containing t-values for Strata categories
strata_tvalue <- c(ttest_ForStrat.watbelowsurf$statistic, ttest_ForStrat.wataroundsurf$statistic, 
                       ttest_ForStrat.ground$statistic, ttest_ForStrat.understory$statistic,
                       ttest_ForStrat.midhigh$statistic, ttest_ForStrat.canopy$statistic,
                       ttest_ForStrat.aerial$statistic)

### Make a vector containing P-values for Strata categories
strata_pvalue <- c(ttest_ForStrat.watbelowsurf$p.value, ttest_ForStrat.wataroundsurf$p.value, 
                 ttest_ForStrat.ground$p.value, ttest_ForStrat.understory$p.value,
                 ttest_ForStrat.midhigh$p.value, ttest_ForStrat.canopy$p.value,
                 ttest_ForStrat.aerial$p.value)

### Merge vectors containing Strata categories
strata_table <- cbind.data.frame(strata_name, strata_mean_value, strata_tvalue, strata_pvalue)

### Calculate Holm correction for multiple comparisons (Diet categories)
strata_table$Holm =
  p.adjust(strata_table$strata_pvalue,
           method = "holm")

### Get data frame containing only diet categories 
mean_subs_diet_cate <- as.data.frame(mean_subs_cate[1:10])

### Get data frame containing only strata categories 
mean_subs_strata_cate <- as.data.frame(mean_subs_cate[11:17])

### Get data frames as input for percentile t bootstrap method 
mean_subs_diet_cate_input <- reshape2::melt(mean_subs_diet_cate, 
                           measure.vars = c('Diet.Inv', 'Diet.Vend', 'Diet.Vect',
                                            'Diet.Vfish', 'Diet.Vunk', 'Diet.Scav', 'Diet.Fruit',
                                            'Diet.Nect', 'Diet.Seed', 'Diet.PlantO'),
                           variable.name = 'Diet_category')

mean_subs_strata_cate_input <- reshape2::melt(mean_subs_strata_cate, 
                                            measure.vars = c('ForStrat.watbelowsurf', 'ForStrat.wataroundsurf', 
                                                             'ForStrat.ground', 'ForStrat.understory',
                                                             'ForStrat.midhigh', 'ForStrat.canopy', 
                                                             'ForStrat.aerial'),
                                            variable.name = 'Strata_category')


### Set seed
set.seed(777)

### Perform ANOVA with the percentile t bootstrap method for (functional) strata traits
anova_boot_strata_notrim <- t1waybt(value ~ Strata_category, data = mean_subs_strata_cate_input, tr = 0, alpha = 0.05, nboot = 99999)

### Set seed
set.seed(777)

### Perform ANOVA with the percentile t bootstrap method for (functional) diet traits
anova_boot_diet_notrim <- t1waybt(value ~ Diet_category, data = mean_subs_diet_cate_input, tr = 0, alpha = 0.05, nboot = 99999)


### Save Workspace
save.image("../data/output_data/Workspaces/Bird_order_funct_trait_delta.Rdata")

#############################################################################



mean_only_urban_subs_cont <- cbind.data.frame(colMeans(functional_Aanekoski_urban[3:12]),
                                              colMeans(functional_Ain_Beida_urban[3:12]),
                                              colMeans(functional_Asuncion_urban[3:12]),
                                              colMeans(functional_Belem_urban[3:12]),
                                              colMeans(functional_Bernal_urban[3:12]),
                                              colMeans(functional_Biala_Podlaska_urban[3:12]),
                                              colMeans(functional_Bialystok_urban[3:12]),
                                              colMeans(functional_Bydgoszcz_urban[3:12]),
                                              colMeans(functional_Ciudad_Juarez_urban[3:12]),
                                              colMeans(functional_Czestochowa_urban[3:12]),
                                              colMeans(functional_Durango_urban[3:12]),
                                              colMeans(functional_Fresno_urban[3:12]),
                                              colMeans(functional_Gdansk_urban[3:12]),
                                              colMeans(functional_Gdynia_urban[3:12]),
                                              colMeans(functional_Gorzow_Wielkopolski_urban[3:12]),
                                              colMeans(functional_Gujranwala_urban[3:12]),
                                              colMeans(functional_Gwangju_urban[3:12]),
                                              colMeans(functional_Hamina_urban[3:12]),
                                              colMeans(functional_Helsinki_urban[3:12]),
                                              colMeans(functional_Inowroclaw_urban[3:12]),
                                              colMeans(functional_Joensuu_urban[3:12]),
                                              colMeans(functional_Jyvaskyla_urban[3:12]),
                                              colMeans(functional_Kajaani_urban[3:12]),
                                              colMeans(functional_Kathmandu_urban[3:12]),
                                              colMeans(functional_Kauhava_urban[3:12]),
                                              colMeans(functional_Kemi_urban[3:12]),
                                              colMeans(functional_Kemijarvi_urban[3:12]),
                                              colMeans(functional_Kotka_urban[3:12]),
                                              colMeans(functional_Kouvola_urban[3:12]),
                                              colMeans(functional_Krakow_urban[3:12]),
                                              colMeans(functional_Laitila_urban[3:12]),
                                              colMeans(functional_Lappeenranta_urban[3:12]),
                                              colMeans(functional_Lavras_urban[3:12]),
                                              colMeans(functional_Layyah_urban[3:12]),
                                              colMeans(functional_Lodz_urban[3:12]),
                                              colMeans(functional_Loimaa_urban[3:12]),
                                              colMeans(functional_Lublin_urban[3:12]),
                                              colMeans(functional_Mar_del_Plata_urban[3:12]),
                                              colMeans(functional_Montpellier_urban[3:12]),
                                              colMeans(functional_Naantali_urban[3:12]),
                                              colMeans(functional_Nanning_urban[3:12]),
                                              colMeans(functional_Olsztyn_urban[3:12]),
                                              colMeans(functional_Ostrow_Wielkopolski_urban[3:12]),
                                              colMeans(functional_Oulu_urban[3:12]),
                                              colMeans(functional_Pachuca_urban[3:12]),
                                              colMeans(functional_Paimio_urban[3:12]),
                                              colMeans(functional_Pargas_urban[3:12]),
                                              colMeans(functional_Patras_urban[3:12]),
                                              colMeans(functional_Pelotas_urban[3:12]),
                                              colMeans(functional_Phoenix_urban[3:12]),
                                              colMeans(functional_Pila_urban[3:12]),
                                              colMeans(functional_Piotrkow_Trybunalski_urban[3:12]),
                                              colMeans(functional_Poznan_urban[3:12]),
                                              colMeans(functional_Pretoria_urban[3:12]),
                                              colMeans(functional_Przemysl_urban[3:12]),
                                              colMeans(functional_Pyhasalmi_urban[3:12]),
                                              colMeans(functional_Quezon_City_urban[3:12]),
                                              colMeans(functional_Raisio_urban[3:12]),
                                              colMeans(functional_Rovaniemi_urban[3:12]),
                                              colMeans(functional_Rzeszow_urban[3:12]),
                                              colMeans(functional_Salo_urban[3:12]),
                                              colMeans(functional_Santiago_urban[3:12]),
                                              colMeans(functional_Siedlce_urban[3:12]),
                                              colMeans(functional_Slupsk_urban[3:12]),
                                              colMeans(functional_Sofia_urban[3:12]),
                                              colMeans(functional_Suolahti_urban[3:12]),
                                              colMeans(functional_Swidnica_urban[3:12]),
                                              colMeans(functional_Szczecin_urban[3:12]),
                                              colMeans(functional_Taubate_urban[3:12]),
                                              colMeans(functional_Tornio_urban[3:12]),
                                              colMeans(functional_Torun_urban[3:12]),
                                              colMeans(functional_Tucson_urban[3:12]),
                                              colMeans(functional_Turku_urban[3:12]),
                                              colMeans(functional_Uusikaupunki_urban[3:12]),
                                              colMeans(functional_Vantaa_urban[3:12]),
                                              colMeans(functional_Vienna_urban[3:12]),
                                              colMeans(functional_Vitoria_Gasteiz_urban[3:12]),
                                              colMeans(functional_Warsaw_urban[3:12]),
                                              colMeans(functional_Wroclaw_urban[3:12]),
                                              colMeans(functional_Xalapa_urban[3:12]),
                                              colMeans(functional_Zielona_Gora_urban[3:12]))

mean_only_regional_subs_cont <- cbind.data.frame(colMeans(functional_Aanekoski_regional[3:12]),
                                                 colMeans(functional_Ain_Beida_regional[3:12]),
                                                 colMeans(functional_Asuncion_regional[3:12]),
                                                 colMeans(functional_Belem_regional[3:12]),
                                                 colMeans(functional_Bernal_regional[3:12]),
                                                 colMeans(functional_Biala_Podlaska_regional[3:12]),
                                                 colMeans(functional_Bialystok_regional[3:12]),
                                                 colMeans(functional_Bydgoszcz_regional[3:12]),
                                                 colMeans(functional_Ciudad_Juarez_regional[3:12]),
                                                 colMeans(functional_Czestochowa_regional[3:12]),
                                                 colMeans(functional_Durango_regional[3:12]),
                                                 colMeans(functional_Fresno_regional[3:12]),
                                                 colMeans(functional_Gdansk_regional[3:12]),
                                                 colMeans(functional_Gdynia_regional[3:12]),
                                                 colMeans(functional_Gorzow_Wielkopolski_regional[3:12]),
                                                 colMeans(functional_Gujranwala_regional[3:12]),
                                                 colMeans(functional_Gwangju_regional[3:12]),
                                                 colMeans(functional_Hamina_regional[3:12]),
                                                 colMeans(functional_Helsinki_regional[3:12]),
                                                 colMeans(functional_Inowroclaw_regional[3:12]),
                                                 colMeans(functional_Joensuu_regional[3:12]),
                                                 colMeans(functional_Jyvaskyla_regional[3:12]),
                                                 colMeans(functional_Kajaani_regional[3:12]),
                                                 colMeans(functional_Kathmandu_regional[3:12]),
                                                 colMeans(functional_Kauhava_regional[3:12]),
                                                 colMeans(functional_Kemi_regional[3:12]),
                                                 colMeans(functional_Kemijarvi_regional[3:12]),
                                                 colMeans(functional_Kotka_regional[3:12]),
                                                 colMeans(functional_Kouvola_regional[3:12]),
                                                 colMeans(functional_Krakow_regional[3:12]),
                                                 colMeans(functional_Laitila_regional[3:12]),
                                                 colMeans(functional_Lappeenranta_regional[3:12]),
                                                 colMeans(functional_Lavras_regional[3:12]),
                                                 colMeans(functional_Layyah_regional[3:12]),
                                                 colMeans(functional_Lodz_regional[3:12]),
                                                 colMeans(functional_Loimaa_regional[3:12]),
                                                 colMeans(functional_Lublin_regional[3:12]),
                                                 colMeans(functional_Mar_del_Plata_regional[3:12]),
                                                 colMeans(functional_Montpellier_regional[3:12]),
                                                 colMeans(functional_Naantali_regional[3:12]),
                                                 colMeans(functional_Nanning_regional[3:12]),
                                                 colMeans(functional_Olsztyn_regional[3:12]),
                                                 colMeans(functional_Ostrow_Wielkopolski_regional[3:12]),
                                                 colMeans(functional_Oulu_regional[3:12]),
                                                 colMeans(functional_Pachuca_regional[3:12]),
                                                 colMeans(functional_Paimio_regional[3:12]),
                                                 colMeans(functional_Pargas_regional[3:12]),
                                                 colMeans(functional_Patras_regional[3:12]),
                                                 colMeans(functional_Pelotas_regional[3:12]),
                                                 colMeans(functional_Phoenix_regional[3:12]),
                                                 colMeans(functional_Pila_regional[3:12]),
                                                 colMeans(functional_Piotrkow_Trybunalski_regional[3:12]),
                                                 colMeans(functional_Poznan_regional[3:12]),
                                                 colMeans(functional_Pretoria_regional[3:12]),
                                                 colMeans(functional_Przemysl_regional[3:12]),
                                                 colMeans(functional_Pyhasalmi_regional[3:12]),
                                                 colMeans(functional_Quezon_City_regional[3:12]),
                                                 colMeans(functional_Raisio_regional[3:12]),
                                                 colMeans(functional_Rovaniemi_regional[3:12]),
                                                 colMeans(functional_Rzeszow_regional[3:12]),
                                                 colMeans(functional_Salo_regional[3:12]),
                                                 colMeans(functional_Santiago_regional[3:12]),
                                                 colMeans(functional_Siedlce_regional[3:12]),
                                                 colMeans(functional_Slupsk_regional[3:12]),
                                                 colMeans(functional_Sofia_regional[3:12]),
                                                 colMeans(functional_Suolahti_regional[3:12]),
                                                 colMeans(functional_Swidnica_regional[3:12]),
                                                 colMeans(functional_Szczecin_regional[3:12]),
                                                 colMeans(functional_Taubate_regional[3:12]),
                                                 colMeans(functional_Tornio_regional[3:12]),
                                                 colMeans(functional_Torun_regional[3:12]),
                                                 colMeans(functional_Tucson_regional[3:12]),
                                                 colMeans(functional_Turku_regional[3:12]),
                                                 colMeans(functional_Uusikaupunki_regional[3:12]),
                                                 colMeans(functional_Vantaa_regional[3:12]),
                                                 colMeans(functional_Vienna_regional[3:12]),
                                                 colMeans(functional_Vitoria_Gasteiz_regional[3:12]),
                                                 colMeans(functional_Warsaw_regional[3:12]),
                                                 colMeans(functional_Wroclaw_regional[3:12]),
                                                 colMeans(functional_Xalapa_regional[3:12]),
                                                 colMeans(functional_Zielona_Gora_regional[3:12]))

### Transpose merged data frames
## For urban species pools
mean_only_urban_subs_cont_trans <- as.data.frame(t(mean_only_urban_subs_cont))

## For regional species pools
mean_only_regional_subs_cont_trans <- as.data.frame(t(mean_only_regional_subs_cont))

### Get averaged values of range size for urban species pools
mean(mean_only_urban_subs_cont_trans$Range_size_km)

### Get averaged values of range size for regional species pools
mean(mean_only_regional_subs_cont_trans$Range_size_km)

### Get averaged values of body size for urban species pools
mean(mean_only_urban_subs_cont_trans$Mass)

### Get averaged values of body size for regional species pools
mean(mean_only_regional_subs_cont_trans$Mass)

### Merge categorical functional traits for urban species pools
mean_only_urban_subs_cate <- cbind.data.frame(colMeans(functional_Aanekoski_urban[13:29]),
                                              colMeans(functional_Ain_Beida_urban[13:29]),
                                              colMeans(functional_Asuncion_urban[13:29]),
                                              colMeans(functional_Belem_urban[13:29]),
                                              colMeans(functional_Bernal_urban[13:29]),
                                              colMeans(functional_Biala_Podlaska_urban[13:29]),
                                              colMeans(functional_Bialystok_urban[13:29]),
                                              colMeans(functional_Bydgoszcz_urban[13:29]),
                                              colMeans(functional_Ciudad_Juarez_urban[13:29]),
                                              colMeans(functional_Czestochowa_urban[13:29]),
                                              colMeans(functional_Durango_urban[13:29]),
                                              colMeans(functional_Fresno_urban[13:29]),
                                              colMeans(functional_Gdansk_urban[13:29]),
                                              colMeans(functional_Gdynia_urban[13:29]),
                                              colMeans(functional_Gorzow_Wielkopolski_urban[13:29]),
                                              colMeans(functional_Gujranwala_urban[13:29]),
                                              colMeans(functional_Gwangju_urban[13:29]),
                                              colMeans(functional_Hamina_urban[13:29]),
                                              colMeans(functional_Helsinki_urban[13:29]),
                                              colMeans(functional_Inowroclaw_urban[13:29]),
                                              colMeans(functional_Joensuu_urban[13:29]),
                                              colMeans(functional_Jyvaskyla_urban[13:29]),
                                              colMeans(functional_Kajaani_urban[13:29]),
                                              colMeans(functional_Kathmandu_urban[13:29]),
                                              colMeans(functional_Kauhava_urban[13:29]),
                                              colMeans(functional_Kemi_urban[13:29]),
                                              colMeans(functional_Kemijarvi_urban[13:29]),
                                              colMeans(functional_Kotka_urban[13:29]),
                                              colMeans(functional_Kouvola_urban[13:29]),
                                              colMeans(functional_Krakow_urban[13:29]),
                                              colMeans(functional_Laitila_urban[13:29]),
                                              colMeans(functional_Lappeenranta_urban[13:29]),
                                              colMeans(functional_Lavras_urban[13:29]),
                                              colMeans(functional_Layyah_urban[13:29]),
                                              colMeans(functional_Lodz_urban[13:29]),
                                              colMeans(functional_Loimaa_urban[13:29]),
                                              colMeans(functional_Lublin_urban[13:29]),
                                              colMeans(functional_Mar_del_Plata_urban[13:29]),
                                              colMeans(functional_Montpellier_urban[13:29]),
                                              colMeans(functional_Naantali_urban[13:29]),
                                              colMeans(functional_Nanning_urban[13:29]),
                                              colMeans(functional_Olsztyn_urban[13:29]),
                                              colMeans(functional_Ostrow_Wielkopolski_urban[13:29]),
                                              colMeans(functional_Oulu_urban[13:29]),
                                              colMeans(functional_Pachuca_urban[13:29]),
                                              colMeans(functional_Paimio_urban[13:29]),
                                              colMeans(functional_Pargas_urban[13:29]),
                                              colMeans(functional_Patras_urban[13:29]),
                                              colMeans(functional_Pelotas_urban[13:29]),
                                              colMeans(functional_Phoenix_urban[13:29]),
                                              colMeans(functional_Pila_urban[13:29]),
                                              colMeans(functional_Piotrkow_Trybunalski_urban[13:29]),
                                              colMeans(functional_Poznan_urban[13:29]),
                                              colMeans(functional_Pretoria_urban[13:29]),
                                              colMeans(functional_Przemysl_urban[13:29]),
                                              colMeans(functional_Pyhasalmi_urban[13:29]),
                                              colMeans(functional_Quezon_City_urban[13:29]),
                                              colMeans(functional_Raisio_urban[13:29]),
                                              colMeans(functional_Rovaniemi_urban[13:29]),
                                              colMeans(functional_Rzeszow_urban[13:29]),
                                              colMeans(functional_Salo_urban[13:29]),
                                              colMeans(functional_Santiago_urban[13:29]),
                                              colMeans(functional_Siedlce_urban[13:29]),
                                              colMeans(functional_Slupsk_urban[13:29]),
                                              colMeans(functional_Sofia_urban[13:29]),
                                              colMeans(functional_Suolahti_urban[13:29]),
                                              colMeans(functional_Swidnica_urban[13:29]),
                                              colMeans(functional_Szczecin_urban[13:29]),
                                              colMeans(functional_Taubate_urban[13:29]),
                                              colMeans(functional_Tornio_urban[13:29]),
                                              colMeans(functional_Torun_urban[13:29]),
                                              colMeans(functional_Tucson_urban[13:29]),
                                              colMeans(functional_Turku_urban[13:29]),
                                              colMeans(functional_Uusikaupunki_urban[13:29]),
                                              colMeans(functional_Vantaa_urban[13:29]),
                                              colMeans(functional_Vienna_urban[13:29]),
                                              colMeans(functional_Vitoria_Gasteiz_urban[13:29]),
                                              colMeans(functional_Warsaw_urban[13:29]),
                                              colMeans(functional_Wroclaw_urban[13:29]),
                                              colMeans(functional_Xalapa_urban[13:29]),
                                              colMeans(functional_Zielona_Gora_urban[13:29]))

### Merge categorical functional traits for regional species pools
mean_only_regional_subs_cate <- cbind.data.frame(colMeans(functional_Aanekoski_regional[13:29]),
                                                 colMeans(functional_Ain_Beida_regional[13:29]),
                                                 colMeans(functional_Asuncion_regional[13:29]),
                                                 colMeans(functional_Belem_regional[13:29]),
                                                 colMeans(functional_Bernal_regional[13:29]),
                                                 colMeans(functional_Biala_Podlaska_regional[13:29]),
                                                 colMeans(functional_Bialystok_regional[13:29]),
                                                 colMeans(functional_Bydgoszcz_regional[13:29]),
                                                 colMeans(functional_Ciudad_Juarez_regional[13:29]),
                                                 colMeans(functional_Czestochowa_regional[13:29]),
                                                 colMeans(functional_Durango_regional[13:29]),
                                                 colMeans(functional_Fresno_regional[13:29]),
                                                 colMeans(functional_Gdansk_regional[13:29]),
                                                 colMeans(functional_Gdynia_regional[13:29]),
                                                 colMeans(functional_Gorzow_Wielkopolski_regional[13:29]),
                                                 colMeans(functional_Gujranwala_regional[13:29]),
                                                 colMeans(functional_Gwangju_regional[13:29]),
                                                 colMeans(functional_Hamina_regional[13:29]),
                                                 colMeans(functional_Helsinki_regional[13:29]),
                                                 colMeans(functional_Inowroclaw_regional[13:29]),
                                                 colMeans(functional_Joensuu_regional[13:29]),
                                                 colMeans(functional_Jyvaskyla_regional[13:29]),
                                                 colMeans(functional_Kajaani_regional[13:29]),
                                                 colMeans(functional_Kathmandu_regional[13:29]),
                                                 colMeans(functional_Kauhava_regional[13:29]),
                                                 colMeans(functional_Kemi_regional[13:29]),
                                                 colMeans(functional_Kemijarvi_regional[13:29]),
                                                 colMeans(functional_Kotka_regional[13:29]),
                                                 colMeans(functional_Kouvola_regional[13:29]),
                                                 colMeans(functional_Krakow_regional[13:29]),
                                                 colMeans(functional_Laitila_regional[13:29]),
                                                 colMeans(functional_Lappeenranta_regional[13:29]),
                                                 colMeans(functional_Lavras_regional[13:29]),
                                                 colMeans(functional_Layyah_regional[13:29]),
                                                 colMeans(functional_Lodz_regional[13:29]),
                                                 colMeans(functional_Loimaa_regional[13:29]),
                                                 colMeans(functional_Lublin_regional[13:29]),
                                                 colMeans(functional_Mar_del_Plata_regional[13:29]),
                                                 colMeans(functional_Montpellier_regional[13:29]),
                                                 colMeans(functional_Naantali_regional[13:29]),
                                                 colMeans(functional_Nanning_regional[13:29]),
                                                 colMeans(functional_Olsztyn_regional[13:29]),
                                                 colMeans(functional_Ostrow_Wielkopolski_regional[13:29]),
                                                 colMeans(functional_Oulu_regional[13:29]),
                                                 colMeans(functional_Pachuca_regional[13:29]),
                                                 colMeans(functional_Paimio_regional[13:29]),
                                                 colMeans(functional_Pargas_regional[13:29]),
                                                 colMeans(functional_Patras_regional[13:29]),
                                                 colMeans(functional_Pelotas_regional[13:29]),
                                                 colMeans(functional_Phoenix_regional[13:29]),
                                                 colMeans(functional_Pila_regional[13:29]),
                                                 colMeans(functional_Piotrkow_Trybunalski_regional[13:29]),
                                                 colMeans(functional_Poznan_regional[13:29]),
                                                 colMeans(functional_Pretoria_regional[13:29]),
                                                 colMeans(functional_Przemysl_regional[13:29]),
                                                 colMeans(functional_Pyhasalmi_regional[13:29]),
                                                 colMeans(functional_Quezon_City_regional[13:29]),
                                                 colMeans(functional_Raisio_regional[13:29]),
                                                 colMeans(functional_Rovaniemi_regional[13:29]),
                                                 colMeans(functional_Rzeszow_regional[13:29]),
                                                 colMeans(functional_Salo_regional[13:29]),
                                                 colMeans(functional_Santiago_regional[13:29]),
                                                 colMeans(functional_Siedlce_regional[13:29]),
                                                 colMeans(functional_Slupsk_regional[13:29]),
                                                 colMeans(functional_Sofia_regional[13:29]),
                                                 colMeans(functional_Suolahti_regional[13:29]),
                                                 colMeans(functional_Swidnica_regional[13:29]),
                                                 colMeans(functional_Szczecin_regional[13:29]),
                                                 colMeans(functional_Taubate_regional[13:29]),
                                                 colMeans(functional_Tornio_regional[13:29]),
                                                 colMeans(functional_Torun_regional[13:29]),
                                                 colMeans(functional_Tucson_regional[13:29]),
                                                 colMeans(functional_Turku_regional[13:29]),
                                                 colMeans(functional_Uusikaupunki_regional[13:29]),
                                                 colMeans(functional_Vantaa_regional[13:29]),
                                                 colMeans(functional_Vienna_regional[13:29]),
                                                 colMeans(functional_Vitoria_Gasteiz_regional[13:29]),
                                                 colMeans(functional_Warsaw_regional[13:29]),
                                                 colMeans(functional_Wroclaw_regional[13:29]),
                                                 colMeans(functional_Xalapa_regional[13:29]),
                                                 colMeans(functional_Zielona_Gora_regional[13:29]))


### Transpose merged data frames
## For urban species pools
mean_only_urban_subs_cate_trans <- as.data.frame(t(mean_only_urban_subs_cate))

## For regional species pools
mean_only_regional_subs_cate_trans <- as.data.frame(t(mean_only_regional_subs_cate))

### Get averaged values of understorey strata for urban species pools
mean(mean_only_urban_subs_cate_trans$ForStrat.understory)

### Get averaged values of understorey strata for regional species pools
mean(mean_only_regional_subs_cate_trans$ForStrat.understory)


### Create boxplots for morphological traits (Tobias et al.)

colorder_morpho <- c("#344325", "#344325", "#344325", "#344325", "#344325", 
                     "#344325", "#344325", "#344325")

morpho_boxplot <- ggplot(mean_subs_morpho_cont_input, aes(x=value, y=Morphology_trait, fill = Morphology_trait)) +
  geom_boxplot(alpha = 0.5) +
  geom_vline(xintercept = 0, colour = "chocolate1", size = 0.8, linetype = "longdash") +
  xlab("\u0394 Mean variation") + ylab("") +
  scale_y_discrete(labels=c("Bill_Nares"=expression(plain("Beak length (TN)")), 
                            "Bill_Width"=expression(bold("Beak width")), 
                            "Bill_Depth"=expression(bold("Beak depth")), 
                            "Tarsus_Length"=expression(bold("Tarsus length")),
                            "Secondary1"=expression(bold("First secondary length")), 
                            "Wing_Chord"=expression(bold("Wing chord")), 
                            "Hand.Wing.Index..Claramunt.2011."=expression(plain("Hand-wing index")), 
                            "Tail_Length"=expression(bold("Tail length")))) +
  scale_x_continuous(limits = c(-80, 80), breaks = c(-80, -60, -40, -20, 0, 20, 40, 60, 80)) +
  scale_fill_manual(values=colorder_morpho) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 20, face = "plain", colour = "black"), 
        axis.text.y = element_text(size = 20, face = "plain", colour = "black"),
        axis.line.x.bottom = element_line(size = 1.3),
        axis.line.x.top = element_line(size = 1),
        axis.line.y.left = element_line(size = 1.3),
        axis.line.y.right = element_line(size = 1),
        axis.title.x = element_text(size = 20, face = "bold"), 
        axis.title.y = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20), 
        axis.ticks=element_line(size = 1.3, colour = "black"),
        legend.margin = element_blank(), axis.ticks.length = unit(.30, "cm"),
        legend.position = "none")


### Create boxplots for diet traits (Wilman et al.)

colorder_diet <- c("#68b859", "#68b859", "#68b859", "#68b859", "#68b859", 
                   "#68b859", "#68b859", "#68b859", "#68b859", "#68b859")


diet_boxplot <- ggplot(mean_subs_diet_cate_input, aes(x=value, y=Diet_category, fill = Diet_category)) +
  geom_boxplot(alpha = 0.5) +
  geom_vline(xintercept = 0, colour = "chocolate1", size = 0.8, linetype = "longdash") +
  xlab("\u0394 Mean percentage use") + ylab("") +
  scale_y_discrete(labels=c("Diet.Inv"=expression(bold("Invertebrates")),
                            "Diet.Vend"=expression(bold("Mammals & birds")),
                            "Diet.Vect"=expression(plain("   Reptiles & amphibians")),
                            "Diet.Vfish"=expression(bold("Fish")),
                            "Diet.Vunk"=expression(plain("Garbage")),
                            "Diet.Scav"=expression(bold("Carrion")),
                            "Diet.Fruit"=expression(bold("Fruit")),
                            "Diet.Nect"=expression(bold("Nectar")),
                            "Diet.Seed"=expression(bold("Seeds")),
                            "Diet.PlantO"=expression(bold("Other plant material")))) +
  scale_x_continuous(limits = c(-40, 40), breaks = c(-40, -30, -20, -10, 0, 10, 20, 30, 40)) +
  scale_fill_manual(values=colorder_diet) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 20, face = "plain", colour = "black"), 
        axis.text.y = element_text(size = 20, face = "plain", colour = "black"),
        axis.line.x.bottom = element_line(size = 1.3),
        axis.line.x.top = element_line(size = 1),
        axis.line.y.left = element_line(size = 1.3),
        axis.line.y.right = element_line(size = 1),
        axis.title.x = element_text(size = 20, face = "bold"), 
        axis.title.y = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20), 
        axis.ticks=element_line(size = 1.3, colour = "black"),
        legend.margin = element_blank(), axis.ticks.length = unit(.30, "cm"),
        legend.position = "none")


### Create boxplots for strata traits (Wilman et al.)

colorder_strata <- c("#bedbb6", "#bedbb6", "#bedbb6", "#bedbb6", 
                     "#bedbb6", "#bedbb6", "#bedbb6")

strata_boxplot <- ggplot(mean_subs_strata_cate_input, aes(x=value, y=Strata_category, fill = Strata_category)) +
  geom_boxplot(alpha = 0.5) +
  geom_vline(xintercept = 0, colour = "chocolate1", size = 0.8, linetype = "longdash") +
  xlab("\u0394 Mean prevalence") + ylab("") +
  scale_y_discrete(labels=c("ForStrat.watbelowsurf"=expression(bold("    Below water surface")),
                            "ForStrat.wataroundsurf"=expression(bold("Water surface")),
                            "ForStrat.ground"=expression(plain("Ground")),
                            "ForStrat.understory"=expression(bold("Understorey")),
                            "ForStrat.midhigh"=expression(plain("Mid-storey")),
                            "ForStrat.canopy"=expression(plain("Canopy")),
                            "ForStrat.aerial"=expression(plain("Aerial")))) +
  scale_x_continuous(limits = c(-23, 23), breaks = c(-20, -15, -10, -5, 0, 5, 10, 15, 20)) +
  scale_fill_manual(values=colorder_strata) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 20, face = "plain", colour = "black"), 
        axis.text.y = element_text(size = 20, face = "plain", colour = "black"),
        axis.line.x.bottom = element_line(size = 1.3),
        axis.line.x.top = element_line(size = 1),
        axis.line.y.left = element_line(size = 1.3),
        axis.line.y.right = element_line(size = 1),
        axis.title.x = element_text(size = 20, face = "bold"), 
        axis.title.y = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20), 
        axis.ticks=element_line(size = 1.3, colour = "black"),
        legend.margin = element_blank(), axis.ticks.length = unit(.30, "cm"),
        legend.position = "none")


### Merge boxplots of Functional traits
functional_boxplots <- ggarrange(morpho_boxplot, diet_boxplot, strata_boxplot,
                                 ncol = 1, nrow = 3)

### Export boxplots of Functional traits
png(filename="..\\data\\output_data\\Figures\\Boxplots\\functional_boxplots.png", units="mm", width=200, height=300, pointsize=15, res=2000)
ggdraw(functional_boxplots) +
  draw_label("(a)", x = 0.03, y = 0.98, fontface = "bold", size = 20) +
  draw_label("(b)", x = 0.03, y = 0.65, fontface = "bold", size = 20) +
  draw_label("(c)", x = 0.03, y = 0.31, fontface = "bold", size = 20)
dev.off()

### Save Workspace
save.image("../data/output_data/Workspaces/Bird_order_funct_trait_delta.Rdata")

################################################################################

#### Proportion of avian Orders (urban and regional) ####

### Aanekoski
### Extract species and orders for urban and regional assemblages
order_Aanekoski_urban <- filter(bird_taxonomy_regional, grepl(paste(Aanekoski_urban_filtering, collapse="|"), TipLabel))

order_Aanekoski_regional <- filter(bird_taxonomy_regional, grepl(paste(Aanekoski_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Aanekoski_urban <- order_Aanekoski_urban %>% count(IOCOrder)

number_species_order_Aanekoski_regional <- order_Aanekoski_regional %>% count(IOCOrder)

### Get proportions
prop_order_Aanekoski_urban <- prop.table(number_species_order_Aanekoski_urban$n)

prop_order_Aanekoski_regional <- prop.table(number_species_order_Aanekoski_regional$n)

### Add proportions
number_species_order_Aanekoski_urban$prop <- cbind.data.frame(number_species_order_Aanekoski_urban, prop_order_Aanekoski_urban)

number_species_order_Aanekoski_regional$prop <- cbind.data.frame(number_species_order_Aanekoski_regional, prop_order_Aanekoski_regional)

### Convert proportions into matrix
number_species_order_Aanekoski_urban <- as.matrix(number_species_order_Aanekoski_urban)
number_species_order_Aanekoski_regional <- as.matrix(number_species_order_Aanekoski_regional)

### Convert matrix into data frame
number_species_order_Aanekoski_urban <- as.data.frame(number_species_order_Aanekoski_urban)
number_species_order_Aanekoski_regional <- as.data.frame(number_species_order_Aanekoski_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Aanekoski_urban_regional <- bind_rows(number_species_order_Aanekoski_urban,anti_join(number_species_order_Aanekoski_regional,number_species_order_Aanekoski_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Aanekoski_urban_regional <- number_species_order_Aanekoski_urban_regional[order(number_species_order_Aanekoski_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Aanekoski_urban_regional$prop.prop_order_Aanekoski_regional <- number_species_order_Aanekoski_regional$prop.prop_order_Aanekoski_regional

### Replace NA by ceros in the urban proportions
number_species_order_Aanekoski_urban_regional[is.na(number_species_order_Aanekoski_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Aanekoski_urban_regional_2 <- bind_rows(number_species_order_Aanekoski_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Aanekoski_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Aanekoski_urban_regional_2 <- number_species_order_Aanekoski_urban_regional_2[order(number_species_order_Aanekoski_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Aanekoski_urban_regional_2[is.na(number_species_order_Aanekoski_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Aanekoski_urban_regional_3 <- data.frame(number_species_order_Aanekoski_urban_regional_2[1],
                                                              number_species_order_Aanekoski_urban_regional_2[5],
                                                              number_species_order_Aanekoski_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Aanekoski_urban_regional_3 <- setNames(number_species_order_Aanekoski_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Aanekoski_urban_regional_3$prop_urban <- as.numeric(number_species_order_Aanekoski_urban_regional_3$prop_urban)
number_species_order_Aanekoski_urban_regional_3$prop_regional <- as.numeric(number_species_order_Aanekoski_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Aanekoski_order_prop_vector <- (number_species_order_Aanekoski_urban_regional_3$prop_urban - number_species_order_Aanekoski_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Aanekoski_order_prop_df <- cbind.data.frame(number_species_order_Aanekoski_urban_regional_3[1],
                                            Aanekoski_order_prop_vector)

### Change column names
colnames(Aanekoski_order_prop_df) <- c('Order', 'Aanekoski')



### Ain_Beida
### Extract species and orders for urban and regional assemblages
order_Ain_Beida_urban <- filter(bird_taxonomy_regional, grepl(paste(Ain_Beida_urban_filtering, collapse="|"), TipLabel))

order_Ain_Beida_regional <- filter(bird_taxonomy_regional, grepl(paste(Ain_Beida_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Ain_Beida_urban <- order_Ain_Beida_urban %>% count(IOCOrder)

number_species_order_Ain_Beida_regional <- order_Ain_Beida_regional %>% count(IOCOrder)

### Get proportions
prop_order_Ain_Beida_urban <- prop.table(number_species_order_Ain_Beida_urban$n)

prop_order_Ain_Beida_regional <- prop.table(number_species_order_Ain_Beida_regional$n)

### Add proportions
number_species_order_Ain_Beida_urban$prop <- cbind.data.frame(number_species_order_Ain_Beida_urban, prop_order_Ain_Beida_urban)

number_species_order_Ain_Beida_regional$prop <- cbind.data.frame(number_species_order_Ain_Beida_regional, prop_order_Ain_Beida_regional)

### Convert proportions into matrix
number_species_order_Ain_Beida_urban <- as.matrix(number_species_order_Ain_Beida_urban)
number_species_order_Ain_Beida_regional <- as.matrix(number_species_order_Ain_Beida_regional)

### Convert matrix into data frame
number_species_order_Ain_Beida_urban <- as.data.frame(number_species_order_Ain_Beida_urban)
number_species_order_Ain_Beida_regional <- as.data.frame(number_species_order_Ain_Beida_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Ain_Beida_urban_regional <- bind_rows(number_species_order_Ain_Beida_urban,anti_join(number_species_order_Ain_Beida_regional,number_species_order_Ain_Beida_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Ain_Beida_urban_regional <- number_species_order_Ain_Beida_urban_regional[order(number_species_order_Ain_Beida_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Ain_Beida_urban_regional$prop.prop_order_Ain_Beida_regional <- number_species_order_Ain_Beida_regional$prop.prop_order_Ain_Beida_regional

### Replace NA by ceros in the urban proportions
number_species_order_Ain_Beida_urban_regional[is.na(number_species_order_Ain_Beida_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Ain_Beida_urban_regional_2 <- bind_rows(number_species_order_Ain_Beida_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Ain_Beida_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Ain_Beida_urban_regional_2 <- number_species_order_Ain_Beida_urban_regional_2[order(number_species_order_Ain_Beida_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Ain_Beida_urban_regional_2[is.na(number_species_order_Ain_Beida_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Ain_Beida_urban_regional_3 <- data.frame(number_species_order_Ain_Beida_urban_regional_2[1],
                                                              number_species_order_Ain_Beida_urban_regional_2[5],
                                                              number_species_order_Ain_Beida_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Ain_Beida_urban_regional_3 <- setNames(number_species_order_Ain_Beida_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Ain_Beida_urban_regional_3$prop_urban <- as.numeric(number_species_order_Ain_Beida_urban_regional_3$prop_urban)
number_species_order_Ain_Beida_urban_regional_3$prop_regional <- as.numeric(number_species_order_Ain_Beida_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Ain_Beida_order_prop_vector <- (number_species_order_Ain_Beida_urban_regional_3$prop_urban - number_species_order_Ain_Beida_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Ain_Beida_order_prop_df <- cbind.data.frame(number_species_order_Ain_Beida_urban_regional_3[1],
                                            Ain_Beida_order_prop_vector)

### Change column names
colnames(Ain_Beida_order_prop_df) <- c('Order', 'Ain_Beida')



### Asuncion
### Extract species and orders for urban and regional assemblages
order_Asuncion_urban <- filter(bird_taxonomy_regional, grepl(paste(Asuncion_urban_filtering, collapse="|"), TipLabel))

order_Asuncion_regional <- filter(bird_taxonomy_regional, grepl(paste(Asuncion_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Asuncion_urban <- order_Asuncion_urban %>% count(IOCOrder)

number_species_order_Asuncion_regional <- order_Asuncion_regional %>% count(IOCOrder)

### Get proportions
prop_order_Asuncion_urban <- prop.table(number_species_order_Asuncion_urban$n)

prop_order_Asuncion_regional <- prop.table(number_species_order_Asuncion_regional$n)

### Add proportions
number_species_order_Asuncion_urban$prop <- cbind.data.frame(number_species_order_Asuncion_urban, prop_order_Asuncion_urban)

number_species_order_Asuncion_regional$prop <- cbind.data.frame(number_species_order_Asuncion_regional, prop_order_Asuncion_regional)

### Convert proportions into matrix
number_species_order_Asuncion_urban <- as.matrix(number_species_order_Asuncion_urban)
number_species_order_Asuncion_regional <- as.matrix(number_species_order_Asuncion_regional)

### Convert matrix into data frame
number_species_order_Asuncion_urban <- as.data.frame(number_species_order_Asuncion_urban)
number_species_order_Asuncion_regional <- as.data.frame(number_species_order_Asuncion_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Asuncion_urban_regional <- bind_rows(number_species_order_Asuncion_urban,anti_join(number_species_order_Asuncion_regional,number_species_order_Asuncion_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Asuncion_urban_regional <- number_species_order_Asuncion_urban_regional[order(number_species_order_Asuncion_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Asuncion_urban_regional$prop.prop_order_Asuncion_regional <- number_species_order_Asuncion_regional$prop.prop_order_Asuncion_regional

### Replace NA by ceros in the urban proportions
number_species_order_Asuncion_urban_regional[is.na(number_species_order_Asuncion_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Asuncion_urban_regional_2 <- bind_rows(number_species_order_Asuncion_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Asuncion_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Asuncion_urban_regional_2 <- number_species_order_Asuncion_urban_regional_2[order(number_species_order_Asuncion_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Asuncion_urban_regional_2[is.na(number_species_order_Asuncion_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Asuncion_urban_regional_3 <- data.frame(number_species_order_Asuncion_urban_regional_2[1],
                                                             number_species_order_Asuncion_urban_regional_2[5],
                                                             number_species_order_Asuncion_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Asuncion_urban_regional_3 <- setNames(number_species_order_Asuncion_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Asuncion_urban_regional_3$prop_urban <- as.numeric(number_species_order_Asuncion_urban_regional_3$prop_urban)
number_species_order_Asuncion_urban_regional_3$prop_regional <- as.numeric(number_species_order_Asuncion_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Asuncion_order_prop_vector <- (number_species_order_Asuncion_urban_regional_3$prop_urban - number_species_order_Asuncion_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Asuncion_order_prop_df <- cbind.data.frame(number_species_order_Asuncion_urban_regional_3[1],
                                           Asuncion_order_prop_vector)

### Change column names
colnames(Asuncion_order_prop_df) <- c('Order', 'Asuncion')



### Belem
### Extract species and orders for urban and regional assemblages
order_Belem_urban <- filter(bird_taxonomy_regional, grepl(paste(Belem_urban_filtering, collapse="|"), TipLabel))

order_Belem_regional <- filter(bird_taxonomy_regional, grepl(paste(Belem_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Belem_urban <- order_Belem_urban %>% count(IOCOrder)

number_species_order_Belem_regional <- order_Belem_regional %>% count(IOCOrder)

### Get proportions
prop_order_Belem_urban <- prop.table(number_species_order_Belem_urban$n)

prop_order_Belem_regional <- prop.table(number_species_order_Belem_regional$n)

### Add proportions
number_species_order_Belem_urban$prop <- cbind.data.frame(number_species_order_Belem_urban, prop_order_Belem_urban)

number_species_order_Belem_regional$prop <- cbind.data.frame(number_species_order_Belem_regional, prop_order_Belem_regional)

### Convert proportions into matrix
number_species_order_Belem_urban <- as.matrix(number_species_order_Belem_urban)
number_species_order_Belem_regional <- as.matrix(number_species_order_Belem_regional)

### Convert matrix into data frame
number_species_order_Belem_urban <- as.data.frame(number_species_order_Belem_urban)
number_species_order_Belem_regional <- as.data.frame(number_species_order_Belem_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Belem_urban_regional <- bind_rows(number_species_order_Belem_urban,anti_join(number_species_order_Belem_regional,number_species_order_Belem_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Belem_urban_regional <- number_species_order_Belem_urban_regional[order(number_species_order_Belem_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Belem_urban_regional$prop.prop_order_Belem_regional <- number_species_order_Belem_regional$prop.prop_order_Belem_regional

### Replace NA by ceros in the urban proportions
number_species_order_Belem_urban_regional[is.na(number_species_order_Belem_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Belem_urban_regional_2 <- bind_rows(number_species_order_Belem_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Belem_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Belem_urban_regional_2 <- number_species_order_Belem_urban_regional_2[order(number_species_order_Belem_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Belem_urban_regional_2[is.na(number_species_order_Belem_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Belem_urban_regional_3 <- data.frame(number_species_order_Belem_urban_regional_2[1],
                                                          number_species_order_Belem_urban_regional_2[5],
                                                          number_species_order_Belem_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Belem_urban_regional_3 <- setNames(number_species_order_Belem_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Belem_urban_regional_3$prop_urban <- as.numeric(number_species_order_Belem_urban_regional_3$prop_urban)
number_species_order_Belem_urban_regional_3$prop_regional <- as.numeric(number_species_order_Belem_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Belem_order_prop_vector <- (number_species_order_Belem_urban_regional_3$prop_urban - number_species_order_Belem_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Belem_order_prop_df <- cbind.data.frame(number_species_order_Belem_urban_regional_3[1],
                                        Belem_order_prop_vector)

### Change column names
colnames(Belem_order_prop_df) <- c('Order', 'Belem')



### Bernal
### Extract species and orders for urban and regional assemblages
order_Bernal_urban <- filter(bird_taxonomy_regional, grepl(paste(Bernal_urban_filtering, collapse="|"), TipLabel))

order_Bernal_regional <- filter(bird_taxonomy_regional, grepl(paste(Bernal_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Bernal_urban <- order_Bernal_urban %>% count(IOCOrder)

number_species_order_Bernal_regional <- order_Bernal_regional %>% count(IOCOrder)

### Get proportions
prop_order_Bernal_urban <- prop.table(number_species_order_Bernal_urban$n)

prop_order_Bernal_regional <- prop.table(number_species_order_Bernal_regional$n)

### Add proportions
number_species_order_Bernal_urban$prop <- cbind.data.frame(number_species_order_Bernal_urban, prop_order_Bernal_urban)

number_species_order_Bernal_regional$prop <- cbind.data.frame(number_species_order_Bernal_regional, prop_order_Bernal_regional)

### Convert proportions into matrix
number_species_order_Bernal_urban <- as.matrix(number_species_order_Bernal_urban)
number_species_order_Bernal_regional <- as.matrix(number_species_order_Bernal_regional)

### Convert matrix into data frame
number_species_order_Bernal_urban <- as.data.frame(number_species_order_Bernal_urban)
number_species_order_Bernal_regional <- as.data.frame(number_species_order_Bernal_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Bernal_urban_regional <- bind_rows(number_species_order_Bernal_urban,anti_join(number_species_order_Bernal_regional,number_species_order_Bernal_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Bernal_urban_regional <- number_species_order_Bernal_urban_regional[order(number_species_order_Bernal_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Bernal_urban_regional$prop.prop_order_Bernal_regional <- number_species_order_Bernal_regional$prop.prop_order_Bernal_regional

### Replace NA by ceros in the urban proportions
number_species_order_Bernal_urban_regional[is.na(number_species_order_Bernal_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Bernal_urban_regional_2 <- bind_rows(number_species_order_Bernal_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Bernal_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Bernal_urban_regional_2 <- number_species_order_Bernal_urban_regional_2[order(number_species_order_Bernal_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Bernal_urban_regional_2[is.na(number_species_order_Bernal_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Bernal_urban_regional_3 <- data.frame(number_species_order_Bernal_urban_regional_2[1],
                                                           number_species_order_Bernal_urban_regional_2[5],
                                                           number_species_order_Bernal_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Bernal_urban_regional_3 <- setNames(number_species_order_Bernal_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Bernal_urban_regional_3$prop_urban <- as.numeric(number_species_order_Bernal_urban_regional_3$prop_urban)
number_species_order_Bernal_urban_regional_3$prop_regional <- as.numeric(number_species_order_Bernal_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Bernal_order_prop_vector <- (number_species_order_Bernal_urban_regional_3$prop_urban - number_species_order_Bernal_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Bernal_order_prop_df <- cbind.data.frame(number_species_order_Bernal_urban_regional_3[1],
                                         Bernal_order_prop_vector)

### Change column names
colnames(Bernal_order_prop_df) <- c('Order', 'Bernal')



### Biala_Podlaska
### Extract species and orders for urban and regional assemblages
order_Biala_Podlaska_urban <- filter(bird_taxonomy_regional, grepl(paste(Biala_Podlaska_urban_filtering, collapse="|"), TipLabel))

order_Biala_Podlaska_regional <- filter(bird_taxonomy_regional, grepl(paste(Biala_Podlaska_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Biala_Podlaska_urban <- order_Biala_Podlaska_urban %>% count(IOCOrder)

number_species_order_Biala_Podlaska_regional <- order_Biala_Podlaska_regional %>% count(IOCOrder)

### Get proportions
prop_order_Biala_Podlaska_urban <- prop.table(number_species_order_Biala_Podlaska_urban$n)

prop_order_Biala_Podlaska_regional <- prop.table(number_species_order_Biala_Podlaska_regional$n)

### Add proportions
number_species_order_Biala_Podlaska_urban$prop <- cbind.data.frame(number_species_order_Biala_Podlaska_urban, prop_order_Biala_Podlaska_urban)

number_species_order_Biala_Podlaska_regional$prop <- cbind.data.frame(number_species_order_Biala_Podlaska_regional, prop_order_Biala_Podlaska_regional)

### Convert proportions into matrix
number_species_order_Biala_Podlaska_urban <- as.matrix(number_species_order_Biala_Podlaska_urban)
number_species_order_Biala_Podlaska_regional <- as.matrix(number_species_order_Biala_Podlaska_regional)

### Convert matrix into data frame
number_species_order_Biala_Podlaska_urban <- as.data.frame(number_species_order_Biala_Podlaska_urban)
number_species_order_Biala_Podlaska_regional <- as.data.frame(number_species_order_Biala_Podlaska_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Biala_Podlaska_urban_regional <- bind_rows(number_species_order_Biala_Podlaska_urban,anti_join(number_species_order_Biala_Podlaska_regional,number_species_order_Biala_Podlaska_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Biala_Podlaska_urban_regional <- number_species_order_Biala_Podlaska_urban_regional[order(number_species_order_Biala_Podlaska_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Biala_Podlaska_urban_regional$prop.prop_order_Biala_Podlaska_regional <- number_species_order_Biala_Podlaska_regional$prop.prop_order_Biala_Podlaska_regional

### Replace NA by ceros in the urban proportions
number_species_order_Biala_Podlaska_urban_regional[is.na(number_species_order_Biala_Podlaska_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Biala_Podlaska_urban_regional_2 <- bind_rows(number_species_order_Biala_Podlaska_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Biala_Podlaska_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Biala_Podlaska_urban_regional_2 <- number_species_order_Biala_Podlaska_urban_regional_2[order(number_species_order_Biala_Podlaska_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Biala_Podlaska_urban_regional_2[is.na(number_species_order_Biala_Podlaska_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Biala_Podlaska_urban_regional_3 <- data.frame(number_species_order_Biala_Podlaska_urban_regional_2[1],
                                                                   number_species_order_Biala_Podlaska_urban_regional_2[5],
                                                                   number_species_order_Biala_Podlaska_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Biala_Podlaska_urban_regional_3 <- setNames(number_species_order_Biala_Podlaska_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Biala_Podlaska_urban_regional_3$prop_urban <- as.numeric(number_species_order_Biala_Podlaska_urban_regional_3$prop_urban)
number_species_order_Biala_Podlaska_urban_regional_3$prop_regional <- as.numeric(number_species_order_Biala_Podlaska_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Biala_Podlaska_order_prop_vector <- (number_species_order_Biala_Podlaska_urban_regional_3$prop_urban - number_species_order_Biala_Podlaska_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Biala_Podlaska_order_prop_df <- cbind.data.frame(number_species_order_Biala_Podlaska_urban_regional_3[1],
                                                 Biala_Podlaska_order_prop_vector)

### Change column names
colnames(Biala_Podlaska_order_prop_df) <- c('Order', 'Biala_Podlaska')



### Bialystok
### Extract species and orders for urban and regional assemblages
order_Bialystok_urban <- filter(bird_taxonomy_regional, grepl(paste(Bialystok_urban_filtering, collapse="|"), TipLabel))

order_Bialystok_regional <- filter(bird_taxonomy_regional, grepl(paste(Bialystok_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Bialystok_urban <- order_Bialystok_urban %>% count(IOCOrder)

number_species_order_Bialystok_regional <- order_Bialystok_regional %>% count(IOCOrder)

### Get proportions
prop_order_Bialystok_urban <- prop.table(number_species_order_Bialystok_urban$n)

prop_order_Bialystok_regional <- prop.table(number_species_order_Bialystok_regional$n)

### Add proportions
number_species_order_Bialystok_urban$prop <- cbind.data.frame(number_species_order_Bialystok_urban, prop_order_Bialystok_urban)

number_species_order_Bialystok_regional$prop <- cbind.data.frame(number_species_order_Bialystok_regional, prop_order_Bialystok_regional)

### Convert proportions into matrix
number_species_order_Bialystok_urban <- as.matrix(number_species_order_Bialystok_urban)
number_species_order_Bialystok_regional <- as.matrix(number_species_order_Bialystok_regional)

### Convert matrix into data frame
number_species_order_Bialystok_urban <- as.data.frame(number_species_order_Bialystok_urban)
number_species_order_Bialystok_regional <- as.data.frame(number_species_order_Bialystok_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Bialystok_urban_regional <- bind_rows(number_species_order_Bialystok_urban,anti_join(number_species_order_Bialystok_regional,number_species_order_Bialystok_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Bialystok_urban_regional <- number_species_order_Bialystok_urban_regional[order(number_species_order_Bialystok_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Bialystok_urban_regional$prop.prop_order_Bialystok_regional <- number_species_order_Bialystok_regional$prop.prop_order_Bialystok_regional

### Replace NA by ceros in the urban proportions
number_species_order_Bialystok_urban_regional[is.na(number_species_order_Bialystok_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Bialystok_urban_regional_2 <- bind_rows(number_species_order_Bialystok_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Bialystok_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Bialystok_urban_regional_2 <- number_species_order_Bialystok_urban_regional_2[order(number_species_order_Bialystok_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Bialystok_urban_regional_2[is.na(number_species_order_Bialystok_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Bialystok_urban_regional_3 <- data.frame(number_species_order_Bialystok_urban_regional_2[1],
                                                              number_species_order_Bialystok_urban_regional_2[5],
                                                              number_species_order_Bialystok_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Bialystok_urban_regional_3 <- setNames(number_species_order_Bialystok_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Bialystok_urban_regional_3$prop_urban <- as.numeric(number_species_order_Bialystok_urban_regional_3$prop_urban)
number_species_order_Bialystok_urban_regional_3$prop_regional <- as.numeric(number_species_order_Bialystok_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Bialystok_order_prop_vector <- (number_species_order_Bialystok_urban_regional_3$prop_urban - number_species_order_Bialystok_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Bialystok_order_prop_df <- cbind.data.frame(number_species_order_Bialystok_urban_regional_3[1],
                                            Bialystok_order_prop_vector)

### Change column names
colnames(Bialystok_order_prop_df) <- c('Order', 'Bialystok')



### Bydgoszcz
### Extract species and orders for urban and regional assemblages
order_Bydgoszcz_urban <- filter(bird_taxonomy_regional, grepl(paste(Bydgoszcz_urban_filtering, collapse="|"), TipLabel))

order_Bydgoszcz_regional <- filter(bird_taxonomy_regional, grepl(paste(Bydgoszcz_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Bydgoszcz_urban <- order_Bydgoszcz_urban %>% count(IOCOrder)

number_species_order_Bydgoszcz_regional <- order_Bydgoszcz_regional %>% count(IOCOrder)

### Get proportions
prop_order_Bydgoszcz_urban <- prop.table(number_species_order_Bydgoszcz_urban$n)

prop_order_Bydgoszcz_regional <- prop.table(number_species_order_Bydgoszcz_regional$n)

### Add proportions
number_species_order_Bydgoszcz_urban$prop <- cbind.data.frame(number_species_order_Bydgoszcz_urban, prop_order_Bydgoszcz_urban)

number_species_order_Bydgoszcz_regional$prop <- cbind.data.frame(number_species_order_Bydgoszcz_regional, prop_order_Bydgoszcz_regional)

### Convert proportions into matrix
number_species_order_Bydgoszcz_urban <- as.matrix(number_species_order_Bydgoszcz_urban)
number_species_order_Bydgoszcz_regional <- as.matrix(number_species_order_Bydgoszcz_regional)

### Convert matrix into data frame
number_species_order_Bydgoszcz_urban <- as.data.frame(number_species_order_Bydgoszcz_urban)
number_species_order_Bydgoszcz_regional <- as.data.frame(number_species_order_Bydgoszcz_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Bydgoszcz_urban_regional <- bind_rows(number_species_order_Bydgoszcz_urban,anti_join(number_species_order_Bydgoszcz_regional,number_species_order_Bydgoszcz_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Bydgoszcz_urban_regional <- number_species_order_Bydgoszcz_urban_regional[order(number_species_order_Bydgoszcz_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Bydgoszcz_urban_regional$prop.prop_order_Bydgoszcz_regional <- number_species_order_Bydgoszcz_regional$prop.prop_order_Bydgoszcz_regional

### Replace NA by ceros in the urban proportions
number_species_order_Bydgoszcz_urban_regional[is.na(number_species_order_Bydgoszcz_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Bydgoszcz_urban_regional_2 <- bind_rows(number_species_order_Bydgoszcz_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Bydgoszcz_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Bydgoszcz_urban_regional_2 <- number_species_order_Bydgoszcz_urban_regional_2[order(number_species_order_Bydgoszcz_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Bydgoszcz_urban_regional_2[is.na(number_species_order_Bydgoszcz_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Bydgoszcz_urban_regional_3 <- data.frame(number_species_order_Bydgoszcz_urban_regional_2[1],
                                                              number_species_order_Bydgoszcz_urban_regional_2[5],
                                                              number_species_order_Bydgoszcz_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Bydgoszcz_urban_regional_3 <- setNames(number_species_order_Bydgoszcz_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Bydgoszcz_urban_regional_3$prop_urban <- as.numeric(number_species_order_Bydgoszcz_urban_regional_3$prop_urban)
number_species_order_Bydgoszcz_urban_regional_3$prop_regional <- as.numeric(number_species_order_Bydgoszcz_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Bydgoszcz_order_prop_vector <- (number_species_order_Bydgoszcz_urban_regional_3$prop_urban - number_species_order_Bydgoszcz_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Bydgoszcz_order_prop_df <- cbind.data.frame(number_species_order_Bydgoszcz_urban_regional_3[1],
                                            Bydgoszcz_order_prop_vector)

### Change column names
colnames(Bydgoszcz_order_prop_df) <- c('Order', 'Bydgoszcz')



### Ciudad_Juarez
### Extract species and orders for urban and regional assemblages
order_Ciudad_Juarez_urban <- filter(bird_taxonomy_regional, grepl(paste(Ciudad_Juarez_urban_filtering, collapse="|"), TipLabel))

order_Ciudad_Juarez_regional <- filter(bird_taxonomy_regional, grepl(paste(Ciudad_Juarez_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Ciudad_Juarez_urban <- order_Ciudad_Juarez_urban %>% count(IOCOrder)

number_species_order_Ciudad_Juarez_regional <- order_Ciudad_Juarez_regional %>% count(IOCOrder)

### Get proportions
prop_order_Ciudad_Juarez_urban <- prop.table(number_species_order_Ciudad_Juarez_urban$n)

prop_order_Ciudad_Juarez_regional <- prop.table(number_species_order_Ciudad_Juarez_regional$n)

### Add proportions
number_species_order_Ciudad_Juarez_urban$prop <- cbind.data.frame(number_species_order_Ciudad_Juarez_urban, prop_order_Ciudad_Juarez_urban)

number_species_order_Ciudad_Juarez_regional$prop <- cbind.data.frame(number_species_order_Ciudad_Juarez_regional, prop_order_Ciudad_Juarez_regional)

### Convert proportions into matrix
number_species_order_Ciudad_Juarez_urban <- as.matrix(number_species_order_Ciudad_Juarez_urban)
number_species_order_Ciudad_Juarez_regional <- as.matrix(number_species_order_Ciudad_Juarez_regional)

### Convert matrix into data frame
number_species_order_Ciudad_Juarez_urban <- as.data.frame(number_species_order_Ciudad_Juarez_urban)
number_species_order_Ciudad_Juarez_regional <- as.data.frame(number_species_order_Ciudad_Juarez_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Ciudad_Juarez_urban_regional <- bind_rows(number_species_order_Ciudad_Juarez_urban,anti_join(number_species_order_Ciudad_Juarez_regional,number_species_order_Ciudad_Juarez_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Ciudad_Juarez_urban_regional <- number_species_order_Ciudad_Juarez_urban_regional[order(number_species_order_Ciudad_Juarez_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Ciudad_Juarez_urban_regional$prop.prop_order_Ciudad_Juarez_regional <- number_species_order_Ciudad_Juarez_regional$prop.prop_order_Ciudad_Juarez_regional

### Replace NA by ceros in the urban proportions
number_species_order_Ciudad_Juarez_urban_regional[is.na(number_species_order_Ciudad_Juarez_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Ciudad_Juarez_urban_regional_2 <- bind_rows(number_species_order_Ciudad_Juarez_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Ciudad_Juarez_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Ciudad_Juarez_urban_regional_2 <- number_species_order_Ciudad_Juarez_urban_regional_2[order(number_species_order_Ciudad_Juarez_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Ciudad_Juarez_urban_regional_2[is.na(number_species_order_Ciudad_Juarez_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Ciudad_Juarez_urban_regional_3 <- data.frame(number_species_order_Ciudad_Juarez_urban_regional_2[1],
                                                                  number_species_order_Ciudad_Juarez_urban_regional_2[5],
                                                                  number_species_order_Ciudad_Juarez_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Ciudad_Juarez_urban_regional_3 <- setNames(number_species_order_Ciudad_Juarez_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Ciudad_Juarez_urban_regional_3$prop_urban <- as.numeric(number_species_order_Ciudad_Juarez_urban_regional_3$prop_urban)
number_species_order_Ciudad_Juarez_urban_regional_3$prop_regional <- as.numeric(number_species_order_Ciudad_Juarez_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Ciudad_Juarez_order_prop_vector <- (number_species_order_Ciudad_Juarez_urban_regional_3$prop_urban - number_species_order_Ciudad_Juarez_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Ciudad_Juarez_order_prop_df <- cbind.data.frame(number_species_order_Ciudad_Juarez_urban_regional_3[1],
                                                Ciudad_Juarez_order_prop_vector)

### Change column names
colnames(Ciudad_Juarez_order_prop_df) <- c('Order', 'Ciudad_Juarez')



### Czestochowa
### Extract species and orders for urban and regional assemblages
order_Czestochowa_urban <- filter(bird_taxonomy_regional, grepl(paste(Czestochowa_urban_filtering, collapse="|"), TipLabel))

order_Czestochowa_regional <- filter(bird_taxonomy_regional, grepl(paste(Czestochowa_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Czestochowa_urban <- order_Czestochowa_urban %>% count(IOCOrder)

number_species_order_Czestochowa_regional <- order_Czestochowa_regional %>% count(IOCOrder)

### Get proportions
prop_order_Czestochowa_urban <- prop.table(number_species_order_Czestochowa_urban$n)

prop_order_Czestochowa_regional <- prop.table(number_species_order_Czestochowa_regional$n)

### Add proportions
number_species_order_Czestochowa_urban$prop <- cbind.data.frame(number_species_order_Czestochowa_urban, prop_order_Czestochowa_urban)

number_species_order_Czestochowa_regional$prop <- cbind.data.frame(number_species_order_Czestochowa_regional, prop_order_Czestochowa_regional)

### Convert proportions into matrix
number_species_order_Czestochowa_urban <- as.matrix(number_species_order_Czestochowa_urban)
number_species_order_Czestochowa_regional <- as.matrix(number_species_order_Czestochowa_regional)

### Convert matrix into data frame
number_species_order_Czestochowa_urban <- as.data.frame(number_species_order_Czestochowa_urban)
number_species_order_Czestochowa_regional <- as.data.frame(number_species_order_Czestochowa_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Czestochowa_urban_regional <- bind_rows(number_species_order_Czestochowa_urban,anti_join(number_species_order_Czestochowa_regional,number_species_order_Czestochowa_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Czestochowa_urban_regional <- number_species_order_Czestochowa_urban_regional[order(number_species_order_Czestochowa_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Czestochowa_urban_regional$prop.prop_order_Czestochowa_regional <- number_species_order_Czestochowa_regional$prop.prop_order_Czestochowa_regional

### Replace NA by ceros in the urban proportions
number_species_order_Czestochowa_urban_regional[is.na(number_species_order_Czestochowa_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Czestochowa_urban_regional_2 <- bind_rows(number_species_order_Czestochowa_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Czestochowa_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Czestochowa_urban_regional_2 <- number_species_order_Czestochowa_urban_regional_2[order(number_species_order_Czestochowa_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Czestochowa_urban_regional_2[is.na(number_species_order_Czestochowa_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Czestochowa_urban_regional_3 <- data.frame(number_species_order_Czestochowa_urban_regional_2[1],
                                                                number_species_order_Czestochowa_urban_regional_2[5],
                                                                number_species_order_Czestochowa_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Czestochowa_urban_regional_3 <- setNames(number_species_order_Czestochowa_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Czestochowa_urban_regional_3$prop_urban <- as.numeric(number_species_order_Czestochowa_urban_regional_3$prop_urban)
number_species_order_Czestochowa_urban_regional_3$prop_regional <- as.numeric(number_species_order_Czestochowa_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Czestochowa_order_prop_vector <- (number_species_order_Czestochowa_urban_regional_3$prop_urban - number_species_order_Czestochowa_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Czestochowa_order_prop_df <- cbind.data.frame(number_species_order_Czestochowa_urban_regional_3[1],
                                              Czestochowa_order_prop_vector)

### Change column names
colnames(Czestochowa_order_prop_df) <- c('Order', 'Czestochowa')



### Durango
### Extract species and orders for urban and regional assemblages
order_Durango_urban <- filter(bird_taxonomy_regional, grepl(paste(Durango_urban_filtering, collapse="|"), TipLabel))

order_Durango_regional <- filter(bird_taxonomy_regional, grepl(paste(Durango_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Durango_urban <- order_Durango_urban %>% count(IOCOrder)

number_species_order_Durango_regional <- order_Durango_regional %>% count(IOCOrder)

### Get proportions
prop_order_Durango_urban <- prop.table(number_species_order_Durango_urban$n)

prop_order_Durango_regional <- prop.table(number_species_order_Durango_regional$n)

### Add proportions
number_species_order_Durango_urban$prop <- cbind.data.frame(number_species_order_Durango_urban, prop_order_Durango_urban)

number_species_order_Durango_regional$prop <- cbind.data.frame(number_species_order_Durango_regional, prop_order_Durango_regional)

### Convert proportions into matrix
number_species_order_Durango_urban <- as.matrix(number_species_order_Durango_urban)
number_species_order_Durango_regional <- as.matrix(number_species_order_Durango_regional)

### Convert matrix into data frame
number_species_order_Durango_urban <- as.data.frame(number_species_order_Durango_urban)
number_species_order_Durango_regional <- as.data.frame(number_species_order_Durango_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Durango_urban_regional <- bind_rows(number_species_order_Durango_urban,anti_join(number_species_order_Durango_regional,number_species_order_Durango_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Durango_urban_regional <- number_species_order_Durango_urban_regional[order(number_species_order_Durango_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Durango_urban_regional$prop.prop_order_Durango_regional <- number_species_order_Durango_regional$prop.prop_order_Durango_regional

### Replace NA by ceros in the urban proportions
number_species_order_Durango_urban_regional[is.na(number_species_order_Durango_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Durango_urban_regional_2 <- bind_rows(number_species_order_Durango_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Durango_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Durango_urban_regional_2 <- number_species_order_Durango_urban_regional_2[order(number_species_order_Durango_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Durango_urban_regional_2[is.na(number_species_order_Durango_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Durango_urban_regional_3 <- data.frame(number_species_order_Durango_urban_regional_2[1],
                                                            number_species_order_Durango_urban_regional_2[5],
                                                            number_species_order_Durango_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Durango_urban_regional_3 <- setNames(number_species_order_Durango_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Durango_urban_regional_3$prop_urban <- as.numeric(number_species_order_Durango_urban_regional_3$prop_urban)
number_species_order_Durango_urban_regional_3$prop_regional <- as.numeric(number_species_order_Durango_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Durango_order_prop_vector <- (number_species_order_Durango_urban_regional_3$prop_urban - number_species_order_Durango_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Durango_order_prop_df <- cbind.data.frame(number_species_order_Durango_urban_regional_3[1],
                                          Durango_order_prop_vector)

### Change column names
colnames(Durango_order_prop_df) <- c('Order', 'Durango')



### Fresno
### Extract species and orders for urban and regional assemblages
order_Fresno_urban <- filter(bird_taxonomy_regional, grepl(paste(Fresno_urban_filtering, collapse="|"), TipLabel))

order_Fresno_regional <- filter(bird_taxonomy_regional, grepl(paste(Fresno_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Fresno_urban <- order_Fresno_urban %>% count(IOCOrder)

number_species_order_Fresno_regional <- order_Fresno_regional %>% count(IOCOrder)

### Get proportions
prop_order_Fresno_urban <- prop.table(number_species_order_Fresno_urban$n)

prop_order_Fresno_regional <- prop.table(number_species_order_Fresno_regional$n)

### Add proportions
number_species_order_Fresno_urban$prop <- cbind.data.frame(number_species_order_Fresno_urban, prop_order_Fresno_urban)

number_species_order_Fresno_regional$prop <- cbind.data.frame(number_species_order_Fresno_regional, prop_order_Fresno_regional)

### Convert proportions into matrix
number_species_order_Fresno_urban <- as.matrix(number_species_order_Fresno_urban)
number_species_order_Fresno_regional <- as.matrix(number_species_order_Fresno_regional)

### Convert matrix into data frame
number_species_order_Fresno_urban <- as.data.frame(number_species_order_Fresno_urban)
number_species_order_Fresno_regional <- as.data.frame(number_species_order_Fresno_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Fresno_urban_regional <- bind_rows(number_species_order_Fresno_urban,anti_join(number_species_order_Fresno_regional,number_species_order_Fresno_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Fresno_urban_regional <- number_species_order_Fresno_urban_regional[order(number_species_order_Fresno_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Fresno_urban_regional$prop.prop_order_Fresno_regional <- number_species_order_Fresno_regional$prop.prop_order_Fresno_regional

### Replace NA by ceros in the urban proportions
number_species_order_Fresno_urban_regional[is.na(number_species_order_Fresno_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Fresno_urban_regional_2 <- bind_rows(number_species_order_Fresno_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Fresno_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Fresno_urban_regional_2 <- number_species_order_Fresno_urban_regional_2[order(number_species_order_Fresno_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Fresno_urban_regional_2[is.na(number_species_order_Fresno_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Fresno_urban_regional_3 <- data.frame(number_species_order_Fresno_urban_regional_2[1],
                                                           number_species_order_Fresno_urban_regional_2[5],
                                                           number_species_order_Fresno_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Fresno_urban_regional_3 <- setNames(number_species_order_Fresno_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Fresno_urban_regional_3$prop_urban <- as.numeric(number_species_order_Fresno_urban_regional_3$prop_urban)
number_species_order_Fresno_urban_regional_3$prop_regional <- as.numeric(number_species_order_Fresno_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Fresno_order_prop_vector <- (number_species_order_Fresno_urban_regional_3$prop_urban - number_species_order_Fresno_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Fresno_order_prop_df <- cbind.data.frame(number_species_order_Fresno_urban_regional_3[1],
                                         Fresno_order_prop_vector)

### Change column names
colnames(Fresno_order_prop_df) <- c('Order', 'Fresno')


### Gdansk
### Extract species and orders for urban and regional assemblages
order_Gdansk_urban <- filter(bird_taxonomy_regional, grepl(paste(Gdansk_urban_filtering, collapse="|"), TipLabel))

order_Gdansk_regional <- filter(bird_taxonomy_regional, grepl(paste(Gdansk_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Gdansk_urban <- order_Gdansk_urban %>% count(IOCOrder)

number_species_order_Gdansk_regional <- order_Gdansk_regional %>% count(IOCOrder)

### Get proportions
prop_order_Gdansk_urban <- prop.table(number_species_order_Gdansk_urban$n)

prop_order_Gdansk_regional <- prop.table(number_species_order_Gdansk_regional$n)

### Add proportions
number_species_order_Gdansk_urban$prop <- cbind.data.frame(number_species_order_Gdansk_urban, prop_order_Gdansk_urban)

number_species_order_Gdansk_regional$prop <- cbind.data.frame(number_species_order_Gdansk_regional, prop_order_Gdansk_regional)

### Convert proportions into matrix
number_species_order_Gdansk_urban <- as.matrix(number_species_order_Gdansk_urban)
number_species_order_Gdansk_regional <- as.matrix(number_species_order_Gdansk_regional)

### Convert matrix into data frame
number_species_order_Gdansk_urban <- as.data.frame(number_species_order_Gdansk_urban)
number_species_order_Gdansk_regional <- as.data.frame(number_species_order_Gdansk_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Gdansk_urban_regional <- bind_rows(number_species_order_Gdansk_urban,anti_join(number_species_order_Gdansk_regional,number_species_order_Gdansk_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Gdansk_urban_regional <- number_species_order_Gdansk_urban_regional[order(number_species_order_Gdansk_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Gdansk_urban_regional$prop.prop_order_Gdansk_regional <- number_species_order_Gdansk_regional$prop.prop_order_Gdansk_regional

### Replace NA by ceros in the urban proportions
number_species_order_Gdansk_urban_regional[is.na(number_species_order_Gdansk_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Gdansk_urban_regional_2 <- bind_rows(number_species_order_Gdansk_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Gdansk_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Gdansk_urban_regional_2 <- number_species_order_Gdansk_urban_regional_2[order(number_species_order_Gdansk_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Gdansk_urban_regional_2[is.na(number_species_order_Gdansk_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Gdansk_urban_regional_3 <- data.frame(number_species_order_Gdansk_urban_regional_2[1],
                                                           number_species_order_Gdansk_urban_regional_2[5],
                                                           number_species_order_Gdansk_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Gdansk_urban_regional_3 <- setNames(number_species_order_Gdansk_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Gdansk_urban_regional_3$prop_urban <- as.numeric(number_species_order_Gdansk_urban_regional_3$prop_urban)
number_species_order_Gdansk_urban_regional_3$prop_regional <- as.numeric(number_species_order_Gdansk_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Gdansk_order_prop_vector <- (number_species_order_Gdansk_urban_regional_3$prop_urban - number_species_order_Gdansk_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Gdansk_order_prop_df <- cbind.data.frame(number_species_order_Gdansk_urban_regional_3[1],
                                         Gdansk_order_prop_vector)

### Change column names
colnames(Gdansk_order_prop_df) <- c('Order', 'Gdansk')



### Gdynia
### Extract species and orders for urban and regional assemblages
order_Gdynia_urban <- filter(bird_taxonomy_regional, grepl(paste(Gdynia_urban_filtering, collapse="|"), TipLabel))

order_Gdynia_regional <- filter(bird_taxonomy_regional, grepl(paste(Gdynia_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Gdynia_urban <- order_Gdynia_urban %>% count(IOCOrder)

number_species_order_Gdynia_regional <- order_Gdynia_regional %>% count(IOCOrder)

### Get proportions
prop_order_Gdynia_urban <- prop.table(number_species_order_Gdynia_urban$n)

prop_order_Gdynia_regional <- prop.table(number_species_order_Gdynia_regional$n)

### Add proportions
number_species_order_Gdynia_urban$prop <- cbind.data.frame(number_species_order_Gdynia_urban, prop_order_Gdynia_urban)

number_species_order_Gdynia_regional$prop <- cbind.data.frame(number_species_order_Gdynia_regional, prop_order_Gdynia_regional)

### Convert proportions into matrix
number_species_order_Gdynia_urban <- as.matrix(number_species_order_Gdynia_urban)
number_species_order_Gdynia_regional <- as.matrix(number_species_order_Gdynia_regional)

### Convert matrix into data frame
number_species_order_Gdynia_urban <- as.data.frame(number_species_order_Gdynia_urban)
number_species_order_Gdynia_regional <- as.data.frame(number_species_order_Gdynia_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Gdynia_urban_regional <- bind_rows(number_species_order_Gdynia_urban,anti_join(number_species_order_Gdynia_regional,number_species_order_Gdynia_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Gdynia_urban_regional <- number_species_order_Gdynia_urban_regional[order(number_species_order_Gdynia_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Gdynia_urban_regional$prop.prop_order_Gdynia_regional <- number_species_order_Gdynia_regional$prop.prop_order_Gdynia_regional

### Replace NA by ceros in the urban proportions
number_species_order_Gdynia_urban_regional[is.na(number_species_order_Gdynia_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Gdynia_urban_regional_2 <- bind_rows(number_species_order_Gdynia_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Gdynia_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Gdynia_urban_regional_2 <- number_species_order_Gdynia_urban_regional_2[order(number_species_order_Gdynia_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Gdynia_urban_regional_2[is.na(number_species_order_Gdynia_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Gdynia_urban_regional_3 <- data.frame(number_species_order_Gdynia_urban_regional_2[1],
                                                           number_species_order_Gdynia_urban_regional_2[5],
                                                           number_species_order_Gdynia_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Gdynia_urban_regional_3 <- setNames(number_species_order_Gdynia_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Gdynia_urban_regional_3$prop_urban <- as.numeric(number_species_order_Gdynia_urban_regional_3$prop_urban)
number_species_order_Gdynia_urban_regional_3$prop_regional <- as.numeric(number_species_order_Gdynia_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Gdynia_order_prop_vector <- (number_species_order_Gdynia_urban_regional_3$prop_urban - number_species_order_Gdynia_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Gdynia_order_prop_df <- cbind.data.frame(number_species_order_Gdynia_urban_regional_3[1],
                                         Gdynia_order_prop_vector)

### Change column names
colnames(Gdynia_order_prop_df) <- c('Order', 'Gdynia')



### Gorzow_Wielkopolski
### Extract species and orders for urban and regional assemblages
order_Gorzow_Wielkopolski_urban <- filter(bird_taxonomy_regional, grepl(paste(Gorzow_Wielkopolski_urban_filtering, collapse="|"), TipLabel))

order_Gorzow_Wielkopolski_regional <- filter(bird_taxonomy_regional, grepl(paste(Gorzow_Wielkopolski_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Gorzow_Wielkopolski_urban <- order_Gorzow_Wielkopolski_urban %>% count(IOCOrder)

number_species_order_Gorzow_Wielkopolski_regional <- order_Gorzow_Wielkopolski_regional %>% count(IOCOrder)

### Get proportions
prop_order_Gorzow_Wielkopolski_urban <- prop.table(number_species_order_Gorzow_Wielkopolski_urban$n)

prop_order_Gorzow_Wielkopolski_regional <- prop.table(number_species_order_Gorzow_Wielkopolski_regional$n)

### Add proportions
number_species_order_Gorzow_Wielkopolski_urban$prop <- cbind.data.frame(number_species_order_Gorzow_Wielkopolski_urban, prop_order_Gorzow_Wielkopolski_urban)

number_species_order_Gorzow_Wielkopolski_regional$prop <- cbind.data.frame(number_species_order_Gorzow_Wielkopolski_regional, prop_order_Gorzow_Wielkopolski_regional)

### Convert proportions into matrix
number_species_order_Gorzow_Wielkopolski_urban <- as.matrix(number_species_order_Gorzow_Wielkopolski_urban)
number_species_order_Gorzow_Wielkopolski_regional <- as.matrix(number_species_order_Gorzow_Wielkopolski_regional)

### Convert matrix into data frame
number_species_order_Gorzow_Wielkopolski_urban <- as.data.frame(number_species_order_Gorzow_Wielkopolski_urban)
number_species_order_Gorzow_Wielkopolski_regional <- as.data.frame(number_species_order_Gorzow_Wielkopolski_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Gorzow_Wielkopolski_urban_regional <- bind_rows(number_species_order_Gorzow_Wielkopolski_urban,anti_join(number_species_order_Gorzow_Wielkopolski_regional,number_species_order_Gorzow_Wielkopolski_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Gorzow_Wielkopolski_urban_regional <- number_species_order_Gorzow_Wielkopolski_urban_regional[order(number_species_order_Gorzow_Wielkopolski_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Gorzow_Wielkopolski_urban_regional$prop.prop_order_Gorzow_Wielkopolski_regional <- number_species_order_Gorzow_Wielkopolski_regional$prop.prop_order_Gorzow_Wielkopolski_regional

### Replace NA by ceros in the urban proportions
number_species_order_Gorzow_Wielkopolski_urban_regional[is.na(number_species_order_Gorzow_Wielkopolski_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Gorzow_Wielkopolski_urban_regional_2 <- bind_rows(number_species_order_Gorzow_Wielkopolski_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Gorzow_Wielkopolski_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Gorzow_Wielkopolski_urban_regional_2 <- number_species_order_Gorzow_Wielkopolski_urban_regional_2[order(number_species_order_Gorzow_Wielkopolski_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Gorzow_Wielkopolski_urban_regional_2[is.na(number_species_order_Gorzow_Wielkopolski_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Gorzow_Wielkopolski_urban_regional_3 <- data.frame(number_species_order_Gorzow_Wielkopolski_urban_regional_2[1],
                                                                        number_species_order_Gorzow_Wielkopolski_urban_regional_2[5],
                                                                        number_species_order_Gorzow_Wielkopolski_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Gorzow_Wielkopolski_urban_regional_3 <- setNames(number_species_order_Gorzow_Wielkopolski_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Gorzow_Wielkopolski_urban_regional_3$prop_urban <- as.numeric(number_species_order_Gorzow_Wielkopolski_urban_regional_3$prop_urban)
number_species_order_Gorzow_Wielkopolski_urban_regional_3$prop_regional <- as.numeric(number_species_order_Gorzow_Wielkopolski_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Gorzow_Wielkopolski_order_prop_vector <- (number_species_order_Gorzow_Wielkopolski_urban_regional_3$prop_urban - number_species_order_Gorzow_Wielkopolski_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Gorzow_Wielkopolski_order_prop_df <- cbind.data.frame(number_species_order_Gorzow_Wielkopolski_urban_regional_3[1],
                                                      Gorzow_Wielkopolski_order_prop_vector)

### Change column names
colnames(Gorzow_Wielkopolski_order_prop_df) <- c('Order', 'Gorzow_Wielkopolski')



### Gujranwala
### Extract species and orders for urban and regional assemblages
order_Gujranwala_urban <- filter(bird_taxonomy_regional, grepl(paste(Gujranwala_urban_filtering, collapse="|"), TipLabel))

order_Gujranwala_regional <- filter(bird_taxonomy_regional, grepl(paste(Gujranwala_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Gujranwala_urban <- order_Gujranwala_urban %>% count(IOCOrder)

number_species_order_Gujranwala_regional <- order_Gujranwala_regional %>% count(IOCOrder)

### Get proportions
prop_order_Gujranwala_urban <- prop.table(number_species_order_Gujranwala_urban$n)

prop_order_Gujranwala_regional <- prop.table(number_species_order_Gujranwala_regional$n)

### Add proportions
number_species_order_Gujranwala_urban$prop <- cbind.data.frame(number_species_order_Gujranwala_urban, prop_order_Gujranwala_urban)

number_species_order_Gujranwala_regional$prop <- cbind.data.frame(number_species_order_Gujranwala_regional, prop_order_Gujranwala_regional)

### Convert proportions into matrix
number_species_order_Gujranwala_urban <- as.matrix(number_species_order_Gujranwala_urban)
number_species_order_Gujranwala_regional <- as.matrix(number_species_order_Gujranwala_regional)

### Convert matrix into data frame
number_species_order_Gujranwala_urban <- as.data.frame(number_species_order_Gujranwala_urban)
number_species_order_Gujranwala_regional <- as.data.frame(number_species_order_Gujranwala_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Gujranwala_urban_regional <- bind_rows(number_species_order_Gujranwala_urban,anti_join(number_species_order_Gujranwala_regional,number_species_order_Gujranwala_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Gujranwala_urban_regional <- number_species_order_Gujranwala_urban_regional[order(number_species_order_Gujranwala_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Gujranwala_urban_regional$prop.prop_order_Gujranwala_regional <- number_species_order_Gujranwala_regional$prop.prop_order_Gujranwala_regional

### Replace NA by ceros in the urban proportions
number_species_order_Gujranwala_urban_regional[is.na(number_species_order_Gujranwala_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Gujranwala_urban_regional_2 <- bind_rows(number_species_order_Gujranwala_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Gujranwala_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Gujranwala_urban_regional_2 <- number_species_order_Gujranwala_urban_regional_2[order(number_species_order_Gujranwala_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Gujranwala_urban_regional_2[is.na(number_species_order_Gujranwala_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Gujranwala_urban_regional_3 <- data.frame(number_species_order_Gujranwala_urban_regional_2[1],
                                                               number_species_order_Gujranwala_urban_regional_2[5],
                                                               number_species_order_Gujranwala_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Gujranwala_urban_regional_3 <- setNames(number_species_order_Gujranwala_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Gujranwala_urban_regional_3$prop_urban <- as.numeric(number_species_order_Gujranwala_urban_regional_3$prop_urban)
number_species_order_Gujranwala_urban_regional_3$prop_regional <- as.numeric(number_species_order_Gujranwala_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Gujranwala_order_prop_vector <- (number_species_order_Gujranwala_urban_regional_3$prop_urban - number_species_order_Gujranwala_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Gujranwala_order_prop_df <- cbind.data.frame(number_species_order_Gujranwala_urban_regional_3[1],
                                             Gujranwala_order_prop_vector)

### Change column names
colnames(Gujranwala_order_prop_df) <- c('Order', 'Gujranwala')



### Gwangju
### Extract species and orders for urban and regional assemblages
order_Gwangju_urban <- filter(bird_taxonomy_regional, grepl(paste(Gwangju_urban_filtering, collapse="|"), TipLabel))

order_Gwangju_regional <- filter(bird_taxonomy_regional, grepl(paste(Gwangju_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Gwangju_urban <- order_Gwangju_urban %>% count(IOCOrder)

number_species_order_Gwangju_regional <- order_Gwangju_regional %>% count(IOCOrder)

### Get proportions
prop_order_Gwangju_urban <- prop.table(number_species_order_Gwangju_urban$n)

prop_order_Gwangju_regional <- prop.table(number_species_order_Gwangju_regional$n)

### Add proportions
number_species_order_Gwangju_urban$prop <- cbind.data.frame(number_species_order_Gwangju_urban, prop_order_Gwangju_urban)

number_species_order_Gwangju_regional$prop <- cbind.data.frame(number_species_order_Gwangju_regional, prop_order_Gwangju_regional)

### Convert proportions into matrix
number_species_order_Gwangju_urban <- as.matrix(number_species_order_Gwangju_urban)
number_species_order_Gwangju_regional <- as.matrix(number_species_order_Gwangju_regional)

### Convert matrix into data frame
number_species_order_Gwangju_urban <- as.data.frame(number_species_order_Gwangju_urban)
number_species_order_Gwangju_regional <- as.data.frame(number_species_order_Gwangju_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Gwangju_urban_regional <- bind_rows(number_species_order_Gwangju_urban,anti_join(number_species_order_Gwangju_regional,number_species_order_Gwangju_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Gwangju_urban_regional <- number_species_order_Gwangju_urban_regional[order(number_species_order_Gwangju_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Gwangju_urban_regional$prop.prop_order_Gwangju_regional <- number_species_order_Gwangju_regional$prop.prop_order_Gwangju_regional

### Replace NA by ceros in the urban proportions
number_species_order_Gwangju_urban_regional[is.na(number_species_order_Gwangju_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Gwangju_urban_regional_2 <- bind_rows(number_species_order_Gwangju_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Gwangju_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Gwangju_urban_regional_2 <- number_species_order_Gwangju_urban_regional_2[order(number_species_order_Gwangju_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Gwangju_urban_regional_2[is.na(number_species_order_Gwangju_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Gwangju_urban_regional_3 <- data.frame(number_species_order_Gwangju_urban_regional_2[1],
                                                            number_species_order_Gwangju_urban_regional_2[5],
                                                            number_species_order_Gwangju_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Gwangju_urban_regional_3 <- setNames(number_species_order_Gwangju_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Gwangju_urban_regional_3$prop_urban <- as.numeric(number_species_order_Gwangju_urban_regional_3$prop_urban)
number_species_order_Gwangju_urban_regional_3$prop_regional <- as.numeric(number_species_order_Gwangju_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Gwangju_order_prop_vector <- (number_species_order_Gwangju_urban_regional_3$prop_urban - number_species_order_Gwangju_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Gwangju_order_prop_df <- cbind.data.frame(number_species_order_Gwangju_urban_regional_3[1],
                                          Gwangju_order_prop_vector)

### Change column names
colnames(Gwangju_order_prop_df) <- c('Order', 'Gwangju')



### Hamina
### Extract species and orders for urban and regional assemblages
order_Hamina_urban <- filter(bird_taxonomy_regional, grepl(paste(Hamina_urban_filtering, collapse="|"), TipLabel))

order_Hamina_regional <- filter(bird_taxonomy_regional, grepl(paste(Hamina_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Hamina_urban <- order_Hamina_urban %>% count(IOCOrder)

number_species_order_Hamina_regional <- order_Hamina_regional %>% count(IOCOrder)

### Get proportions
prop_order_Hamina_urban <- prop.table(number_species_order_Hamina_urban$n)

prop_order_Hamina_regional <- prop.table(number_species_order_Hamina_regional$n)

### Add proportions
number_species_order_Hamina_urban$prop <- cbind.data.frame(number_species_order_Hamina_urban, prop_order_Hamina_urban)

number_species_order_Hamina_regional$prop <- cbind.data.frame(number_species_order_Hamina_regional, prop_order_Hamina_regional)

### Convert proportions into matrix
number_species_order_Hamina_urban <- as.matrix(number_species_order_Hamina_urban)
number_species_order_Hamina_regional <- as.matrix(number_species_order_Hamina_regional)

### Convert matrix into data frame
number_species_order_Hamina_urban <- as.data.frame(number_species_order_Hamina_urban)
number_species_order_Hamina_regional <- as.data.frame(number_species_order_Hamina_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Hamina_urban_regional <- bind_rows(number_species_order_Hamina_urban,anti_join(number_species_order_Hamina_regional,number_species_order_Hamina_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Hamina_urban_regional <- number_species_order_Hamina_urban_regional[order(number_species_order_Hamina_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Hamina_urban_regional$prop.prop_order_Hamina_regional <- number_species_order_Hamina_regional$prop.prop_order_Hamina_regional

### Replace NA by ceros in the urban proportions
number_species_order_Hamina_urban_regional[is.na(number_species_order_Hamina_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Hamina_urban_regional_2 <- bind_rows(number_species_order_Hamina_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Hamina_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Hamina_urban_regional_2 <- number_species_order_Hamina_urban_regional_2[order(number_species_order_Hamina_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Hamina_urban_regional_2[is.na(number_species_order_Hamina_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Hamina_urban_regional_3 <- data.frame(number_species_order_Hamina_urban_regional_2[1],
                                                           number_species_order_Hamina_urban_regional_2[5],
                                                           number_species_order_Hamina_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Hamina_urban_regional_3 <- setNames(number_species_order_Hamina_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Hamina_urban_regional_3$prop_urban <- as.numeric(number_species_order_Hamina_urban_regional_3$prop_urban)
number_species_order_Hamina_urban_regional_3$prop_regional <- as.numeric(number_species_order_Hamina_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Hamina_order_prop_vector <- (number_species_order_Hamina_urban_regional_3$prop_urban - number_species_order_Hamina_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Hamina_order_prop_df <- cbind.data.frame(number_species_order_Hamina_urban_regional_3[1],
                                         Hamina_order_prop_vector)

### Change column names
colnames(Hamina_order_prop_df) <- c('Order', 'Hamina')



### Helsinki
### Extract species and orders for urban and regional assemblages
order_Helsinki_urban <- filter(bird_taxonomy_regional, grepl(paste(Helsinki_urban_filtering, collapse="|"), TipLabel))

order_Helsinki_regional <- filter(bird_taxonomy_regional, grepl(paste(Helsinki_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Helsinki_urban <- order_Helsinki_urban %>% count(IOCOrder)

number_species_order_Helsinki_regional <- order_Helsinki_regional %>% count(IOCOrder)

### Get proportions
prop_order_Helsinki_urban <- prop.table(number_species_order_Helsinki_urban$n)

prop_order_Helsinki_regional <- prop.table(number_species_order_Helsinki_regional$n)

### Add proportions
number_species_order_Helsinki_urban$prop <- cbind.data.frame(number_species_order_Helsinki_urban, prop_order_Helsinki_urban)

number_species_order_Helsinki_regional$prop <- cbind.data.frame(number_species_order_Helsinki_regional, prop_order_Helsinki_regional)

### Convert proportions into matrix
number_species_order_Helsinki_urban <- as.matrix(number_species_order_Helsinki_urban)
number_species_order_Helsinki_regional <- as.matrix(number_species_order_Helsinki_regional)

### Convert matrix into data frame
number_species_order_Helsinki_urban <- as.data.frame(number_species_order_Helsinki_urban)
number_species_order_Helsinki_regional <- as.data.frame(number_species_order_Helsinki_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Helsinki_urban_regional <- bind_rows(number_species_order_Helsinki_urban,anti_join(number_species_order_Helsinki_regional,number_species_order_Helsinki_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Helsinki_urban_regional <- number_species_order_Helsinki_urban_regional[order(number_species_order_Helsinki_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Helsinki_urban_regional$prop.prop_order_Helsinki_regional <- number_species_order_Helsinki_regional$prop.prop_order_Helsinki_regional

### Replace NA by ceros in the urban proportions
number_species_order_Helsinki_urban_regional[is.na(number_species_order_Helsinki_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Helsinki_urban_regional_2 <- bind_rows(number_species_order_Helsinki_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Helsinki_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Helsinki_urban_regional_2 <- number_species_order_Helsinki_urban_regional_2[order(number_species_order_Helsinki_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Helsinki_urban_regional_2[is.na(number_species_order_Helsinki_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Helsinki_urban_regional_3 <- data.frame(number_species_order_Helsinki_urban_regional_2[1],
                                                             number_species_order_Helsinki_urban_regional_2[5],
                                                             number_species_order_Helsinki_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Helsinki_urban_regional_3 <- setNames(number_species_order_Helsinki_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Helsinki_urban_regional_3$prop_urban <- as.numeric(number_species_order_Helsinki_urban_regional_3$prop_urban)
number_species_order_Helsinki_urban_regional_3$prop_regional <- as.numeric(number_species_order_Helsinki_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Helsinki_order_prop_vector <- (number_species_order_Helsinki_urban_regional_3$prop_urban - number_species_order_Helsinki_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Helsinki_order_prop_df <- cbind.data.frame(number_species_order_Helsinki_urban_regional_3[1],
                                           Helsinki_order_prop_vector)

### Change column names
colnames(Helsinki_order_prop_df) <- c('Order', 'Helsinki')



### Inowroclaw
### Extract species and orders for urban and regional assemblages
order_Inowroclaw_urban <- filter(bird_taxonomy_regional, grepl(paste(Inowroclaw_urban_filtering, collapse="|"), TipLabel))

order_Inowroclaw_regional <- filter(bird_taxonomy_regional, grepl(paste(Inowroclaw_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Inowroclaw_urban <- order_Inowroclaw_urban %>% count(IOCOrder)

number_species_order_Inowroclaw_regional <- order_Inowroclaw_regional %>% count(IOCOrder)

### Get proportions
prop_order_Inowroclaw_urban <- prop.table(number_species_order_Inowroclaw_urban$n)

prop_order_Inowroclaw_regional <- prop.table(number_species_order_Inowroclaw_regional$n)

### Add proportions
number_species_order_Inowroclaw_urban$prop <- cbind.data.frame(number_species_order_Inowroclaw_urban, prop_order_Inowroclaw_urban)

number_species_order_Inowroclaw_regional$prop <- cbind.data.frame(number_species_order_Inowroclaw_regional, prop_order_Inowroclaw_regional)

### Convert proportions into matrix
number_species_order_Inowroclaw_urban <- as.matrix(number_species_order_Inowroclaw_urban)
number_species_order_Inowroclaw_regional <- as.matrix(number_species_order_Inowroclaw_regional)

### Convert matrix into data frame
number_species_order_Inowroclaw_urban <- as.data.frame(number_species_order_Inowroclaw_urban)
number_species_order_Inowroclaw_regional <- as.data.frame(number_species_order_Inowroclaw_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Inowroclaw_urban_regional <- bind_rows(number_species_order_Inowroclaw_urban,anti_join(number_species_order_Inowroclaw_regional,number_species_order_Inowroclaw_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Inowroclaw_urban_regional <- number_species_order_Inowroclaw_urban_regional[order(number_species_order_Inowroclaw_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Inowroclaw_urban_regional$prop.prop_order_Inowroclaw_regional <- number_species_order_Inowroclaw_regional$prop.prop_order_Inowroclaw_regional

### Replace NA by ceros in the urban proportions
number_species_order_Inowroclaw_urban_regional[is.na(number_species_order_Inowroclaw_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Inowroclaw_urban_regional_2 <- bind_rows(number_species_order_Inowroclaw_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Inowroclaw_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Inowroclaw_urban_regional_2 <- number_species_order_Inowroclaw_urban_regional_2[order(number_species_order_Inowroclaw_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Inowroclaw_urban_regional_2[is.na(number_species_order_Inowroclaw_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Inowroclaw_urban_regional_3 <- data.frame(number_species_order_Inowroclaw_urban_regional_2[1],
                                                               number_species_order_Inowroclaw_urban_regional_2[5],
                                                               number_species_order_Inowroclaw_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Inowroclaw_urban_regional_3 <- setNames(number_species_order_Inowroclaw_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Inowroclaw_urban_regional_3$prop_urban <- as.numeric(number_species_order_Inowroclaw_urban_regional_3$prop_urban)
number_species_order_Inowroclaw_urban_regional_3$prop_regional <- as.numeric(number_species_order_Inowroclaw_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Inowroclaw_order_prop_vector <- (number_species_order_Inowroclaw_urban_regional_3$prop_urban - number_species_order_Inowroclaw_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Inowroclaw_order_prop_df <- cbind.data.frame(number_species_order_Inowroclaw_urban_regional_3[1],
                                             Inowroclaw_order_prop_vector)

### Change column names
colnames(Inowroclaw_order_prop_df) <- c('Order', 'Inowroclaw')



### Joensuu
### Extract species and orders for urban and regional assemblages
order_Joensuu_urban <- filter(bird_taxonomy_regional, grepl(paste(Joensuu_urban_filtering, collapse="|"), TipLabel))

order_Joensuu_regional <- filter(bird_taxonomy_regional, grepl(paste(Joensuu_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Joensuu_urban <- order_Joensuu_urban %>% count(IOCOrder)

number_species_order_Joensuu_regional <- order_Joensuu_regional %>% count(IOCOrder)

### Get proportions
prop_order_Joensuu_urban <- prop.table(number_species_order_Joensuu_urban$n)

prop_order_Joensuu_regional <- prop.table(number_species_order_Joensuu_regional$n)

### Add proportions
number_species_order_Joensuu_urban$prop <- cbind.data.frame(number_species_order_Joensuu_urban, prop_order_Joensuu_urban)

number_species_order_Joensuu_regional$prop <- cbind.data.frame(number_species_order_Joensuu_regional, prop_order_Joensuu_regional)

### Convert proportions into matrix
number_species_order_Joensuu_urban <- as.matrix(number_species_order_Joensuu_urban)
number_species_order_Joensuu_regional <- as.matrix(number_species_order_Joensuu_regional)

### Convert matrix into data frame
number_species_order_Joensuu_urban <- as.data.frame(number_species_order_Joensuu_urban)
number_species_order_Joensuu_regional <- as.data.frame(number_species_order_Joensuu_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Joensuu_urban_regional <- bind_rows(number_species_order_Joensuu_urban,anti_join(number_species_order_Joensuu_regional,number_species_order_Joensuu_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Joensuu_urban_regional <- number_species_order_Joensuu_urban_regional[order(number_species_order_Joensuu_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Joensuu_urban_regional$prop.prop_order_Joensuu_regional <- number_species_order_Joensuu_regional$prop.prop_order_Joensuu_regional

### Replace NA by ceros in the urban proportions
number_species_order_Joensuu_urban_regional[is.na(number_species_order_Joensuu_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Joensuu_urban_regional_2 <- bind_rows(number_species_order_Joensuu_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Joensuu_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Joensuu_urban_regional_2 <- number_species_order_Joensuu_urban_regional_2[order(number_species_order_Joensuu_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Joensuu_urban_regional_2[is.na(number_species_order_Joensuu_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Joensuu_urban_regional_3 <- data.frame(number_species_order_Joensuu_urban_regional_2[1],
                                                            number_species_order_Joensuu_urban_regional_2[5],
                                                            number_species_order_Joensuu_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Joensuu_urban_regional_3 <- setNames(number_species_order_Joensuu_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Joensuu_urban_regional_3$prop_urban <- as.numeric(number_species_order_Joensuu_urban_regional_3$prop_urban)
number_species_order_Joensuu_urban_regional_3$prop_regional <- as.numeric(number_species_order_Joensuu_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Joensuu_order_prop_vector <- (number_species_order_Joensuu_urban_regional_3$prop_urban - number_species_order_Joensuu_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Joensuu_order_prop_df <- cbind.data.frame(number_species_order_Joensuu_urban_regional_3[1],
                                          Joensuu_order_prop_vector)

### Change column names
colnames(Joensuu_order_prop_df) <- c('Order', 'Joensuu')



### Jyvaskyla
### Extract species and orders for urban and regional assemblages
order_Jyvaskyla_urban <- filter(bird_taxonomy_regional, grepl(paste(Jyvaskyla_urban_filtering, collapse="|"), TipLabel))

order_Jyvaskyla_regional <- filter(bird_taxonomy_regional, grepl(paste(Jyvaskyla_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Jyvaskyla_urban <- order_Jyvaskyla_urban %>% count(IOCOrder)

number_species_order_Jyvaskyla_regional <- order_Jyvaskyla_regional %>% count(IOCOrder)

### Get proportions
prop_order_Jyvaskyla_urban <- prop.table(number_species_order_Jyvaskyla_urban$n)

prop_order_Jyvaskyla_regional <- prop.table(number_species_order_Jyvaskyla_regional$n)

### Add proportions
number_species_order_Jyvaskyla_urban$prop <- cbind.data.frame(number_species_order_Jyvaskyla_urban, prop_order_Jyvaskyla_urban)

number_species_order_Jyvaskyla_regional$prop <- cbind.data.frame(number_species_order_Jyvaskyla_regional, prop_order_Jyvaskyla_regional)

### Convert proportions into matrix
number_species_order_Jyvaskyla_urban <- as.matrix(number_species_order_Jyvaskyla_urban)
number_species_order_Jyvaskyla_regional <- as.matrix(number_species_order_Jyvaskyla_regional)

### Convert matrix into data frame
number_species_order_Jyvaskyla_urban <- as.data.frame(number_species_order_Jyvaskyla_urban)
number_species_order_Jyvaskyla_regional <- as.data.frame(number_species_order_Jyvaskyla_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Jyvaskyla_urban_regional <- bind_rows(number_species_order_Jyvaskyla_urban,anti_join(number_species_order_Jyvaskyla_regional,number_species_order_Jyvaskyla_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Jyvaskyla_urban_regional <- number_species_order_Jyvaskyla_urban_regional[order(number_species_order_Jyvaskyla_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Jyvaskyla_urban_regional$prop.prop_order_Jyvaskyla_regional <- number_species_order_Jyvaskyla_regional$prop.prop_order_Jyvaskyla_regional

### Replace NA by ceros in the urban proportions
number_species_order_Jyvaskyla_urban_regional[is.na(number_species_order_Jyvaskyla_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Jyvaskyla_urban_regional_2 <- bind_rows(number_species_order_Jyvaskyla_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Jyvaskyla_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Jyvaskyla_urban_regional_2 <- number_species_order_Jyvaskyla_urban_regional_2[order(number_species_order_Jyvaskyla_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Jyvaskyla_urban_regional_2[is.na(number_species_order_Jyvaskyla_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Jyvaskyla_urban_regional_3 <- data.frame(number_species_order_Jyvaskyla_urban_regional_2[1],
                                                              number_species_order_Jyvaskyla_urban_regional_2[5],
                                                              number_species_order_Jyvaskyla_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Jyvaskyla_urban_regional_3 <- setNames(number_species_order_Jyvaskyla_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Jyvaskyla_urban_regional_3$prop_urban <- as.numeric(number_species_order_Jyvaskyla_urban_regional_3$prop_urban)
number_species_order_Jyvaskyla_urban_regional_3$prop_regional <- as.numeric(number_species_order_Jyvaskyla_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Jyvaskyla_order_prop_vector <- (number_species_order_Jyvaskyla_urban_regional_3$prop_urban - number_species_order_Jyvaskyla_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Jyvaskyla_order_prop_df <- cbind.data.frame(number_species_order_Jyvaskyla_urban_regional_3[1],
                                            Jyvaskyla_order_prop_vector)

### Change column names
colnames(Jyvaskyla_order_prop_df) <- c('Order', 'Jyvaskyla')



### Kajaani
### Extract species and orders for urban and regional assemblages
order_Kajaani_urban <- filter(bird_taxonomy_regional, grepl(paste(Kajaani_urban_filtering, collapse="|"), TipLabel))

order_Kajaani_regional <- filter(bird_taxonomy_regional, grepl(paste(Kajaani_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Kajaani_urban <- order_Kajaani_urban %>% count(IOCOrder)

number_species_order_Kajaani_regional <- order_Kajaani_regional %>% count(IOCOrder)

### Get proportions
prop_order_Kajaani_urban <- prop.table(number_species_order_Kajaani_urban$n)

prop_order_Kajaani_regional <- prop.table(number_species_order_Kajaani_regional$n)

### Add proportions
number_species_order_Kajaani_urban$prop <- cbind.data.frame(number_species_order_Kajaani_urban, prop_order_Kajaani_urban)

number_species_order_Kajaani_regional$prop <- cbind.data.frame(number_species_order_Kajaani_regional, prop_order_Kajaani_regional)

### Convert proportions into matrix
number_species_order_Kajaani_urban <- as.matrix(number_species_order_Kajaani_urban)
number_species_order_Kajaani_regional <- as.matrix(number_species_order_Kajaani_regional)

### Convert matrix into data frame
number_species_order_Kajaani_urban <- as.data.frame(number_species_order_Kajaani_urban)
number_species_order_Kajaani_regional <- as.data.frame(number_species_order_Kajaani_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Kajaani_urban_regional <- bind_rows(number_species_order_Kajaani_urban,anti_join(number_species_order_Kajaani_regional,number_species_order_Kajaani_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kajaani_urban_regional <- number_species_order_Kajaani_urban_regional[order(number_species_order_Kajaani_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Kajaani_urban_regional$prop.prop_order_Kajaani_regional <- number_species_order_Kajaani_regional$prop.prop_order_Kajaani_regional

### Replace NA by ceros in the urban proportions
number_species_order_Kajaani_urban_regional[is.na(number_species_order_Kajaani_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Kajaani_urban_regional_2 <- bind_rows(number_species_order_Kajaani_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Kajaani_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kajaani_urban_regional_2 <- number_species_order_Kajaani_urban_regional_2[order(number_species_order_Kajaani_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Kajaani_urban_regional_2[is.na(number_species_order_Kajaani_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Kajaani_urban_regional_3 <- data.frame(number_species_order_Kajaani_urban_regional_2[1],
                                                            number_species_order_Kajaani_urban_regional_2[5],
                                                            number_species_order_Kajaani_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Kajaani_urban_regional_3 <- setNames(number_species_order_Kajaani_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Kajaani_urban_regional_3$prop_urban <- as.numeric(number_species_order_Kajaani_urban_regional_3$prop_urban)
number_species_order_Kajaani_urban_regional_3$prop_regional <- as.numeric(number_species_order_Kajaani_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Kajaani_order_prop_vector <- (number_species_order_Kajaani_urban_regional_3$prop_urban - number_species_order_Kajaani_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Kajaani_order_prop_df <- cbind.data.frame(number_species_order_Kajaani_urban_regional_3[1],
                                          Kajaani_order_prop_vector)

### Change column names
colnames(Kajaani_order_prop_df) <- c('Order', 'Kajaani')



### Kathmandu
### Extract species and orders for urban and regional assemblages
order_Kathmandu_urban <- filter(bird_taxonomy_regional, grepl(paste(Kathmandu_urban_filtering, collapse="|"), TipLabel))

order_Kathmandu_regional <- filter(bird_taxonomy_regional, grepl(paste(Kathmandu_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Kathmandu_urban <- order_Kathmandu_urban %>% count(IOCOrder)

number_species_order_Kathmandu_regional <- order_Kathmandu_regional %>% count(IOCOrder)

### Get proportions
prop_order_Kathmandu_urban <- prop.table(number_species_order_Kathmandu_urban$n)

prop_order_Kathmandu_regional <- prop.table(number_species_order_Kathmandu_regional$n)

### Add proportions
number_species_order_Kathmandu_urban$prop <- cbind.data.frame(number_species_order_Kathmandu_urban, prop_order_Kathmandu_urban)

number_species_order_Kathmandu_regional$prop <- cbind.data.frame(number_species_order_Kathmandu_regional, prop_order_Kathmandu_regional)

### Convert proportions into matrix
number_species_order_Kathmandu_urban <- as.matrix(number_species_order_Kathmandu_urban)
number_species_order_Kathmandu_regional <- as.matrix(number_species_order_Kathmandu_regional)

### Convert matrix into data frame
number_species_order_Kathmandu_urban <- as.data.frame(number_species_order_Kathmandu_urban)
number_species_order_Kathmandu_regional <- as.data.frame(number_species_order_Kathmandu_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Kathmandu_urban_regional <- bind_rows(number_species_order_Kathmandu_urban,anti_join(number_species_order_Kathmandu_regional,number_species_order_Kathmandu_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kathmandu_urban_regional <- number_species_order_Kathmandu_urban_regional[order(number_species_order_Kathmandu_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Kathmandu_urban_regional$prop.prop_order_Kathmandu_regional <- number_species_order_Kathmandu_regional$prop.prop_order_Kathmandu_regional

### Replace NA by ceros in the urban proportions
number_species_order_Kathmandu_urban_regional[is.na(number_species_order_Kathmandu_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Kathmandu_urban_regional_2 <- bind_rows(number_species_order_Kathmandu_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Kathmandu_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kathmandu_urban_regional_2 <- number_species_order_Kathmandu_urban_regional_2[order(number_species_order_Kathmandu_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Kathmandu_urban_regional_2[is.na(number_species_order_Kathmandu_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Kathmandu_urban_regional_3 <- data.frame(number_species_order_Kathmandu_urban_regional_2[1],
                                                              number_species_order_Kathmandu_urban_regional_2[5],
                                                              number_species_order_Kathmandu_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Kathmandu_urban_regional_3 <- setNames(number_species_order_Kathmandu_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Kathmandu_urban_regional_3$prop_urban <- as.numeric(number_species_order_Kathmandu_urban_regional_3$prop_urban)
number_species_order_Kathmandu_urban_regional_3$prop_regional <- as.numeric(number_species_order_Kathmandu_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Kathmandu_order_prop_vector <- (number_species_order_Kathmandu_urban_regional_3$prop_urban - number_species_order_Kathmandu_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Kathmandu_order_prop_df <- cbind.data.frame(number_species_order_Kathmandu_urban_regional_3[1],
                                            Kathmandu_order_prop_vector)

### Change column names
colnames(Kathmandu_order_prop_df) <- c('Order', 'Kathmandu')



### Kauhava
### Extract species and orders for urban and regional assemblages
order_Kauhava_urban <- filter(bird_taxonomy_regional, grepl(paste(Kauhava_urban_filtering, collapse="|"), TipLabel))

order_Kauhava_regional <- filter(bird_taxonomy_regional, grepl(paste(Kauhava_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Kauhava_urban <- order_Kauhava_urban %>% count(IOCOrder)

number_species_order_Kauhava_regional <- order_Kauhava_regional %>% count(IOCOrder)

### Get proportions
prop_order_Kauhava_urban <- prop.table(number_species_order_Kauhava_urban$n)

prop_order_Kauhava_regional <- prop.table(number_species_order_Kauhava_regional$n)

### Add proportions
number_species_order_Kauhava_urban$prop <- cbind.data.frame(number_species_order_Kauhava_urban, prop_order_Kauhava_urban)

number_species_order_Kauhava_regional$prop <- cbind.data.frame(number_species_order_Kauhava_regional, prop_order_Kauhava_regional)

### Convert proportions into matrix
number_species_order_Kauhava_urban <- as.matrix(number_species_order_Kauhava_urban)
number_species_order_Kauhava_regional <- as.matrix(number_species_order_Kauhava_regional)

### Convert matrix into data frame
number_species_order_Kauhava_urban <- as.data.frame(number_species_order_Kauhava_urban)
number_species_order_Kauhava_regional <- as.data.frame(number_species_order_Kauhava_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Kauhava_urban_regional <- bind_rows(number_species_order_Kauhava_urban,anti_join(number_species_order_Kauhava_regional,number_species_order_Kauhava_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kauhava_urban_regional <- number_species_order_Kauhava_urban_regional[order(number_species_order_Kauhava_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Kauhava_urban_regional$prop.prop_order_Kauhava_regional <- number_species_order_Kauhava_regional$prop.prop_order_Kauhava_regional

### Replace NA by ceros in the urban proportions
number_species_order_Kauhava_urban_regional[is.na(number_species_order_Kauhava_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Kauhava_urban_regional_2 <- bind_rows(number_species_order_Kauhava_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Kauhava_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kauhava_urban_regional_2 <- number_species_order_Kauhava_urban_regional_2[order(number_species_order_Kauhava_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Kauhava_urban_regional_2[is.na(number_species_order_Kauhava_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Kauhava_urban_regional_3 <- data.frame(number_species_order_Kauhava_urban_regional_2[1],
                                                            number_species_order_Kauhava_urban_regional_2[5],
                                                            number_species_order_Kauhava_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Kauhava_urban_regional_3 <- setNames(number_species_order_Kauhava_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Kauhava_urban_regional_3$prop_urban <- as.numeric(number_species_order_Kauhava_urban_regional_3$prop_urban)
number_species_order_Kauhava_urban_regional_3$prop_regional <- as.numeric(number_species_order_Kauhava_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Kauhava_order_prop_vector <- (number_species_order_Kauhava_urban_regional_3$prop_urban - number_species_order_Kauhava_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Kauhava_order_prop_df <- cbind.data.frame(number_species_order_Kauhava_urban_regional_3[1],
                                          Kauhava_order_prop_vector)

### Change column names
colnames(Kauhava_order_prop_df) <- c('Order', 'Kauhava')



### Kemi
### Extract species and orders for urban and regional assemblages
order_Kemi_urban <- filter(bird_taxonomy_regional, grepl(paste(Kemi_urban_filtering, collapse="|"), TipLabel))

order_Kemi_regional <- filter(bird_taxonomy_regional, grepl(paste(Kemi_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Kemi_urban <- order_Kemi_urban %>% count(IOCOrder)

number_species_order_Kemi_regional <- order_Kemi_regional %>% count(IOCOrder)

### Get proportions
prop_order_Kemi_urban <- prop.table(number_species_order_Kemi_urban$n)

prop_order_Kemi_regional <- prop.table(number_species_order_Kemi_regional$n)

### Add proportions
number_species_order_Kemi_urban$prop <- cbind.data.frame(number_species_order_Kemi_urban, prop_order_Kemi_urban)

number_species_order_Kemi_regional$prop <- cbind.data.frame(number_species_order_Kemi_regional, prop_order_Kemi_regional)

### Convert proportions into matrix
number_species_order_Kemi_urban <- as.matrix(number_species_order_Kemi_urban)
number_species_order_Kemi_regional <- as.matrix(number_species_order_Kemi_regional)

### Convert matrix into data frame
number_species_order_Kemi_urban <- as.data.frame(number_species_order_Kemi_urban)
number_species_order_Kemi_regional <- as.data.frame(number_species_order_Kemi_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Kemi_urban_regional <- bind_rows(number_species_order_Kemi_urban,anti_join(number_species_order_Kemi_regional,number_species_order_Kemi_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kemi_urban_regional <- number_species_order_Kemi_urban_regional[order(number_species_order_Kemi_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Kemi_urban_regional$prop.prop_order_Kemi_regional <- number_species_order_Kemi_regional$prop.prop_order_Kemi_regional

### Replace NA by ceros in the urban proportions
number_species_order_Kemi_urban_regional[is.na(number_species_order_Kemi_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Kemi_urban_regional_2 <- bind_rows(number_species_order_Kemi_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Kemi_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kemi_urban_regional_2 <- number_species_order_Kemi_urban_regional_2[order(number_species_order_Kemi_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Kemi_urban_regional_2[is.na(number_species_order_Kemi_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Kemi_urban_regional_3 <- data.frame(number_species_order_Kemi_urban_regional_2[1],
                                                         number_species_order_Kemi_urban_regional_2[5],
                                                         number_species_order_Kemi_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Kemi_urban_regional_3 <- setNames(number_species_order_Kemi_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Kemi_urban_regional_3$prop_urban <- as.numeric(number_species_order_Kemi_urban_regional_3$prop_urban)
number_species_order_Kemi_urban_regional_3$prop_regional <- as.numeric(number_species_order_Kemi_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Kemi_order_prop_vector <- (number_species_order_Kemi_urban_regional_3$prop_urban - number_species_order_Kemi_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Kemi_order_prop_df <- cbind.data.frame(number_species_order_Kemi_urban_regional_3[1],
                                       Kemi_order_prop_vector)

### Change column names
colnames(Kemi_order_prop_df) <- c('Order', 'Kemi')



### Kemijarvi
### Extract species and orders for urban and regional assemblages
order_Kemijarvi_urban <- filter(bird_taxonomy_regional, grepl(paste(Kemijarvi_urban_filtering, collapse="|"), TipLabel))

order_Kemijarvi_regional <- filter(bird_taxonomy_regional, grepl(paste(Kemijarvi_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Kemijarvi_urban <- order_Kemijarvi_urban %>% count(IOCOrder)

number_species_order_Kemijarvi_regional <- order_Kemijarvi_regional %>% count(IOCOrder)

### Get proportions
prop_order_Kemijarvi_urban <- prop.table(number_species_order_Kemijarvi_urban$n)

prop_order_Kemijarvi_regional <- prop.table(number_species_order_Kemijarvi_regional$n)

### Add proportions
number_species_order_Kemijarvi_urban$prop <- cbind.data.frame(number_species_order_Kemijarvi_urban, prop_order_Kemijarvi_urban)

number_species_order_Kemijarvi_regional$prop <- cbind.data.frame(number_species_order_Kemijarvi_regional, prop_order_Kemijarvi_regional)

### Convert proportions into matrix
number_species_order_Kemijarvi_urban <- as.matrix(number_species_order_Kemijarvi_urban)
number_species_order_Kemijarvi_regional <- as.matrix(number_species_order_Kemijarvi_regional)

### Convert matrix into data frame
number_species_order_Kemijarvi_urban <- as.data.frame(number_species_order_Kemijarvi_urban)
number_species_order_Kemijarvi_regional <- as.data.frame(number_species_order_Kemijarvi_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Kemijarvi_urban_regional <- bind_rows(number_species_order_Kemijarvi_urban,anti_join(number_species_order_Kemijarvi_regional,number_species_order_Kemijarvi_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kemijarvi_urban_regional <- number_species_order_Kemijarvi_urban_regional[order(number_species_order_Kemijarvi_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Kemijarvi_urban_regional$prop.prop_order_Kemijarvi_regional <- number_species_order_Kemijarvi_regional$prop.prop_order_Kemijarvi_regional

### Replace NA by ceros in the urban proportions
number_species_order_Kemijarvi_urban_regional[is.na(number_species_order_Kemijarvi_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Kemijarvi_urban_regional_2 <- bind_rows(number_species_order_Kemijarvi_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Kemijarvi_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kemijarvi_urban_regional_2 <- number_species_order_Kemijarvi_urban_regional_2[order(number_species_order_Kemijarvi_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Kemijarvi_urban_regional_2[is.na(number_species_order_Kemijarvi_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Kemijarvi_urban_regional_3 <- data.frame(number_species_order_Kemijarvi_urban_regional_2[1],
                                                              number_species_order_Kemijarvi_urban_regional_2[5],
                                                              number_species_order_Kemijarvi_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Kemijarvi_urban_regional_3 <- setNames(number_species_order_Kemijarvi_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Kemijarvi_urban_regional_3$prop_urban <- as.numeric(number_species_order_Kemijarvi_urban_regional_3$prop_urban)
number_species_order_Kemijarvi_urban_regional_3$prop_regional <- as.numeric(number_species_order_Kemijarvi_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Kemijarvi_order_prop_vector <- (number_species_order_Kemijarvi_urban_regional_3$prop_urban - number_species_order_Kemijarvi_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Kemijarvi_order_prop_df <- cbind.data.frame(number_species_order_Kemijarvi_urban_regional_3[1],
                                            Kemijarvi_order_prop_vector)

### Change column names
colnames(Kemijarvi_order_prop_df) <- c('Order', 'Kemijarvi')



### Kotka
### Extract species and orders for urban and regional assemblages
order_Kotka_urban <- filter(bird_taxonomy_regional, grepl(paste(Kotka_urban_filtering, collapse="|"), TipLabel))

order_Kotka_regional <- filter(bird_taxonomy_regional, grepl(paste(Kotka_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Kotka_urban <- order_Kotka_urban %>% count(IOCOrder)

number_species_order_Kotka_regional <- order_Kotka_regional %>% count(IOCOrder)

### Get proportions
prop_order_Kotka_urban <- prop.table(number_species_order_Kotka_urban$n)

prop_order_Kotka_regional <- prop.table(number_species_order_Kotka_regional$n)

### Add proportions
number_species_order_Kotka_urban$prop <- cbind.data.frame(number_species_order_Kotka_urban, prop_order_Kotka_urban)

number_species_order_Kotka_regional$prop <- cbind.data.frame(number_species_order_Kotka_regional, prop_order_Kotka_regional)

### Convert proportions into matrix
number_species_order_Kotka_urban <- as.matrix(number_species_order_Kotka_urban)
number_species_order_Kotka_regional <- as.matrix(number_species_order_Kotka_regional)

### Convert matrix into data frame
number_species_order_Kotka_urban <- as.data.frame(number_species_order_Kotka_urban)
number_species_order_Kotka_regional <- as.data.frame(number_species_order_Kotka_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Kotka_urban_regional <- bind_rows(number_species_order_Kotka_urban,anti_join(number_species_order_Kotka_regional,number_species_order_Kotka_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kotka_urban_regional <- number_species_order_Kotka_urban_regional[order(number_species_order_Kotka_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Kotka_urban_regional$prop.prop_order_Kotka_regional <- number_species_order_Kotka_regional$prop.prop_order_Kotka_regional

### Replace NA by ceros in the urban proportions
number_species_order_Kotka_urban_regional[is.na(number_species_order_Kotka_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Kotka_urban_regional_2 <- bind_rows(number_species_order_Kotka_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Kotka_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kotka_urban_regional_2 <- number_species_order_Kotka_urban_regional_2[order(number_species_order_Kotka_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Kotka_urban_regional_2[is.na(number_species_order_Kotka_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Kotka_urban_regional_3 <- data.frame(number_species_order_Kotka_urban_regional_2[1],
                                                          number_species_order_Kotka_urban_regional_2[5],
                                                          number_species_order_Kotka_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Kotka_urban_regional_3 <- setNames(number_species_order_Kotka_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Kotka_urban_regional_3$prop_urban <- as.numeric(number_species_order_Kotka_urban_regional_3$prop_urban)
number_species_order_Kotka_urban_regional_3$prop_regional <- as.numeric(number_species_order_Kotka_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Kotka_order_prop_vector <- (number_species_order_Kotka_urban_regional_3$prop_urban - number_species_order_Kotka_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Kotka_order_prop_df <- cbind.data.frame(number_species_order_Kotka_urban_regional_3[1],
                                        Kotka_order_prop_vector)

### Change column names
colnames(Kotka_order_prop_df) <- c('Order', 'Kotka')



### Kouvola
### Extract species and orders for urban and regional assemblages
order_Kouvola_urban <- filter(bird_taxonomy_regional, grepl(paste(Kouvola_urban_filtering, collapse="|"), TipLabel))

order_Kouvola_regional <- filter(bird_taxonomy_regional, grepl(paste(Kouvola_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Kouvola_urban <- order_Kouvola_urban %>% count(IOCOrder)

number_species_order_Kouvola_regional <- order_Kouvola_regional %>% count(IOCOrder)

### Get proportions
prop_order_Kouvola_urban <- prop.table(number_species_order_Kouvola_urban$n)

prop_order_Kouvola_regional <- prop.table(number_species_order_Kouvola_regional$n)

### Add proportions
number_species_order_Kouvola_urban$prop <- cbind.data.frame(number_species_order_Kouvola_urban, prop_order_Kouvola_urban)

number_species_order_Kouvola_regional$prop <- cbind.data.frame(number_species_order_Kouvola_regional, prop_order_Kouvola_regional)

### Convert proportions into matrix
number_species_order_Kouvola_urban <- as.matrix(number_species_order_Kouvola_urban)
number_species_order_Kouvola_regional <- as.matrix(number_species_order_Kouvola_regional)

### Convert matrix into data frame
number_species_order_Kouvola_urban <- as.data.frame(number_species_order_Kouvola_urban)
number_species_order_Kouvola_regional <- as.data.frame(number_species_order_Kouvola_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Kouvola_urban_regional <- bind_rows(number_species_order_Kouvola_urban,anti_join(number_species_order_Kouvola_regional,number_species_order_Kouvola_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kouvola_urban_regional <- number_species_order_Kouvola_urban_regional[order(number_species_order_Kouvola_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Kouvola_urban_regional$prop.prop_order_Kouvola_regional <- number_species_order_Kouvola_regional$prop.prop_order_Kouvola_regional

### Replace NA by ceros in the urban proportions
number_species_order_Kouvola_urban_regional[is.na(number_species_order_Kouvola_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Kouvola_urban_regional_2 <- bind_rows(number_species_order_Kouvola_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Kouvola_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Kouvola_urban_regional_2 <- number_species_order_Kouvola_urban_regional_2[order(number_species_order_Kouvola_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Kouvola_urban_regional_2[is.na(number_species_order_Kouvola_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Kouvola_urban_regional_3 <- data.frame(number_species_order_Kouvola_urban_regional_2[1],
                                                            number_species_order_Kouvola_urban_regional_2[5],
                                                            number_species_order_Kouvola_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Kouvola_urban_regional_3 <- setNames(number_species_order_Kouvola_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Kouvola_urban_regional_3$prop_urban <- as.numeric(number_species_order_Kouvola_urban_regional_3$prop_urban)
number_species_order_Kouvola_urban_regional_3$prop_regional <- as.numeric(number_species_order_Kouvola_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Kouvola_order_prop_vector <- (number_species_order_Kouvola_urban_regional_3$prop_urban - number_species_order_Kouvola_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Kouvola_order_prop_df <- cbind.data.frame(number_species_order_Kouvola_urban_regional_3[1],
                                          Kouvola_order_prop_vector)

### Change column names
colnames(Kouvola_order_prop_df) <- c('Order', 'Kouvola')



### Krakow
### Extract species and orders for urban and regional assemblages
order_Krakow_urban <- filter(bird_taxonomy_regional, grepl(paste(Krakow_urban_filtering, collapse="|"), TipLabel))

order_Krakow_regional <- filter(bird_taxonomy_regional, grepl(paste(Krakow_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Krakow_urban <- order_Krakow_urban %>% count(IOCOrder)

number_species_order_Krakow_regional <- order_Krakow_regional %>% count(IOCOrder)

### Get proportions
prop_order_Krakow_urban <- prop.table(number_species_order_Krakow_urban$n)

prop_order_Krakow_regional <- prop.table(number_species_order_Krakow_regional$n)

### Add proportions
number_species_order_Krakow_urban$prop <- cbind.data.frame(number_species_order_Krakow_urban, prop_order_Krakow_urban)

number_species_order_Krakow_regional$prop <- cbind.data.frame(number_species_order_Krakow_regional, prop_order_Krakow_regional)

### Convert proportions into matrix
number_species_order_Krakow_urban <- as.matrix(number_species_order_Krakow_urban)
number_species_order_Krakow_regional <- as.matrix(number_species_order_Krakow_regional)

### Convert matrix into data frame
number_species_order_Krakow_urban <- as.data.frame(number_species_order_Krakow_urban)
number_species_order_Krakow_regional <- as.data.frame(number_species_order_Krakow_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Krakow_urban_regional <- bind_rows(number_species_order_Krakow_urban,anti_join(number_species_order_Krakow_regional,number_species_order_Krakow_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Krakow_urban_regional <- number_species_order_Krakow_urban_regional[order(number_species_order_Krakow_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Krakow_urban_regional$prop.prop_order_Krakow_regional <- number_species_order_Krakow_regional$prop.prop_order_Krakow_regional

### Replace NA by ceros in the urban proportions
number_species_order_Krakow_urban_regional[is.na(number_species_order_Krakow_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Krakow_urban_regional_2 <- bind_rows(number_species_order_Krakow_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Krakow_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Krakow_urban_regional_2 <- number_species_order_Krakow_urban_regional_2[order(number_species_order_Krakow_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Krakow_urban_regional_2[is.na(number_species_order_Krakow_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Krakow_urban_regional_3 <- data.frame(number_species_order_Krakow_urban_regional_2[1],
                                                           number_species_order_Krakow_urban_regional_2[5],
                                                           number_species_order_Krakow_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Krakow_urban_regional_3 <- setNames(number_species_order_Krakow_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Krakow_urban_regional_3$prop_urban <- as.numeric(number_species_order_Krakow_urban_regional_3$prop_urban)
number_species_order_Krakow_urban_regional_3$prop_regional <- as.numeric(number_species_order_Krakow_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Krakow_order_prop_vector <- (number_species_order_Krakow_urban_regional_3$prop_urban - number_species_order_Krakow_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Krakow_order_prop_df <- cbind.data.frame(number_species_order_Krakow_urban_regional_3[1],
                                         Krakow_order_prop_vector)

### Change column names
colnames(Krakow_order_prop_df) <- c('Order', 'Krakow')



### Laitila
### Extract species and orders for urban and regional assemblages
order_Laitila_urban <- filter(bird_taxonomy_regional, grepl(paste(Laitila_urban_filtering, collapse="|"), TipLabel))

order_Laitila_regional <- filter(bird_taxonomy_regional, grepl(paste(Laitila_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Laitila_urban <- order_Laitila_urban %>% count(IOCOrder)

number_species_order_Laitila_regional <- order_Laitila_regional %>% count(IOCOrder)

### Get proportions
prop_order_Laitila_urban <- prop.table(number_species_order_Laitila_urban$n)

prop_order_Laitila_regional <- prop.table(number_species_order_Laitila_regional$n)

### Add proportions
number_species_order_Laitila_urban$prop <- cbind.data.frame(number_species_order_Laitila_urban, prop_order_Laitila_urban)

number_species_order_Laitila_regional$prop <- cbind.data.frame(number_species_order_Laitila_regional, prop_order_Laitila_regional)

### Convert proportions into matrix
number_species_order_Laitila_urban <- as.matrix(number_species_order_Laitila_urban)
number_species_order_Laitila_regional <- as.matrix(number_species_order_Laitila_regional)

### Convert matrix into data frame
number_species_order_Laitila_urban <- as.data.frame(number_species_order_Laitila_urban)
number_species_order_Laitila_regional <- as.data.frame(number_species_order_Laitila_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Laitila_urban_regional <- bind_rows(number_species_order_Laitila_urban,anti_join(number_species_order_Laitila_regional,number_species_order_Laitila_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Laitila_urban_regional <- number_species_order_Laitila_urban_regional[order(number_species_order_Laitila_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Laitila_urban_regional$prop.prop_order_Laitila_regional <- number_species_order_Laitila_regional$prop.prop_order_Laitila_regional

### Replace NA by ceros in the urban proportions
number_species_order_Laitila_urban_regional[is.na(number_species_order_Laitila_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Laitila_urban_regional_2 <- bind_rows(number_species_order_Laitila_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Laitila_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Laitila_urban_regional_2 <- number_species_order_Laitila_urban_regional_2[order(number_species_order_Laitila_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Laitila_urban_regional_2[is.na(number_species_order_Laitila_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Laitila_urban_regional_3 <- data.frame(number_species_order_Laitila_urban_regional_2[1],
                                                            number_species_order_Laitila_urban_regional_2[5],
                                                            number_species_order_Laitila_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Laitila_urban_regional_3 <- setNames(number_species_order_Laitila_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Laitila_urban_regional_3$prop_urban <- as.numeric(number_species_order_Laitila_urban_regional_3$prop_urban)
number_species_order_Laitila_urban_regional_3$prop_regional <- as.numeric(number_species_order_Laitila_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Laitila_order_prop_vector <- (number_species_order_Laitila_urban_regional_3$prop_urban - number_species_order_Laitila_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Laitila_order_prop_df <- cbind.data.frame(number_species_order_Laitila_urban_regional_3[1],
                                          Laitila_order_prop_vector)

### Change column names
colnames(Laitila_order_prop_df) <- c('Order', 'Laitila')



### Lappeenranta
### Extract species and orders for urban and regional assemblages
order_Lappeenranta_urban <- filter(bird_taxonomy_regional, grepl(paste(Lappeenranta_urban_filtering, collapse="|"), TipLabel))

order_Lappeenranta_regional <- filter(bird_taxonomy_regional, grepl(paste(Lappeenranta_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Lappeenranta_urban <- order_Lappeenranta_urban %>% count(IOCOrder)

number_species_order_Lappeenranta_regional <- order_Lappeenranta_regional %>% count(IOCOrder)

### Get proportions
prop_order_Lappeenranta_urban <- prop.table(number_species_order_Lappeenranta_urban$n)

prop_order_Lappeenranta_regional <- prop.table(number_species_order_Lappeenranta_regional$n)

### Add proportions
number_species_order_Lappeenranta_urban$prop <- cbind.data.frame(number_species_order_Lappeenranta_urban, prop_order_Lappeenranta_urban)

number_species_order_Lappeenranta_regional$prop <- cbind.data.frame(number_species_order_Lappeenranta_regional, prop_order_Lappeenranta_regional)

### Convert proportions into matrix
number_species_order_Lappeenranta_urban <- as.matrix(number_species_order_Lappeenranta_urban)
number_species_order_Lappeenranta_regional <- as.matrix(number_species_order_Lappeenranta_regional)

### Convert matrix into data frame
number_species_order_Lappeenranta_urban <- as.data.frame(number_species_order_Lappeenranta_urban)
number_species_order_Lappeenranta_regional <- as.data.frame(number_species_order_Lappeenranta_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Lappeenranta_urban_regional <- bind_rows(number_species_order_Lappeenranta_urban,anti_join(number_species_order_Lappeenranta_regional,number_species_order_Lappeenranta_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Lappeenranta_urban_regional <- number_species_order_Lappeenranta_urban_regional[order(number_species_order_Lappeenranta_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Lappeenranta_urban_regional$prop.prop_order_Lappeenranta_regional <- number_species_order_Lappeenranta_regional$prop.prop_order_Lappeenranta_regional

### Replace NA by ceros in the urban proportions
number_species_order_Lappeenranta_urban_regional[is.na(number_species_order_Lappeenranta_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Lappeenranta_urban_regional_2 <- bind_rows(number_species_order_Lappeenranta_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Lappeenranta_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Lappeenranta_urban_regional_2 <- number_species_order_Lappeenranta_urban_regional_2[order(number_species_order_Lappeenranta_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Lappeenranta_urban_regional_2[is.na(number_species_order_Lappeenranta_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Lappeenranta_urban_regional_3 <- data.frame(number_species_order_Lappeenranta_urban_regional_2[1],
                                                                 number_species_order_Lappeenranta_urban_regional_2[5],
                                                                 number_species_order_Lappeenranta_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Lappeenranta_urban_regional_3 <- setNames(number_species_order_Lappeenranta_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Lappeenranta_urban_regional_3$prop_urban <- as.numeric(number_species_order_Lappeenranta_urban_regional_3$prop_urban)
number_species_order_Lappeenranta_urban_regional_3$prop_regional <- as.numeric(number_species_order_Lappeenranta_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Lappeenranta_order_prop_vector <- (number_species_order_Lappeenranta_urban_regional_3$prop_urban - number_species_order_Lappeenranta_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Lappeenranta_order_prop_df <- cbind.data.frame(number_species_order_Lappeenranta_urban_regional_3[1],
                                               Lappeenranta_order_prop_vector)

### Change column names
colnames(Lappeenranta_order_prop_df) <- c('Order', 'Lappeenranta')



### Lavras
### Extract species and orders for urban and regional assemblages
order_Lavras_urban <- filter(bird_taxonomy_regional, grepl(paste(Lavras_urban_filtering, collapse="|"), TipLabel))

order_Lavras_regional <- filter(bird_taxonomy_regional, grepl(paste(Lavras_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Lavras_urban <- order_Lavras_urban %>% count(IOCOrder)

number_species_order_Lavras_regional <- order_Lavras_regional %>% count(IOCOrder)

### Get proportions
prop_order_Lavras_urban <- prop.table(number_species_order_Lavras_urban$n)

prop_order_Lavras_regional <- prop.table(number_species_order_Lavras_regional$n)

### Add proportions
number_species_order_Lavras_urban$prop <- cbind.data.frame(number_species_order_Lavras_urban, prop_order_Lavras_urban)

number_species_order_Lavras_regional$prop <- cbind.data.frame(number_species_order_Lavras_regional, prop_order_Lavras_regional)

### Convert proportions into matrix
number_species_order_Lavras_urban <- as.matrix(number_species_order_Lavras_urban)
number_species_order_Lavras_regional <- as.matrix(number_species_order_Lavras_regional)

### Convert matrix into data frame
number_species_order_Lavras_urban <- as.data.frame(number_species_order_Lavras_urban)
number_species_order_Lavras_regional <- as.data.frame(number_species_order_Lavras_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Lavras_urban_regional <- bind_rows(number_species_order_Lavras_urban,anti_join(number_species_order_Lavras_regional,number_species_order_Lavras_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Lavras_urban_regional <- number_species_order_Lavras_urban_regional[order(number_species_order_Lavras_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Lavras_urban_regional$prop.prop_order_Lavras_regional <- number_species_order_Lavras_regional$prop.prop_order_Lavras_regional

### Replace NA by ceros in the urban proportions
number_species_order_Lavras_urban_regional[is.na(number_species_order_Lavras_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Lavras_urban_regional_2 <- bind_rows(number_species_order_Lavras_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Lavras_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Lavras_urban_regional_2 <- number_species_order_Lavras_urban_regional_2[order(number_species_order_Lavras_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Lavras_urban_regional_2[is.na(number_species_order_Lavras_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Lavras_urban_regional_3 <- data.frame(number_species_order_Lavras_urban_regional_2[1],
                                                           number_species_order_Lavras_urban_regional_2[5],
                                                           number_species_order_Lavras_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Lavras_urban_regional_3 <- setNames(number_species_order_Lavras_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Lavras_urban_regional_3$prop_urban <- as.numeric(number_species_order_Lavras_urban_regional_3$prop_urban)
number_species_order_Lavras_urban_regional_3$prop_regional <- as.numeric(number_species_order_Lavras_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Lavras_order_prop_vector <- (number_species_order_Lavras_urban_regional_3$prop_urban - number_species_order_Lavras_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Lavras_order_prop_df <- cbind.data.frame(number_species_order_Lavras_urban_regional_3[1],
                                         Lavras_order_prop_vector)

### Change column names
colnames(Lavras_order_prop_df) <- c('Order', 'Lavras')



### Layyah
### Extract species and orders for urban and regional assemblages
order_Layyah_urban <- filter(bird_taxonomy_regional, grepl(paste(Layyah_urban_filtering, collapse="|"), TipLabel))

order_Layyah_regional <- filter(bird_taxonomy_regional, grepl(paste(Layyah_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Layyah_urban <- order_Layyah_urban %>% count(IOCOrder)

number_species_order_Layyah_regional <- order_Layyah_regional %>% count(IOCOrder)

### Get proportions
prop_order_Layyah_urban <- prop.table(number_species_order_Layyah_urban$n)

prop_order_Layyah_regional <- prop.table(number_species_order_Layyah_regional$n)

### Add proportions
number_species_order_Layyah_urban$prop <- cbind.data.frame(number_species_order_Layyah_urban, prop_order_Layyah_urban)

number_species_order_Layyah_regional$prop <- cbind.data.frame(number_species_order_Layyah_regional, prop_order_Layyah_regional)

### Convert proportions into matrix
number_species_order_Layyah_urban <- as.matrix(number_species_order_Layyah_urban)
number_species_order_Layyah_regional <- as.matrix(number_species_order_Layyah_regional)

### Convert matrix into data frame
number_species_order_Layyah_urban <- as.data.frame(number_species_order_Layyah_urban)
number_species_order_Layyah_regional <- as.data.frame(number_species_order_Layyah_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Layyah_urban_regional <- bind_rows(number_species_order_Layyah_urban,anti_join(number_species_order_Layyah_regional,number_species_order_Layyah_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Layyah_urban_regional <- number_species_order_Layyah_urban_regional[order(number_species_order_Layyah_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Layyah_urban_regional$prop.prop_order_Layyah_regional <- number_species_order_Layyah_regional$prop.prop_order_Layyah_regional

### Replace NA by ceros in the urban proportions
number_species_order_Layyah_urban_regional[is.na(number_species_order_Layyah_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Layyah_urban_regional_2 <- bind_rows(number_species_order_Layyah_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Layyah_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Layyah_urban_regional_2 <- number_species_order_Layyah_urban_regional_2[order(number_species_order_Layyah_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Layyah_urban_regional_2[is.na(number_species_order_Layyah_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Layyah_urban_regional_3 <- data.frame(number_species_order_Layyah_urban_regional_2[1],
                                                           number_species_order_Layyah_urban_regional_2[5],
                                                           number_species_order_Layyah_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Layyah_urban_regional_3 <- setNames(number_species_order_Layyah_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Layyah_urban_regional_3$prop_urban <- as.numeric(number_species_order_Layyah_urban_regional_3$prop_urban)
number_species_order_Layyah_urban_regional_3$prop_regional <- as.numeric(number_species_order_Layyah_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Layyah_order_prop_vector <- (number_species_order_Layyah_urban_regional_3$prop_urban - number_species_order_Layyah_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Layyah_order_prop_df <- cbind.data.frame(number_species_order_Layyah_urban_regional_3[1],
                                         Layyah_order_prop_vector)

### Change column names
colnames(Layyah_order_prop_df) <- c('Order', 'Layyah')



### Lodz
### Extract species and orders for urban and regional assemblages
order_Lodz_urban <- filter(bird_taxonomy_regional, grepl(paste(Lodz_urban_filtering, collapse="|"), TipLabel))

order_Lodz_regional <- filter(bird_taxonomy_regional, grepl(paste(Lodz_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Lodz_urban <- order_Lodz_urban %>% count(IOCOrder)

number_species_order_Lodz_regional <- order_Lodz_regional %>% count(IOCOrder)

### Get proportions
prop_order_Lodz_urban <- prop.table(number_species_order_Lodz_urban$n)

prop_order_Lodz_regional <- prop.table(number_species_order_Lodz_regional$n)

### Add proportions
number_species_order_Lodz_urban$prop <- cbind.data.frame(number_species_order_Lodz_urban, prop_order_Lodz_urban)

number_species_order_Lodz_regional$prop <- cbind.data.frame(number_species_order_Lodz_regional, prop_order_Lodz_regional)

### Convert proportions into matrix
number_species_order_Lodz_urban <- as.matrix(number_species_order_Lodz_urban)
number_species_order_Lodz_regional <- as.matrix(number_species_order_Lodz_regional)

### Convert matrix into data frame
number_species_order_Lodz_urban <- as.data.frame(number_species_order_Lodz_urban)
number_species_order_Lodz_regional <- as.data.frame(number_species_order_Lodz_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Lodz_urban_regional <- bind_rows(number_species_order_Lodz_urban,anti_join(number_species_order_Lodz_regional,number_species_order_Lodz_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Lodz_urban_regional <- number_species_order_Lodz_urban_regional[order(number_species_order_Lodz_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Lodz_urban_regional$prop.prop_order_Lodz_regional <- number_species_order_Lodz_regional$prop.prop_order_Lodz_regional

### Replace NA by ceros in the urban proportions
number_species_order_Lodz_urban_regional[is.na(number_species_order_Lodz_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Lodz_urban_regional_2 <- bind_rows(number_species_order_Lodz_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Lodz_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Lodz_urban_regional_2 <- number_species_order_Lodz_urban_regional_2[order(number_species_order_Lodz_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Lodz_urban_regional_2[is.na(number_species_order_Lodz_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Lodz_urban_regional_3 <- data.frame(number_species_order_Lodz_urban_regional_2[1],
                                                         number_species_order_Lodz_urban_regional_2[5],
                                                         number_species_order_Lodz_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Lodz_urban_regional_3 <- setNames(number_species_order_Lodz_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Lodz_urban_regional_3$prop_urban <- as.numeric(number_species_order_Lodz_urban_regional_3$prop_urban)
number_species_order_Lodz_urban_regional_3$prop_regional <- as.numeric(number_species_order_Lodz_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Lodz_order_prop_vector <- (number_species_order_Lodz_urban_regional_3$prop_urban - number_species_order_Lodz_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Lodz_order_prop_df <- cbind.data.frame(number_species_order_Lodz_urban_regional_3[1],
                                       Lodz_order_prop_vector)

### Change column names
colnames(Lodz_order_prop_df) <- c('Order', 'Lodz')



### Loimaa
### Extract species and orders for urban and regional assemblages
order_Loimaa_urban <- filter(bird_taxonomy_regional, grepl(paste(Loimaa_urban_filtering, collapse="|"), TipLabel))

order_Loimaa_regional <- filter(bird_taxonomy_regional, grepl(paste(Loimaa_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Loimaa_urban <- order_Loimaa_urban %>% count(IOCOrder)

number_species_order_Loimaa_regional <- order_Loimaa_regional %>% count(IOCOrder)

### Get proportions
prop_order_Loimaa_urban <- prop.table(number_species_order_Loimaa_urban$n)

prop_order_Loimaa_regional <- prop.table(number_species_order_Loimaa_regional$n)

### Add proportions
number_species_order_Loimaa_urban$prop <- cbind.data.frame(number_species_order_Loimaa_urban, prop_order_Loimaa_urban)

number_species_order_Loimaa_regional$prop <- cbind.data.frame(number_species_order_Loimaa_regional, prop_order_Loimaa_regional)

### Convert proportions into matrix
number_species_order_Loimaa_urban <- as.matrix(number_species_order_Loimaa_urban)
number_species_order_Loimaa_regional <- as.matrix(number_species_order_Loimaa_regional)

### Convert matrix into data frame
number_species_order_Loimaa_urban <- as.data.frame(number_species_order_Loimaa_urban)
number_species_order_Loimaa_regional <- as.data.frame(number_species_order_Loimaa_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Loimaa_urban_regional <- bind_rows(number_species_order_Loimaa_urban,anti_join(number_species_order_Loimaa_regional,number_species_order_Loimaa_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Loimaa_urban_regional <- number_species_order_Loimaa_urban_regional[order(number_species_order_Loimaa_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Loimaa_urban_regional$prop.prop_order_Loimaa_regional <- number_species_order_Loimaa_regional$prop.prop_order_Loimaa_regional

### Replace NA by ceros in the urban proportions
number_species_order_Loimaa_urban_regional[is.na(number_species_order_Loimaa_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Loimaa_urban_regional_2 <- bind_rows(number_species_order_Loimaa_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Loimaa_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Loimaa_urban_regional_2 <- number_species_order_Loimaa_urban_regional_2[order(number_species_order_Loimaa_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Loimaa_urban_regional_2[is.na(number_species_order_Loimaa_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Loimaa_urban_regional_3 <- data.frame(number_species_order_Loimaa_urban_regional_2[1],
                                                           number_species_order_Loimaa_urban_regional_2[5],
                                                           number_species_order_Loimaa_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Loimaa_urban_regional_3 <- setNames(number_species_order_Loimaa_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Loimaa_urban_regional_3$prop_urban <- as.numeric(number_species_order_Loimaa_urban_regional_3$prop_urban)
number_species_order_Loimaa_urban_regional_3$prop_regional <- as.numeric(number_species_order_Loimaa_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Loimaa_order_prop_vector <- (number_species_order_Loimaa_urban_regional_3$prop_urban - number_species_order_Loimaa_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Loimaa_order_prop_df <- cbind.data.frame(number_species_order_Loimaa_urban_regional_3[1],
                                         Loimaa_order_prop_vector)

### Change column names
colnames(Loimaa_order_prop_df) <- c('Order', 'Loimaa')



### Lublin
### Extract species and orders for urban and regional assemblages
order_Lublin_urban <- filter(bird_taxonomy_regional, grepl(paste(Lublin_urban_filtering, collapse="|"), TipLabel))

order_Lublin_regional <- filter(bird_taxonomy_regional, grepl(paste(Lublin_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Lublin_urban <- order_Lublin_urban %>% count(IOCOrder)

number_species_order_Lublin_regional <- order_Lublin_regional %>% count(IOCOrder)

### Get proportions
prop_order_Lublin_urban <- prop.table(number_species_order_Lublin_urban$n)

prop_order_Lublin_regional <- prop.table(number_species_order_Lublin_regional$n)

### Add proportions
number_species_order_Lublin_urban$prop <- cbind.data.frame(number_species_order_Lublin_urban, prop_order_Lublin_urban)

number_species_order_Lublin_regional$prop <- cbind.data.frame(number_species_order_Lublin_regional, prop_order_Lublin_regional)

### Convert proportions into matrix
number_species_order_Lublin_urban <- as.matrix(number_species_order_Lublin_urban)
number_species_order_Lublin_regional <- as.matrix(number_species_order_Lublin_regional)

### Convert matrix into data frame
number_species_order_Lublin_urban <- as.data.frame(number_species_order_Lublin_urban)
number_species_order_Lublin_regional <- as.data.frame(number_species_order_Lublin_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Lublin_urban_regional <- bind_rows(number_species_order_Lublin_urban,anti_join(number_species_order_Lublin_regional,number_species_order_Lublin_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Lublin_urban_regional <- number_species_order_Lublin_urban_regional[order(number_species_order_Lublin_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Lublin_urban_regional$prop.prop_order_Lublin_regional <- number_species_order_Lublin_regional$prop.prop_order_Lublin_regional

### Replace NA by ceros in the urban proportions
number_species_order_Lublin_urban_regional[is.na(number_species_order_Lublin_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Lublin_urban_regional_2 <- bind_rows(number_species_order_Lublin_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Lublin_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Lublin_urban_regional_2 <- number_species_order_Lublin_urban_regional_2[order(number_species_order_Lublin_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Lublin_urban_regional_2[is.na(number_species_order_Lublin_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Lublin_urban_regional_3 <- data.frame(number_species_order_Lublin_urban_regional_2[1],
                                                           number_species_order_Lublin_urban_regional_2[5],
                                                           number_species_order_Lublin_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Lublin_urban_regional_3 <- setNames(number_species_order_Lublin_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Lublin_urban_regional_3$prop_urban <- as.numeric(number_species_order_Lublin_urban_regional_3$prop_urban)
number_species_order_Lublin_urban_regional_3$prop_regional <- as.numeric(number_species_order_Lublin_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Lublin_order_prop_vector <- (number_species_order_Lublin_urban_regional_3$prop_urban - number_species_order_Lublin_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Lublin_order_prop_df <- cbind.data.frame(number_species_order_Lublin_urban_regional_3[1],
                                         Lublin_order_prop_vector)

### Change column names
colnames(Lublin_order_prop_df) <- c('Order', 'Lublin')



### Mar_del_Plata
### Extract species and orders for urban and regional assemblages
order_Mar_del_Plata_urban <- filter(bird_taxonomy_regional, grepl(paste(Mar_del_Plata_urban_filtering, collapse="|"), TipLabel))

order_Mar_del_Plata_regional <- filter(bird_taxonomy_regional, grepl(paste(Mar_del_Plata_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Mar_del_Plata_urban <- order_Mar_del_Plata_urban %>% count(IOCOrder)

number_species_order_Mar_del_Plata_regional <- order_Mar_del_Plata_regional %>% count(IOCOrder)

### Get proportions
prop_order_Mar_del_Plata_urban <- prop.table(number_species_order_Mar_del_Plata_urban$n)

prop_order_Mar_del_Plata_regional <- prop.table(number_species_order_Mar_del_Plata_regional$n)

### Add proportions
number_species_order_Mar_del_Plata_urban$prop <- cbind.data.frame(number_species_order_Mar_del_Plata_urban, prop_order_Mar_del_Plata_urban)

number_species_order_Mar_del_Plata_regional$prop <- cbind.data.frame(number_species_order_Mar_del_Plata_regional, prop_order_Mar_del_Plata_regional)

### Convert proportions into matrix
number_species_order_Mar_del_Plata_urban <- as.matrix(number_species_order_Mar_del_Plata_urban)
number_species_order_Mar_del_Plata_regional <- as.matrix(number_species_order_Mar_del_Plata_regional)

### Convert matrix into data frame
number_species_order_Mar_del_Plata_urban <- as.data.frame(number_species_order_Mar_del_Plata_urban)
number_species_order_Mar_del_Plata_regional <- as.data.frame(number_species_order_Mar_del_Plata_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Mar_del_Plata_urban_regional <- bind_rows(number_species_order_Mar_del_Plata_urban,anti_join(number_species_order_Mar_del_Plata_regional,number_species_order_Mar_del_Plata_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Mar_del_Plata_urban_regional <- number_species_order_Mar_del_Plata_urban_regional[order(number_species_order_Mar_del_Plata_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Mar_del_Plata_urban_regional$prop.prop_order_Mar_del_Plata_regional <- number_species_order_Mar_del_Plata_regional$prop.prop_order_Mar_del_Plata_regional

### Replace NA by ceros in the urban proportions
number_species_order_Mar_del_Plata_urban_regional[is.na(number_species_order_Mar_del_Plata_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Mar_del_Plata_urban_regional_2 <- bind_rows(number_species_order_Mar_del_Plata_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Mar_del_Plata_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Mar_del_Plata_urban_regional_2 <- number_species_order_Mar_del_Plata_urban_regional_2[order(number_species_order_Mar_del_Plata_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Mar_del_Plata_urban_regional_2[is.na(number_species_order_Mar_del_Plata_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Mar_del_Plata_urban_regional_3 <- data.frame(number_species_order_Mar_del_Plata_urban_regional_2[1],
                                                                  number_species_order_Mar_del_Plata_urban_regional_2[5],
                                                                  number_species_order_Mar_del_Plata_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Mar_del_Plata_urban_regional_3 <- setNames(number_species_order_Mar_del_Plata_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Mar_del_Plata_urban_regional_3$prop_urban <- as.numeric(number_species_order_Mar_del_Plata_urban_regional_3$prop_urban)
number_species_order_Mar_del_Plata_urban_regional_3$prop_regional <- as.numeric(number_species_order_Mar_del_Plata_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Mar_del_Plata_order_prop_vector <- (number_species_order_Mar_del_Plata_urban_regional_3$prop_urban - number_species_order_Mar_del_Plata_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Mar_del_Plata_order_prop_df <- cbind.data.frame(number_species_order_Mar_del_Plata_urban_regional_3[1],
                                                Mar_del_Plata_order_prop_vector)

### Change column names
colnames(Mar_del_Plata_order_prop_df) <- c('Order', 'Mar_del_Plata')



### Montpellier
### Extract species and orders for urban and regional assemblages
order_Montpellier_urban <- filter(bird_taxonomy_regional, grepl(paste(Montpellier_urban_filtering, collapse="|"), TipLabel))

order_Montpellier_regional <- filter(bird_taxonomy_regional, grepl(paste(Montpellier_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Montpellier_urban <- order_Montpellier_urban %>% count(IOCOrder)

number_species_order_Montpellier_regional <- order_Montpellier_regional %>% count(IOCOrder)

### Get proportions
prop_order_Montpellier_urban <- prop.table(number_species_order_Montpellier_urban$n)

prop_order_Montpellier_regional <- prop.table(number_species_order_Montpellier_regional$n)

### Add proportions
number_species_order_Montpellier_urban$prop <- cbind.data.frame(number_species_order_Montpellier_urban, prop_order_Montpellier_urban)

number_species_order_Montpellier_regional$prop <- cbind.data.frame(number_species_order_Montpellier_regional, prop_order_Montpellier_regional)

### Convert proportions into matrix
number_species_order_Montpellier_urban <- as.matrix(number_species_order_Montpellier_urban)
number_species_order_Montpellier_regional <- as.matrix(number_species_order_Montpellier_regional)

### Convert matrix into data frame
number_species_order_Montpellier_urban <- as.data.frame(number_species_order_Montpellier_urban)
number_species_order_Montpellier_regional <- as.data.frame(number_species_order_Montpellier_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Montpellier_urban_regional <- bind_rows(number_species_order_Montpellier_urban,anti_join(number_species_order_Montpellier_regional,number_species_order_Montpellier_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Montpellier_urban_regional <- number_species_order_Montpellier_urban_regional[order(number_species_order_Montpellier_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Montpellier_urban_regional$prop.prop_order_Montpellier_regional <- number_species_order_Montpellier_regional$prop.prop_order_Montpellier_regional

### Replace NA by ceros in the urban proportions
number_species_order_Montpellier_urban_regional[is.na(number_species_order_Montpellier_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Montpellier_urban_regional_2 <- bind_rows(number_species_order_Montpellier_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Montpellier_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Montpellier_urban_regional_2 <- number_species_order_Montpellier_urban_regional_2[order(number_species_order_Montpellier_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Montpellier_urban_regional_2[is.na(number_species_order_Montpellier_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Montpellier_urban_regional_3 <- data.frame(number_species_order_Montpellier_urban_regional_2[1],
                                                                number_species_order_Montpellier_urban_regional_2[5],
                                                                number_species_order_Montpellier_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Montpellier_urban_regional_3 <- setNames(number_species_order_Montpellier_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Montpellier_urban_regional_3$prop_urban <- as.numeric(number_species_order_Montpellier_urban_regional_3$prop_urban)
number_species_order_Montpellier_urban_regional_3$prop_regional <- as.numeric(number_species_order_Montpellier_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Montpellier_order_prop_vector <- (number_species_order_Montpellier_urban_regional_3$prop_urban - number_species_order_Montpellier_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Montpellier_order_prop_df <- cbind.data.frame(number_species_order_Montpellier_urban_regional_3[1],
                                              Montpellier_order_prop_vector)

### Change column names
colnames(Montpellier_order_prop_df) <- c('Order', 'Montpellier')



### Naantali
### Extract species and orders for urban and regional assemblages
order_Naantali_urban <- filter(bird_taxonomy_regional, grepl(paste(Naantali_urban_filtering, collapse="|"), TipLabel))

order_Naantali_regional <- filter(bird_taxonomy_regional, grepl(paste(Naantali_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Naantali_urban <- order_Naantali_urban %>% count(IOCOrder)

number_species_order_Naantali_regional <- order_Naantali_regional %>% count(IOCOrder)

### Get proportions
prop_order_Naantali_urban <- prop.table(number_species_order_Naantali_urban$n)

prop_order_Naantali_regional <- prop.table(number_species_order_Naantali_regional$n)

### Add proportions
number_species_order_Naantali_urban$prop <- cbind.data.frame(number_species_order_Naantali_urban, prop_order_Naantali_urban)

number_species_order_Naantali_regional$prop <- cbind.data.frame(number_species_order_Naantali_regional, prop_order_Naantali_regional)

### Convert proportions into matrix
number_species_order_Naantali_urban <- as.matrix(number_species_order_Naantali_urban)
number_species_order_Naantali_regional <- as.matrix(number_species_order_Naantali_regional)

### Convert matrix into data frame
number_species_order_Naantali_urban <- as.data.frame(number_species_order_Naantali_urban)
number_species_order_Naantali_regional <- as.data.frame(number_species_order_Naantali_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Naantali_urban_regional <- bind_rows(number_species_order_Naantali_urban,anti_join(number_species_order_Naantali_regional,number_species_order_Naantali_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Naantali_urban_regional <- number_species_order_Naantali_urban_regional[order(number_species_order_Naantali_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Naantali_urban_regional$prop.prop_order_Naantali_regional <- number_species_order_Naantali_regional$prop.prop_order_Naantali_regional

### Replace NA by ceros in the urban proportions
number_species_order_Naantali_urban_regional[is.na(number_species_order_Naantali_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Naantali_urban_regional_2 <- bind_rows(number_species_order_Naantali_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Naantali_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Naantali_urban_regional_2 <- number_species_order_Naantali_urban_regional_2[order(number_species_order_Naantali_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Naantali_urban_regional_2[is.na(number_species_order_Naantali_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Naantali_urban_regional_3 <- data.frame(number_species_order_Naantali_urban_regional_2[1],
                                                             number_species_order_Naantali_urban_regional_2[5],
                                                             number_species_order_Naantali_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Naantali_urban_regional_3 <- setNames(number_species_order_Naantali_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Naantali_urban_regional_3$prop_urban <- as.numeric(number_species_order_Naantali_urban_regional_3$prop_urban)
number_species_order_Naantali_urban_regional_3$prop_regional <- as.numeric(number_species_order_Naantali_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Naantali_order_prop_vector <- (number_species_order_Naantali_urban_regional_3$prop_urban - number_species_order_Naantali_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Naantali_order_prop_df <- cbind.data.frame(number_species_order_Naantali_urban_regional_3[1],
                                           Naantali_order_prop_vector)

### Change column names
colnames(Naantali_order_prop_df) <- c('Order', 'Naantali')



### Nanning
### Extract species and orders for urban and regional assemblages
order_Nanning_urban <- filter(bird_taxonomy_regional, grepl(paste(Nanning_urban_filtering, collapse="|"), TipLabel))

order_Nanning_regional <- filter(bird_taxonomy_regional, grepl(paste(Nanning_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Nanning_urban <- order_Nanning_urban %>% count(IOCOrder)

number_species_order_Nanning_regional <- order_Nanning_regional %>% count(IOCOrder)

### Get proportions
prop_order_Nanning_urban <- prop.table(number_species_order_Nanning_urban$n)

prop_order_Nanning_regional <- prop.table(number_species_order_Nanning_regional$n)

### Add proportions
number_species_order_Nanning_urban$prop <- cbind.data.frame(number_species_order_Nanning_urban, prop_order_Nanning_urban)

number_species_order_Nanning_regional$prop <- cbind.data.frame(number_species_order_Nanning_regional, prop_order_Nanning_regional)

### Convert proportions into matrix
number_species_order_Nanning_urban <- as.matrix(number_species_order_Nanning_urban)
number_species_order_Nanning_regional <- as.matrix(number_species_order_Nanning_regional)

### Convert matrix into data frame
number_species_order_Nanning_urban <- as.data.frame(number_species_order_Nanning_urban)
number_species_order_Nanning_regional <- as.data.frame(number_species_order_Nanning_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Nanning_urban_regional <- bind_rows(number_species_order_Nanning_urban,anti_join(number_species_order_Nanning_regional,number_species_order_Nanning_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Nanning_urban_regional <- number_species_order_Nanning_urban_regional[order(number_species_order_Nanning_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Nanning_urban_regional$prop.prop_order_Nanning_regional <- number_species_order_Nanning_regional$prop.prop_order_Nanning_regional

### Replace NA by ceros in the urban proportions
number_species_order_Nanning_urban_regional[is.na(number_species_order_Nanning_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Nanning_urban_regional_2 <- bind_rows(number_species_order_Nanning_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Nanning_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Nanning_urban_regional_2 <- number_species_order_Nanning_urban_regional_2[order(number_species_order_Nanning_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Nanning_urban_regional_2[is.na(number_species_order_Nanning_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Nanning_urban_regional_3 <- data.frame(number_species_order_Nanning_urban_regional_2[1],
                                                            number_species_order_Nanning_urban_regional_2[5],
                                                            number_species_order_Nanning_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Nanning_urban_regional_3 <- setNames(number_species_order_Nanning_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Nanning_urban_regional_3$prop_urban <- as.numeric(number_species_order_Nanning_urban_regional_3$prop_urban)
number_species_order_Nanning_urban_regional_3$prop_regional <- as.numeric(number_species_order_Nanning_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Nanning_order_prop_vector <- (number_species_order_Nanning_urban_regional_3$prop_urban - number_species_order_Nanning_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Nanning_order_prop_df <- cbind.data.frame(number_species_order_Nanning_urban_regional_3[1],
                                          Nanning_order_prop_vector)

### Change column names
colnames(Nanning_order_prop_df) <- c('Order', 'Nanning')



### Olsztyn
### Extract species and orders for urban and regional assemblages
order_Olsztyn_urban <- filter(bird_taxonomy_regional, grepl(paste(Olsztyn_urban_filtering, collapse="|"), TipLabel))

order_Olsztyn_regional <- filter(bird_taxonomy_regional, grepl(paste(Olsztyn_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Olsztyn_urban <- order_Olsztyn_urban %>% count(IOCOrder)

number_species_order_Olsztyn_regional <- order_Olsztyn_regional %>% count(IOCOrder)

### Get proportions
prop_order_Olsztyn_urban <- prop.table(number_species_order_Olsztyn_urban$n)

prop_order_Olsztyn_regional <- prop.table(number_species_order_Olsztyn_regional$n)

### Add proportions
number_species_order_Olsztyn_urban$prop <- cbind.data.frame(number_species_order_Olsztyn_urban, prop_order_Olsztyn_urban)

number_species_order_Olsztyn_regional$prop <- cbind.data.frame(number_species_order_Olsztyn_regional, prop_order_Olsztyn_regional)

### Convert proportions into matrix
number_species_order_Olsztyn_urban <- as.matrix(number_species_order_Olsztyn_urban)
number_species_order_Olsztyn_regional <- as.matrix(number_species_order_Olsztyn_regional)

### Convert matrix into data frame
number_species_order_Olsztyn_urban <- as.data.frame(number_species_order_Olsztyn_urban)
number_species_order_Olsztyn_regional <- as.data.frame(number_species_order_Olsztyn_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Olsztyn_urban_regional <- bind_rows(number_species_order_Olsztyn_urban,anti_join(number_species_order_Olsztyn_regional,number_species_order_Olsztyn_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Olsztyn_urban_regional <- number_species_order_Olsztyn_urban_regional[order(number_species_order_Olsztyn_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Olsztyn_urban_regional$prop.prop_order_Olsztyn_regional <- number_species_order_Olsztyn_regional$prop.prop_order_Olsztyn_regional

### Replace NA by ceros in the urban proportions
number_species_order_Olsztyn_urban_regional[is.na(number_species_order_Olsztyn_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Olsztyn_urban_regional_2 <- bind_rows(number_species_order_Olsztyn_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Olsztyn_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Olsztyn_urban_regional_2 <- number_species_order_Olsztyn_urban_regional_2[order(number_species_order_Olsztyn_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Olsztyn_urban_regional_2[is.na(number_species_order_Olsztyn_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Olsztyn_urban_regional_3 <- data.frame(number_species_order_Olsztyn_urban_regional_2[1],
                                                            number_species_order_Olsztyn_urban_regional_2[5],
                                                            number_species_order_Olsztyn_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Olsztyn_urban_regional_3 <- setNames(number_species_order_Olsztyn_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Olsztyn_urban_regional_3$prop_urban <- as.numeric(number_species_order_Olsztyn_urban_regional_3$prop_urban)
number_species_order_Olsztyn_urban_regional_3$prop_regional <- as.numeric(number_species_order_Olsztyn_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Olsztyn_order_prop_vector <- (number_species_order_Olsztyn_urban_regional_3$prop_urban - number_species_order_Olsztyn_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Olsztyn_order_prop_df <- cbind.data.frame(number_species_order_Olsztyn_urban_regional_3[1],
                                          Olsztyn_order_prop_vector)

### Change column names
colnames(Olsztyn_order_prop_df) <- c('Order', 'Olsztyn')



### Ostrow_Wielkopolski
### Extract species and orders for urban and regional assemblages
order_Ostrow_Wielkopolski_urban <- filter(bird_taxonomy_regional, grepl(paste(Ostrow_Wielkopolski_urban_filtering, collapse="|"), TipLabel))

order_Ostrow_Wielkopolski_regional <- filter(bird_taxonomy_regional, grepl(paste(Ostrow_Wielkopolski_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Ostrow_Wielkopolski_urban <- order_Ostrow_Wielkopolski_urban %>% count(IOCOrder)

number_species_order_Ostrow_Wielkopolski_regional <- order_Ostrow_Wielkopolski_regional %>% count(IOCOrder)

### Get proportions
prop_order_Ostrow_Wielkopolski_urban <- prop.table(number_species_order_Ostrow_Wielkopolski_urban$n)

prop_order_Ostrow_Wielkopolski_regional <- prop.table(number_species_order_Ostrow_Wielkopolski_regional$n)

### Add proportions
number_species_order_Ostrow_Wielkopolski_urban$prop <- cbind.data.frame(number_species_order_Ostrow_Wielkopolski_urban, prop_order_Ostrow_Wielkopolski_urban)

number_species_order_Ostrow_Wielkopolski_regional$prop <- cbind.data.frame(number_species_order_Ostrow_Wielkopolski_regional, prop_order_Ostrow_Wielkopolski_regional)

### Convert proportions into matrix
number_species_order_Ostrow_Wielkopolski_urban <- as.matrix(number_species_order_Ostrow_Wielkopolski_urban)
number_species_order_Ostrow_Wielkopolski_regional <- as.matrix(number_species_order_Ostrow_Wielkopolski_regional)

### Convert matrix into data frame
number_species_order_Ostrow_Wielkopolski_urban <- as.data.frame(number_species_order_Ostrow_Wielkopolski_urban)
number_species_order_Ostrow_Wielkopolski_regional <- as.data.frame(number_species_order_Ostrow_Wielkopolski_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Ostrow_Wielkopolski_urban_regional <- bind_rows(number_species_order_Ostrow_Wielkopolski_urban,anti_join(number_species_order_Ostrow_Wielkopolski_regional,number_species_order_Ostrow_Wielkopolski_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Ostrow_Wielkopolski_urban_regional <- number_species_order_Ostrow_Wielkopolski_urban_regional[order(number_species_order_Ostrow_Wielkopolski_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Ostrow_Wielkopolski_urban_regional$prop.prop_order_Ostrow_Wielkopolski_regional <- number_species_order_Ostrow_Wielkopolski_regional$prop.prop_order_Ostrow_Wielkopolski_regional

### Replace NA by ceros in the urban proportions
number_species_order_Ostrow_Wielkopolski_urban_regional[is.na(number_species_order_Ostrow_Wielkopolski_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Ostrow_Wielkopolski_urban_regional_2 <- bind_rows(number_species_order_Ostrow_Wielkopolski_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Ostrow_Wielkopolski_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Ostrow_Wielkopolski_urban_regional_2 <- number_species_order_Ostrow_Wielkopolski_urban_regional_2[order(number_species_order_Ostrow_Wielkopolski_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Ostrow_Wielkopolski_urban_regional_2[is.na(number_species_order_Ostrow_Wielkopolski_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Ostrow_Wielkopolski_urban_regional_3 <- data.frame(number_species_order_Ostrow_Wielkopolski_urban_regional_2[1],
                                                                        number_species_order_Ostrow_Wielkopolski_urban_regional_2[5],
                                                                        number_species_order_Ostrow_Wielkopolski_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Ostrow_Wielkopolski_urban_regional_3 <- setNames(number_species_order_Ostrow_Wielkopolski_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Ostrow_Wielkopolski_urban_regional_3$prop_urban <- as.numeric(number_species_order_Ostrow_Wielkopolski_urban_regional_3$prop_urban)
number_species_order_Ostrow_Wielkopolski_urban_regional_3$prop_regional <- as.numeric(number_species_order_Ostrow_Wielkopolski_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Ostrow_Wielkopolski_order_prop_vector <- (number_species_order_Ostrow_Wielkopolski_urban_regional_3$prop_urban - number_species_order_Ostrow_Wielkopolski_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Ostrow_Wielkopolski_order_prop_df <- cbind.data.frame(number_species_order_Ostrow_Wielkopolski_urban_regional_3[1],
                                                      Ostrow_Wielkopolski_order_prop_vector)

### Change column names
colnames(Ostrow_Wielkopolski_order_prop_df) <- c('Order', 'Ostrow_Wielkopolski')



### Oulu
### Extract species and orders for urban and regional assemblages
order_Oulu_urban <- filter(bird_taxonomy_regional, grepl(paste(Oulu_urban_filtering, collapse="|"), TipLabel))

order_Oulu_regional <- filter(bird_taxonomy_regional, grepl(paste(Oulu_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Oulu_urban <- order_Oulu_urban %>% count(IOCOrder)

number_species_order_Oulu_regional <- order_Oulu_regional %>% count(IOCOrder)

### Get proportions
prop_order_Oulu_urban <- prop.table(number_species_order_Oulu_urban$n)

prop_order_Oulu_regional <- prop.table(number_species_order_Oulu_regional$n)

### Add proportions
number_species_order_Oulu_urban$prop <- cbind.data.frame(number_species_order_Oulu_urban, prop_order_Oulu_urban)

number_species_order_Oulu_regional$prop <- cbind.data.frame(number_species_order_Oulu_regional, prop_order_Oulu_regional)

### Convert proportions into matrix
number_species_order_Oulu_urban <- as.matrix(number_species_order_Oulu_urban)
number_species_order_Oulu_regional <- as.matrix(number_species_order_Oulu_regional)

### Convert matrix into data frame
number_species_order_Oulu_urban <- as.data.frame(number_species_order_Oulu_urban)
number_species_order_Oulu_regional <- as.data.frame(number_species_order_Oulu_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Oulu_urban_regional <- bind_rows(number_species_order_Oulu_urban,anti_join(number_species_order_Oulu_regional,number_species_order_Oulu_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Oulu_urban_regional <- number_species_order_Oulu_urban_regional[order(number_species_order_Oulu_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Oulu_urban_regional$prop.prop_order_Oulu_regional <- number_species_order_Oulu_regional$prop.prop_order_Oulu_regional

### Replace NA by ceros in the urban proportions
number_species_order_Oulu_urban_regional[is.na(number_species_order_Oulu_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Oulu_urban_regional_2 <- bind_rows(number_species_order_Oulu_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Oulu_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Oulu_urban_regional_2 <- number_species_order_Oulu_urban_regional_2[order(number_species_order_Oulu_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Oulu_urban_regional_2[is.na(number_species_order_Oulu_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Oulu_urban_regional_3 <- data.frame(number_species_order_Oulu_urban_regional_2[1],
                                                         number_species_order_Oulu_urban_regional_2[5],
                                                         number_species_order_Oulu_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Oulu_urban_regional_3 <- setNames(number_species_order_Oulu_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Oulu_urban_regional_3$prop_urban <- as.numeric(number_species_order_Oulu_urban_regional_3$prop_urban)
number_species_order_Oulu_urban_regional_3$prop_regional <- as.numeric(number_species_order_Oulu_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Oulu_order_prop_vector <- (number_species_order_Oulu_urban_regional_3$prop_urban - number_species_order_Oulu_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Oulu_order_prop_df <- cbind.data.frame(number_species_order_Oulu_urban_regional_3[1],
                                       Oulu_order_prop_vector)

### Change column names
colnames(Oulu_order_prop_df) <- c('Order', 'Oulu')



### Pachuca
### Extract species and orders for urban and regional assemblages
order_Pachuca_urban <- filter(bird_taxonomy_regional, grepl(paste(Pachuca_urban_filtering, collapse="|"), TipLabel))

order_Pachuca_regional <- filter(bird_taxonomy_regional, grepl(paste(Pachuca_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Pachuca_urban <- order_Pachuca_urban %>% count(IOCOrder)

number_species_order_Pachuca_regional <- order_Pachuca_regional %>% count(IOCOrder)

### Get proportions
prop_order_Pachuca_urban <- prop.table(number_species_order_Pachuca_urban$n)

prop_order_Pachuca_regional <- prop.table(number_species_order_Pachuca_regional$n)

### Add proportions
number_species_order_Pachuca_urban$prop <- cbind.data.frame(number_species_order_Pachuca_urban, prop_order_Pachuca_urban)

number_species_order_Pachuca_regional$prop <- cbind.data.frame(number_species_order_Pachuca_regional, prop_order_Pachuca_regional)

### Convert proportions into matrix
number_species_order_Pachuca_urban <- as.matrix(number_species_order_Pachuca_urban)
number_species_order_Pachuca_regional <- as.matrix(number_species_order_Pachuca_regional)

### Convert matrix into data frame
number_species_order_Pachuca_urban <- as.data.frame(number_species_order_Pachuca_urban)
number_species_order_Pachuca_regional <- as.data.frame(number_species_order_Pachuca_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Pachuca_urban_regional <- bind_rows(number_species_order_Pachuca_urban,anti_join(number_species_order_Pachuca_regional,number_species_order_Pachuca_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pachuca_urban_regional <- number_species_order_Pachuca_urban_regional[order(number_species_order_Pachuca_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Pachuca_urban_regional$prop.prop_order_Pachuca_regional <- number_species_order_Pachuca_regional$prop.prop_order_Pachuca_regional

### Replace NA by ceros in the urban proportions
number_species_order_Pachuca_urban_regional[is.na(number_species_order_Pachuca_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Pachuca_urban_regional_2 <- bind_rows(number_species_order_Pachuca_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Pachuca_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pachuca_urban_regional_2 <- number_species_order_Pachuca_urban_regional_2[order(number_species_order_Pachuca_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Pachuca_urban_regional_2[is.na(number_species_order_Pachuca_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Pachuca_urban_regional_3 <- data.frame(number_species_order_Pachuca_urban_regional_2[1],
                                                            number_species_order_Pachuca_urban_regional_2[5],
                                                            number_species_order_Pachuca_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Pachuca_urban_regional_3 <- setNames(number_species_order_Pachuca_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Pachuca_urban_regional_3$prop_urban <- as.numeric(number_species_order_Pachuca_urban_regional_3$prop_urban)
number_species_order_Pachuca_urban_regional_3$prop_regional <- as.numeric(number_species_order_Pachuca_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Pachuca_order_prop_vector <- (number_species_order_Pachuca_urban_regional_3$prop_urban - number_species_order_Pachuca_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Pachuca_order_prop_df <- cbind.data.frame(number_species_order_Pachuca_urban_regional_3[1],
                                          Pachuca_order_prop_vector)

### Change column names
colnames(Pachuca_order_prop_df) <- c('Order', 'Pachuca')



### Paimio
### Extract species and orders for urban and regional assemblages
order_Paimio_urban <- filter(bird_taxonomy_regional, grepl(paste(Paimio_urban_filtering, collapse="|"), TipLabel))

order_Paimio_regional <- filter(bird_taxonomy_regional, grepl(paste(Paimio_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Paimio_urban <- order_Paimio_urban %>% count(IOCOrder)

number_species_order_Paimio_regional <- order_Paimio_regional %>% count(IOCOrder)

### Get proportions
prop_order_Paimio_urban <- prop.table(number_species_order_Paimio_urban$n)

prop_order_Paimio_regional <- prop.table(number_species_order_Paimio_regional$n)

### Add proportions
number_species_order_Paimio_urban$prop <- cbind.data.frame(number_species_order_Paimio_urban, prop_order_Paimio_urban)

number_species_order_Paimio_regional$prop <- cbind.data.frame(number_species_order_Paimio_regional, prop_order_Paimio_regional)

### Convert proportions into matrix
number_species_order_Paimio_urban <- as.matrix(number_species_order_Paimio_urban)
number_species_order_Paimio_regional <- as.matrix(number_species_order_Paimio_regional)

### Convert matrix into data frame
number_species_order_Paimio_urban <- as.data.frame(number_species_order_Paimio_urban)
number_species_order_Paimio_regional <- as.data.frame(number_species_order_Paimio_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Paimio_urban_regional <- bind_rows(number_species_order_Paimio_urban,anti_join(number_species_order_Paimio_regional,number_species_order_Paimio_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Paimio_urban_regional <- number_species_order_Paimio_urban_regional[order(number_species_order_Paimio_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Paimio_urban_regional$prop.prop_order_Paimio_regional <- number_species_order_Paimio_regional$prop.prop_order_Paimio_regional

### Replace NA by ceros in the urban proportions
number_species_order_Paimio_urban_regional[is.na(number_species_order_Paimio_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Paimio_urban_regional_2 <- bind_rows(number_species_order_Paimio_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Paimio_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Paimio_urban_regional_2 <- number_species_order_Paimio_urban_regional_2[order(number_species_order_Paimio_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Paimio_urban_regional_2[is.na(number_species_order_Paimio_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Paimio_urban_regional_3 <- data.frame(number_species_order_Paimio_urban_regional_2[1],
                                                           number_species_order_Paimio_urban_regional_2[5],
                                                           number_species_order_Paimio_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Paimio_urban_regional_3 <- setNames(number_species_order_Paimio_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Paimio_urban_regional_3$prop_urban <- as.numeric(number_species_order_Paimio_urban_regional_3$prop_urban)
number_species_order_Paimio_urban_regional_3$prop_regional <- as.numeric(number_species_order_Paimio_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Paimio_order_prop_vector <- (number_species_order_Paimio_urban_regional_3$prop_urban - number_species_order_Paimio_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Paimio_order_prop_df <- cbind.data.frame(number_species_order_Paimio_urban_regional_3[1],
                                         Paimio_order_prop_vector)

### Change column names
colnames(Paimio_order_prop_df) <- c('Order', 'Paimio')



### Pargas
### Extract species and orders for urban and regional assemblages
order_Pargas_urban <- filter(bird_taxonomy_regional, grepl(paste(Pargas_urban_filtering, collapse="|"), TipLabel))

order_Pargas_regional <- filter(bird_taxonomy_regional, grepl(paste(Pargas_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Pargas_urban <- order_Pargas_urban %>% count(IOCOrder)

number_species_order_Pargas_regional <- order_Pargas_regional %>% count(IOCOrder)

### Get proportions
prop_order_Pargas_urban <- prop.table(number_species_order_Pargas_urban$n)

prop_order_Pargas_regional <- prop.table(number_species_order_Pargas_regional$n)

### Add proportions
number_species_order_Pargas_urban$prop <- cbind.data.frame(number_species_order_Pargas_urban, prop_order_Pargas_urban)

number_species_order_Pargas_regional$prop <- cbind.data.frame(number_species_order_Pargas_regional, prop_order_Pargas_regional)

### Convert proportions into matrix
number_species_order_Pargas_urban <- as.matrix(number_species_order_Pargas_urban)
number_species_order_Pargas_regional <- as.matrix(number_species_order_Pargas_regional)

### Convert matrix into data frame
number_species_order_Pargas_urban <- as.data.frame(number_species_order_Pargas_urban)
number_species_order_Pargas_regional <- as.data.frame(number_species_order_Pargas_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Pargas_urban_regional <- bind_rows(number_species_order_Pargas_urban,anti_join(number_species_order_Pargas_regional,number_species_order_Pargas_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pargas_urban_regional <- number_species_order_Pargas_urban_regional[order(number_species_order_Pargas_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Pargas_urban_regional$prop.prop_order_Pargas_regional <- number_species_order_Pargas_regional$prop.prop_order_Pargas_regional

### Replace NA by ceros in the urban proportions
number_species_order_Pargas_urban_regional[is.na(number_species_order_Pargas_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Pargas_urban_regional_2 <- bind_rows(number_species_order_Pargas_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Pargas_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pargas_urban_regional_2 <- number_species_order_Pargas_urban_regional_2[order(number_species_order_Pargas_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Pargas_urban_regional_2[is.na(number_species_order_Pargas_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Pargas_urban_regional_3 <- data.frame(number_species_order_Pargas_urban_regional_2[1],
                                                           number_species_order_Pargas_urban_regional_2[5],
                                                           number_species_order_Pargas_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Pargas_urban_regional_3 <- setNames(number_species_order_Pargas_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Pargas_urban_regional_3$prop_urban <- as.numeric(number_species_order_Pargas_urban_regional_3$prop_urban)
number_species_order_Pargas_urban_regional_3$prop_regional <- as.numeric(number_species_order_Pargas_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Pargas_order_prop_vector <- (number_species_order_Pargas_urban_regional_3$prop_urban - number_species_order_Pargas_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Pargas_order_prop_df <- cbind.data.frame(number_species_order_Pargas_urban_regional_3[1],
                                         Pargas_order_prop_vector)

### Change column names
colnames(Pargas_order_prop_df) <- c('Order', 'Pargas')



### Patras
### Extract species and orders for urban and regional assemblages
order_Patras_urban <- filter(bird_taxonomy_regional, grepl(paste(Patras_urban_filtering, collapse="|"), TipLabel))

order_Patras_regional <- filter(bird_taxonomy_regional, grepl(paste(Patras_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Patras_urban <- order_Patras_urban %>% count(IOCOrder)

number_species_order_Patras_regional <- order_Patras_regional %>% count(IOCOrder)

### Get proportions
prop_order_Patras_urban <- prop.table(number_species_order_Patras_urban$n)

prop_order_Patras_regional <- prop.table(number_species_order_Patras_regional$n)

### Add proportions
number_species_order_Patras_urban$prop <- cbind.data.frame(number_species_order_Patras_urban, prop_order_Patras_urban)

number_species_order_Patras_regional$prop <- cbind.data.frame(number_species_order_Patras_regional, prop_order_Patras_regional)

### Convert proportions into matrix
number_species_order_Patras_urban <- as.matrix(number_species_order_Patras_urban)
number_species_order_Patras_regional <- as.matrix(number_species_order_Patras_regional)

### Convert matrix into data frame
number_species_order_Patras_urban <- as.data.frame(number_species_order_Patras_urban)
number_species_order_Patras_regional <- as.data.frame(number_species_order_Patras_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Patras_urban_regional <- bind_rows(number_species_order_Patras_urban,anti_join(number_species_order_Patras_regional,number_species_order_Patras_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Patras_urban_regional <- number_species_order_Patras_urban_regional[order(number_species_order_Patras_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Patras_urban_regional$prop.prop_order_Patras_regional <- number_species_order_Patras_regional$prop.prop_order_Patras_regional

### Replace NA by ceros in the urban proportions
number_species_order_Patras_urban_regional[is.na(number_species_order_Patras_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Patras_urban_regional_2 <- bind_rows(number_species_order_Patras_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Patras_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Patras_urban_regional_2 <- number_species_order_Patras_urban_regional_2[order(number_species_order_Patras_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Patras_urban_regional_2[is.na(number_species_order_Patras_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Patras_urban_regional_3 <- data.frame(number_species_order_Patras_urban_regional_2[1],
                                                           number_species_order_Patras_urban_regional_2[5],
                                                           number_species_order_Patras_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Patras_urban_regional_3 <- setNames(number_species_order_Patras_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Patras_urban_regional_3$prop_urban <- as.numeric(number_species_order_Patras_urban_regional_3$prop_urban)
number_species_order_Patras_urban_regional_3$prop_regional <- as.numeric(number_species_order_Patras_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Patras_order_prop_vector <- (number_species_order_Patras_urban_regional_3$prop_urban - number_species_order_Patras_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Patras_order_prop_df <- cbind.data.frame(number_species_order_Patras_urban_regional_3[1],
                                         Patras_order_prop_vector)

### Change column names
colnames(Patras_order_prop_df) <- c('Order', 'Patras')



### Pelotas
### Extract species and orders for urban and regional assemblages
order_Pelotas_urban <- filter(bird_taxonomy_regional, grepl(paste(Pelotas_urban_filtering, collapse="|"), TipLabel))

order_Pelotas_regional <- filter(bird_taxonomy_regional, grepl(paste(Pelotas_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Pelotas_urban <- order_Pelotas_urban %>% count(IOCOrder)

number_species_order_Pelotas_regional <- order_Pelotas_regional %>% count(IOCOrder)

### Get proportions
prop_order_Pelotas_urban <- prop.table(number_species_order_Pelotas_urban$n)

prop_order_Pelotas_regional <- prop.table(number_species_order_Pelotas_regional$n)

### Add proportions
number_species_order_Pelotas_urban$prop <- cbind.data.frame(number_species_order_Pelotas_urban, prop_order_Pelotas_urban)

number_species_order_Pelotas_regional$prop <- cbind.data.frame(number_species_order_Pelotas_regional, prop_order_Pelotas_regional)

### Convert proportions into matrix
number_species_order_Pelotas_urban <- as.matrix(number_species_order_Pelotas_urban)
number_species_order_Pelotas_regional <- as.matrix(number_species_order_Pelotas_regional)

### Convert matrix into data frame
number_species_order_Pelotas_urban <- as.data.frame(number_species_order_Pelotas_urban)
number_species_order_Pelotas_regional <- as.data.frame(number_species_order_Pelotas_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Pelotas_urban_regional <- bind_rows(number_species_order_Pelotas_urban,anti_join(number_species_order_Pelotas_regional,number_species_order_Pelotas_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pelotas_urban_regional <- number_species_order_Pelotas_urban_regional[order(number_species_order_Pelotas_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Pelotas_urban_regional$prop.prop_order_Pelotas_regional <- number_species_order_Pelotas_regional$prop.prop_order_Pelotas_regional

### Replace NA by ceros in the urban proportions
number_species_order_Pelotas_urban_regional[is.na(number_species_order_Pelotas_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Pelotas_urban_regional_2 <- bind_rows(number_species_order_Pelotas_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Pelotas_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pelotas_urban_regional_2 <- number_species_order_Pelotas_urban_regional_2[order(number_species_order_Pelotas_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Pelotas_urban_regional_2[is.na(number_species_order_Pelotas_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Pelotas_urban_regional_3 <- data.frame(number_species_order_Pelotas_urban_regional_2[1],
                                                            number_species_order_Pelotas_urban_regional_2[5],
                                                            number_species_order_Pelotas_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Pelotas_urban_regional_3 <- setNames(number_species_order_Pelotas_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Pelotas_urban_regional_3$prop_urban <- as.numeric(number_species_order_Pelotas_urban_regional_3$prop_urban)
number_species_order_Pelotas_urban_regional_3$prop_regional <- as.numeric(number_species_order_Pelotas_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Pelotas_order_prop_vector <- (number_species_order_Pelotas_urban_regional_3$prop_urban - number_species_order_Pelotas_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Pelotas_order_prop_df <- cbind.data.frame(number_species_order_Pelotas_urban_regional_3[1],
                                          Pelotas_order_prop_vector)

### Change column names
colnames(Pelotas_order_prop_df) <- c('Order', 'Pelotas')



### Phoenix
### Extract species and orders for urban and regional assemblages
order_Phoenix_urban <- filter(bird_taxonomy_regional, grepl(paste(Phoenix_urban_filtering, collapse="|"), TipLabel))

order_Phoenix_regional <- filter(bird_taxonomy_regional, grepl(paste(Phoenix_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Phoenix_urban <- order_Phoenix_urban %>% count(IOCOrder)

number_species_order_Phoenix_regional <- order_Phoenix_regional %>% count(IOCOrder)

### Get proportions
prop_order_Phoenix_urban <- prop.table(number_species_order_Phoenix_urban$n)

prop_order_Phoenix_regional <- prop.table(number_species_order_Phoenix_regional$n)

### Add proportions
number_species_order_Phoenix_urban$prop <- cbind.data.frame(number_species_order_Phoenix_urban, prop_order_Phoenix_urban)

number_species_order_Phoenix_regional$prop <- cbind.data.frame(number_species_order_Phoenix_regional, prop_order_Phoenix_regional)

### Convert proportions into matrix
number_species_order_Phoenix_urban <- as.matrix(number_species_order_Phoenix_urban)
number_species_order_Phoenix_regional <- as.matrix(number_species_order_Phoenix_regional)

### Convert matrix into data frame
number_species_order_Phoenix_urban <- as.data.frame(number_species_order_Phoenix_urban)
number_species_order_Phoenix_regional <- as.data.frame(number_species_order_Phoenix_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Phoenix_urban_regional <- bind_rows(number_species_order_Phoenix_urban,anti_join(number_species_order_Phoenix_regional,number_species_order_Phoenix_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Phoenix_urban_regional <- number_species_order_Phoenix_urban_regional[order(number_species_order_Phoenix_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Phoenix_urban_regional$prop.prop_order_Phoenix_regional <- number_species_order_Phoenix_regional$prop.prop_order_Phoenix_regional

### Replace NA by ceros in the urban proportions
number_species_order_Phoenix_urban_regional[is.na(number_species_order_Phoenix_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Phoenix_urban_regional_2 <- bind_rows(number_species_order_Phoenix_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Phoenix_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Phoenix_urban_regional_2 <- number_species_order_Phoenix_urban_regional_2[order(number_species_order_Phoenix_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Phoenix_urban_regional_2[is.na(number_species_order_Phoenix_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Phoenix_urban_regional_3 <- data.frame(number_species_order_Phoenix_urban_regional_2[1],
                                                            number_species_order_Phoenix_urban_regional_2[5],
                                                            number_species_order_Phoenix_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Phoenix_urban_regional_3 <- setNames(number_species_order_Phoenix_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Phoenix_urban_regional_3$prop_urban <- as.numeric(number_species_order_Phoenix_urban_regional_3$prop_urban)
number_species_order_Phoenix_urban_regional_3$prop_regional <- as.numeric(number_species_order_Phoenix_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Phoenix_order_prop_vector <- (number_species_order_Phoenix_urban_regional_3$prop_urban - number_species_order_Phoenix_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Phoenix_order_prop_df <- cbind.data.frame(number_species_order_Phoenix_urban_regional_3[1],
                                          Phoenix_order_prop_vector)

### Change column names
colnames(Phoenix_order_prop_df) <- c('Order', 'Phoenix')



### Pila
### Extract species and orders for urban and regional assemblages
order_Pila_urban <- filter(bird_taxonomy_regional, grepl(paste(Pila_urban_filtering, collapse="|"), TipLabel))

order_Pila_regional <- filter(bird_taxonomy_regional, grepl(paste(Pila_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Pila_urban <- order_Pila_urban %>% count(IOCOrder)

number_species_order_Pila_regional <- order_Pila_regional %>% count(IOCOrder)

### Get proportions
prop_order_Pila_urban <- prop.table(number_species_order_Pila_urban$n)

prop_order_Pila_regional <- prop.table(number_species_order_Pila_regional$n)

### Add proportions
number_species_order_Pila_urban$prop <- cbind.data.frame(number_species_order_Pila_urban, prop_order_Pila_urban)

number_species_order_Pila_regional$prop <- cbind.data.frame(number_species_order_Pila_regional, prop_order_Pila_regional)

### Convert proportions into matrix
number_species_order_Pila_urban <- as.matrix(number_species_order_Pila_urban)
number_species_order_Pila_regional <- as.matrix(number_species_order_Pila_regional)

### Convert matrix into data frame
number_species_order_Pila_urban <- as.data.frame(number_species_order_Pila_urban)
number_species_order_Pila_regional <- as.data.frame(number_species_order_Pila_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Pila_urban_regional <- bind_rows(number_species_order_Pila_urban,anti_join(number_species_order_Pila_regional,number_species_order_Pila_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pila_urban_regional <- number_species_order_Pila_urban_regional[order(number_species_order_Pila_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Pila_urban_regional$prop.prop_order_Pila_regional <- number_species_order_Pila_regional$prop.prop_order_Pila_regional

### Replace NA by ceros in the urban proportions
number_species_order_Pila_urban_regional[is.na(number_species_order_Pila_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Pila_urban_regional_2 <- bind_rows(number_species_order_Pila_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Pila_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pila_urban_regional_2 <- number_species_order_Pila_urban_regional_2[order(number_species_order_Pila_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Pila_urban_regional_2[is.na(number_species_order_Pila_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Pila_urban_regional_3 <- data.frame(number_species_order_Pila_urban_regional_2[1],
                                                         number_species_order_Pila_urban_regional_2[5],
                                                         number_species_order_Pila_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Pila_urban_regional_3 <- setNames(number_species_order_Pila_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Pila_urban_regional_3$prop_urban <- as.numeric(number_species_order_Pila_urban_regional_3$prop_urban)
number_species_order_Pila_urban_regional_3$prop_regional <- as.numeric(number_species_order_Pila_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Pila_order_prop_vector <- (number_species_order_Pila_urban_regional_3$prop_urban - number_species_order_Pila_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Pila_order_prop_df <- cbind.data.frame(number_species_order_Pila_urban_regional_3[1],
                                       Pila_order_prop_vector)

### Change column names
colnames(Pila_order_prop_df) <- c('Order', 'Pila')



### Piotrkow_Trybunalski
### Extract species and orders for urban and regional assemblages
order_Piotrkow_Trybunalski_urban <- filter(bird_taxonomy_regional, grepl(paste(Piotrkow_Trybunalski_urban_filtering, collapse="|"), TipLabel))

order_Piotrkow_Trybunalski_regional <- filter(bird_taxonomy_regional, grepl(paste(Piotrkow_Trybunalski_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Piotrkow_Trybunalski_urban <- order_Piotrkow_Trybunalski_urban %>% count(IOCOrder)

number_species_order_Piotrkow_Trybunalski_regional <- order_Piotrkow_Trybunalski_regional %>% count(IOCOrder)

### Get proportions
prop_order_Piotrkow_Trybunalski_urban <- prop.table(number_species_order_Piotrkow_Trybunalski_urban$n)

prop_order_Piotrkow_Trybunalski_regional <- prop.table(number_species_order_Piotrkow_Trybunalski_regional$n)

### Add proportions
number_species_order_Piotrkow_Trybunalski_urban$prop <- cbind.data.frame(number_species_order_Piotrkow_Trybunalski_urban, prop_order_Piotrkow_Trybunalski_urban)

number_species_order_Piotrkow_Trybunalski_regional$prop <- cbind.data.frame(number_species_order_Piotrkow_Trybunalski_regional, prop_order_Piotrkow_Trybunalski_regional)

### Convert proportions into matrix
number_species_order_Piotrkow_Trybunalski_urban <- as.matrix(number_species_order_Piotrkow_Trybunalski_urban)
number_species_order_Piotrkow_Trybunalski_regional <- as.matrix(number_species_order_Piotrkow_Trybunalski_regional)

### Convert matrix into data frame
number_species_order_Piotrkow_Trybunalski_urban <- as.data.frame(number_species_order_Piotrkow_Trybunalski_urban)
number_species_order_Piotrkow_Trybunalski_regional <- as.data.frame(number_species_order_Piotrkow_Trybunalski_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Piotrkow_Trybunalski_urban_regional <- bind_rows(number_species_order_Piotrkow_Trybunalski_urban,anti_join(number_species_order_Piotrkow_Trybunalski_regional,number_species_order_Piotrkow_Trybunalski_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Piotrkow_Trybunalski_urban_regional <- number_species_order_Piotrkow_Trybunalski_urban_regional[order(number_species_order_Piotrkow_Trybunalski_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Piotrkow_Trybunalski_urban_regional$prop.prop_order_Piotrkow_Trybunalski_regional <- number_species_order_Piotrkow_Trybunalski_regional$prop.prop_order_Piotrkow_Trybunalski_regional

### Replace NA by ceros in the urban proportions
number_species_order_Piotrkow_Trybunalski_urban_regional[is.na(number_species_order_Piotrkow_Trybunalski_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Piotrkow_Trybunalski_urban_regional_2 <- bind_rows(number_species_order_Piotrkow_Trybunalski_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Piotrkow_Trybunalski_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Piotrkow_Trybunalski_urban_regional_2 <- number_species_order_Piotrkow_Trybunalski_urban_regional_2[order(number_species_order_Piotrkow_Trybunalski_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Piotrkow_Trybunalski_urban_regional_2[is.na(number_species_order_Piotrkow_Trybunalski_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Piotrkow_Trybunalski_urban_regional_3 <- data.frame(number_species_order_Piotrkow_Trybunalski_urban_regional_2[1],
                                                                         number_species_order_Piotrkow_Trybunalski_urban_regional_2[5],
                                                                         number_species_order_Piotrkow_Trybunalski_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Piotrkow_Trybunalski_urban_regional_3 <- setNames(number_species_order_Piotrkow_Trybunalski_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Piotrkow_Trybunalski_urban_regional_3$prop_urban <- as.numeric(number_species_order_Piotrkow_Trybunalski_urban_regional_3$prop_urban)
number_species_order_Piotrkow_Trybunalski_urban_regional_3$prop_regional <- as.numeric(number_species_order_Piotrkow_Trybunalski_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Piotrkow_Trybunalski_order_prop_vector <- (number_species_order_Piotrkow_Trybunalski_urban_regional_3$prop_urban - number_species_order_Piotrkow_Trybunalski_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Piotrkow_Trybunalski_order_prop_df <- cbind.data.frame(number_species_order_Piotrkow_Trybunalski_urban_regional_3[1],
                                                       Piotrkow_Trybunalski_order_prop_vector)

### Change column names
colnames(Piotrkow_Trybunalski_order_prop_df) <- c('Order', 'Piotrkow_Trybunalski')



### Poznan
### Extract species and orders for urban and regional assemblages
order_Poznan_urban <- filter(bird_taxonomy_regional, grepl(paste(Poznan_urban_filtering, collapse="|"), TipLabel))

order_Poznan_regional <- filter(bird_taxonomy_regional, grepl(paste(Poznan_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Poznan_urban <- order_Poznan_urban %>% count(IOCOrder)

number_species_order_Poznan_regional <- order_Poznan_regional %>% count(IOCOrder)

### Get proportions
prop_order_Poznan_urban <- prop.table(number_species_order_Poznan_urban$n)

prop_order_Poznan_regional <- prop.table(number_species_order_Poznan_regional$n)

### Add proportions
number_species_order_Poznan_urban$prop <- cbind.data.frame(number_species_order_Poznan_urban, prop_order_Poznan_urban)

number_species_order_Poznan_regional$prop <- cbind.data.frame(number_species_order_Poznan_regional, prop_order_Poznan_regional)

### Convert proportions into matrix
number_species_order_Poznan_urban <- as.matrix(number_species_order_Poznan_urban)
number_species_order_Poznan_regional <- as.matrix(number_species_order_Poznan_regional)

### Convert matrix into data frame
number_species_order_Poznan_urban <- as.data.frame(number_species_order_Poznan_urban)
number_species_order_Poznan_regional <- as.data.frame(number_species_order_Poznan_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Poznan_urban_regional <- bind_rows(number_species_order_Poznan_urban,anti_join(number_species_order_Poznan_regional,number_species_order_Poznan_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Poznan_urban_regional <- number_species_order_Poznan_urban_regional[order(number_species_order_Poznan_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Poznan_urban_regional$prop.prop_order_Poznan_regional <- number_species_order_Poznan_regional$prop.prop_order_Poznan_regional

### Replace NA by ceros in the urban proportions
number_species_order_Poznan_urban_regional[is.na(number_species_order_Poznan_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Poznan_urban_regional_2 <- bind_rows(number_species_order_Poznan_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Poznan_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Poznan_urban_regional_2 <- number_species_order_Poznan_urban_regional_2[order(number_species_order_Poznan_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Poznan_urban_regional_2[is.na(number_species_order_Poznan_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Poznan_urban_regional_3 <- data.frame(number_species_order_Poznan_urban_regional_2[1],
                                                           number_species_order_Poznan_urban_regional_2[5],
                                                           number_species_order_Poznan_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Poznan_urban_regional_3 <- setNames(number_species_order_Poznan_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Poznan_urban_regional_3$prop_urban <- as.numeric(number_species_order_Poznan_urban_regional_3$prop_urban)
number_species_order_Poznan_urban_regional_3$prop_regional <- as.numeric(number_species_order_Poznan_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Poznan_order_prop_vector <- (number_species_order_Poznan_urban_regional_3$prop_urban - number_species_order_Poznan_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Poznan_order_prop_df <- cbind.data.frame(number_species_order_Poznan_urban_regional_3[1],
                                         Poznan_order_prop_vector)

### Change column names
colnames(Poznan_order_prop_df) <- c('Order', 'Poznan')



### Pretoria
### Extract species and orders for urban and regional assemblages
order_Pretoria_urban <- filter(bird_taxonomy_regional, grepl(paste(Pretoria_urban_filtering, collapse="|"), TipLabel))

order_Pretoria_regional <- filter(bird_taxonomy_regional, grepl(paste(Pretoria_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Pretoria_urban <- order_Pretoria_urban %>% count(IOCOrder)

number_species_order_Pretoria_regional <- order_Pretoria_regional %>% count(IOCOrder)

### Get proportions
prop_order_Pretoria_urban <- prop.table(number_species_order_Pretoria_urban$n)

prop_order_Pretoria_regional <- prop.table(number_species_order_Pretoria_regional$n)

### Add proportions
number_species_order_Pretoria_urban$prop <- cbind.data.frame(number_species_order_Pretoria_urban, prop_order_Pretoria_urban)

number_species_order_Pretoria_regional$prop <- cbind.data.frame(number_species_order_Pretoria_regional, prop_order_Pretoria_regional)

### Convert proportions into matrix
number_species_order_Pretoria_urban <- as.matrix(number_species_order_Pretoria_urban)
number_species_order_Pretoria_regional <- as.matrix(number_species_order_Pretoria_regional)

### Convert matrix into data frame
number_species_order_Pretoria_urban <- as.data.frame(number_species_order_Pretoria_urban)
number_species_order_Pretoria_regional <- as.data.frame(number_species_order_Pretoria_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Pretoria_urban_regional <- bind_rows(number_species_order_Pretoria_urban,anti_join(number_species_order_Pretoria_regional,number_species_order_Pretoria_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pretoria_urban_regional <- number_species_order_Pretoria_urban_regional[order(number_species_order_Pretoria_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Pretoria_urban_regional$prop.prop_order_Pretoria_regional <- number_species_order_Pretoria_regional$prop.prop_order_Pretoria_regional

### Replace NA by ceros in the urban proportions
number_species_order_Pretoria_urban_regional[is.na(number_species_order_Pretoria_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Pretoria_urban_regional_2 <- bind_rows(number_species_order_Pretoria_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Pretoria_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pretoria_urban_regional_2 <- number_species_order_Pretoria_urban_regional_2[order(number_species_order_Pretoria_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Pretoria_urban_regional_2[is.na(number_species_order_Pretoria_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Pretoria_urban_regional_3 <- data.frame(number_species_order_Pretoria_urban_regional_2[1],
                                                             number_species_order_Pretoria_urban_regional_2[5],
                                                             number_species_order_Pretoria_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Pretoria_urban_regional_3 <- setNames(number_species_order_Pretoria_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Pretoria_urban_regional_3$prop_urban <- as.numeric(number_species_order_Pretoria_urban_regional_3$prop_urban)
number_species_order_Pretoria_urban_regional_3$prop_regional <- as.numeric(number_species_order_Pretoria_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Pretoria_order_prop_vector <- (number_species_order_Pretoria_urban_regional_3$prop_urban - number_species_order_Pretoria_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Pretoria_order_prop_df <- cbind.data.frame(number_species_order_Pretoria_urban_regional_3[1],
                                           Pretoria_order_prop_vector)

### Change column names
colnames(Pretoria_order_prop_df) <- c('Order', 'Pretoria')



### Przemysl
### Extract species and orders for urban and regional assemblages
order_Przemysl_urban <- filter(bird_taxonomy_regional, grepl(paste(Przemysl_urban_filtering, collapse="|"), TipLabel))

order_Przemysl_regional <- filter(bird_taxonomy_regional, grepl(paste(Przemysl_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Przemysl_urban <- order_Przemysl_urban %>% count(IOCOrder)

number_species_order_Przemysl_regional <- order_Przemysl_regional %>% count(IOCOrder)

### Get proportions
prop_order_Przemysl_urban <- prop.table(number_species_order_Przemysl_urban$n)

prop_order_Przemysl_regional <- prop.table(number_species_order_Przemysl_regional$n)

### Add proportions
number_species_order_Przemysl_urban$prop <- cbind.data.frame(number_species_order_Przemysl_urban, prop_order_Przemysl_urban)

number_species_order_Przemysl_regional$prop <- cbind.data.frame(number_species_order_Przemysl_regional, prop_order_Przemysl_regional)

### Convert proportions into matrix
number_species_order_Przemysl_urban <- as.matrix(number_species_order_Przemysl_urban)
number_species_order_Przemysl_regional <- as.matrix(number_species_order_Przemysl_regional)

### Convert matrix into data frame
number_species_order_Przemysl_urban <- as.data.frame(number_species_order_Przemysl_urban)
number_species_order_Przemysl_regional <- as.data.frame(number_species_order_Przemysl_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Przemysl_urban_regional <- bind_rows(number_species_order_Przemysl_urban,anti_join(number_species_order_Przemysl_regional,number_species_order_Przemysl_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Przemysl_urban_regional <- number_species_order_Przemysl_urban_regional[order(number_species_order_Przemysl_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Przemysl_urban_regional$prop.prop_order_Przemysl_regional <- number_species_order_Przemysl_regional$prop.prop_order_Przemysl_regional

### Replace NA by ceros in the urban proportions
number_species_order_Przemysl_urban_regional[is.na(number_species_order_Przemysl_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Przemysl_urban_regional_2 <- bind_rows(number_species_order_Przemysl_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Przemysl_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Przemysl_urban_regional_2 <- number_species_order_Przemysl_urban_regional_2[order(number_species_order_Przemysl_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Przemysl_urban_regional_2[is.na(number_species_order_Przemysl_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Przemysl_urban_regional_3 <- data.frame(number_species_order_Przemysl_urban_regional_2[1],
                                                             number_species_order_Przemysl_urban_regional_2[5],
                                                             number_species_order_Przemysl_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Przemysl_urban_regional_3 <- setNames(number_species_order_Przemysl_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Przemysl_urban_regional_3$prop_urban <- as.numeric(number_species_order_Przemysl_urban_regional_3$prop_urban)
number_species_order_Przemysl_urban_regional_3$prop_regional <- as.numeric(number_species_order_Przemysl_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Przemysl_order_prop_vector <- (number_species_order_Przemysl_urban_regional_3$prop_urban - number_species_order_Przemysl_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Przemysl_order_prop_df <- cbind.data.frame(number_species_order_Przemysl_urban_regional_3[1],
                                           Przemysl_order_prop_vector)

### Change column names
colnames(Przemysl_order_prop_df) <- c('Order', 'Przemysl')



### Pyhasalmi
### Extract species and orders for urban and regional assemblages
order_Pyhasalmi_urban <- filter(bird_taxonomy_regional, grepl(paste(Pyhasalmi_urban_filtering, collapse="|"), TipLabel))

order_Pyhasalmi_regional <- filter(bird_taxonomy_regional, grepl(paste(Pyhasalmi_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Pyhasalmi_urban <- order_Pyhasalmi_urban %>% count(IOCOrder)

number_species_order_Pyhasalmi_regional <- order_Pyhasalmi_regional %>% count(IOCOrder)

### Get proportions
prop_order_Pyhasalmi_urban <- prop.table(number_species_order_Pyhasalmi_urban$n)

prop_order_Pyhasalmi_regional <- prop.table(number_species_order_Pyhasalmi_regional$n)

### Add proportions
number_species_order_Pyhasalmi_urban$prop <- cbind.data.frame(number_species_order_Pyhasalmi_urban, prop_order_Pyhasalmi_urban)

number_species_order_Pyhasalmi_regional$prop <- cbind.data.frame(number_species_order_Pyhasalmi_regional, prop_order_Pyhasalmi_regional)

### Convert proportions into matrix
number_species_order_Pyhasalmi_urban <- as.matrix(number_species_order_Pyhasalmi_urban)
number_species_order_Pyhasalmi_regional <- as.matrix(number_species_order_Pyhasalmi_regional)

### Convert matrix into data frame
number_species_order_Pyhasalmi_urban <- as.data.frame(number_species_order_Pyhasalmi_urban)
number_species_order_Pyhasalmi_regional <- as.data.frame(number_species_order_Pyhasalmi_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Pyhasalmi_urban_regional <- bind_rows(number_species_order_Pyhasalmi_urban,anti_join(number_species_order_Pyhasalmi_regional,number_species_order_Pyhasalmi_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pyhasalmi_urban_regional <- number_species_order_Pyhasalmi_urban_regional[order(number_species_order_Pyhasalmi_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Pyhasalmi_urban_regional$prop.prop_order_Pyhasalmi_regional <- number_species_order_Pyhasalmi_regional$prop.prop_order_Pyhasalmi_regional

### Replace NA by ceros in the urban proportions
number_species_order_Pyhasalmi_urban_regional[is.na(number_species_order_Pyhasalmi_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Pyhasalmi_urban_regional_2 <- bind_rows(number_species_order_Pyhasalmi_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Pyhasalmi_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Pyhasalmi_urban_regional_2 <- number_species_order_Pyhasalmi_urban_regional_2[order(number_species_order_Pyhasalmi_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Pyhasalmi_urban_regional_2[is.na(number_species_order_Pyhasalmi_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Pyhasalmi_urban_regional_3 <- data.frame(number_species_order_Pyhasalmi_urban_regional_2[1],
                                                              number_species_order_Pyhasalmi_urban_regional_2[5],
                                                              number_species_order_Pyhasalmi_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Pyhasalmi_urban_regional_3 <- setNames(number_species_order_Pyhasalmi_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Pyhasalmi_urban_regional_3$prop_urban <- as.numeric(number_species_order_Pyhasalmi_urban_regional_3$prop_urban)
number_species_order_Pyhasalmi_urban_regional_3$prop_regional <- as.numeric(number_species_order_Pyhasalmi_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Pyhasalmi_order_prop_vector <- (number_species_order_Pyhasalmi_urban_regional_3$prop_urban - number_species_order_Pyhasalmi_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Pyhasalmi_order_prop_df <- cbind.data.frame(number_species_order_Pyhasalmi_urban_regional_3[1],
                                            Pyhasalmi_order_prop_vector)

### Change column names
colnames(Pyhasalmi_order_prop_df) <- c('Order', 'Pyhasalmi')



### Quezon_City
### Extract species and orders for urban and regional assemblages
order_Quezon_City_urban <- filter(bird_taxonomy_regional, grepl(paste(Quezon_City_urban_filtering, collapse="|"), TipLabel))

order_Quezon_City_regional <- filter(bird_taxonomy_regional, grepl(paste(Quezon_City_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Quezon_City_urban <- order_Quezon_City_urban %>% count(IOCOrder)

number_species_order_Quezon_City_regional <- order_Quezon_City_regional %>% count(IOCOrder)

### Get proportions
prop_order_Quezon_City_urban <- prop.table(number_species_order_Quezon_City_urban$n)

prop_order_Quezon_City_regional <- prop.table(number_species_order_Quezon_City_regional$n)

### Add proportions
number_species_order_Quezon_City_urban$prop <- cbind.data.frame(number_species_order_Quezon_City_urban, prop_order_Quezon_City_urban)

number_species_order_Quezon_City_regional$prop <- cbind.data.frame(number_species_order_Quezon_City_regional, prop_order_Quezon_City_regional)

### Convert proportions into matrix
number_species_order_Quezon_City_urban <- as.matrix(number_species_order_Quezon_City_urban)
number_species_order_Quezon_City_regional <- as.matrix(number_species_order_Quezon_City_regional)

### Convert matrix into data frame
number_species_order_Quezon_City_urban <- as.data.frame(number_species_order_Quezon_City_urban)
number_species_order_Quezon_City_regional <- as.data.frame(number_species_order_Quezon_City_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Quezon_City_urban_regional <- bind_rows(number_species_order_Quezon_City_urban,anti_join(number_species_order_Quezon_City_regional,number_species_order_Quezon_City_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Quezon_City_urban_regional <- number_species_order_Quezon_City_urban_regional[order(number_species_order_Quezon_City_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Quezon_City_urban_regional$prop.prop_order_Quezon_City_regional <- number_species_order_Quezon_City_regional$prop.prop_order_Quezon_City_regional

### Replace NA by ceros in the urban proportions
number_species_order_Quezon_City_urban_regional[is.na(number_species_order_Quezon_City_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Quezon_City_urban_regional_2 <- bind_rows(number_species_order_Quezon_City_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Quezon_City_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Quezon_City_urban_regional_2 <- number_species_order_Quezon_City_urban_regional_2[order(number_species_order_Quezon_City_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Quezon_City_urban_regional_2[is.na(number_species_order_Quezon_City_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Quezon_City_urban_regional_3 <- data.frame(number_species_order_Quezon_City_urban_regional_2[1],
                                                                number_species_order_Quezon_City_urban_regional_2[5],
                                                                number_species_order_Quezon_City_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Quezon_City_urban_regional_3 <- setNames(number_species_order_Quezon_City_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Quezon_City_urban_regional_3$prop_urban <- as.numeric(number_species_order_Quezon_City_urban_regional_3$prop_urban)
number_species_order_Quezon_City_urban_regional_3$prop_regional <- as.numeric(number_species_order_Quezon_City_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Quezon_City_order_prop_vector <- (number_species_order_Quezon_City_urban_regional_3$prop_urban - number_species_order_Quezon_City_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Quezon_City_order_prop_df <- cbind.data.frame(number_species_order_Quezon_City_urban_regional_3[1],
                                              Quezon_City_order_prop_vector)

### Change column names
colnames(Quezon_City_order_prop_df) <- c('Order', 'Quezon_City')



### Raisio
### Extract species and orders for urban and regional assemblages
order_Raisio_urban <- filter(bird_taxonomy_regional, grepl(paste(Raisio_urban_filtering, collapse="|"), TipLabel))

order_Raisio_regional <- filter(bird_taxonomy_regional, grepl(paste(Raisio_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Raisio_urban <- order_Raisio_urban %>% count(IOCOrder)

number_species_order_Raisio_regional <- order_Raisio_regional %>% count(IOCOrder)

### Get proportions
prop_order_Raisio_urban <- prop.table(number_species_order_Raisio_urban$n)

prop_order_Raisio_regional <- prop.table(number_species_order_Raisio_regional$n)

### Add proportions
number_species_order_Raisio_urban$prop <- cbind.data.frame(number_species_order_Raisio_urban, prop_order_Raisio_urban)

number_species_order_Raisio_regional$prop <- cbind.data.frame(number_species_order_Raisio_regional, prop_order_Raisio_regional)

### Convert proportions into matrix
number_species_order_Raisio_urban <- as.matrix(number_species_order_Raisio_urban)
number_species_order_Raisio_regional <- as.matrix(number_species_order_Raisio_regional)

### Convert matrix into data frame
number_species_order_Raisio_urban <- as.data.frame(number_species_order_Raisio_urban)
number_species_order_Raisio_regional <- as.data.frame(number_species_order_Raisio_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Raisio_urban_regional <- bind_rows(number_species_order_Raisio_urban,anti_join(number_species_order_Raisio_regional,number_species_order_Raisio_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Raisio_urban_regional <- number_species_order_Raisio_urban_regional[order(number_species_order_Raisio_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Raisio_urban_regional$prop.prop_order_Raisio_regional <- number_species_order_Raisio_regional$prop.prop_order_Raisio_regional

### Replace NA by ceros in the urban proportions
number_species_order_Raisio_urban_regional[is.na(number_species_order_Raisio_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Raisio_urban_regional_2 <- bind_rows(number_species_order_Raisio_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Raisio_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Raisio_urban_regional_2 <- number_species_order_Raisio_urban_regional_2[order(number_species_order_Raisio_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Raisio_urban_regional_2[is.na(number_species_order_Raisio_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Raisio_urban_regional_3 <- data.frame(number_species_order_Raisio_urban_regional_2[1],
                                                           number_species_order_Raisio_urban_regional_2[5],
                                                           number_species_order_Raisio_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Raisio_urban_regional_3 <- setNames(number_species_order_Raisio_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Raisio_urban_regional_3$prop_urban <- as.numeric(number_species_order_Raisio_urban_regional_3$prop_urban)
number_species_order_Raisio_urban_regional_3$prop_regional <- as.numeric(number_species_order_Raisio_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Raisio_order_prop_vector <- (number_species_order_Raisio_urban_regional_3$prop_urban - number_species_order_Raisio_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Raisio_order_prop_df <- cbind.data.frame(number_species_order_Raisio_urban_regional_3[1],
                                         Raisio_order_prop_vector)

### Change column names
colnames(Raisio_order_prop_df) <- c('Order', 'Raisio')



### Rovaniemi
### Extract species and orders for urban and regional assemblages
order_Rovaniemi_urban <- filter(bird_taxonomy_regional, grepl(paste(Rovaniemi_urban_filtering, collapse="|"), TipLabel))

order_Rovaniemi_regional <- filter(bird_taxonomy_regional, grepl(paste(Rovaniemi_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Rovaniemi_urban <- order_Rovaniemi_urban %>% count(IOCOrder)

number_species_order_Rovaniemi_regional <- order_Rovaniemi_regional %>% count(IOCOrder)

### Get proportions
prop_order_Rovaniemi_urban <- prop.table(number_species_order_Rovaniemi_urban$n)

prop_order_Rovaniemi_regional <- prop.table(number_species_order_Rovaniemi_regional$n)

### Add proportions
number_species_order_Rovaniemi_urban$prop <- cbind.data.frame(number_species_order_Rovaniemi_urban, prop_order_Rovaniemi_urban)

number_species_order_Rovaniemi_regional$prop <- cbind.data.frame(number_species_order_Rovaniemi_regional, prop_order_Rovaniemi_regional)

### Convert proportions into matrix
number_species_order_Rovaniemi_urban <- as.matrix(number_species_order_Rovaniemi_urban)
number_species_order_Rovaniemi_regional <- as.matrix(number_species_order_Rovaniemi_regional)

### Convert matrix into data frame
number_species_order_Rovaniemi_urban <- as.data.frame(number_species_order_Rovaniemi_urban)
number_species_order_Rovaniemi_regional <- as.data.frame(number_species_order_Rovaniemi_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Rovaniemi_urban_regional <- bind_rows(number_species_order_Rovaniemi_urban,anti_join(number_species_order_Rovaniemi_regional,number_species_order_Rovaniemi_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Rovaniemi_urban_regional <- number_species_order_Rovaniemi_urban_regional[order(number_species_order_Rovaniemi_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Rovaniemi_urban_regional$prop.prop_order_Rovaniemi_regional <- number_species_order_Rovaniemi_regional$prop.prop_order_Rovaniemi_regional

### Replace NA by ceros in the urban proportions
number_species_order_Rovaniemi_urban_regional[is.na(number_species_order_Rovaniemi_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Rovaniemi_urban_regional_2 <- bind_rows(number_species_order_Rovaniemi_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Rovaniemi_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Rovaniemi_urban_regional_2 <- number_species_order_Rovaniemi_urban_regional_2[order(number_species_order_Rovaniemi_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Rovaniemi_urban_regional_2[is.na(number_species_order_Rovaniemi_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Rovaniemi_urban_regional_3 <- data.frame(number_species_order_Rovaniemi_urban_regional_2[1],
                                                              number_species_order_Rovaniemi_urban_regional_2[5],
                                                              number_species_order_Rovaniemi_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Rovaniemi_urban_regional_3 <- setNames(number_species_order_Rovaniemi_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Rovaniemi_urban_regional_3$prop_urban <- as.numeric(number_species_order_Rovaniemi_urban_regional_3$prop_urban)
number_species_order_Rovaniemi_urban_regional_3$prop_regional <- as.numeric(number_species_order_Rovaniemi_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Rovaniemi_order_prop_vector <- (number_species_order_Rovaniemi_urban_regional_3$prop_urban - number_species_order_Rovaniemi_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Rovaniemi_order_prop_df <- cbind.data.frame(number_species_order_Rovaniemi_urban_regional_3[1],
                                            Rovaniemi_order_prop_vector)

### Change column names
colnames(Rovaniemi_order_prop_df) <- c('Order', 'Rovaniemi')



### Rzeszow
### Extract species and orders for urban and regional assemblages
order_Rzeszow_urban <- filter(bird_taxonomy_regional, grepl(paste(Rzeszow_urban_filtering, collapse="|"), TipLabel))

order_Rzeszow_regional <- filter(bird_taxonomy_regional, grepl(paste(Rzeszow_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Rzeszow_urban <- order_Rzeszow_urban %>% count(IOCOrder)

number_species_order_Rzeszow_regional <- order_Rzeszow_regional %>% count(IOCOrder)

### Get proportions
prop_order_Rzeszow_urban <- prop.table(number_species_order_Rzeszow_urban$n)

prop_order_Rzeszow_regional <- prop.table(number_species_order_Rzeszow_regional$n)

### Add proportions
number_species_order_Rzeszow_urban$prop <- cbind.data.frame(number_species_order_Rzeszow_urban, prop_order_Rzeszow_urban)

number_species_order_Rzeszow_regional$prop <- cbind.data.frame(number_species_order_Rzeszow_regional, prop_order_Rzeszow_regional)

### Convert proportions into matrix
number_species_order_Rzeszow_urban <- as.matrix(number_species_order_Rzeszow_urban)
number_species_order_Rzeszow_regional <- as.matrix(number_species_order_Rzeszow_regional)

### Convert matrix into data frame
number_species_order_Rzeszow_urban <- as.data.frame(number_species_order_Rzeszow_urban)
number_species_order_Rzeszow_regional <- as.data.frame(number_species_order_Rzeszow_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Rzeszow_urban_regional <- bind_rows(number_species_order_Rzeszow_urban,anti_join(number_species_order_Rzeszow_regional,number_species_order_Rzeszow_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Rzeszow_urban_regional <- number_species_order_Rzeszow_urban_regional[order(number_species_order_Rzeszow_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Rzeszow_urban_regional$prop.prop_order_Rzeszow_regional <- number_species_order_Rzeszow_regional$prop.prop_order_Rzeszow_regional

### Replace NA by ceros in the urban proportions
number_species_order_Rzeszow_urban_regional[is.na(number_species_order_Rzeszow_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Rzeszow_urban_regional_2 <- bind_rows(number_species_order_Rzeszow_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Rzeszow_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Rzeszow_urban_regional_2 <- number_species_order_Rzeszow_urban_regional_2[order(number_species_order_Rzeszow_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Rzeszow_urban_regional_2[is.na(number_species_order_Rzeszow_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Rzeszow_urban_regional_3 <- data.frame(number_species_order_Rzeszow_urban_regional_2[1],
                                                            number_species_order_Rzeszow_urban_regional_2[5],
                                                            number_species_order_Rzeszow_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Rzeszow_urban_regional_3 <- setNames(number_species_order_Rzeszow_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Rzeszow_urban_regional_3$prop_urban <- as.numeric(number_species_order_Rzeszow_urban_regional_3$prop_urban)
number_species_order_Rzeszow_urban_regional_3$prop_regional <- as.numeric(number_species_order_Rzeszow_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Rzeszow_order_prop_vector <- (number_species_order_Rzeszow_urban_regional_3$prop_urban - number_species_order_Rzeszow_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Rzeszow_order_prop_df <- cbind.data.frame(number_species_order_Rzeszow_urban_regional_3[1],
                                          Rzeszow_order_prop_vector)

### Change column names
colnames(Rzeszow_order_prop_df) <- c('Order', 'Rzeszow')



### Salo
### Extract species and orders for urban and regional assemblages
order_Salo_urban <- filter(bird_taxonomy_regional, grepl(paste(Salo_urban_filtering, collapse="|"), TipLabel))

order_Salo_regional <- filter(bird_taxonomy_regional, grepl(paste(Salo_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Salo_urban <- order_Salo_urban %>% count(IOCOrder)

number_species_order_Salo_regional <- order_Salo_regional %>% count(IOCOrder)

### Get proportions
prop_order_Salo_urban <- prop.table(number_species_order_Salo_urban$n)

prop_order_Salo_regional <- prop.table(number_species_order_Salo_regional$n)

### Add proportions
number_species_order_Salo_urban$prop <- cbind.data.frame(number_species_order_Salo_urban, prop_order_Salo_urban)

number_species_order_Salo_regional$prop <- cbind.data.frame(number_species_order_Salo_regional, prop_order_Salo_regional)

### Convert proportions into matrix
number_species_order_Salo_urban <- as.matrix(number_species_order_Salo_urban)
number_species_order_Salo_regional <- as.matrix(number_species_order_Salo_regional)

### Convert matrix into data frame
number_species_order_Salo_urban <- as.data.frame(number_species_order_Salo_urban)
number_species_order_Salo_regional <- as.data.frame(number_species_order_Salo_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Salo_urban_regional <- bind_rows(number_species_order_Salo_urban,anti_join(number_species_order_Salo_regional,number_species_order_Salo_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Salo_urban_regional <- number_species_order_Salo_urban_regional[order(number_species_order_Salo_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Salo_urban_regional$prop.prop_order_Salo_regional <- number_species_order_Salo_regional$prop.prop_order_Salo_regional

### Replace NA by ceros in the urban proportions
number_species_order_Salo_urban_regional[is.na(number_species_order_Salo_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Salo_urban_regional_2 <- bind_rows(number_species_order_Salo_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Salo_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Salo_urban_regional_2 <- number_species_order_Salo_urban_regional_2[order(number_species_order_Salo_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Salo_urban_regional_2[is.na(number_species_order_Salo_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Salo_urban_regional_3 <- data.frame(number_species_order_Salo_urban_regional_2[1],
                                                         number_species_order_Salo_urban_regional_2[5],
                                                         number_species_order_Salo_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Salo_urban_regional_3 <- setNames(number_species_order_Salo_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Salo_urban_regional_3$prop_urban <- as.numeric(number_species_order_Salo_urban_regional_3$prop_urban)
number_species_order_Salo_urban_regional_3$prop_regional <- as.numeric(number_species_order_Salo_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Salo_order_prop_vector <- (number_species_order_Salo_urban_regional_3$prop_urban - number_species_order_Salo_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Salo_order_prop_df <- cbind.data.frame(number_species_order_Salo_urban_regional_3[1],
                                       Salo_order_prop_vector)

### Change column names
colnames(Salo_order_prop_df) <- c('Order', 'Salo')



### Santiago
### Extract species and orders for urban and regional assemblages
order_Santiago_urban <- filter(bird_taxonomy_regional, grepl(paste(Santiago_urban_filtering, collapse="|"), TipLabel))

order_Santiago_regional <- filter(bird_taxonomy_regional, grepl(paste(Santiago_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Santiago_urban <- order_Santiago_urban %>% count(IOCOrder)

number_species_order_Santiago_regional <- order_Santiago_regional %>% count(IOCOrder)

### Get proportions
prop_order_Santiago_urban <- prop.table(number_species_order_Santiago_urban$n)

prop_order_Santiago_regional <- prop.table(number_species_order_Santiago_regional$n)

### Add proportions
number_species_order_Santiago_urban$prop <- cbind.data.frame(number_species_order_Santiago_urban, prop_order_Santiago_urban)

number_species_order_Santiago_regional$prop <- cbind.data.frame(number_species_order_Santiago_regional, prop_order_Santiago_regional)

### Convert proportions into matrix
number_species_order_Santiago_urban <- as.matrix(number_species_order_Santiago_urban)
number_species_order_Santiago_regional <- as.matrix(number_species_order_Santiago_regional)

### Convert matrix into data frame
number_species_order_Santiago_urban <- as.data.frame(number_species_order_Santiago_urban)
number_species_order_Santiago_regional <- as.data.frame(number_species_order_Santiago_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Santiago_urban_regional <- bind_rows(number_species_order_Santiago_urban,anti_join(number_species_order_Santiago_regional,number_species_order_Santiago_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Santiago_urban_regional <- number_species_order_Santiago_urban_regional[order(number_species_order_Santiago_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Santiago_urban_regional$prop.prop_order_Santiago_regional <- number_species_order_Santiago_regional$prop.prop_order_Santiago_regional

### Replace NA by ceros in the urban proportions
number_species_order_Santiago_urban_regional[is.na(number_species_order_Santiago_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Santiago_urban_regional_2 <- bind_rows(number_species_order_Santiago_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Santiago_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Santiago_urban_regional_2 <- number_species_order_Santiago_urban_regional_2[order(number_species_order_Santiago_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Santiago_urban_regional_2[is.na(number_species_order_Santiago_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Santiago_urban_regional_3 <- data.frame(number_species_order_Santiago_urban_regional_2[1],
                                                             number_species_order_Santiago_urban_regional_2[5],
                                                             number_species_order_Santiago_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Santiago_urban_regional_3 <- setNames(number_species_order_Santiago_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Santiago_urban_regional_3$prop_urban <- as.numeric(number_species_order_Santiago_urban_regional_3$prop_urban)
number_species_order_Santiago_urban_regional_3$prop_regional <- as.numeric(number_species_order_Santiago_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Santiago_order_prop_vector <- (number_species_order_Santiago_urban_regional_3$prop_urban - number_species_order_Santiago_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Santiago_order_prop_df <- cbind.data.frame(number_species_order_Santiago_urban_regional_3[1],
                                           Santiago_order_prop_vector)

### Change column names
colnames(Santiago_order_prop_df) <- c('Order', 'Santiago')



### Siedlce
### Extract species and orders for urban and regional assemblages
order_Siedlce_urban <- filter(bird_taxonomy_regional, grepl(paste(Siedlce_urban_filtering, collapse="|"), TipLabel))

order_Siedlce_regional <- filter(bird_taxonomy_regional, grepl(paste(Siedlce_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Siedlce_urban <- order_Siedlce_urban %>% count(IOCOrder)

number_species_order_Siedlce_regional <- order_Siedlce_regional %>% count(IOCOrder)

### Get proportions
prop_order_Siedlce_urban <- prop.table(number_species_order_Siedlce_urban$n)

prop_order_Siedlce_regional <- prop.table(number_species_order_Siedlce_regional$n)

### Add proportions
number_species_order_Siedlce_urban$prop <- cbind.data.frame(number_species_order_Siedlce_urban, prop_order_Siedlce_urban)

number_species_order_Siedlce_regional$prop <- cbind.data.frame(number_species_order_Siedlce_regional, prop_order_Siedlce_regional)

### Convert proportions into matrix
number_species_order_Siedlce_urban <- as.matrix(number_species_order_Siedlce_urban)
number_species_order_Siedlce_regional <- as.matrix(number_species_order_Siedlce_regional)

### Convert matrix into data frame
number_species_order_Siedlce_urban <- as.data.frame(number_species_order_Siedlce_urban)
number_species_order_Siedlce_regional <- as.data.frame(number_species_order_Siedlce_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Siedlce_urban_regional <- bind_rows(number_species_order_Siedlce_urban,anti_join(number_species_order_Siedlce_regional,number_species_order_Siedlce_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Siedlce_urban_regional <- number_species_order_Siedlce_urban_regional[order(number_species_order_Siedlce_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Siedlce_urban_regional$prop.prop_order_Siedlce_regional <- number_species_order_Siedlce_regional$prop.prop_order_Siedlce_regional

### Replace NA by ceros in the urban proportions
number_species_order_Siedlce_urban_regional[is.na(number_species_order_Siedlce_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Siedlce_urban_regional_2 <- bind_rows(number_species_order_Siedlce_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Siedlce_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Siedlce_urban_regional_2 <- number_species_order_Siedlce_urban_regional_2[order(number_species_order_Siedlce_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Siedlce_urban_regional_2[is.na(number_species_order_Siedlce_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Siedlce_urban_regional_3 <- data.frame(number_species_order_Siedlce_urban_regional_2[1],
                                                            number_species_order_Siedlce_urban_regional_2[5],
                                                            number_species_order_Siedlce_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Siedlce_urban_regional_3 <- setNames(number_species_order_Siedlce_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Siedlce_urban_regional_3$prop_urban <- as.numeric(number_species_order_Siedlce_urban_regional_3$prop_urban)
number_species_order_Siedlce_urban_regional_3$prop_regional <- as.numeric(number_species_order_Siedlce_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Siedlce_order_prop_vector <- (number_species_order_Siedlce_urban_regional_3$prop_urban - number_species_order_Siedlce_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Siedlce_order_prop_df <- cbind.data.frame(number_species_order_Siedlce_urban_regional_3[1],
                                          Siedlce_order_prop_vector)

### Change column names
colnames(Siedlce_order_prop_df) <- c('Order', 'Siedlce')



### Slupsk
### Extract species and orders for urban and regional assemblages
order_Slupsk_urban <- filter(bird_taxonomy_regional, grepl(paste(Slupsk_urban_filtering, collapse="|"), TipLabel))

order_Slupsk_regional <- filter(bird_taxonomy_regional, grepl(paste(Slupsk_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Slupsk_urban <- order_Slupsk_urban %>% count(IOCOrder)

number_species_order_Slupsk_regional <- order_Slupsk_regional %>% count(IOCOrder)

### Get proportions
prop_order_Slupsk_urban <- prop.table(number_species_order_Slupsk_urban$n)

prop_order_Slupsk_regional <- prop.table(number_species_order_Slupsk_regional$n)

### Add proportions
number_species_order_Slupsk_urban$prop <- cbind.data.frame(number_species_order_Slupsk_urban, prop_order_Slupsk_urban)

number_species_order_Slupsk_regional$prop <- cbind.data.frame(number_species_order_Slupsk_regional, prop_order_Slupsk_regional)

### Convert proportions into matrix
number_species_order_Slupsk_urban <- as.matrix(number_species_order_Slupsk_urban)
number_species_order_Slupsk_regional <- as.matrix(number_species_order_Slupsk_regional)

### Convert matrix into data frame
number_species_order_Slupsk_urban <- as.data.frame(number_species_order_Slupsk_urban)
number_species_order_Slupsk_regional <- as.data.frame(number_species_order_Slupsk_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Slupsk_urban_regional <- bind_rows(number_species_order_Slupsk_urban,anti_join(number_species_order_Slupsk_regional,number_species_order_Slupsk_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Slupsk_urban_regional <- number_species_order_Slupsk_urban_regional[order(number_species_order_Slupsk_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Slupsk_urban_regional$prop.prop_order_Slupsk_regional <- number_species_order_Slupsk_regional$prop.prop_order_Slupsk_regional

### Replace NA by ceros in the urban proportions
number_species_order_Slupsk_urban_regional[is.na(number_species_order_Slupsk_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Slupsk_urban_regional_2 <- bind_rows(number_species_order_Slupsk_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Slupsk_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Slupsk_urban_regional_2 <- number_species_order_Slupsk_urban_regional_2[order(number_species_order_Slupsk_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Slupsk_urban_regional_2[is.na(number_species_order_Slupsk_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Slupsk_urban_regional_3 <- data.frame(number_species_order_Slupsk_urban_regional_2[1],
                                                           number_species_order_Slupsk_urban_regional_2[5],
                                                           number_species_order_Slupsk_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Slupsk_urban_regional_3 <- setNames(number_species_order_Slupsk_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Slupsk_urban_regional_3$prop_urban <- as.numeric(number_species_order_Slupsk_urban_regional_3$prop_urban)
number_species_order_Slupsk_urban_regional_3$prop_regional <- as.numeric(number_species_order_Slupsk_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Slupsk_order_prop_vector <- (number_species_order_Slupsk_urban_regional_3$prop_urban - number_species_order_Slupsk_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Slupsk_order_prop_df <- cbind.data.frame(number_species_order_Slupsk_urban_regional_3[1],
                                         Slupsk_order_prop_vector)

### Change column names
colnames(Slupsk_order_prop_df) <- c('Order', 'Slupsk')



### Sofia
### Extract species and orders for urban and regional assemblages
order_Sofia_urban <- filter(bird_taxonomy_regional, grepl(paste(Sofia_urban_filtering, collapse="|"), TipLabel))

order_Sofia_regional <- filter(bird_taxonomy_regional, grepl(paste(Sofia_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Sofia_urban <- order_Sofia_urban %>% count(IOCOrder)

number_species_order_Sofia_regional <- order_Sofia_regional %>% count(IOCOrder)

### Get proportions
prop_order_Sofia_urban <- prop.table(number_species_order_Sofia_urban$n)

prop_order_Sofia_regional <- prop.table(number_species_order_Sofia_regional$n)

### Add proportions
number_species_order_Sofia_urban$prop <- cbind.data.frame(number_species_order_Sofia_urban, prop_order_Sofia_urban)

number_species_order_Sofia_regional$prop <- cbind.data.frame(number_species_order_Sofia_regional, prop_order_Sofia_regional)

### Convert proportions into matrix
number_species_order_Sofia_urban <- as.matrix(number_species_order_Sofia_urban)
number_species_order_Sofia_regional <- as.matrix(number_species_order_Sofia_regional)

### Convert matrix into data frame
number_species_order_Sofia_urban <- as.data.frame(number_species_order_Sofia_urban)
number_species_order_Sofia_regional <- as.data.frame(number_species_order_Sofia_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Sofia_urban_regional <- bind_rows(number_species_order_Sofia_urban,anti_join(number_species_order_Sofia_regional,number_species_order_Sofia_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Sofia_urban_regional <- number_species_order_Sofia_urban_regional[order(number_species_order_Sofia_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Sofia_urban_regional$prop.prop_order_Sofia_regional <- number_species_order_Sofia_regional$prop.prop_order_Sofia_regional

### Replace NA by ceros in the urban proportions
number_species_order_Sofia_urban_regional[is.na(number_species_order_Sofia_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Sofia_urban_regional_2 <- bind_rows(number_species_order_Sofia_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Sofia_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Sofia_urban_regional_2 <- number_species_order_Sofia_urban_regional_2[order(number_species_order_Sofia_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Sofia_urban_regional_2[is.na(number_species_order_Sofia_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Sofia_urban_regional_3 <- data.frame(number_species_order_Sofia_urban_regional_2[1],
                                                          number_species_order_Sofia_urban_regional_2[5],
                                                          number_species_order_Sofia_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Sofia_urban_regional_3 <- setNames(number_species_order_Sofia_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Sofia_urban_regional_3$prop_urban <- as.numeric(number_species_order_Sofia_urban_regional_3$prop_urban)
number_species_order_Sofia_urban_regional_3$prop_regional <- as.numeric(number_species_order_Sofia_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Sofia_order_prop_vector <- (number_species_order_Sofia_urban_regional_3$prop_urban - number_species_order_Sofia_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Sofia_order_prop_df <- cbind.data.frame(number_species_order_Sofia_urban_regional_3[1],
                                        Sofia_order_prop_vector)

### Change column names
colnames(Sofia_order_prop_df) <- c('Order', 'Sofia')



### Suolahti
### Extract species and orders for urban and regional assemblages
order_Suolahti_urban <- filter(bird_taxonomy_regional, grepl(paste(Suolahti_urban_filtering, collapse="|"), TipLabel))

order_Suolahti_regional <- filter(bird_taxonomy_regional, grepl(paste(Suolahti_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Suolahti_urban <- order_Suolahti_urban %>% count(IOCOrder)

number_species_order_Suolahti_regional <- order_Suolahti_regional %>% count(IOCOrder)

### Get proportions
prop_order_Suolahti_urban <- prop.table(number_species_order_Suolahti_urban$n)

prop_order_Suolahti_regional <- prop.table(number_species_order_Suolahti_regional$n)

### Add proportions
number_species_order_Suolahti_urban$prop <- cbind.data.frame(number_species_order_Suolahti_urban, prop_order_Suolahti_urban)

number_species_order_Suolahti_regional$prop <- cbind.data.frame(number_species_order_Suolahti_regional, prop_order_Suolahti_regional)

### Convert proportions into matrix
number_species_order_Suolahti_urban <- as.matrix(number_species_order_Suolahti_urban)
number_species_order_Suolahti_regional <- as.matrix(number_species_order_Suolahti_regional)

### Convert matrix into data frame
number_species_order_Suolahti_urban <- as.data.frame(number_species_order_Suolahti_urban)
number_species_order_Suolahti_regional <- as.data.frame(number_species_order_Suolahti_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Suolahti_urban_regional <- bind_rows(number_species_order_Suolahti_urban,anti_join(number_species_order_Suolahti_regional,number_species_order_Suolahti_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Suolahti_urban_regional <- number_species_order_Suolahti_urban_regional[order(number_species_order_Suolahti_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Suolahti_urban_regional$prop.prop_order_Suolahti_regional <- number_species_order_Suolahti_regional$prop.prop_order_Suolahti_regional

### Replace NA by ceros in the urban proportions
number_species_order_Suolahti_urban_regional[is.na(number_species_order_Suolahti_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Suolahti_urban_regional_2 <- bind_rows(number_species_order_Suolahti_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Suolahti_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Suolahti_urban_regional_2 <- number_species_order_Suolahti_urban_regional_2[order(number_species_order_Suolahti_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Suolahti_urban_regional_2[is.na(number_species_order_Suolahti_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Suolahti_urban_regional_3 <- data.frame(number_species_order_Suolahti_urban_regional_2[1],
                                                             number_species_order_Suolahti_urban_regional_2[5],
                                                             number_species_order_Suolahti_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Suolahti_urban_regional_3 <- setNames(number_species_order_Suolahti_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Suolahti_urban_regional_3$prop_urban <- as.numeric(number_species_order_Suolahti_urban_regional_3$prop_urban)
number_species_order_Suolahti_urban_regional_3$prop_regional <- as.numeric(number_species_order_Suolahti_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Suolahti_order_prop_vector <- (number_species_order_Suolahti_urban_regional_3$prop_urban - number_species_order_Suolahti_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Suolahti_order_prop_df <- cbind.data.frame(number_species_order_Suolahti_urban_regional_3[1],
                                           Suolahti_order_prop_vector)

### Change column names
colnames(Suolahti_order_prop_df) <- c('Order', 'Suolahti')



### Swidnica
### Extract species and orders for urban and regional assemblages
order_Swidnica_urban <- filter(bird_taxonomy_regional, grepl(paste(Swidnica_urban_filtering, collapse="|"), TipLabel))

order_Swidnica_regional <- filter(bird_taxonomy_regional, grepl(paste(Swidnica_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Swidnica_urban <- order_Swidnica_urban %>% count(IOCOrder)

number_species_order_Swidnica_regional <- order_Swidnica_regional %>% count(IOCOrder)

### Get proportions
prop_order_Swidnica_urban <- prop.table(number_species_order_Swidnica_urban$n)

prop_order_Swidnica_regional <- prop.table(number_species_order_Swidnica_regional$n)

### Add proportions
number_species_order_Swidnica_urban$prop <- cbind.data.frame(number_species_order_Swidnica_urban, prop_order_Swidnica_urban)

number_species_order_Swidnica_regional$prop <- cbind.data.frame(number_species_order_Swidnica_regional, prop_order_Swidnica_regional)

### Convert proportions into matrix
number_species_order_Swidnica_urban <- as.matrix(number_species_order_Swidnica_urban)
number_species_order_Swidnica_regional <- as.matrix(number_species_order_Swidnica_regional)

### Convert matrix into data frame
number_species_order_Swidnica_urban <- as.data.frame(number_species_order_Swidnica_urban)
number_species_order_Swidnica_regional <- as.data.frame(number_species_order_Swidnica_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Swidnica_urban_regional <- bind_rows(number_species_order_Swidnica_urban,anti_join(number_species_order_Swidnica_regional,number_species_order_Swidnica_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Swidnica_urban_regional <- number_species_order_Swidnica_urban_regional[order(number_species_order_Swidnica_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Swidnica_urban_regional$prop.prop_order_Swidnica_regional <- number_species_order_Swidnica_regional$prop.prop_order_Swidnica_regional

### Replace NA by ceros in the urban proportions
number_species_order_Swidnica_urban_regional[is.na(number_species_order_Swidnica_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Swidnica_urban_regional_2 <- bind_rows(number_species_order_Swidnica_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Swidnica_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Swidnica_urban_regional_2 <- number_species_order_Swidnica_urban_regional_2[order(number_species_order_Swidnica_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Swidnica_urban_regional_2[is.na(number_species_order_Swidnica_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Swidnica_urban_regional_3 <- data.frame(number_species_order_Swidnica_urban_regional_2[1],
                                                             number_species_order_Swidnica_urban_regional_2[5],
                                                             number_species_order_Swidnica_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Swidnica_urban_regional_3 <- setNames(number_species_order_Swidnica_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Swidnica_urban_regional_3$prop_urban <- as.numeric(number_species_order_Swidnica_urban_regional_3$prop_urban)
number_species_order_Swidnica_urban_regional_3$prop_regional <- as.numeric(number_species_order_Swidnica_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Swidnica_order_prop_vector <- (number_species_order_Swidnica_urban_regional_3$prop_urban - number_species_order_Swidnica_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Swidnica_order_prop_df <- cbind.data.frame(number_species_order_Swidnica_urban_regional_3[1],
                                           Swidnica_order_prop_vector)

### Change column names
colnames(Swidnica_order_prop_df) <- c('Order', 'Swidnica')



### Szczecin
### Extract species and orders for urban and regional assemblages
order_Szczecin_urban <- filter(bird_taxonomy_regional, grepl(paste(Szczecin_urban_filtering, collapse="|"), TipLabel))

order_Szczecin_regional <- filter(bird_taxonomy_regional, grepl(paste(Szczecin_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Szczecin_urban <- order_Szczecin_urban %>% count(IOCOrder)

number_species_order_Szczecin_regional <- order_Szczecin_regional %>% count(IOCOrder)

### Get proportions
prop_order_Szczecin_urban <- prop.table(number_species_order_Szczecin_urban$n)

prop_order_Szczecin_regional <- prop.table(number_species_order_Szczecin_regional$n)

### Add proportions
number_species_order_Szczecin_urban$prop <- cbind.data.frame(number_species_order_Szczecin_urban, prop_order_Szczecin_urban)

number_species_order_Szczecin_regional$prop <- cbind.data.frame(number_species_order_Szczecin_regional, prop_order_Szczecin_regional)

### Convert proportions into matrix
number_species_order_Szczecin_urban <- as.matrix(number_species_order_Szczecin_urban)
number_species_order_Szczecin_regional <- as.matrix(number_species_order_Szczecin_regional)

### Convert matrix into data frame
number_species_order_Szczecin_urban <- as.data.frame(number_species_order_Szczecin_urban)
number_species_order_Szczecin_regional <- as.data.frame(number_species_order_Szczecin_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Szczecin_urban_regional <- bind_rows(number_species_order_Szczecin_urban,anti_join(number_species_order_Szczecin_regional,number_species_order_Szczecin_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Szczecin_urban_regional <- number_species_order_Szczecin_urban_regional[order(number_species_order_Szczecin_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Szczecin_urban_regional$prop.prop_order_Szczecin_regional <- number_species_order_Szczecin_regional$prop.prop_order_Szczecin_regional

### Replace NA by ceros in the urban proportions
number_species_order_Szczecin_urban_regional[is.na(number_species_order_Szczecin_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Szczecin_urban_regional_2 <- bind_rows(number_species_order_Szczecin_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Szczecin_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Szczecin_urban_regional_2 <- number_species_order_Szczecin_urban_regional_2[order(number_species_order_Szczecin_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Szczecin_urban_regional_2[is.na(number_species_order_Szczecin_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Szczecin_urban_regional_3 <- data.frame(number_species_order_Szczecin_urban_regional_2[1],
                                                             number_species_order_Szczecin_urban_regional_2[5],
                                                             number_species_order_Szczecin_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Szczecin_urban_regional_3 <- setNames(number_species_order_Szczecin_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Szczecin_urban_regional_3$prop_urban <- as.numeric(number_species_order_Szczecin_urban_regional_3$prop_urban)
number_species_order_Szczecin_urban_regional_3$prop_regional <- as.numeric(number_species_order_Szczecin_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Szczecin_order_prop_vector <- (number_species_order_Szczecin_urban_regional_3$prop_urban - number_species_order_Szczecin_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Szczecin_order_prop_df <- cbind.data.frame(number_species_order_Szczecin_urban_regional_3[1],
                                           Szczecin_order_prop_vector)

### Change column names
colnames(Szczecin_order_prop_df) <- c('Order', 'Szczecin')



### Taubate
### Extract species and orders for urban and regional assemblages
order_Taubate_urban <- filter(bird_taxonomy_regional, grepl(paste(Taubate_urban_filtering, collapse="|"), TipLabel))

order_Taubate_regional <- filter(bird_taxonomy_regional, grepl(paste(Taubate_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Taubate_urban <- order_Taubate_urban %>% count(IOCOrder)

number_species_order_Taubate_regional <- order_Taubate_regional %>% count(IOCOrder)

### Get proportions
prop_order_Taubate_urban <- prop.table(number_species_order_Taubate_urban$n)

prop_order_Taubate_regional <- prop.table(number_species_order_Taubate_regional$n)

### Add proportions
number_species_order_Taubate_urban$prop <- cbind.data.frame(number_species_order_Taubate_urban, prop_order_Taubate_urban)

number_species_order_Taubate_regional$prop <- cbind.data.frame(number_species_order_Taubate_regional, prop_order_Taubate_regional)

### Convert proportions into matrix
number_species_order_Taubate_urban <- as.matrix(number_species_order_Taubate_urban)
number_species_order_Taubate_regional <- as.matrix(number_species_order_Taubate_regional)

### Convert matrix into data frame
number_species_order_Taubate_urban <- as.data.frame(number_species_order_Taubate_urban)
number_species_order_Taubate_regional <- as.data.frame(number_species_order_Taubate_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Taubate_urban_regional <- bind_rows(number_species_order_Taubate_urban,anti_join(number_species_order_Taubate_regional,number_species_order_Taubate_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Taubate_urban_regional <- number_species_order_Taubate_urban_regional[order(number_species_order_Taubate_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Taubate_urban_regional$prop.prop_order_Taubate_regional <- number_species_order_Taubate_regional$prop.prop_order_Taubate_regional

### Replace NA by ceros in the urban proportions
number_species_order_Taubate_urban_regional[is.na(number_species_order_Taubate_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Taubate_urban_regional_2 <- bind_rows(number_species_order_Taubate_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Taubate_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Taubate_urban_regional_2 <- number_species_order_Taubate_urban_regional_2[order(number_species_order_Taubate_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Taubate_urban_regional_2[is.na(number_species_order_Taubate_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Taubate_urban_regional_3 <- data.frame(number_species_order_Taubate_urban_regional_2[1],
                                                            number_species_order_Taubate_urban_regional_2[5],
                                                            number_species_order_Taubate_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Taubate_urban_regional_3 <- setNames(number_species_order_Taubate_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Taubate_urban_regional_3$prop_urban <- as.numeric(number_species_order_Taubate_urban_regional_3$prop_urban)
number_species_order_Taubate_urban_regional_3$prop_regional <- as.numeric(number_species_order_Taubate_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Taubate_order_prop_vector <- (number_species_order_Taubate_urban_regional_3$prop_urban - number_species_order_Taubate_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Taubate_order_prop_df <- cbind.data.frame(number_species_order_Taubate_urban_regional_3[1],
                                          Taubate_order_prop_vector)

### Change column names
colnames(Taubate_order_prop_df) <- c('Order', 'Taubate')



### Tornio
### Extract species and orders for urban and regional assemblages
order_Tornio_urban <- filter(bird_taxonomy_regional, grepl(paste(Tornio_urban_filtering, collapse="|"), TipLabel))

order_Tornio_regional <- filter(bird_taxonomy_regional, grepl(paste(Tornio_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Tornio_urban <- order_Tornio_urban %>% count(IOCOrder)

number_species_order_Tornio_regional <- order_Tornio_regional %>% count(IOCOrder)

### Get proportions
prop_order_Tornio_urban <- prop.table(number_species_order_Tornio_urban$n)

prop_order_Tornio_regional <- prop.table(number_species_order_Tornio_regional$n)

### Add proportions
number_species_order_Tornio_urban$prop <- cbind.data.frame(number_species_order_Tornio_urban, prop_order_Tornio_urban)

number_species_order_Tornio_regional$prop <- cbind.data.frame(number_species_order_Tornio_regional, prop_order_Tornio_regional)

### Convert proportions into matrix
number_species_order_Tornio_urban <- as.matrix(number_species_order_Tornio_urban)
number_species_order_Tornio_regional <- as.matrix(number_species_order_Tornio_regional)

### Convert matrix into data frame
number_species_order_Tornio_urban <- as.data.frame(number_species_order_Tornio_urban)
number_species_order_Tornio_regional <- as.data.frame(number_species_order_Tornio_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Tornio_urban_regional <- bind_rows(number_species_order_Tornio_urban,anti_join(number_species_order_Tornio_regional,number_species_order_Tornio_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Tornio_urban_regional <- number_species_order_Tornio_urban_regional[order(number_species_order_Tornio_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Tornio_urban_regional$prop.prop_order_Tornio_regional <- number_species_order_Tornio_regional$prop.prop_order_Tornio_regional

### Replace NA by ceros in the urban proportions
number_species_order_Tornio_urban_regional[is.na(number_species_order_Tornio_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Tornio_urban_regional_2 <- bind_rows(number_species_order_Tornio_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Tornio_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Tornio_urban_regional_2 <- number_species_order_Tornio_urban_regional_2[order(number_species_order_Tornio_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Tornio_urban_regional_2[is.na(number_species_order_Tornio_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Tornio_urban_regional_3 <- data.frame(number_species_order_Tornio_urban_regional_2[1],
                                                           number_species_order_Tornio_urban_regional_2[5],
                                                           number_species_order_Tornio_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Tornio_urban_regional_3 <- setNames(number_species_order_Tornio_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Tornio_urban_regional_3$prop_urban <- as.numeric(number_species_order_Tornio_urban_regional_3$prop_urban)
number_species_order_Tornio_urban_regional_3$prop_regional <- as.numeric(number_species_order_Tornio_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Tornio_order_prop_vector <- (number_species_order_Tornio_urban_regional_3$prop_urban - number_species_order_Tornio_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Tornio_order_prop_df <- cbind.data.frame(number_species_order_Tornio_urban_regional_3[1],
                                         Tornio_order_prop_vector)

### Change column names
colnames(Tornio_order_prop_df) <- c('Order', 'Tornio')



### Torun
### Extract species and orders for urban and regional assemblages
order_Torun_urban <- filter(bird_taxonomy_regional, grepl(paste(Torun_urban_filtering, collapse="|"), TipLabel))

order_Torun_regional <- filter(bird_taxonomy_regional, grepl(paste(Torun_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Torun_urban <- order_Torun_urban %>% count(IOCOrder)

number_species_order_Torun_regional <- order_Torun_regional %>% count(IOCOrder)

### Get proportions
prop_order_Torun_urban <- prop.table(number_species_order_Torun_urban$n)

prop_order_Torun_regional <- prop.table(number_species_order_Torun_regional$n)

### Add proportions
number_species_order_Torun_urban$prop <- cbind.data.frame(number_species_order_Torun_urban, prop_order_Torun_urban)

number_species_order_Torun_regional$prop <- cbind.data.frame(number_species_order_Torun_regional, prop_order_Torun_regional)

### Convert proportions into matrix
number_species_order_Torun_urban <- as.matrix(number_species_order_Torun_urban)
number_species_order_Torun_regional <- as.matrix(number_species_order_Torun_regional)

### Convert matrix into data frame
number_species_order_Torun_urban <- as.data.frame(number_species_order_Torun_urban)
number_species_order_Torun_regional <- as.data.frame(number_species_order_Torun_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Torun_urban_regional <- bind_rows(number_species_order_Torun_urban,anti_join(number_species_order_Torun_regional,number_species_order_Torun_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Torun_urban_regional <- number_species_order_Torun_urban_regional[order(number_species_order_Torun_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Torun_urban_regional$prop.prop_order_Torun_regional <- number_species_order_Torun_regional$prop.prop_order_Torun_regional

### Replace NA by ceros in the urban proportions
number_species_order_Torun_urban_regional[is.na(number_species_order_Torun_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Torun_urban_regional_2 <- bind_rows(number_species_order_Torun_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Torun_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Torun_urban_regional_2 <- number_species_order_Torun_urban_regional_2[order(number_species_order_Torun_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Torun_urban_regional_2[is.na(number_species_order_Torun_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Torun_urban_regional_3 <- data.frame(number_species_order_Torun_urban_regional_2[1],
                                                          number_species_order_Torun_urban_regional_2[5],
                                                          number_species_order_Torun_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Torun_urban_regional_3 <- setNames(number_species_order_Torun_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Torun_urban_regional_3$prop_urban <- as.numeric(number_species_order_Torun_urban_regional_3$prop_urban)
number_species_order_Torun_urban_regional_3$prop_regional <- as.numeric(number_species_order_Torun_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Torun_order_prop_vector <- (number_species_order_Torun_urban_regional_3$prop_urban - number_species_order_Torun_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Torun_order_prop_df <- cbind.data.frame(number_species_order_Torun_urban_regional_3[1],
                                        Torun_order_prop_vector)

### Change column names
colnames(Torun_order_prop_df) <- c('Order', 'Torun')



### Tucson
### Extract species and orders for urban and regional assemblages
order_Tucson_urban <- filter(bird_taxonomy_regional, grepl(paste(Tucson_urban_filtering, collapse="|"), TipLabel))

order_Tucson_regional <- filter(bird_taxonomy_regional, grepl(paste(Tucson_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Tucson_urban <- order_Tucson_urban %>% count(IOCOrder)

number_species_order_Tucson_regional <- order_Tucson_regional %>% count(IOCOrder)

### Get proportions
prop_order_Tucson_urban <- prop.table(number_species_order_Tucson_urban$n)

prop_order_Tucson_regional <- prop.table(number_species_order_Tucson_regional$n)

### Add proportions
number_species_order_Tucson_urban$prop <- cbind.data.frame(number_species_order_Tucson_urban, prop_order_Tucson_urban)

number_species_order_Tucson_regional$prop <- cbind.data.frame(number_species_order_Tucson_regional, prop_order_Tucson_regional)

### Convert proportions into matrix
number_species_order_Tucson_urban <- as.matrix(number_species_order_Tucson_urban)
number_species_order_Tucson_regional <- as.matrix(number_species_order_Tucson_regional)

### Convert matrix into data frame
number_species_order_Tucson_urban <- as.data.frame(number_species_order_Tucson_urban)
number_species_order_Tucson_regional <- as.data.frame(number_species_order_Tucson_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Tucson_urban_regional <- bind_rows(number_species_order_Tucson_urban,anti_join(number_species_order_Tucson_regional,number_species_order_Tucson_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Tucson_urban_regional <- number_species_order_Tucson_urban_regional[order(number_species_order_Tucson_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Tucson_urban_regional$prop.prop_order_Tucson_regional <- number_species_order_Tucson_regional$prop.prop_order_Tucson_regional

### Replace NA by ceros in the urban proportions
number_species_order_Tucson_urban_regional[is.na(number_species_order_Tucson_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Tucson_urban_regional_2 <- bind_rows(number_species_order_Tucson_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Tucson_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Tucson_urban_regional_2 <- number_species_order_Tucson_urban_regional_2[order(number_species_order_Tucson_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Tucson_urban_regional_2[is.na(number_species_order_Tucson_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Tucson_urban_regional_3 <- data.frame(number_species_order_Tucson_urban_regional_2[1],
                                                           number_species_order_Tucson_urban_regional_2[5],
                                                           number_species_order_Tucson_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Tucson_urban_regional_3 <- setNames(number_species_order_Tucson_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Tucson_urban_regional_3$prop_urban <- as.numeric(number_species_order_Tucson_urban_regional_3$prop_urban)
number_species_order_Tucson_urban_regional_3$prop_regional <- as.numeric(number_species_order_Tucson_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Tucson_order_prop_vector <- (number_species_order_Tucson_urban_regional_3$prop_urban - number_species_order_Tucson_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Tucson_order_prop_df <- cbind.data.frame(number_species_order_Tucson_urban_regional_3[1],
                                         Tucson_order_prop_vector)

### Change column names
colnames(Tucson_order_prop_df) <- c('Order', 'Tucson')



### Turku
### Extract species and orders for urban and regional assemblages
order_Turku_urban <- filter(bird_taxonomy_regional, grepl(paste(Turku_urban_filtering, collapse="|"), TipLabel))

order_Turku_regional <- filter(bird_taxonomy_regional, grepl(paste(Turku_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Turku_urban <- order_Turku_urban %>% count(IOCOrder)

number_species_order_Turku_regional <- order_Turku_regional %>% count(IOCOrder)

### Get proportions
prop_order_Turku_urban <- prop.table(number_species_order_Turku_urban$n)

prop_order_Turku_regional <- prop.table(number_species_order_Turku_regional$n)

### Add proportions
number_species_order_Turku_urban$prop <- cbind.data.frame(number_species_order_Turku_urban, prop_order_Turku_urban)

number_species_order_Turku_regional$prop <- cbind.data.frame(number_species_order_Turku_regional, prop_order_Turku_regional)

### Convert proportions into matrix
number_species_order_Turku_urban <- as.matrix(number_species_order_Turku_urban)
number_species_order_Turku_regional <- as.matrix(number_species_order_Turku_regional)

### Convert matrix into data frame
number_species_order_Turku_urban <- as.data.frame(number_species_order_Turku_urban)
number_species_order_Turku_regional <- as.data.frame(number_species_order_Turku_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Turku_urban_regional <- bind_rows(number_species_order_Turku_urban,anti_join(number_species_order_Turku_regional,number_species_order_Turku_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Turku_urban_regional <- number_species_order_Turku_urban_regional[order(number_species_order_Turku_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Turku_urban_regional$prop.prop_order_Turku_regional <- number_species_order_Turku_regional$prop.prop_order_Turku_regional

### Replace NA by ceros in the urban proportions
number_species_order_Turku_urban_regional[is.na(number_species_order_Turku_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Turku_urban_regional_2 <- bind_rows(number_species_order_Turku_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Turku_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Turku_urban_regional_2 <- number_species_order_Turku_urban_regional_2[order(number_species_order_Turku_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Turku_urban_regional_2[is.na(number_species_order_Turku_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Turku_urban_regional_3 <- data.frame(number_species_order_Turku_urban_regional_2[1],
                                                          number_species_order_Turku_urban_regional_2[5],
                                                          number_species_order_Turku_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Turku_urban_regional_3 <- setNames(number_species_order_Turku_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Turku_urban_regional_3$prop_urban <- as.numeric(number_species_order_Turku_urban_regional_3$prop_urban)
number_species_order_Turku_urban_regional_3$prop_regional <- as.numeric(number_species_order_Turku_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Turku_order_prop_vector <- (number_species_order_Turku_urban_regional_3$prop_urban - number_species_order_Turku_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Turku_order_prop_df <- cbind.data.frame(number_species_order_Turku_urban_regional_3[1],
                                        Turku_order_prop_vector)

### Change column names
colnames(Turku_order_prop_df) <- c('Order', 'Turku')



### Uusikaupunki
### Extract species and orders for urban and regional assemblages
order_Uusikaupunki_urban <- filter(bird_taxonomy_regional, grepl(paste(Uusikaupunki_urban_filtering, collapse="|"), TipLabel))

order_Uusikaupunki_regional <- filter(bird_taxonomy_regional, grepl(paste(Uusikaupunki_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Uusikaupunki_urban <- order_Uusikaupunki_urban %>% count(IOCOrder)

number_species_order_Uusikaupunki_regional <- order_Uusikaupunki_regional %>% count(IOCOrder)

### Get proportions
prop_order_Uusikaupunki_urban <- prop.table(number_species_order_Uusikaupunki_urban$n)

prop_order_Uusikaupunki_regional <- prop.table(number_species_order_Uusikaupunki_regional$n)

### Add proportions
number_species_order_Uusikaupunki_urban$prop <- cbind.data.frame(number_species_order_Uusikaupunki_urban, prop_order_Uusikaupunki_urban)

number_species_order_Uusikaupunki_regional$prop <- cbind.data.frame(number_species_order_Uusikaupunki_regional, prop_order_Uusikaupunki_regional)

### Convert proportions into matrix
number_species_order_Uusikaupunki_urban <- as.matrix(number_species_order_Uusikaupunki_urban)
number_species_order_Uusikaupunki_regional <- as.matrix(number_species_order_Uusikaupunki_regional)

### Convert matrix into data frame
number_species_order_Uusikaupunki_urban <- as.data.frame(number_species_order_Uusikaupunki_urban)
number_species_order_Uusikaupunki_regional <- as.data.frame(number_species_order_Uusikaupunki_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Uusikaupunki_urban_regional <- bind_rows(number_species_order_Uusikaupunki_urban,anti_join(number_species_order_Uusikaupunki_regional,number_species_order_Uusikaupunki_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Uusikaupunki_urban_regional <- number_species_order_Uusikaupunki_urban_regional[order(number_species_order_Uusikaupunki_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Uusikaupunki_urban_regional$prop.prop_order_Uusikaupunki_regional <- number_species_order_Uusikaupunki_regional$prop.prop_order_Uusikaupunki_regional

### Replace NA by ceros in the urban proportions
number_species_order_Uusikaupunki_urban_regional[is.na(number_species_order_Uusikaupunki_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Uusikaupunki_urban_regional_2 <- bind_rows(number_species_order_Uusikaupunki_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Uusikaupunki_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Uusikaupunki_urban_regional_2 <- number_species_order_Uusikaupunki_urban_regional_2[order(number_species_order_Uusikaupunki_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Uusikaupunki_urban_regional_2[is.na(number_species_order_Uusikaupunki_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Uusikaupunki_urban_regional_3 <- data.frame(number_species_order_Uusikaupunki_urban_regional_2[1],
                                                                 number_species_order_Uusikaupunki_urban_regional_2[5],
                                                                 number_species_order_Uusikaupunki_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Uusikaupunki_urban_regional_3 <- setNames(number_species_order_Uusikaupunki_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Uusikaupunki_urban_regional_3$prop_urban <- as.numeric(number_species_order_Uusikaupunki_urban_regional_3$prop_urban)
number_species_order_Uusikaupunki_urban_regional_3$prop_regional <- as.numeric(number_species_order_Uusikaupunki_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Uusikaupunki_order_prop_vector <- (number_species_order_Uusikaupunki_urban_regional_3$prop_urban - number_species_order_Uusikaupunki_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Uusikaupunki_order_prop_df <- cbind.data.frame(number_species_order_Uusikaupunki_urban_regional_3[1],
                                               Uusikaupunki_order_prop_vector)

### Change column names
colnames(Uusikaupunki_order_prop_df) <- c('Order', 'Uusikaupunki')



### Vantaa
### Extract species and orders for urban and regional assemblages
order_Vantaa_urban <- filter(bird_taxonomy_regional, grepl(paste(Vantaa_urban_filtering, collapse="|"), TipLabel))

order_Vantaa_regional <- filter(bird_taxonomy_regional, grepl(paste(Vantaa_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Vantaa_urban <- order_Vantaa_urban %>% count(IOCOrder)

number_species_order_Vantaa_regional <- order_Vantaa_regional %>% count(IOCOrder)

### Get proportions
prop_order_Vantaa_urban <- prop.table(number_species_order_Vantaa_urban$n)

prop_order_Vantaa_regional <- prop.table(number_species_order_Vantaa_regional$n)

### Add proportions
number_species_order_Vantaa_urban$prop <- cbind.data.frame(number_species_order_Vantaa_urban, prop_order_Vantaa_urban)

number_species_order_Vantaa_regional$prop <- cbind.data.frame(number_species_order_Vantaa_regional, prop_order_Vantaa_regional)

### Convert proportions into matrix
number_species_order_Vantaa_urban <- as.matrix(number_species_order_Vantaa_urban)
number_species_order_Vantaa_regional <- as.matrix(number_species_order_Vantaa_regional)

### Convert matrix into data frame
number_species_order_Vantaa_urban <- as.data.frame(number_species_order_Vantaa_urban)
number_species_order_Vantaa_regional <- as.data.frame(number_species_order_Vantaa_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Vantaa_urban_regional <- bind_rows(number_species_order_Vantaa_urban,anti_join(number_species_order_Vantaa_regional,number_species_order_Vantaa_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Vantaa_urban_regional <- number_species_order_Vantaa_urban_regional[order(number_species_order_Vantaa_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Vantaa_urban_regional$prop.prop_order_Vantaa_regional <- number_species_order_Vantaa_regional$prop.prop_order_Vantaa_regional

### Replace NA by ceros in the urban proportions
number_species_order_Vantaa_urban_regional[is.na(number_species_order_Vantaa_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Vantaa_urban_regional_2 <- bind_rows(number_species_order_Vantaa_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Vantaa_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Vantaa_urban_regional_2 <- number_species_order_Vantaa_urban_regional_2[order(number_species_order_Vantaa_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Vantaa_urban_regional_2[is.na(number_species_order_Vantaa_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Vantaa_urban_regional_3 <- data.frame(number_species_order_Vantaa_urban_regional_2[1],
                                                           number_species_order_Vantaa_urban_regional_2[5],
                                                           number_species_order_Vantaa_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Vantaa_urban_regional_3 <- setNames(number_species_order_Vantaa_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Vantaa_urban_regional_3$prop_urban <- as.numeric(number_species_order_Vantaa_urban_regional_3$prop_urban)
number_species_order_Vantaa_urban_regional_3$prop_regional <- as.numeric(number_species_order_Vantaa_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Vantaa_order_prop_vector <- (number_species_order_Vantaa_urban_regional_3$prop_urban - number_species_order_Vantaa_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Vantaa_order_prop_df <- cbind.data.frame(number_species_order_Vantaa_urban_regional_3[1],
                                         Vantaa_order_prop_vector)

### Change column names
colnames(Vantaa_order_prop_df) <- c('Order', 'Vantaa')



### Vienna
### Extract species and orders for urban and regional assemblages
order_Vienna_urban <- filter(bird_taxonomy_regional, grepl(paste(Vienna_urban_filtering, collapse="|"), TipLabel))

order_Vienna_regional <- filter(bird_taxonomy_regional, grepl(paste(Vienna_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Vienna_urban <- order_Vienna_urban %>% count(IOCOrder)

number_species_order_Vienna_regional <- order_Vienna_regional %>% count(IOCOrder)

### Get proportions
prop_order_Vienna_urban <- prop.table(number_species_order_Vienna_urban$n)

prop_order_Vienna_regional <- prop.table(number_species_order_Vienna_regional$n)

### Add proportions
number_species_order_Vienna_urban$prop <- cbind.data.frame(number_species_order_Vienna_urban, prop_order_Vienna_urban)

number_species_order_Vienna_regional$prop <- cbind.data.frame(number_species_order_Vienna_regional, prop_order_Vienna_regional)

### Convert proportions into matrix
number_species_order_Vienna_urban <- as.matrix(number_species_order_Vienna_urban)
number_species_order_Vienna_regional <- as.matrix(number_species_order_Vienna_regional)

### Convert matrix into data frame
number_species_order_Vienna_urban <- as.data.frame(number_species_order_Vienna_urban)
number_species_order_Vienna_regional <- as.data.frame(number_species_order_Vienna_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Vienna_urban_regional <- bind_rows(number_species_order_Vienna_urban,anti_join(number_species_order_Vienna_regional,number_species_order_Vienna_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Vienna_urban_regional <- number_species_order_Vienna_urban_regional[order(number_species_order_Vienna_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Vienna_urban_regional$prop.prop_order_Vienna_regional <- number_species_order_Vienna_regional$prop.prop_order_Vienna_regional

### Replace NA by ceros in the urban proportions
number_species_order_Vienna_urban_regional[is.na(number_species_order_Vienna_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Vienna_urban_regional_2 <- bind_rows(number_species_order_Vienna_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Vienna_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Vienna_urban_regional_2 <- number_species_order_Vienna_urban_regional_2[order(number_species_order_Vienna_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Vienna_urban_regional_2[is.na(number_species_order_Vienna_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Vienna_urban_regional_3 <- data.frame(number_species_order_Vienna_urban_regional_2[1],
                                                           number_species_order_Vienna_urban_regional_2[5],
                                                           number_species_order_Vienna_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Vienna_urban_regional_3 <- setNames(number_species_order_Vienna_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Vienna_urban_regional_3$prop_urban <- as.numeric(number_species_order_Vienna_urban_regional_3$prop_urban)
number_species_order_Vienna_urban_regional_3$prop_regional <- as.numeric(number_species_order_Vienna_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Vienna_order_prop_vector <- (number_species_order_Vienna_urban_regional_3$prop_urban - number_species_order_Vienna_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Vienna_order_prop_df <- cbind.data.frame(number_species_order_Vienna_urban_regional_3[1],
                                         Vienna_order_prop_vector)

### Change column names
colnames(Vienna_order_prop_df) <- c('Order', 'Vienna')



### Vitoria_Gasteiz
### Extract species and orders for urban and regional assemblages
order_Vitoria_Gasteiz_urban <- filter(bird_taxonomy_regional, grepl(paste(Vitoria_Gasteiz_urban_filtering, collapse="|"), TipLabel))

order_Vitoria_Gasteiz_regional <- filter(bird_taxonomy_regional, grepl(paste(Vitoria_Gasteiz_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Vitoria_Gasteiz_urban <- order_Vitoria_Gasteiz_urban %>% count(IOCOrder)

number_species_order_Vitoria_Gasteiz_regional <- order_Vitoria_Gasteiz_regional %>% count(IOCOrder)

### Get proportions
prop_order_Vitoria_Gasteiz_urban <- prop.table(number_species_order_Vitoria_Gasteiz_urban$n)

prop_order_Vitoria_Gasteiz_regional <- prop.table(number_species_order_Vitoria_Gasteiz_regional$n)

### Add proportions
number_species_order_Vitoria_Gasteiz_urban$prop <- cbind.data.frame(number_species_order_Vitoria_Gasteiz_urban, prop_order_Vitoria_Gasteiz_urban)

number_species_order_Vitoria_Gasteiz_regional$prop <- cbind.data.frame(number_species_order_Vitoria_Gasteiz_regional, prop_order_Vitoria_Gasteiz_regional)

### Convert proportions into matrix
number_species_order_Vitoria_Gasteiz_urban <- as.matrix(number_species_order_Vitoria_Gasteiz_urban)
number_species_order_Vitoria_Gasteiz_regional <- as.matrix(number_species_order_Vitoria_Gasteiz_regional)

### Convert matrix into data frame
number_species_order_Vitoria_Gasteiz_urban <- as.data.frame(number_species_order_Vitoria_Gasteiz_urban)
number_species_order_Vitoria_Gasteiz_regional <- as.data.frame(number_species_order_Vitoria_Gasteiz_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Vitoria_Gasteiz_urban_regional <- bind_rows(number_species_order_Vitoria_Gasteiz_urban,anti_join(number_species_order_Vitoria_Gasteiz_regional,number_species_order_Vitoria_Gasteiz_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Vitoria_Gasteiz_urban_regional <- number_species_order_Vitoria_Gasteiz_urban_regional[order(number_species_order_Vitoria_Gasteiz_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Vitoria_Gasteiz_urban_regional$prop.prop_order_Vitoria_Gasteiz_regional <- number_species_order_Vitoria_Gasteiz_regional$prop.prop_order_Vitoria_Gasteiz_regional

### Replace NA by ceros in the urban proportions
number_species_order_Vitoria_Gasteiz_urban_regional[is.na(number_species_order_Vitoria_Gasteiz_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Vitoria_Gasteiz_urban_regional_2 <- bind_rows(number_species_order_Vitoria_Gasteiz_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Vitoria_Gasteiz_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Vitoria_Gasteiz_urban_regional_2 <- number_species_order_Vitoria_Gasteiz_urban_regional_2[order(number_species_order_Vitoria_Gasteiz_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Vitoria_Gasteiz_urban_regional_2[is.na(number_species_order_Vitoria_Gasteiz_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Vitoria_Gasteiz_urban_regional_3 <- data.frame(number_species_order_Vitoria_Gasteiz_urban_regional_2[1],
                                                                    number_species_order_Vitoria_Gasteiz_urban_regional_2[5],
                                                                    number_species_order_Vitoria_Gasteiz_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Vitoria_Gasteiz_urban_regional_3 <- setNames(number_species_order_Vitoria_Gasteiz_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Vitoria_Gasteiz_urban_regional_3$prop_urban <- as.numeric(number_species_order_Vitoria_Gasteiz_urban_regional_3$prop_urban)
number_species_order_Vitoria_Gasteiz_urban_regional_3$prop_regional <- as.numeric(number_species_order_Vitoria_Gasteiz_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Vitoria_Gasteiz_order_prop_vector <- (number_species_order_Vitoria_Gasteiz_urban_regional_3$prop_urban - number_species_order_Vitoria_Gasteiz_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Vitoria_Gasteiz_order_prop_df <- cbind.data.frame(number_species_order_Vitoria_Gasteiz_urban_regional_3[1],
                                                  Vitoria_Gasteiz_order_prop_vector)

### Change column names
colnames(Vitoria_Gasteiz_order_prop_df) <- c('Order', 'Vitoria_Gasteiz')



### Warsaw
### Extract species and orders for urban and regional assemblages
order_Warsaw_urban <- filter(bird_taxonomy_regional, grepl(paste(Warsaw_urban_filtering, collapse="|"), TipLabel))

order_Warsaw_regional <- filter(bird_taxonomy_regional, grepl(paste(Warsaw_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Warsaw_urban <- order_Warsaw_urban %>% count(IOCOrder)

number_species_order_Warsaw_regional <- order_Warsaw_regional %>% count(IOCOrder)

### Get proportions
prop_order_Warsaw_urban <- prop.table(number_species_order_Warsaw_urban$n)

prop_order_Warsaw_regional <- prop.table(number_species_order_Warsaw_regional$n)

### Add proportions
number_species_order_Warsaw_urban$prop <- cbind.data.frame(number_species_order_Warsaw_urban, prop_order_Warsaw_urban)

number_species_order_Warsaw_regional$prop <- cbind.data.frame(number_species_order_Warsaw_regional, prop_order_Warsaw_regional)

### Convert proportions into matrix
number_species_order_Warsaw_urban <- as.matrix(number_species_order_Warsaw_urban)
number_species_order_Warsaw_regional <- as.matrix(number_species_order_Warsaw_regional)

### Convert matrix into data frame
number_species_order_Warsaw_urban <- as.data.frame(number_species_order_Warsaw_urban)
number_species_order_Warsaw_regional <- as.data.frame(number_species_order_Warsaw_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Warsaw_urban_regional <- bind_rows(number_species_order_Warsaw_urban,anti_join(number_species_order_Warsaw_regional,number_species_order_Warsaw_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Warsaw_urban_regional <- number_species_order_Warsaw_urban_regional[order(number_species_order_Warsaw_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Warsaw_urban_regional$prop.prop_order_Warsaw_regional <- number_species_order_Warsaw_regional$prop.prop_order_Warsaw_regional

### Replace NA by ceros in the urban proportions
number_species_order_Warsaw_urban_regional[is.na(number_species_order_Warsaw_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Warsaw_urban_regional_2 <- bind_rows(number_species_order_Warsaw_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Warsaw_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Warsaw_urban_regional_2 <- number_species_order_Warsaw_urban_regional_2[order(number_species_order_Warsaw_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Warsaw_urban_regional_2[is.na(number_species_order_Warsaw_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Warsaw_urban_regional_3 <- data.frame(number_species_order_Warsaw_urban_regional_2[1],
                                                           number_species_order_Warsaw_urban_regional_2[5],
                                                           number_species_order_Warsaw_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Warsaw_urban_regional_3 <- setNames(number_species_order_Warsaw_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Warsaw_urban_regional_3$prop_urban <- as.numeric(number_species_order_Warsaw_urban_regional_3$prop_urban)
number_species_order_Warsaw_urban_regional_3$prop_regional <- as.numeric(number_species_order_Warsaw_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Warsaw_order_prop_vector <- (number_species_order_Warsaw_urban_regional_3$prop_urban - number_species_order_Warsaw_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Warsaw_order_prop_df <- cbind.data.frame(number_species_order_Warsaw_urban_regional_3[1],
                                         Warsaw_order_prop_vector)

### Change column names
colnames(Warsaw_order_prop_df) <- c('Order', 'Warsaw')



### Wroclaw
### Extract species and orders for urban and regional assemblages
order_Wroclaw_urban <- filter(bird_taxonomy_regional, grepl(paste(Wroclaw_urban_filtering, collapse="|"), TipLabel))

order_Wroclaw_regional <- filter(bird_taxonomy_regional, grepl(paste(Wroclaw_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Wroclaw_urban <- order_Wroclaw_urban %>% count(IOCOrder)

number_species_order_Wroclaw_regional <- order_Wroclaw_regional %>% count(IOCOrder)

### Get proportions
prop_order_Wroclaw_urban <- prop.table(number_species_order_Wroclaw_urban$n)

prop_order_Wroclaw_regional <- prop.table(number_species_order_Wroclaw_regional$n)

### Add proportions
number_species_order_Wroclaw_urban$prop <- cbind.data.frame(number_species_order_Wroclaw_urban, prop_order_Wroclaw_urban)

number_species_order_Wroclaw_regional$prop <- cbind.data.frame(number_species_order_Wroclaw_regional, prop_order_Wroclaw_regional)

### Convert proportions into matrix
number_species_order_Wroclaw_urban <- as.matrix(number_species_order_Wroclaw_urban)
number_species_order_Wroclaw_regional <- as.matrix(number_species_order_Wroclaw_regional)

### Convert matrix into data frame
number_species_order_Wroclaw_urban <- as.data.frame(number_species_order_Wroclaw_urban)
number_species_order_Wroclaw_regional <- as.data.frame(number_species_order_Wroclaw_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Wroclaw_urban_regional <- bind_rows(number_species_order_Wroclaw_urban,anti_join(number_species_order_Wroclaw_regional,number_species_order_Wroclaw_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Wroclaw_urban_regional <- number_species_order_Wroclaw_urban_regional[order(number_species_order_Wroclaw_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Wroclaw_urban_regional$prop.prop_order_Wroclaw_regional <- number_species_order_Wroclaw_regional$prop.prop_order_Wroclaw_regional

### Replace NA by ceros in the urban proportions
number_species_order_Wroclaw_urban_regional[is.na(number_species_order_Wroclaw_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Wroclaw_urban_regional_2 <- bind_rows(number_species_order_Wroclaw_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Wroclaw_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Wroclaw_urban_regional_2 <- number_species_order_Wroclaw_urban_regional_2[order(number_species_order_Wroclaw_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Wroclaw_urban_regional_2[is.na(number_species_order_Wroclaw_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Wroclaw_urban_regional_3 <- data.frame(number_species_order_Wroclaw_urban_regional_2[1],
                                                            number_species_order_Wroclaw_urban_regional_2[5],
                                                            number_species_order_Wroclaw_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Wroclaw_urban_regional_3 <- setNames(number_species_order_Wroclaw_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Wroclaw_urban_regional_3$prop_urban <- as.numeric(number_species_order_Wroclaw_urban_regional_3$prop_urban)
number_species_order_Wroclaw_urban_regional_3$prop_regional <- as.numeric(number_species_order_Wroclaw_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Wroclaw_order_prop_vector <- (number_species_order_Wroclaw_urban_regional_3$prop_urban - number_species_order_Wroclaw_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Wroclaw_order_prop_df <- cbind.data.frame(number_species_order_Wroclaw_urban_regional_3[1],
                                          Wroclaw_order_prop_vector)

### Change column names
colnames(Wroclaw_order_prop_df) <- c('Order', 'Wroclaw')



### Xalapa
### Extract species and orders for urban and regional assemblages
order_Xalapa_urban <- filter(bird_taxonomy_regional, grepl(paste(Xalapa_urban_filtering, collapse="|"), TipLabel))

order_Xalapa_regional <- filter(bird_taxonomy_regional, grepl(paste(Xalapa_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Xalapa_urban <- order_Xalapa_urban %>% count(IOCOrder)

number_species_order_Xalapa_regional <- order_Xalapa_regional %>% count(IOCOrder)

### Get proportions
prop_order_Xalapa_urban <- prop.table(number_species_order_Xalapa_urban$n)

prop_order_Xalapa_regional <- prop.table(number_species_order_Xalapa_regional$n)

### Add proportions
number_species_order_Xalapa_urban$prop <- cbind.data.frame(number_species_order_Xalapa_urban, prop_order_Xalapa_urban)

number_species_order_Xalapa_regional$prop <- cbind.data.frame(number_species_order_Xalapa_regional, prop_order_Xalapa_regional)

### Convert proportions into matrix
number_species_order_Xalapa_urban <- as.matrix(number_species_order_Xalapa_urban)
number_species_order_Xalapa_regional <- as.matrix(number_species_order_Xalapa_regional)

### Convert matrix into data frame
number_species_order_Xalapa_urban <- as.data.frame(number_species_order_Xalapa_urban)
number_species_order_Xalapa_regional <- as.data.frame(number_species_order_Xalapa_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Xalapa_urban_regional <- bind_rows(number_species_order_Xalapa_urban,anti_join(number_species_order_Xalapa_regional,number_species_order_Xalapa_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Xalapa_urban_regional <- number_species_order_Xalapa_urban_regional[order(number_species_order_Xalapa_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Xalapa_urban_regional$prop.prop_order_Xalapa_regional <- number_species_order_Xalapa_regional$prop.prop_order_Xalapa_regional

### Replace NA by ceros in the urban proportions
number_species_order_Xalapa_urban_regional[is.na(number_species_order_Xalapa_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assamblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Xalapa_urban_regional_2 <- bind_rows(number_species_order_Xalapa_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Xalapa_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Xalapa_urban_regional_2 <- number_species_order_Xalapa_urban_regional_2[order(number_species_order_Xalapa_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Xalapa_urban_regional_2[is.na(number_species_order_Xalapa_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Xalapa_urban_regional_3 <- data.frame(number_species_order_Xalapa_urban_regional_2[1],
                                                           number_species_order_Xalapa_urban_regional_2[5],
                                                           number_species_order_Xalapa_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Xalapa_urban_regional_3 <- setNames(number_species_order_Xalapa_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Xalapa_urban_regional_3$prop_urban <- as.numeric(number_species_order_Xalapa_urban_regional_3$prop_urban)
number_species_order_Xalapa_urban_regional_3$prop_regional <- as.numeric(number_species_order_Xalapa_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Xalapa_order_prop_vector <- (number_species_order_Xalapa_urban_regional_3$prop_urban - number_species_order_Xalapa_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Xalapa_order_prop_df <- cbind.data.frame(number_species_order_Xalapa_urban_regional_3[1],
                                         Xalapa_order_prop_vector)

### Change column names
colnames(Xalapa_order_prop_df) <- c('Order', 'Xalapa')



### Zielona_Gora
### Extract species and orders for urban and regional assemblages
order_Zielona_Gora_urban <- filter(bird_taxonomy_regional, grepl(paste(Zielona_Gora_urban_filtering, collapse="|"), TipLabel))

order_Zielona_Gora_regional <- filter(bird_taxonomy_regional, grepl(paste(Zielona_Gora_regional_filtering, collapse="|"), TipLabel))

### Count the number of times a number (factor) occurs within each group: https://stackoverflow.com/questions/35246537/count-the-number-of-times-a-number-factor-occurs-within-each-group
number_species_order_Zielona_Gora_urban <- order_Zielona_Gora_urban %>% count(IOCOrder)

number_species_order_Zielona_Gora_regional <- order_Zielona_Gora_regional %>% count(IOCOrder)

### Get proportions
prop_order_Zielona_Gora_urban <- prop.table(number_species_order_Zielona_Gora_urban$n)

prop_order_Zielona_Gora_regional <- prop.table(number_species_order_Zielona_Gora_regional$n)

### Add proportions
number_species_order_Zielona_Gora_urban$prop <- cbind.data.frame(number_species_order_Zielona_Gora_urban, prop_order_Zielona_Gora_urban)

number_species_order_Zielona_Gora_regional$prop <- cbind.data.frame(number_species_order_Zielona_Gora_regional, prop_order_Zielona_Gora_regional)

### Convert proportions into matrix
number_species_order_Zielona_Gora_urban <- as.matrix(number_species_order_Zielona_Gora_urban)
number_species_order_Zielona_Gora_regional <- as.matrix(number_species_order_Zielona_Gora_regional)

### Convert matrix into data frame
number_species_order_Zielona_Gora_urban <- as.data.frame(number_species_order_Zielona_Gora_urban)
number_species_order_Zielona_Gora_regional <- as.data.frame(number_species_order_Zielona_Gora_regional)

### Using anti_join (https://stackoverflow.com/questions/52775262/add-rows-to-dataframe-from-another-dataframe-based-on-a-vector)
number_species_order_Zielona_Gora_urban_regional <- bind_rows(number_species_order_Zielona_Gora_urban,anti_join(number_species_order_Zielona_Gora_regional,number_species_order_Zielona_Gora_urban,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Zielona_Gora_urban_regional <- number_species_order_Zielona_Gora_urban_regional[order(number_species_order_Zielona_Gora_urban_regional$IOCOrder),]

### Fill values of the regional proportions
number_species_order_Zielona_Gora_urban_regional$prop.prop_order_Zielona_Gora_regional <- number_species_order_Zielona_Gora_regional$prop.prop_order_Zielona_Gora_regional

### Replace NA by ceros in the urban proportions
number_species_order_Zielona_Gora_urban_regional[is.na(number_species_order_Zielona_Gora_urban_regional)] = 0

### Change name of the object
number_species_order_all_birds <- number_species_order_regional

### Get proportions
prop_order_all_birds <- prop.table(number_species_order_all_birds$n)

### Add proportions for the overall assemblages (considering all cities)
number_species_order_all_birds$prop <- cbind.data.frame(number_species_order_all_birds, prop_order_all_birds)

### Convert proportions into matrix
number_species_order_all_birds <- as.matrix(number_species_order_all_birds)

### Convert matrix into data frame
number_species_order_all_birds <- as.data.frame(number_species_order_all_birds)

### Using anti_join 
number_species_order_Zielona_Gora_urban_regional_2 <- bind_rows(number_species_order_Zielona_Gora_urban_regional,anti_join(number_species_order_all_birds,number_species_order_Zielona_Gora_urban_regional,by="IOCOrder"))

### Order column containing most proportion values (i.e., regional)
number_species_order_Zielona_Gora_urban_regional_2 <- number_species_order_Zielona_Gora_urban_regional_2[order(number_species_order_Zielona_Gora_urban_regional_2$IOCOrder),]

### Replace NA by ceros in the urban proportions
number_species_order_Zielona_Gora_urban_regional_2[is.na(number_species_order_Zielona_Gora_urban_regional_2)] = 0

### Get only IOCOrder, and proportions (both urban and regional)
number_species_order_Zielona_Gora_urban_regional_3 <- data.frame(number_species_order_Zielona_Gora_urban_regional_2[1],
                                                                 number_species_order_Zielona_Gora_urban_regional_2[5],
                                                                 number_species_order_Zielona_Gora_urban_regional_2[6])

### Renaming all the column names of data frame 
number_species_order_Zielona_Gora_urban_regional_3 <- setNames(number_species_order_Zielona_Gora_urban_regional_3, c("Order","prop_urban","prop_regional"))

### Convert as numeric both proportion columns
number_species_order_Zielona_Gora_urban_regional_3$prop_urban <- as.numeric(number_species_order_Zielona_Gora_urban_regional_3$prop_urban)
number_species_order_Zielona_Gora_urban_regional_3$prop_regional <- as.numeric(number_species_order_Zielona_Gora_urban_regional_3$prop_regional)

### Compute proportion using delta formula
Zielona_Gora_order_prop_vector <- (number_species_order_Zielona_Gora_urban_regional_3$prop_urban - number_species_order_Zielona_Gora_urban_regional_3$prop_regional) * 100

### Generate data frame containing Order and Proportion columns
Zielona_Gora_order_prop_df <- cbind.data.frame(number_species_order_Zielona_Gora_urban_regional_3[1],
                                               Zielona_Gora_order_prop_vector)

### Change column names
colnames(Zielona_Gora_order_prop_df) <- c('Order', 'Zielona_Gora')


### Merge all proportions of avian Orders for each urban and region species pools comaprison
mean_subs_prop_order <- cbind.data.frame(Aanekoski_order_prop_df[1],
                                         Aanekoski_order_prop_df[2],
                                         Ain_Beida_order_prop_df[2],
                                         Asuncion_order_prop_df[2],
                                         Belem_order_prop_df[2],
                                         Bernal_order_prop_df[2],
                                         Biala_Podlaska_order_prop_df[2],
                                         Bialystok_order_prop_df[2],
                                         Bydgoszcz_order_prop_df[2],
                                         Ciudad_Juarez_order_prop_df[2],
                                         Czestochowa_order_prop_df[2],
                                         Durango_order_prop_df[2],
                                         Fresno_order_prop_df[2],
                                         Gdansk_order_prop_df[2],
                                         Gdynia_order_prop_df[2],
                                         Gorzow_Wielkopolski_order_prop_df[2],
                                         Gujranwala_order_prop_df[2],
                                         Gwangju_order_prop_df[2],
                                         Hamina_order_prop_df[2],
                                         Helsinki_order_prop_df[2],
                                         Inowroclaw_order_prop_df[2],
                                         Joensuu_order_prop_df[2],
                                         Jyvaskyla_order_prop_df[2],
                                         Kajaani_order_prop_df[2],
                                         Kathmandu_order_prop_df[2],
                                         Kauhava_order_prop_df[2],
                                         Kemi_order_prop_df[2],
                                         Kemijarvi_order_prop_df[2],
                                         Kotka_order_prop_df[2],
                                         Kouvola_order_prop_df[2],
                                         Krakow_order_prop_df[2],
                                         Laitila_order_prop_df[2],
                                         Lappeenranta_order_prop_df[2],
                                         Lavras_order_prop_df[2],
                                         Layyah_order_prop_df[2],
                                         Lodz_order_prop_df[2],
                                         Loimaa_order_prop_df[2],
                                         Lublin_order_prop_df[2],
                                         Mar_del_Plata_order_prop_df[2],
                                         Montpellier_order_prop_df[2],
                                         Naantali_order_prop_df[2],
                                         Nanning_order_prop_df[2],
                                         Olsztyn_order_prop_df[2],
                                         Ostrow_Wielkopolski_order_prop_df[2],
                                         Oulu_order_prop_df[2],
                                         Pachuca_order_prop_df[2],
                                         Paimio_order_prop_df[2],
                                         Pargas_order_prop_df[2],
                                         Patras_order_prop_df[2],
                                         Pelotas_order_prop_df[2],
                                         Phoenix_order_prop_df[2],
                                         Pila_order_prop_df[2],
                                         Piotrkow_Trybunalski_order_prop_df[2],
                                         Poznan_order_prop_df[2],
                                         Pretoria_order_prop_df[2],
                                         Przemysl_order_prop_df[2],
                                         Pyhasalmi_order_prop_df[2],
                                         Quezon_City_order_prop_df[2],
                                         Raisio_order_prop_df[2],
                                         Rovaniemi_order_prop_df[2],
                                         Rzeszow_order_prop_df[2],
                                         Salo_order_prop_df[2],
                                         Santiago_order_prop_df[2],
                                         Siedlce_order_prop_df[2],
                                         Slupsk_order_prop_df[2],
                                         Sofia_order_prop_df[2],
                                         Suolahti_order_prop_df[2],
                                         Swidnica_order_prop_df[2],
                                         Szczecin_order_prop_df[2],
                                         Taubate_order_prop_df[2],
                                         Tornio_order_prop_df[2],
                                         Torun_order_prop_df[2],
                                         Tucson_order_prop_df[2],
                                         Turku_order_prop_df[2],
                                         Uusikaupunki_order_prop_df[2],
                                         Vantaa_order_prop_df[2],
                                         Vienna_order_prop_df[2],
                                         Vitoria_Gasteiz_order_prop_df[2],
                                         Warsaw_order_prop_df[2],
                                         Wroclaw_order_prop_df[2],
                                         Xalapa_order_prop_df[2],
                                         Zielona_Gora_order_prop_df[2])


### Export Proportions of avian Orders functional traits for comparison
write.csv(mean_subs_prop_order, file = "..\\data\\output_data\\Delta_avian_orders\\mean_subs_prop_order.csv", row.names = FALSE)

### Read Proportions of avian Orders functional traits for comparison
mean_subs_prop_order <- read.csv(file = "..\\data\\output_data\\Delta_avian_orders\\mean_subs_prop_order.csv", header = TRUE, row.names = 1)

### Transpose merged data frames
mean_subs_prop_order <- as.data.frame(t(mean_subs_prop_order))

### Change column names (Avian order)
colnames(mean_subs_prop_order) <- c("Accipitriformes",
                                    "Apodiformes",
                                    "Bucerotiformes",
                                    "Caprimulgiformes",
                                    "Cariamiformes",
                                    "Coliiformes",
                                    "Columbiformes",
                                    "Coraciiformes",
                                    "Cuculiformes",
                                    "Falconiformes",
                                    "Galliformes",
                                    "Musophagiformes",
                                    "Opisthocomiformes",
                                    "Otidiformes",
                                    "Passeriformes",
                                    "Piciformes",
                                    "Psittaciformes",
                                    "Pteroclidiformes",
                                    "Rheiformes",
                                    "Strigiformes",
                                    "Struthioniformes",
                                    "Tinamiformes",
                                    "Trogoniformes")
  
### One sample t-test for each continuous traits
ttest_order_Accipitriformes <- t.test(mean_subs_prop_order$Accipitriformes, mu = 0, alternative = "two.sided")
ttest_order_Apodiformes <- t.test(mean_subs_prop_order$Apodiformes, mu = 0, alternative = "two.sided")
ttest_order_Bucerotiformes <- t.test(mean_subs_prop_order$Bucerotiformes, mu = 0, alternative = "two.sided")
ttest_order_Caprimulgiformes <- t.test(mean_subs_prop_order$Caprimulgiformes, mu = 0, alternative = "two.sided")
ttest_order_Cariamiformes <- t.test(mean_subs_prop_order$Cariamiformes, mu = 0, alternative = "two.sided")
ttest_order_Coliiformes <- t.test(mean_subs_prop_order$Coliiformes, mu = 0, alternative = "two.sided")
ttest_order_Columbiformes <- t.test(mean_subs_prop_order$Columbiformes, mu = 0, alternative = "two.sided")
ttest_order_Coraciiformes <- t.test(mean_subs_prop_order$Coraciiformes, mu = 0, alternative = "two.sided")
ttest_order_Cuculiformes <- t.test(mean_subs_prop_order$Cuculiformes, mu = 0, alternative = "two.sided")
ttest_order_Falconiformes <- t.test(mean_subs_prop_order$Falconiformes, mu = 0, alternative = "two.sided")
ttest_order_Galliformes <- t.test(mean_subs_prop_order$Galliformes, mu = 0, alternative = "two.sided")
ttest_order_Musophagiformes <- t.test(mean_subs_prop_order$Musophagiformes, mu = 0, alternative = "two.sided")
ttest_order_Opisthocomiformes <- t.test(mean_subs_prop_order$Opisthocomiformes, mu = 0, alternative = "two.sided")
ttest_order_Otidiformes <- t.test(mean_subs_prop_order$Otidiformes, mu = 0, alternative = "two.sided")
ttest_order_Passeriformes <- t.test(mean_subs_prop_order$Passeriformes, mu = 0, alternative = "two.sided")
ttest_order_Piciformes <- t.test(mean_subs_prop_order$Piciformes, mu = 0, alternative = "two.sided")
ttest_order_Psittaciformes <- t.test(mean_subs_prop_order$Psittaciformes, mu = 0, alternative = "two.sided")
ttest_order_Pteroclidiformes <- t.test(mean_subs_prop_order$Pteroclidiformes, mu = 0, alternative = "two.sided")
ttest_order_Rheiformes <- t.test(mean_subs_prop_order$Rheiformes, mu = 0, alternative = "two.sided")
ttest_order_Strigiformes <- t.test(mean_subs_prop_order$Strigiformes, mu = 0, alternative = "two.sided")
ttest_order_Struthioniformes <- t.test(mean_subs_prop_order$Struthioniformes, mu = 0, alternative = "two.sided")
ttest_order_Tinamiformes <- t.test(mean_subs_prop_order$Tinamiformes, mu = 0, alternative = "two.sided")
ttest_order_Trogoniformes <- t.test(mean_subs_prop_order$Trogoniformes, mu = 0, alternative = "two.sided")

### Make a vector containing Avian order names
avian_order_name <- c("Accipitriformes",
                "Apodiformes",
                "Bucerotiformes",
                "Caprimulgiformes",
                "Cariamiformes",
                "Coliiformes",
                "Columbiformes",
                "Coraciiformes",
                "Cuculiformes",
                "Falconiformes",
                "Galliformes",
                "Musophagiformes",
                "Opisthocomiformes",
                "Otidiformes",
                "Passeriformes",
                "Piciformes",
                "Psittaciformes",
                "Pteroclidiformes",
                "Rheiformes",
                "Strigiformes",
                "Struthioniformes",
                "Tinamiformes",
                "Trogoniformes")

### Make a vector containing mean values for Avian orders
avian_order_mean_value <- c(ttest_order_Accipitriformes$estimate,
                       ttest_order_Apodiformes$estimate,
                       ttest_order_Bucerotiformes$estimate,
                       ttest_order_Caprimulgiformes$estimate,
                       ttest_order_Cariamiformes$estimate,
                       ttest_order_Coliiformes$estimate,
                       ttest_order_Columbiformes$estimate,
                       ttest_order_Coraciiformes$estimate,
                       ttest_order_Cuculiformes$estimate,
                       ttest_order_Falconiformes$estimate,
                       ttest_order_Galliformes$estimate,
                       ttest_order_Musophagiformes$estimate,
                       ttest_order_Opisthocomiformes$estimate,
                       ttest_order_Otidiformes$estimate,
                       ttest_order_Passeriformes$estimate,
                       ttest_order_Piciformes$estimate,
                       ttest_order_Psittaciformes$estimate,
                       ttest_order_Pteroclidiformes$estimate,
                       ttest_order_Rheiformes$estimate,
                       ttest_order_Strigiformes$estimate,
                       ttest_order_Struthioniformes$estimate,
                       ttest_order_Tinamiformes$estimate,
                       ttest_order_Trogoniformes$estimate)

### Make a vector containing t-values for Morphology traits
avian_order_tvalue <- c(ttest_order_Accipitriformes$statistic,
                        ttest_order_Apodiformes$statistic,
                        ttest_order_Bucerotiformes$statistic,
                        ttest_order_Caprimulgiformes$statistic,
                        ttest_order_Cariamiformes$statistic,
                        ttest_order_Coliiformes$statistic,
                        ttest_order_Columbiformes$statistic,
                        ttest_order_Coraciiformes$statistic,
                        ttest_order_Cuculiformes$statistic,
                        ttest_order_Falconiformes$statistic,
                        ttest_order_Galliformes$statistic,
                        ttest_order_Musophagiformes$statistic,
                        ttest_order_Opisthocomiformes$statistic,
                        ttest_order_Otidiformes$statistic,
                        ttest_order_Passeriformes$statistic,
                        ttest_order_Piciformes$statistic,
                        ttest_order_Psittaciformes$statistic,
                        ttest_order_Pteroclidiformes$statistic,
                        ttest_order_Rheiformes$statistic,
                        ttest_order_Strigiformes$statistic,
                        ttest_order_Struthioniformes$statistic,
                        ttest_order_Tinamiformes$statistic,
                        ttest_order_Trogoniformes$statistic)

### Make a vector containing p-values for Avian orders
avian_order_pvalue <- c(ttest_order_Accipitriformes$p.value,
                        ttest_order_Apodiformes$p.value,
                        ttest_order_Bucerotiformes$p.value,
                        ttest_order_Caprimulgiformes$p.value,
                        ttest_order_Cariamiformes$p.value,
                        ttest_order_Coliiformes$p.value,
                        ttest_order_Columbiformes$p.value,
                        ttest_order_Coraciiformes$p.value,
                        ttest_order_Cuculiformes$p.value,
                        ttest_order_Falconiformes$p.value,
                        ttest_order_Galliformes$p.value,
                        ttest_order_Musophagiformes$p.value,
                        ttest_order_Opisthocomiformes$p.value,
                        ttest_order_Otidiformes$p.value,
                        ttest_order_Passeriformes$p.value,
                        ttest_order_Piciformes$p.value,
                        ttest_order_Psittaciformes$p.value,
                        ttest_order_Pteroclidiformes$p.value,
                        ttest_order_Rheiformes$p.value,
                        ttest_order_Strigiformes$p.value,
                        ttest_order_Struthioniformes$p.value,
                        ttest_order_Tinamiformes$p.value,
                        ttest_order_Trogoniformes$p.value)

### Merge vectors containing for Avian orders
avian_order_table <- cbind.data.frame(avian_order_name, avian_order_mean_value, 
                                 avian_order_tvalue, avian_order_pvalue)

### Calculate Holm correction for multiple comparisons (Avian orders)
avian_order_table$Holm =
  p.adjust(avian_order_table$avian_order_pvalue,
           method = "holm")

### Get data frames as input for percentile t bootstrap method 
mean_subs_prop_order_input <- reshape2::melt(mean_subs_prop_order, 
                                             measure.vars = c('Accipitriformes',
                                                              'Apodiformes',
                                                              'Bucerotiformes',
                                                              'Caprimulgiformes',
                                                              'Cariamiformes',
                                                              'Coliiformes',
                                                              'Columbiformes',
                                                              'Coraciiformes',
                                                              'Cuculiformes',
                                                              'Falconiformes',
                                                              'Galliformes',
                                                              'Musophagiformes',
                                                              'Opisthocomiformes',
                                                              'Otidiformes',
                                                              'Passeriformes',
                                                              'Piciformes',
                                                              'Psittaciformes',
                                                              'Pteroclidiformes',
                                                              'Rheiformes',
                                                              'Strigiformes',
                                                              'Struthioniformes',
                                                              'Tinamiformes',
                                                              'Trogoniformes'),
                                             variable.name = 'Avian_order')

### Set seed
set.seed(777)

### Perform ANOVA with the percentile t bootstrap method for Avian Orders
anova_boot_avian_order <- t1waybt(value ~ Avian_order, data = mean_subs_prop_order_input, tr = 0, alpha = 0.05, nboot = 99999)

### Generate boxplotfor Avian Orders using delta index
## Make color pallete for Avian Orders
colorder_avian_order <- c("#337495", "#337495", "#337495", "#337495", "#337495",
                          "#337495", "#337495", "#337495", "#337495", "#337495",
                          "#337495", "#337495", "#337495", "#337495", "#337495",
                          "#337495", "#337495", "#337495", "#337495", "#337495",
                          "#337495", "#337495", "#337495")

avian_order_boxplot <- ggplot(mean_subs_prop_order_input, aes(x=value, y=Avian_order, fill = Avian_order)) +
  geom_boxplot(alpha = 0.5) +
  geom_vline(xintercept = 0, colour = "chocolate1", size = 0.8, linetype = "longdash") +
  xlab("\u0394 Percentage of species") + ylab("") +
  scale_y_discrete(labels=c("Accipitriformes"=expression(bold("Accipitriformes")), 
                            "Apodiformes"=expression(plain("Apodiformes")), 
                            "Bucerotiformes"=expression(plain("Bucerotiformes")), 
                            "Caprimulgiformes"=expression(plain("Caprimulgiformes")),
                            "Cariamiformes"=expression(plain("Cariamiformes")), 
                            "Coliiformes"=expression(plain("Coliiformes")), 
                            "Columbiformes"=expression(bold("Columbiformes")), 
                            "Coraciiformes"=expression(plain("Coraciiformes")),
                            "Cuculiformes"=expression(plain("Cuculiformes")),
                            "Falconiformes"=expression(bold("Falconiformes")),
                            "Galliformes"=expression(bold("Galliformes")),
                            "Musophagiformes"=expression(plain("Musophagiformes")),
                            "Opisthocomiformes"=expression(plain("Opisthocomiformes")),
                            "Otidiformes"=expression(plain("Otidiformes")),
                            "Passeriformes"=expression(bold("Passeriformes")),
                            "Piciformes"=expression(bold("Piciformes")),
                            "Psittaciformes"=expression(plain("Psittaciformes")),
                            "Pteroclidiformes"=expression(plain("Pteroclidiformes")),
                            "Rheiformes"=expression(plain("Rheiformes")),
                            "Strigiformes"=expression(bold("Strigiformes")),
                            "Struthioniformes"=expression(plain("Struthioniformes")),
                            "Tinamiformes"=expression(plain("Tinamiformes")),
                            "Trogoniformes"=expression(plain("Trogoniformes")))) +
  scale_x_continuous(limits = c(-60, 60), breaks = c(-60, -40, -20, 0, 20, 40, 60)) +
  scale_fill_manual(values=colorder_avian_order) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 20, face = "plain", colour = "black"), 
        axis.text.y = element_text(size = 20, face = "plain", colour = "black"),
        axis.line.x.bottom = element_line(size = 1.3),
        axis.line.x.top = element_line(size = 1),
        axis.line.y.left = element_line(size = 1.3),
        axis.line.y.right = element_line(size = 1),
        axis.title.x = element_text(size = 20, face = "bold"), 
        axis.title.y = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20), 
        axis.ticks=element_line(size = 1.3, colour = "black"),
        legend.margin = element_blank(), axis.ticks.length = unit(.30, "cm"),
        legend.position = "none")


### Export boxplot of Avian order proportions
png(filename="..\\data\\output_data\\Figures\\Boxplots\\avian_order_boxplots.png", units="mm", width=200, height=300, pointsize=15, res=2000)
ggdraw(avian_order_boxplot) 
dev.off()

### Save Workspace
save.image("../data/output_data/Workspaces/Bird_order_funct_trait_delta.Rdata")
