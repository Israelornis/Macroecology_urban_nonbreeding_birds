#######################################
##### Species accumulation curves #####
#######################################

### Script elaborated by Israel Moreno-Contreras (Ornithology, UNAM)

### Call libraries
library("vegan")
library("ggplot2")
library("tidyverse")
library("ggpubr")
library("cowplot")

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspace
load("../data/output_data/Workspaces/Species_Accumulation_Curves.Rdata")

### Read community-level data
bird_sample_urban_regional <- read.csv(file = "..\\data\\raw_data\\Community_level_data\\bird_sample_urban_regional_2.csv", row.names = 1)

### Keep urban species pool only
bird_sample_urban <- bird_sample_urban_regional[82:162,]

comm_urban <- bird_sample_urban %>% select_if(colSums(.) != 0)

### Keep regional species pool only
bird_sample_regional <- bird_sample_urban_regional[1:81,]

comm_regional <- bird_sample_regional %>% select_if(colSums(.) != 0)

### Set seed
set.seed(777)

### Get species accumulation curves for urban species pools ### 
pool_urban_birds_rand <- specaccum(comm_urban[, 1:548], method = "random", permutations = 1000)

### Set seed
set.seed(777)

### Get species accumulation curves for regional species pools ### 
pool_regional_birds_rand <- specaccum(comm_regional[, 1:2175], method = "random", permutations = 1000)

### Plot all: obs richness and  estimators for urban species pools
plot(pool_urban_birds_rand, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(pool_urban_birds_rand, col="yellow", add=TRUE, pch="+")

### Plot all: obs richness and  estimators for regional species pools
plot(pool_regional_birds_rand, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(pool_regional_birds_rand, col="yellow", add=TRUE, pch="+")

### Estimate extrapolation species richness USING Bootstrap for urban species pools
non_param_urban <- specpool(comm_urban[, 1:548], smallsample = TRUE)

### Estimate extrapolation species richness USING Bootstrap for regional species pools
non_param_regional <- specpool(comm_regional[, 1:2175], smallsample = TRUE)

### Create a data frame containing species accumulation curve data for urban species pools
df_quantile_urban = data.frame()
for (i in 1:81){
  # vector output
  quantile_25 <- quantile(pool_urban_birds_rand[[6]][i,], 0.25) #some processing
  quantile_75 <- quantile(pool_urban_birds_rand[[6]][i,], 0.75) #some processing
  # add vector to a dataframe
  df_qauntile_25_75 <- data.frame(quantile_25, quantile_75)
  df_quantile_urban <- rbind(df_quantile_urban,df_qauntile_25_75)
}

data_specaccum_urban <- data.frame(Sites=pool_urban_birds_rand$sites, 
                   Richness=pool_urban_birds_rand$richness, 
                   SD=pool_urban_birds_rand$sd,
                   quant_25=df_quantile_urban$quantile_25,
                   quant_75=df_quantile_urban$quantile_75)

### Create a data frame containing species accumulation curve data for regional species pools
df_quantile_regional = data.frame()
for (i in 1:81){
  # vector output
  quantile_25 <- quantile(pool_regional_birds_rand[[6]][i,], 0.25) #some processing
  quantile_75 <- quantile(pool_regional_birds_rand[[6]][i,], 0.75) #some processing
  # add vector to a dataframe
  df_qauntile_25_75 <- data.frame(quantile_25, quantile_75)
  df_quantile_regional <- rbind(df_quantile_regional,df_qauntile_25_75)
}

data_specaccum_regional <- data.frame(Sites=pool_regional_birds_rand$sites, 
                                   Richness=pool_regional_birds_rand$richness, 
                                   SD=pool_regional_birds_rand$sd,
                                   quant_25=df_quantile_regional$quantile_25,
                                   quant_75=df_quantile_regional$quantile_75)

### Generate species accumulation curve for urban species pools using ggplot2
specaccum_plot_urban <- ggplot() +
  geom_segment(data=data_specaccum_urban, aes(Sites, y = (Richness-2*SD), xend=Sites, yend=(Richness+2*SD)), color = "#acd3bf", size = 1) +
  geom_line(data=data_specaccum_urban, aes(x=Sites, y=Richness), colour = "chocolate1", size = 1.3) +
  xlab("Sites") + ylab("Species richness") + 
  scale_x_continuous(limits = c(0, 81), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(-20, 600), breaks = c(0, 200, 400, 600)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 13, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 13, face = "bold"), 
        axis.title.y = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 13), 
        axis.ticks=element_line(size = 1.3, colour = "black"), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 13, face = "bold"), 
        legend.text = element_text(size = 13))

### Export species accumulation curve figure for urban species pools
png(filename="..\\data\\output_data\\Figures\\Spec_accu_curve\\specaccum_plot_urban.png", units="mm", width=200, height=100, pointsize=15, res=2200)
specaccum_plot_urban
dev.off()

### Generate species accumulation curve for regional species pools using ggplot2
specaccum_plot_regional <- ggplot() +
  geom_segment(data=data_specaccum_regional, aes(Sites, y = (Richness-2*SD), xend=Sites, yend=(Richness+2*SD)), color = "#acd3bf", size = 1) +
  geom_line(data=data_specaccum_regional, aes(x=Sites, y=Richness), colour = "chocolate1", size = 1.3) +
  xlab("Sites") + ylab("Species richness") + 
  scale_x_continuous(limits = c(0, 81), breaks = c(0, 20, 40, 60, 80)) +
  scale_y_continuous(limits = c(-100, 2500), breaks = c(0, 500, 1000, 1500, 2000, 2500)) +
  theme_bw() + 
  theme(axis.text.x = element_text(size = 13, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 13, face = "bold"), 
        axis.title.y = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 13, face = "bold"),
        axis.text = element_text(size = 13), 
        axis.ticks=element_line(size = 1.3, colour = "black"), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 13, face = "bold"), 
        legend.text = element_text(size = 13))

### Export species accumulation curve figure for regional species pools
png(filename="..\\data\\output_data\\Figures\\Spec_accu_curve\\specaccum_plot_regional.png", units="mm", width=200, height=100, pointsize=15, res=2200)
specaccum_plot_regional
dev.off()


### Merge Species Accumulation curves
specaccum_plots <- ggarrange(specaccum_plot_urban, specaccum_plot_regional,
                                        ncol = 2, nrow = 1)


### Export Species Accumulation curves
png(filename="..\\data\\output_data\\Figures\\Spec_accu_curve\\specaccum_plots.png", units="mm", width=300, height=140, pointsize=15, res=2200)
ggdraw(specaccum_plots) +
  draw_label("(a)", x = 0.01, y = 0.985, fontface = "bold", size = 13) +
  draw_label("(b)", x = 0.51, y = 0.985, fontface = "bold", size = 13)
dev.off()


### Save Workspace
save.image("../data/output_data/Workspaces/Species_Accumulation_Curves.Rdata")

#################################################################################################