#####################################################################
######### Script for the Standardised total effect size plots #######
#####################################################################

### Script elaborated by Israel Moreno-Contreras (Ornithology, UNAM)

### Call libraries
library("ggplot2")
library("dplyr")
library("tidyr")
library("ggpubr")
library("cowplot")

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspace
load("../data/output_data/Workspaces/STES_plots.Rdata")


### Taxonomic diversity: Standardised total effect size plots
TD_stes <- structure(list(Variable = c("TMIN", "ELEV", "CAGE", "GDP", 
                                       "UARE", "PREC", "DCOA"), 
                          stes = c(0.511, 0.398, -0.140, 0.095, 
                                   0.304, 0.169, 0.104)), 
                     row.names = c(NA, -7L), class = c("tbl_df", "tbl", "data.frame"))


TD_stes_trends = c("Positive", "Positive", "Negative", "Positive", "Positive", "Positive", "Positive")


### Taxonomic diversity plot
TD_plot <- TD_stes %>% select(c(Variable,stes)) %>%
  pivot_longer(-1) %>%
  ggplot(aes(x=Variable,y=value, fill=TD_stes_trends))+
  geom_bar(stat = "identity", width = 0.5, position = "dodge", colour="black")+
  labs(y= "Standardised total\n effect size")+
  geom_abline(intercept = 0, slope = 0, color="black")+
  scale_y_continuous(limits = c(-1.0, 1.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-1.0, -0.5, 0, 0.5, 1.0)) +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  theme_bw()+
  scale_fill_manual("Relationship", values = c("black", "red"), breaks=c('Positive', 'Negative')) +
  theme(axis.text.x = element_text(size = 20, face = "plain", colour = "black"), 
        axis.text.y = element_text(size = 20, face = "plain", colour = "black"),
        axis.line.x.bottom = element_line(size = 1.3),
        axis.line.x.top = element_line(size = 1),
        axis.line.y.left = element_line(size = 1.3),
        axis.line.y.right = element_line(size = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20), 
        axis.ticks=element_line(size = 1.3, colour = "black"),
        legend.margin = element_blank(), axis.ticks.length = unit(.30, "cm"),
        legend.position = "none",
        plot.margin = margin(1,1,1,1, unit = "mm"))



####################
### Phylogenetic diversity: Standardised total effect size plots
sesMPD_stes <- structure(list(Variable = c("TD", "TMIN", "PREC", 
                                           "ELEV", "UARE", "CAGE", 
                                           "DRIV", "DCOA", "RDEN",
                                           "GDP"), 
                          stes = c(-0.259, 0.186, -0.183, 
                                   -0.537, -0.078, 0.036, 
                                   0.140, 0.315, 0.222,
                                   -0.247)), 
                     row.names = c(NA, -10L), class = c("tbl_df", "tbl", "data.frame"))


sesMPD_stes_trends = c("Negative", "Positive", "Negative",
                       "Negative", "Negative", "Positive", 
                       "Positive", "Positive", "Positive",
                       "Negative")


### Phylogenetic diversity plot
sesMPD_plot <- sesMPD_stes %>% select(c(Variable,stes)) %>%
  pivot_longer(-1) %>%
  ggplot(aes(x=Variable,y=value, fill=sesMPD_stes_trends))+
  geom_bar(stat = "identity", width = 0.5, position = "dodge", colour="black")+
  labs(y= "Standardised total\n effect size")+
  geom_abline(intercept = 0, slope = 0, color="black")+
  scale_y_continuous(limits = c(-1.0, 1.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-1.0, -0.5, 0, 0.5, 1.0)) +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  theme_bw()+
  scale_fill_manual("Relationship", values = c("black", "red"), breaks=c('Positive', 'Negative')) +
  theme(axis.text.x = element_text(size = 20, face = "plain", colour = "black"), 
        axis.text.y = element_text(size = 20, face = "plain", colour = "black"),
        axis.line.x.bottom = element_line(size = 1.3),
        axis.line.x.top = element_line(size = 1),
        axis.line.y.left = element_line(size = 1.3),
        axis.line.y.right = element_line(size = 1),
        axis.title.x = element_blank(), 
        axis.title.y = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20), 
        axis.ticks=element_line(size = 1.3, colour = "black"),
        legend.margin = element_blank(), axis.ticks.length = unit(.30, "cm"),
        legend.position = "none", 
        plot.margin = margin(1,1,1,1, unit = "mm"))


#################################

### Functional diversity: Standardised total effect size plots
sesFMPD_stes <- structure(list(Variable = c("sesMPD", "NDVI", "RDEN", 
                                            "TD", "TMIN", "PREC", 
                                            "ELEV", "UARE", "CAGE", 
                                            "DRIV", "DCOA", "GDP"), 
                              stes = c(0.604, -0.248, 0.362, 
                                       -0.156, -0.109, -0.183, 
                                       -0.324, -0.047, -0.027, 
                                       0.084, 0.088, -0.190)), 
                         row.names = c(NA, -12L), class = c("tbl_df", "tbl", "data.frame"))


sesFMPD_stes_trends = c("Positive", "Negative", "Positive", 
                        "Negative", "Negative", "Negative", 
                        "Negative", "Negative", "Negative", 
                        "Positive", "Positive", "Negative")


### Functional diversity plot
sesFMPD_plot <- sesFMPD_stes %>% select(c(Variable,stes)) %>%
  pivot_longer(-1) %>%
  ggplot(aes(x=Variable,y=value, fill=sesFMPD_stes_trends))+
  geom_bar(stat = "identity", width = 0.5, position = "dodge", colour="black")+
  labs(y= "Standardised total\n effect size")+
  geom_abline(intercept = 0, slope = 0, color="black")+
  scale_y_continuous(limits = c(-1.0, 1.0), labels = scales::number_format(accuracy = 0.1,
                                                                           decimal.mark = '.'), breaks = c(-1.0, -0.5, 0, 0.5, 1.0)) +
  scale_x_discrete(guide = guide_axis(angle = 30)) +
  theme_bw()+
  scale_fill_manual("Relationship", values = c("black", "red"), breaks=c('Positive', 'Negative')) +
  theme(axis.text.x = element_text(size = 20, face = "plain", colour = "black"), 
        axis.text.y = element_text(size = 20, face = "plain", colour = "black"),
        axis.line.x.bottom = element_line(size = 1.3),
        axis.line.x.top = element_line(size = 1),
        axis.line.y.left = element_line(size = 1.3),
        axis.line.y.right = element_line(size = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 20, face = "bold"),
        plot.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20), 
        axis.ticks=element_line(size = 1.3, colour = "black"),
        legend.margin = element_blank(), axis.ticks.length = unit(.30, "cm"),
        legend.position = "none",
        plot.margin = margin(1,1,1,1, unit = "mm"))

### Merge Standardised total effect size plots
effects_plots <- ggarrange(TD_plot, sesMPD_plot, sesFMPD_plot,
                                 ncol = 1, nrow = 3)


### Export Standardised total effect size plots
png(filename="..\\data\\output_data\\Figures\\Effects_plots\\effects_plots.png", units="mm", width=200, height=300, pointsize=15, res=2000)
ggdraw(effects_plots) 
dev.off()

### Save Workspace
save.image("../data/output_data/Workspaces/STES_plots.Rdata")

