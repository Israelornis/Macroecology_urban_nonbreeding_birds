############################################################
#   Script to generate boxplots of multifaceted diversity  #
############################################################

### Script elaborated by Israel Moreno-Contreras (Ornithology, UNAM)

### Call libraries
library("ggpubr")
library("ggplot2")
library("cowplor") # For ggdraw function

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspace
load("../data/output_data/Workspaces/Boxplot_multifaceted_div.Rdata")

### Read csv files containing merged Phylogenetic diversity metrics based on Prum backbone
PD_metrics_prum_merge <- read.csv("..\\data\\output_data\\Diversity_metrics\\PD_metrics_prum_merge.csv", header = TRUE, row.names = 1)

### Read csv files containing merged Functional diversity metrics
FD_metrics_merge <- read.csv("..\\data\\output_data\\Diversity_metrics\\FD_metrics_merge.csv", header = TRUE, row.names = 1)

### Generate boxplot for the taxonomic diversity (Species richness)
SR_boxplot <- PD_metrics_prum_merge %>%
  ggplot(aes(Species_pool, Richness)) +
  geom_violin(trim=FALSE, fill='gray86', color= "#749BC2", alpha=0.1) +
  geom_boxplot(aes(x = Species_pool,
                   y = Richness),
               width=0.3, size=1,
               fatten=1.5,
               colour="black", fill = c("#0D1282", "lightblue"), alpha=0.7) +
  geom_point(aes(x = Species_pool,
                 y = Richness),
             colour="dodgerblue4",
             size=2,
             alpha=0.5) +
  geom_line(aes(x  = Species_pool,
                y = Richness,
                group = City_code),
            colour="grey70",
            linetype="11") +
  xlab("Species pool") + ylab("Species richness") + 
  scale_y_continuous(limits = c(0, 450), breaks = c(0, 100, 200, 300, 400)) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3, colour = "black"), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14))

### Generate boxplot for the ses.MPD based on Prum backbone
PD_prum_boxplot <- PD_metrics_prum_merge %>%
  ggplot(aes(Species_pool, ses.MPD_prum)) +
  geom_hline(yintercept = 1.96, linetype="dashed", colour="black", size=1.5) +
  geom_hline(yintercept = -1.96, linetype="dashed", colour="red", size=1.5) +
  geom_violin(trim=FALSE, fill='gray86', color= "maroon4", alpha=0.1) +
  geom_boxplot(aes(x = Species_pool,
                   y = ses.MPD_prum),
               width=0.3, size=1,
               fatten=1.5,
               colour="black", fill = c("darkorchid4", "plum"), alpha=0.7) +
  geom_point(aes(x = Species_pool,
                 y = ses.MPD_prum),
             colour="dodgerblue4",
             size=2,
             alpha=0.5) +
  geom_line(aes(x  = Species_pool,
                y = ses.MPD_prum,
                group = City_code),
            colour="grey70",
            linetype="11") +
  xlab("Species pool") + ylab("sesMPD") + 
  theme_bw() +
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3, colour = "black"), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14))

### Generate boxplot for the ses.FMPD 
FD_boxplot <- FD_metrics_merge %>%
  ggplot(aes(Species_pool, MFD_estand_gow_vec)) +
  geom_hline(yintercept = 1.96, linetype="dashed", colour="black", size=1.5) +
  geom_hline(yintercept = -1.96, linetype="dashed", colour="red", size=1.5) +
  geom_violin(trim=FALSE, fill='gray86', color= "olivedrab4", alpha=0.1) +
  geom_boxplot(aes(x = Species_pool,
                   y = MFD_estand_gow_vec),
               width=0.3, size=1,
               fatten=1.5,
               colour="black", fill = c("darkgreen", "greenyellow"), alpha=0.7) +
  geom_point(aes(x = Species_pool,
                 y = MFD_estand_gow_vec),
             colour="dodgerblue4",
             size=2,
             alpha=0.5) +
  geom_line(aes(x  = Species_pool,
                y = MFD_estand_gow_vec,
                group = City_code),
            colour="grey70",
            linetype="11") +
  xlab("Species pool") + ylab("sesFMPD") + 
  theme_bw() + 
  theme(axis.text.x = element_text(size = 14, face = "plain", colour = "black"), axis.text.y = element_text(size = 14, face = "plain", colour = "black"),
        panel.border = element_blank(), axis.line = element_line(size = 1.3),
        axis.title.x = element_text(size = 14, face = "bold"), 
        axis.title.y = element_text(size = 14, face = "bold"),
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 14), 
        axis.ticks=element_line(size = 1.3, colour = "black"), axis.ticks.length = unit(.30, "cm"),
        legend.title = element_text(size = 14, face = "bold"), legend.text = element_text(size = 14))

### Merge boxplots for multifaceted diversity (Taxonomic, Phylogenetic, and Functional diversity)
boxplots_sr_phy_func_plots <- ggarrange(SR_boxplot, PD_prum_boxplot, FD_boxplot,
                                        labels = c("(a)", "(b)", "(c)"),
                                        ncol = 3, nrow = 1)


### Export boxplots for multifaceted diversity (Taxonomic, Phylogenetic, and Functional diversity)
png(filename="..\\data\\output_data\\Figures\\Boxplots\\boxplots_sr_phy_func_plots.png", units="mm", width=300, height=140, pointsize=15, res=2000)
ggdraw(boxplots_sr_phy_func_plots) 
dev.off()

### Save Workspace
save.image("../data/output_data/Workspaces/Boxplot_multifaceted_div.Rdata")

###############################################################################

#### Paired t-test ####

### Read csv files containing merged Phylogenetic diversity metrics for regional species pools
PD_metrics.results_prum_regional <- read.csv("..\\data\\output_data\\Diversity_metrics\\PD_metrics.results_prum_regional.csv", header = TRUE, row.names = 1)

### Read csv files containing merged Phylogenetic diversity metrics for urban species pools
PD_metrics.results_prum_urban <- read.csv("..\\data\\output_data\\Diversity_metrics\\PD_metrics.results_prum_urban.csv", header = TRUE, row.names = 1)

### Read csv files containing merged Functional diversity metrics for regional species pools
FD_metrics.results_regional <- read.csv("..\\data\\output_data\\Diversity_metrics\\FD_metrics.results_regional.csv", header = TRUE, row.names = 1)

### Read csv files containing merged Functional diversity metrics for urban species pools
FD_metrics.results_urban <- read.csv("..\\data\\output_data\\Diversity_metrics\\FD_metrics.results_urban.csv", header = TRUE, row.names = 1)

### Perform paired t-test between regional and urban species pools
t.test(PD_metrics.results_prum_regional$Richness, PD_metrics.results_prum_urban$Richness, paired = TRUE)

t.test(PD_metrics.results_prum_regional$ses.MPD_prum, PD_metrics.results_prum_urban$ses.MPD_prum, paired = TRUE)

t.test(FD_metrics.results_regional$MFD_estand_gow_vec, FD_metrics.results_urban$MFD_estand_gow_vec, paired = TRUE)

### Median, mean, SD, and range
median(PD_metrics.results_prum_regional$Richness)

mean(PD_metrics.results_prum_regional$Richness)

sd(PD_metrics.results_prum_regional$Richness)

range(PD_metrics.results_prum_regional$Richness)

median(PD_metrics.results_prum_urban$Richness)

mean(PD_metrics.results_prum_urban$Richness)

sd(PD_metrics.results_prum_urban$Richness)

range(PD_metrics.results_prum_urban$Richness)

### Correlation tests between PD using PRUM backbone and HACKETT backbone
cor.test(PD_metrics.results_prum$ses.MPD_prum, PD_metrics.results_hack$ses.MPD_hack)

### Save Workspace
save.image("../data/output_data/Workspaces/Boxplot_multifaceted_div.Rdata")

#################################################################################################