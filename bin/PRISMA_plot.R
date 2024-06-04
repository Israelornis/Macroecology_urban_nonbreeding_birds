##### PRISMA #####
######### Script to get PRISMA flow diagram #######
library("PRISMA2020")
library("DiagrammeR")
library("rsvg")
library("DiagrammeRsvg")
library("htmltools")
library("magick") ### https://cran.r-project.org/web/packages/magick/vignettes/intro.html

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspace
load("../data/output_data/Workspaces/PRISMA_plot.Rdata")

### Read PRISMA csv template
data_WOS <- read.csv("..\\data\\raw_data\\Prisma_input\\PRISMA3.csv")

### Convert data frame into PRISMA data
data_WOS <- read_PRISMAdata(data_WOS)

### Attach PRISMA data
attach(data_WOS)

### Gnerate PRISMA flowdiagram
prisma_plot <- PRISMA_flowdiagram(data_WOS,
                           fontsize = 15,
                           interactive = TRUE,
                           previous = FALSE,
                           other = TRUE)

### Visualize PRISMA plot
prisma_plot

### Export prism plot to editing
export_svg(prisma_plot) %>%
  charToRaw() %>%
  rsvg() %>%
  png::writePNG("..\\data\\output_data\\Figures\\Prisma_workflow\\prisma_plot.png", dpi = 500)


### Read prima plot to editing
prima_plot_edit <- image_read("..\\data\\output_data\\Figures\\Prisma_workflow\\prisma_plot.png")

### Add labels on blue bars
prima_plot_edit <- image_annotate(prima_plot_edit, "Identification", location = "+13+190",
                                  degrees = 270,
                                  font = 'Helvetica', size = 18)

prima_plot_edit <- image_annotate(prima_plot_edit, "Screening", location = "+13+400",
                                  degrees = 270,
                                  font = 'Helvetica', size = 18)

prima_plot_edit <- image_annotate(prima_plot_edit, "Included", location = "+13+668",
                                  degrees = 270,
                                  font = 'Helvetica', size = 18)

### Visualize PRISMA plot edited
prima_plot_edit

### Export PRISMA plot edited as jpeg
image_write(prima_plot_edit, "..\\data\\output_data\\Figures\\Prisma_workflow\\prima_plot_edit.jpeg", quality = 5)

### Export PRISMA plot edited as high resolution raster including other studies
tiff(filename="..\\data\\output_data\\Figures\\Prisma_workflow\\prisma_plot_high_resolution.tif", compression = "lzw", units="mm", width=500, height=270, pointsize=0.3, res=500)
plot(as.raster(prima_plot_edit))
dev.off()

### Save Workspace
save.image("../data/output_data/Workspaces/PRISMA_plot.Rdata")