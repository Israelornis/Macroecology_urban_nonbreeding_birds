#############################################
######### Script for the pSEM diagram #######
#############################################

### Script elaborated by Israel Moreno-Contreras (Ornithology, UNAM)

### Call libraries
library("DiagrammeR")
library("rsvg")
library("DiagrammeRsvg")
library("htmltools")
library("magick") ### https://cran.r-project.org/web/packages/magick/vignettes/intro.html
library("cowplot")

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspace
load("../data/output_data/Workspaces/piecewiseSEM_flow_diagram_postGCB.Rdata")


############################################################################################################

################# PIECEWISESEM

### Fit final pSEM: 5 EQUATIONS
sem_final_5_ecua_piecewiseSEM <- grViz("
 digraph {
 
   graph [ranksep = 1]
 node[shape = septagon, style = filled, fillcolor = lightblue, color = black, height = 1.0, width = 1.5]
   'Taxonomic diversity' [shape = 'septagon', fontname = 'helvetica-bold', penwidth=3]
 node[shape = oval, style = filled, fillcolor = plum, color = black, width = 1.0]
   'sesMPD' [shape = 'oval', fontname = 'helvetica-bold', penwidth=3]
 node[shape = oval, style = filled, fillcolor = greenyellow, color = black, width = 1.1]
   'sesFMPD' [shape = 'oval', fontname = 'helvetica-bold', penwidth=3]

 node[shape = 'rectangle', style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.5, width = 1.3]
   'Precipitation' [shape = rectangle, fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.7, width = 1.3]
   'Minimum
temperature' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.4, width = 0.7]
   'NDVI' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.4, width = 1.0]
   'Elevation' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = 'rectangle', style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.3]
   'Road density' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 0.9]
   'City age' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 0.7]
   'GDP' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 1.2]
   'Urban area' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.8]
   'Distance to rivers' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.8]
   'Distance to coasts' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]


   'Minimum
temperature'->'Taxonomic diversity' [style = 'solid', fontname = 'Helvetica', label = '  0.511', color = black, penwidth = 5.11]
   'Elevation'->'Taxonomic diversity' [style = 'solid', fontname = 'Helvetica', label = '  0.398', color = black, penwidth = 3.98]
   'City age'->'Taxonomic diversity' [style = 'solid', fontname = 'Helvetica', label = '  -0.140', color = red, penwidth = 1.40]
   'Urban area'->'Taxonomic diversity' [style = 'solid', fontname = 'Helvetica', label = '  0.304', color = black, penwidth = 3.04]

   'Taxonomic diversity'->'sesMPD' [style = 'dashed', fontname = 'Helvetica', label = '-0.259', color = red, penwidth = 2.59]
   'Precipitation'->'sesMPD' [style = 'solid', fontname = 'Helvetica', label = '-0.244', color = red, penwidth = 2.44]
   'Minimum
temperature'->'sesMPD' [style = 'solid', fontname = 'Helvetica', label = ' 0.318', color = black, penwidth = 3.18]
   'Elevation'->'sesMPD' [style = 'solid', fontname = 'Helvetica', label = ' -0.434', color = red, penwidth = 4.34]
   'Road density'->'sesMPD' [style = 'solid', fontname = 'Helvetica', label = ' 0.222', color = black, penwidth = 2.22]
   'GDP'->'sesMPD' [style = 'solid', fontname = 'Helvetica', label = ' -0.281', color = red, penwidth = 2.81]
   'Distance to rivers'->'sesMPD' [style = 'dashed', fontname = 'Helvetica', label = '  0.140', color = black, penwidth = 1.40]
   'Distance to coasts'->'sesMPD' [style = 'solid', fontname = 'Helvetica', label = '   0.278', color = black, penwidth = 2.78]


   'sesMPD'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', label = ' 0.604', color = black, penwidth = 6.04]
   'NDVI'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', label = '-0.248', color = red, penwidth = 2.48]
   'Road density'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', label = ' 0.228', color = black, penwidth = 2.28]


   'Precipitation'->
   'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', label = '  0.332', color = black, penwidth = 3.32]
   'GDP'->
   'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', label = '  0.187', color = black, penwidth = 1.87]
   'Distance to coasts'->
   'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', label = ' 0.204', color = black, penwidth = 2.04]


   'Minimum
temperature'->'NDVI' [style = 'solid', fontname = 'Helvetica', label = '  0.893', color = black, penwidth = 8.93]
   'City age'->'NDVI' [style = 'solid', fontname = 'Helvetica', label = '  0.199', color = black, penwidth = 1.99]
   'Distance to coasts'->'NDVI' [style = 'solid', fontname = 'Helvetica', label = '0.230', color = black, penwidth = 2.3]


  # Additional constraints on the graph
  {rank = 'min'; 'Taxonomic diversity'; 'sesFMPD';}
  {rank = 'max'; 'Road density'; 'GDP'; 'Distance to coasts'; 'Distance to rivers';}
}
")



### Fit theoretical pSEM: 5 EQUATIONS 
sem_theoretical_5_ecua_piecewiseSEM <- grViz("
 digraph {
 
   graph [ranksep = 1]
 node[shape = septagon, style = filled, fillcolor = lightblue, color = black, height = 1.0, width = 1.5]
   'Species richness' [shape = 'septagon', fontname = 'helvetica-bold', fontsize = 50]
 node[shape = oval, style = filled, fillcolor = plum, color = black, width = 0.5]
   'sesMPD' [shape = 'oval', fontname = 'helvetica-bold', fontsize = 50]
 node[shape = oval, style = filled, fillcolor = greenyellow, color = black, width = 0.5]
   'sesFMPD' [shape = 'oval', fontname = 'helvetica-bold', fontsize = 50]


 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 1.5, width = 4.2]
   'Precipitation' [shape = 'rectangle', fontname = 'Helvetica', fontsize = 50]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 1.8, width = 4.2]
   'Minimum
temperature' [shape = 'rectangle', fontname = 'Helvetica', fontsize = 50]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 1.5, width = 2.0]
   'NDVI' [shape = 'rectangle', fontname = 'Helvetica', fontsize = 50]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 1.5, width = 3.9]
   'Elevation' [shape = 'rectangle', fontname = 'Helvetica', fontsize = 50]
 node[shape = 'rectangle', style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 1.5, width = 4.4]
   'Road density' [shape = 'rectangle', fontname = 'Helvetica', fontsize = 50]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 1.5, width = 3.0]
   'City age' [shape = 'rectangle', fontname = 'Helvetica', fontsize = 50]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 1.5, width = 2.0]
   'GDP' [shape = 'rectangle', fontname = 'Helvetica', fontsize = 50]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 1.5, width = 4.2]
   'Urban shape' [shape = rectangle, fontname = 'Helvetica', fontsize = 50]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 1.5, width = 3.9]
   'Urban area' [shape = 'rectangle', fontname = 'Helvetica', fontsize = 50]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 1.5, width = 5.8]
   'Distance to rivers' [shape = 'rectangle', fontname = 'Helvetica', fontsize = 50]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 1.5, width = 5.9]
   'Distance to coasts' [shape = 'rectangle', fontname = 'Helvetica', fontsize = 50]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 1.5, width = 7.2]
   'Distance to mountains' [shape = 'rectangle', fontname = 'Helvetica', fontsize = 50]


   'Precipitation'->'Species richness' [minlen = 10, style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'Minimum
temperature'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'NDVI'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'Elevation'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'Road density'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'GDP'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'City age'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Urban area'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'Urban shape'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'Distance to rivers'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Distance to coasts'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'Distance to mountains'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]


   'Species richness'->'sesMPD' [minlen = 10, style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Precipitation'->'sesMPD' [minlen = 10, style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Minimum
temperature'->'sesMPD' [minlen = 10, style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'NDVI'->'sesMPD' [minlen = 10, style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Elevation'->'sesMPD' [minlen = 10, style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Road density'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'City age'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'GDP'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Urban area'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Urban shape'->'sesMPD' [style = 'solid', x = -4, fontname = 'Helvetica', color = red, penwidth = 10]
   'Distance to rivers'->'sesMPD' [style = 'solid', x = -4, fontname = 'Helvetica', color = black, penwidth = 10]
   'Distance to coasts'->'sesMPD' [style = 'solid', x = -4, fontname = 'Helvetica', color = black, penwidth = 10]
   'Distance to mountains'->'sesMPD' [style = 'solid', x = -4, fontname = 'Helvetica', color = red, penwidth = 10]


   'sesMPD'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'Precipitation'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Minimum
temperature'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'NDVI'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Elevation'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Road density'->'sesFMPD' [minlen = 10, style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'City age'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'GDP'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 10]
   'Urban area'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 10]
   'Urban shape'->'sesFMPD' [style = 'solid', x = -4, fontname = 'Helvetica', color = black, penwidth = 10]
   'Distance to rivers'->'sesFMPD' [minlen = 5, style = 'solid', x = -4, fontname = 'Helvetica', color = red, penwidth = 10]
   'Distance to coasts'->'sesFMPD' [minlen = 5, style = 'solid', x = -4, fontname = 'Helvetica', color = black, penwidth = 10]
   'Distance to mountains'->'sesFMPD' [minlen = 5, style = 'solid', x = -4, fontname = 'Helvetica', color = black, penwidth = 10]


   'Precipitation'->
   'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 5]
   'Road density'->
   'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 5]
   'City age'->
   'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 5]
   'GDP'->
   'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 5]
   'Urban area'->
   'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 5]
   'Distance to rivers'->
   'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 5]
   'Distance to coasts'->
   'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 5]
   'Distance to mountains'->
   'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 5]


   'Precipitation'-> 'NDVI' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 5]
   'Minimum
temperature'->
   'NDVI' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 5]
   'Elevation'-> 'NDVI' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 5]
   'City age'-> 'NDVI' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 5]
   'Distance to coasts'-> 'NDVI' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 5]



  # Additional constraints on the graph
  {rank = 'max'; 'City age'; 'Urban area'; 'Urban shape'; 'Road density';}
  {rank = 'same'; 'sesMPD';}
  {rank = 'min'; 'Species richness';}


}
")




### Create function : https://stackoverflow.com/questions/65669640/save-diagrammer-object-to-png-on-disc
save_png <- function(plot, path){
  DiagrammeRsvg::export_svg(plot) %>%
    charToRaw() %>%
    rsvg::rsvg() %>%
    png::writePNG(path)
}

### Export flowchart for editing
save_png(sem_theoretical_5_ecua_piecewiseSEM, path = "..\\data\\output_data\\Figures\\piecewiseSEM\\sem_theoretical_5_ecua_piecewiseSEM_postGCB.png")

save_png(sem_final_5_ecua_piecewiseSEM, path = "..\\data\\output_data\\Figures\\piecewiseSEM\\sem_final_5_ecua_piecewiseSEM_postGCB.png")

### Save Workspace
save.image("../data/output_data/Workspaces/piecewiseSEM_flow_diagram_postGCB.Rdata")

##############################################################################


### Fit theoretical pSEM for equation 1
equa1_ecua_piecewiseSEM <- grViz("
 digraph {
 
   graph [ranksep = 1]
 node[shape = septagon, style = filled, fillcolor = lightblue, color = black, height = 1.0, width = 1.5]
   'Species richness' [shape = 'septagon', fontname = 'helvetica-bold', penwidth=3]

 node[shape = 'rectangle', style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.5, width = 1.3]
   'Precipitation' [shape = rectangle, fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.7, width = 1.3]
   'Minimum
temperature' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.4, width = 0.7]
   'NDVI' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.4, width = 1.0]
   'Elevation' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = 'rectangle', style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.3]
   'Road density' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 0.9]
   'City age' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 0.7]
   'GDP' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 1.2]
   'Urban shape' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 1.2]
   'Urban area' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.8]
   'Distance to rivers' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 2.1]
   'Distance to mountains' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.8]
   'Distance to coasts' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]


   'Precipitation'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Minimum
temperature'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'NDVI'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Elevation'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Road density'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'City age'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'GDP'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Urban area'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Urban shape'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Distance to rivers'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Distance to coasts'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Distance to mountains'->'Species richness' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]


  # Additional constraints on the graph
  {rank = 'max'; 'GDP'; 'Precipitation'; 'Distance to rivers'; 'Distance to mountains'; 'Elevation';}
}
")


### Fit theoretical pSEM for equation 2
equa2_ecua_piecewiseSEM <- grViz("
 digraph {
 
   graph [ranksep = 1]
 node[shape = septagon, style = filled, fillcolor = lightblue, color = black, height = 1.0, width = 1.5]
   'Species richness' [shape = 'septagon', fontname = 'helvetica-bold', penwidth=3]
    node[shape = oval, style = filled, fillcolor = plum, color = black, width = 1.0]
   'sesMPD' [shape = 'oval', fontname = 'helvetica-bold', penwidth=3]


 node[shape = 'rectangle', style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.5, width = 1.3]
   'Precipitation' [shape = rectangle, fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.7, width = 1.3]
   'Minimum
temperature' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.4, width = 0.7]
   'NDVI' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.4, width = 1.0]
   'Elevation' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = 'rectangle', style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.3]
   'Road density' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 0.9]
   'City age' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 0.7]
   'GDP' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 1.2]
   'Urban shape' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 1.2]
   'Urban area' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.8]
   'Distance to rivers' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 2.1]
   'Distance to mountains' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.8]
   'Distance to coasts' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]


   'Species richness'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Precipitation'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Minimum
temperature'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'NDVI'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Elevation'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Road density'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'City age'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'GDP'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Urban area'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Urban shape'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Distance to rivers'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Distance to coasts'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Distance to mountains'->'sesMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]


  # Additional constraints on the graph
  {rank = 'max'; 'Species richness'; 'GDP'; 'Precipitation'; 'Distance to rivers'; 'Distance to mountains'; 'Elevation';}
}
")

### Fit theoretical pSEM for equation 3
equa3_ecua_piecewiseSEM <- grViz("
 digraph {
 
   graph [ranksep = 1]
    node[shape = oval, style = filled, fillcolor = plum, color = black, width = 1.0]
   'sesMPD' [shape = 'oval', fontname = 'helvetica-bold', penwidth=3]
    node[shape = oval, style = filled, fillcolor = greenyellow, color = black, width = 1.1]
   'sesFMPD' [shape = 'oval', fontname = 'helvetica-bold', penwidth=3]


 node[shape = 'rectangle', style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.5, width = 1.3]
   'Precipitation' [shape = rectangle, fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.7, width = 1.3]
   'Minimum
temperature' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.4, width = 0.7]
   'NDVI' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.4, width = 1.0]
   'Elevation' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = 'rectangle', style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.3]
   'Road density' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 0.9]
   'City age' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 0.7]
   'GDP' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 1.2]
   'Urban shape' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 1.2]
   'Urban area' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.8]
   'Distance to rivers' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 2.1]
   'Distance to mountains' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.8]
   'Distance to coasts' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]


   'sesMPD'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Precipitation'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Minimum
temperature'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'NDVI'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Elevation'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Road density'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'City age'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'GDP'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Urban area'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Urban shape'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Distance to rivers'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Distance to coasts'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Distance to mountains'->'sesFMPD' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]


  # Additional constraints on the graph
  {rank = 'max'; 'GDP'; 'Precipitation';  'Distance to coasts'; 'Distance to rivers'; 'Distance to mountains'; 'Elevation';}
}
")


### Fit theoretical pSEM for equation 4
equa4_ecua_piecewiseSEM <- grViz("
 digraph {
 
   graph [ranksep = 1]


 node[shape = 'rectangle', style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.5, width = 1.3]
   'Precipitation' [shape = rectangle, fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.7, width = 1.3]
   'Minimum
temperature' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = 'rectangle', style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.3]
   'Road density' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 0.9]
   'City age' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 0.7]
   'GDP' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 1.2]
   'Urban area' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.8]
   'Distance to rivers' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.8]
   'Distance to coasts' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 2.1]
   'Distance to mountains' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]


   'Precipitation'->'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Road density'->'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'City age'->'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'GDP'->'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Urban area'->'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'Distance to rivers'->'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Distance to coasts'->'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Distance to mountains'->'Minimum
temperature' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]


  # Additional constraints on the graph
  {rank = 'max'; 'Precipitation';  'Distance to coasts'; 'Distance to rivers'; 'Distance to mountains';}
}
")


### Fit theoretical pSEM for equation 5
equa5_ecua_piecewiseSEM <- grViz("
 digraph {
 
   graph [ranksep = 1]


 node[shape = 'rectangle', style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.5, width = 1.3]
   'Precipitation' [shape = rectangle, fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.7, width = 1.3]
   'Minimum
temperature' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.4, width = 0.7]
   'NDVI' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
 node[shape = rectangle, style = rounded, fillcolor = snow, color = black, fixedsize = true, height = 0.4, width = 1.0]
   'Elevation' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.4, width = 0.9]
   'City age' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]
  node[shape = rectangle, style = filled, fillcolor = bisque3, color = black, fixedsize = true, height = 0.5, width = 1.8]
   'Distance to coasts' [shape = 'rectangle', fontname = 'Helvetica', penwidth=2]




   'Precipitation'->'NDVI' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Minimum
temperature'->'NDVI' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Elevation'->'NDVI' [style = 'solid', fontname = 'Helvetica', color = red, penwidth = 1.5]
   'City age'->'NDVI' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]
   'Distance to coasts'->'NDVI' [style = 'solid', fontname = 'Helvetica', color = black, penwidth = 1.5]


  # Additional constraints on the graph
  {rank = 'max'; 'Precipitation'; 'Elevation';}
}
")



### Export theoretical pSEM for each equation
save_png(equa1_ecua_piecewiseSEM, path = "..\\data\\output_data\\Figures\\piecewiseSEM\\equa1_ecua_piecewiseSEM_postGCB.png")
save_png(equa2_ecua_piecewiseSEM, path = "..\\data\\output_data\\Figures\\piecewiseSEM\\equa2_ecua_piecewiseSEM_postGCB.png")
save_png(equa3_ecua_piecewiseSEM, path = "..\\data\\output_data\\Figures\\piecewiseSEM\\equa3_ecua_piecewiseSEM_postGCB.png")
save_png(equa4_ecua_piecewiseSEM, path = "..\\data\\output_data\\Figures\\piecewiseSEM\\equa4_ecua_piecewiseSEM_postGCB.png")
save_png(equa5_ecua_piecewiseSEM, path = "..\\data\\output_data\\Figures\\piecewiseSEM\\equa5_ecua_piecewiseSEM_postGCB.png")


### Save Workspace
save.image("../data/output_data/Workspaces/piecewiseSEM_flow_diagram_postGCB.Rdata")


#### Read theoretical pSEM images
psem1 <- magick::image_read("..\\data\\output_data\\Figures\\piecewiseSEM\\equa1_ecua_piecewiseSEM_postGCB.png")
psem2 <- magick::image_read("..\\data\\output_data\\Figures\\piecewiseSEM\\equa2_ecua_piecewiseSEM_postGCB.png")
psem3 <- magick::image_read("..\\data\\output_data\\Figures\\piecewiseSEM\\equa3_ecua_piecewiseSEM_postGCB.png")
psem4 <- magick::image_read("..\\data\\output_data\\Figures\\piecewiseSEM\\equa4_ecua_piecewiseSEM_postGCB.png")
psem5 <- magick::image_read("..\\data\\output_data\\Figures\\piecewiseSEM\\equa5_ecua_piecewiseSEM_postGCB.png")
psem_final <- magick::image_read("..\\data\\output_data\\Figures\\piecewiseSEM\\sem_theoretical_5_ecua_piecewiseSEM_postGCB.png")


### Export merged theoretical pSEM
#560
png(filename="..\\data\\output_data\\Figures\\piecewiseSEM\\psem_final_postGCB.png", units="mm", width=560, height=280, pointsize=0.3, res=500)
ggdraw() +
  draw_image(psem_final, x = 0.85, y = 1, hjust = 1, vjust = 1, width = 0.65, height = 0.65) +
  draw_image(psem1, x = 0.3, y = 0.95, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(psem2, x = 0.3, y = 0.4, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(psem3, x = 0.66, y = 0.3, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(psem4, x = 1, y = 0.4, hjust = 1, vjust = 1, width = 0.3, height = 0.3) +
  draw_image(psem5, x = 0.95, y = 0.95, hjust = 1, vjust = 1, width = 0.25, height = 0.25) +
  draw_label("Equation 1", x = 0.14, y = 0.955, fontface = "bold", size = 20) +
  draw_label("Equation 2", x = 0.14, y = 0.42, fontface = "bold", size = 20) +
  draw_label("Equation 3", x = 0.5, y = 0.285, fontface = "bold", size = 20) +
  draw_label("Equation 4", x = 0.83, y = 0.42, fontface = "bold", size = 20) +
  draw_label("Equation 5", x = 0.82, y = 0.97, fontface = "bold", size = 20)
dev.off()

### Save Workspace
save.image("../data/output_data/Workspaces/piecewiseSEM_flow_diagram_postGCB.Rdata")
