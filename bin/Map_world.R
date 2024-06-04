#######################################################
# Script to elaborate map of the world with 81 cities #
#######################################################

### Script elaborated by Israel Moreno-Contreras (Ornithology, UNAM)

### Call libraries
library("ggplot2")
library("rgdal")
library("sf")
library("Rmisc") # Merge plots
library("ggspatial") ### For north arrow and scale bar
library("rgdal")
library("cowplot") ### Inset maps
library("ggmap")
library("grid") ###For export Map_world_3

### Set working directory. YOUR PATH DIRECTORY!!!
setwd("H:/Passport/Macroecology_urban_birds/bin")

### Load Workspace
load("../data/output_data/Workspaces/Map_world.Rdata")

#########################################################
### Version 2

### Read distribution maps
World_Admin <- readOGR(dsn = "..\\data\\raw_data\\Shapefiles\\World_Admin", layer = "ne_10m_admin_0_countries")

### Convert polygons to sf
World_Admin_sf <- st_as_sf(World_Admin)

### Read shapefile of geographic coordinates of cities
points <- readOGR(dsn = "..\\data\\raw_data\\Shapefiles\\Cities_coordinates", layer = "Cities_coordinates_Project")

### Read shapefile of geographic coordinates of Poland and Finland cities
points_pol_fil <- readOGR(dsn = "..\\data\\raw_data\\Shapefiles\\Cities_coordinates", layer = "Poland_Finland")

### Read shapefile of geographic coordinates of European cities
points_euro_fil <- readOGR(dsn = "..\\data\\raw_data\\Shapefiles\\Cities_coordinates", layer = "Europe_cities")

### Convert points to sf
points_pol_fil_sf <- st_as_sf(points_pol_fil)

points_euro_fil_sf <- st_as_sf(points_euro_fil)

### Generate box enclosing Poland and Finland cities
points_pol_fil_bb = st_as_sfc(st_bbox(points_pol_fil_sf))

points_pol_fil_bb <- st_transform(points_pol_fil_bb, 4326)

### Generate box enclosing European cities
points_euro_fil_bb = st_as_sfc(st_bbox(points_euro_fil_sf))

points_euro_fil_bb <- st_transform(points_euro_fil_bb, 4326)

### Next the shapefile has to be converted to a dataframe for use in ggplot2
World_Admin_df <- fortify(World_Admin)

### Get data frame of points
points_df <- as.data.frame(points)
points_pol_fil_df <- as.data.frame(points_pol_fil)
points_euro_fil_df <- as.data.frame(points_euro_fil)


#################################################

#### MAP 5 ###


### Plot map of the world Using European cities in a box extent
world_map <- ggplot(data = World_Admin_sf) +
  geom_polygon(data = World_Admin_df, 
               aes(x = long, y = lat, group = group),
               color = 'black', fill = 'gray95', linewidth = 0.3) +
  geom_point(data = points_df, aes(x = Longitude, y = Latitude), 
             fill = "chocolate1", color = "black", alpha = 0.7, size = 5, shape = 21) + 
  geom_sf(data = points_euro_fil_bb, fill = NA, color = "#6b9237", linewidth = 1.7) +
  theme_bw() +
  coord_sf(xlim = c(-180, 180), ylim = c(-80, 83), crs = 4326, expand = FALSE) +
  scale_y_continuous(breaks = c(-80, -40, 0, 40, 80)) +
  xlab(expression(bold("Longitude"))) + ylab(expression(bold("Latitude"))) + 
  annotation_north_arrow(location = "tr", 
                         style = north_arrow_fancy_orienteering(text_size = 17),
                         height = unit(1.4, "cm"),
                         width = unit(1.4, "cm"),
                         pad_x = unit(0.05, "cm"),
                         pad_y = unit(0.4, "cm")) +
  theme(axis.text.x = element_text(size = 20, face = "plain", colour = "black"), 
        axis.text.y = element_text(size = 20, face = "plain", colour = "black"),
        axis.title.x = element_text(size = 20, face = "bold"), 
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.ticks=element_line(size = 1.3, colour = "black"), axis.ticks.length = unit(.20, "cm"),
        panel.grid.major = element_line(color = gray(0.5), 
                                        linetype = "dashed", size = 0.3), 
        panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(colour = "black", fill=NA, size=2.1))

### Inset map of European cities
pol_eur_map <- ggplot(data = World_Admin_sf) +
  geom_sf(fill = "gray95", color = "black", linewidth = 0.3) + 
  geom_point(data = points_euro_fil_df, aes(x = Longitude, y = Latitude), 
             fill = "chocolate1", color = "black", alpha = 0.7, size = 5, shape = 21) + xlab("Longitude") + ylab("Latitude") +
  theme_bw() + coord_sf(xlim = c(-10, 30),ylim = c(39, 70)) + 
  scale_x_continuous(breaks = c(0, 20)) +
  scale_y_continuous(breaks = c(40, 50, 60, 70)) +
  xlab(expression(bold("Longitude"))) + ylab(expression(bold("Latitude"))) + 
  annotation_scale(location = "bl", width_hint = 0.2, height = unit(0.2, "cm"),
                   pad_x = unit(1.7, "cm"),
                   text_cex = 1.1) + 
  annotation_north_arrow(location = "tr", 
                         style = north_arrow_fancy_orienteering(text_size = 17),
                         height = unit(1.4, "cm"),
                         width = unit(1.4, "cm"),
                         pad_x = unit(0.05, "cm"),
                         pad_y = unit(0.4, "cm")) +
  theme(axis.text.x = element_text(size = 20, face = "plain", colour = "black"), 
        axis.text.y = element_text(size = 20, face = "plain", colour = "black"),
        axis.title.x = element_text(size = 20, face = "bold"), 
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.ticks=element_line(size = 1.3, colour = "black"), axis.ticks.length = unit(.20, "cm"),
        panel.grid.major = element_line(color = gray(0.5), 
                                        linetype = "dashed", size = 0.3), 
        panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(colour = "black", fill=NA, size=2.1))


### Ridge line of density plot as a function of Latitude

group = as.factor(rep("urban",length(1:81)))
df_exp = data.frame(points_df, GROUP=group)


gg_dist_g1 = ggplot(df_exp, aes(Latitude, fill = "GROUP")) + geom_density(alpha=0.6, fill = "chocolate1", size=1.0, colour = "black") 
gg_dist_g1 = gg_dist_g1 + ylab("G1 density")

# Avoid displaying duplicated legend
gg_dist_g1 = gg_dist_g1 + theme(legend.position="none")


gg_dist_g1 = gg_dist_g1 + xlim(-80, 90)

# Flip axis of gg_dist_g2
gg_dist_g1 = gg_dist_g1 + coord_flip()

# Remove some duplicate axes
gg_dist_g1 = gg_dist_g1 + theme(axis.title.x=element_blank(),
                                axis.title.y=element_blank(),
                                axis.text=element_blank(),
                                axis.line=element_blank(),
                                axis.ticks=element_blank(),
                                panel.background = element_rect(fill = NA),
                                plot.background = element_rect(fill='transparent', color=NA)) 

# Modify margin c(top, right, bottom, left) to reduce the distance between plots
#and align G1 density with the scatterplot
gg_dist_g1 = gg_dist_g1 + theme(plot.margin = unit(c(-2.9, 6.0, 3.15, -0.3), "cm"))


# Combine all plots together and crush graph density with rel_heights
second_col = plot_grid(NULL, gg_dist_g1, ncol = 1, rel_heights = c(1, 3))
perfect = plot_grid(world_map, second_col, ncol = 2, rel_widths = c(3, 1))

### Export map  of the world
png(filename="..\\data\\output_data\\Figures\\Map\\Map_cities5.png", units="mm", width=370, height=170, pointsize=15, res=2200)
perfect
dev.off()

### Export map  of the European cities
png(filename="..\\data\\output_data\\Figures\\Map\\Map_europe.png", units="mm", width=170, height=170, pointsize=15, res=2200)
print(pol_eur_map, vp = viewport(0.5, 0.5, width = 1, height = 1))
dev.off()

#####
### Then, both maps ("perfect" and "pol_eur_map") were plotted together in Photoshop

### Save Workspace
save.image("../data/output_data/Workspaces/Map_world.Rdata")

#################################################################################################
