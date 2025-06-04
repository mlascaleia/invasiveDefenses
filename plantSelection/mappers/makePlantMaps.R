# clear environment

rm(list = ls())

# load packages

library(httr)
library(jsonlite)
library(sf)
library(dplyr)
library(stringr)
library(esri2sf)
library(ggplot2)
library(ggrepel)
library(patchwork)
library(ggspatial)

# load plant data

load("plantSelection/selection/selectionObjects.rdata")


# load shapefiles 

paths <- st_read("plantSelection/mappers/shpFiles/Paths.shp")
structures <- st_read("plantSelection/mappers/shpFiles/Structures.shp")
water <- st_read("plantSelection/mappers/shpFiles/Hydrology.shp")
roads <- st_read("plantSelection/mappers/shpFiles/RoadsMajorPaths.shp")

paths <- st_transform(paths, st_crs(plantAll.sf))
structures <- st_transform(structures, st_crs(plantAll.sf))
water <- st_transform(water, st_crs(plantAll.sf))
roads <- st_transform(roads, st_crs(plantAll.sf))


makeMap <- function(xrange, yrange, tittl){
  
  group_colors <- c(
    "grey" = "black",
    "Adoxaceae" = "#ff7f0e",
    "Berberidaceae" = "#2ca02c",
    "Caprifoliaceae" = "#d62728",
    "Celastraceae" = "#9467bd",
    "Elaeagnaceae" = "#8c564b",
    "Moraceae" = "#e377c2",
    "Rhamnaceae" = "gold",
    "Simaroubaceae" = "blue"
  )
  
  # Extract coordinates and combine with original data
  sample_coords <- samplePlants %>%
    cbind(st_coordinates(.)) %>%
    filter(X >= xrange[1] + buffer * xit, X <= xrange[2] - buffer * xit,
           Y >= yrange[1] + buffer * yit, Y <= yrange[2] - buffer * yit)
  
  allBox <- plantAll.sf %>%
    cbind(st_coordinates(.)) %>%
    filter(X >= xrange[1], X <= xrange[2],
           Y >= yrange[1], Y <= yrange[2])
  
  bbox_matrix <- matrix(
    c(
      xrange[1], yrange[1],  # lower-left
      xrange[2], yrange[1],  # lower-right
      xrange[2], yrange[2],  # upper-right
      xrange[1], yrange[2],  # upper-left
      xrange[1], yrange[1]   # close the polygon
    ),
    ncol = 2,
    byrow = TRUE
  )
  
  # Turn matrix into an sf polygon
  bbox_poly <- st_polygon(list(bbox_matrix)) %>%
    st_sfc(crs = st_crs(samplePlants)) %>%  # use same CRS as your data
    st_sf()  # convert to sf object
  
  # Now plot, using only the filtered data for labels
  main <- ggplot() +
    geom_sf(data = roads, linewidth = 1, fill = "black", color = "grey60", alpha = 0.25) +
    geom_sf(data = paths, linewidth = 1.45, color = "black", alpha = 0.3) +
    geom_sf(data = paths, linewidth = 1.2, color = "black", alpha = 0.1) +
    geom_sf(data = water, linewidth = 1.3, color = "blue", alpha = 0.3) +
    geom_sf(data = structures) +
    geom_sf(data = samplePlants, aes(color = colorgroup)) +
    geom_sf(data = plantAll.sf, alpha = 0.2, aes(color = colorgroup)) +
    geom_text_repel(
      data = sample_coords,
      aes(x = X, y = Y, label = paste(id, Binomial, sep = "\n")),
      size = 2,
      point.padding = .2,
      segment.color = "black",
      max.overlaps = Inf,
      min.segment.length = 0,
      fontface = "bold"
    ) +
    scale_color_manual(values = group_colors) +
    geom_text_repel(
      data = allBox,
      aes(x = X, y = Y, label = ACC_NUM_AND_QUAL),
      size = 0.9,
      point.padding = 0.1,
      segment.alpha = 0.1,
      max.overlaps = 25
    ) +
    coord_sf(xlim = xrange, ylim = yrange) +
    theme_minimal() +
    ggtitle(tittl) +
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position = "none",
          title = element_text(family = "serif", size = 24, face = "bold"),
          panel.grid = element_blank()) +
    annotation_scale(location = "br",
                     width_hint = 0.25,
                     unit_category = "imperial")
  
  return(main)
}

buffer <- 0.0025

group_colors <- c(
  "grey" = "black",
  "Adoxaceae" = "#ff7f0e",
  "Berberidaceae" = "#2ca02c",
  "Caprifoliaceae" = "#d62728",
  "Celastraceae" = "#9467bd",
  "Elaeagnaceae" = "#8c564b",
  "Moraceae" = "#e377c2",
  "Rhamnaceae" = "gold",
  "Simaroubaceae" = "blue"
)

for(i in 1:nrow(grid_sf)){
  bbi <- st_bbox(grid_sf[i,])
  makeMap(as.numeric(unlist(bbi[c(1,3)])) + c(-buffer * xit, buffer * xit),
          as.numeric(unlist(bbi[c(2,4)])) + c(-buffer * yit, buffer * yit),
          i)

  ggsave(paste0(i, "Map.png"), dpi = 1200,
         width = 11, height = 8.5, unit = "in")
}

ggplot() +
  geom_sf(data = paths, alpha = 0.75) +
  geom_sf(data = roads, alpha = 0.75, fill = "grey75") +
  geom_sf(data = water, color = "lightblue") +
  geom_sf(data = structures, alpha = 0.75) +
  geom_sf(data = samplePlants, aes(color = colorgroup), size = 3) +
  # scale_color_discrete(name = "Family") +
  geom_sf(data = grid_sf, fill = NA) +
  geom_sf_text(data = grid_sf, aes(label = grid_id), fontface = "bold") +
  geom_sf_text(data = grid_sf, aes(label = paste(numPlant, "plants")), nudge_y = -0.0006, size = 2) +
  theme_minimal() +
  scale_color_manual(values = group_colors, name = "Family") +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        # legend.position = "none",
        panel.grid = element_blank(),
        text = element_text(size = 18))

# ggsave("cover.png", dpi = 1200,
#        width = 8.5, height = 11, unit = "in")






