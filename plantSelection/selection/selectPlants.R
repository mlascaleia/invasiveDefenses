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

# high-level functions

# function to get JSON data from the arb GIS site and convert to sf

query.arbmap <- function(url){
  
  # Get JSON response
  response <- httr::GET(url)
  
  # Parse JSON
  data_json <- httr::content(response, as = "text", encoding = "UTF-8")
  data_list <- jsonlite::fromJSON(data_json)
  
  # Extract attributes and geometry
  df <- data_list$features$attributes %>%
    filter(!is.na(LATITUDE))
  
  # Convert to sf object (assuming coordinates are in WGS84 - EPSG:4326)
  sf.out <- sf::st_as_sf(df, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
  
  return(sf.out)
}

# load data

plantAll.sf <- query.arbmap("https://map.arboretum.harvard.edu/arcgis/rest/services/CollectionExplorer/MapServer/34/query?where=IS_DEAD%3D0&outFields=ACC_NUM_AND_QUAL,FAMILY,GENUS,SPECIES,INFRASPECIFIC_RANK,INFRASPECIFIC_EPITHET,CULTIVAR,LATITUDE,LONGITUDE&returnGeometry=true&f=json")
plantWant <- read.csv("plantSelection/selection/cutDownList.csv")

# refine plant data

plantAll.sf <- plantAll.sf %>%
  
  mutate(
    fullName = paste0(
      
      #combine genus + species
      GENUS, " ", SPECIES,
      # Add infraspecific rank + epithet if present
      if_else(
        !is.na(INFRASPECIFIC_RANK) & INFRASPECIFIC_RANK != "" & 
          !is.na(INFRASPECIFIC_EPITHET) & INFRASPECIFIC_EPITHET != "",
        paste0(" ", INFRASPECIFIC_RANK, " ", INFRASPECIFIC_EPITHET),
        ""
      ),
      # Else add cultivar in quotes if present
      if_else(
        (is.na(INFRASPECIFIC_RANK) | INFRASPECIFIC_RANK == "" | 
           is.na(INFRASPECIFIC_EPITHET) | INFRASPECIFIC_EPITHET == "") &
          !is.na(CULTIVAR) & CULTIVAR != "",
        paste0(" '", CULTIVAR, "'"),
        ""
      )
    )
  ) %>%
  rename(Family = FAMILY, Genus = GENUS, Species = SPECIES) %>%
  mutate(colorgroup = ifelse(Family %in% plantWant$Family, Family, "grey"))

# what plants are of the species that I want?

targetable <- plantWant %>%
  left_join(plantAll.sf, by = c("Family", "Genus", "Species")) %>%
  filter(!is.na(ACC_NUM_AND_QUAL)) %>%
  mutate(Binomial = paste(Genus, Species))

# how many of them have individuals that are not variants, forms, etc.
easyOnes <- targetable %>%
  filter(is.na(INFRASPECIFIC_EPITHET) &
           is.na(CULTIVAR)) %>%
  distinct(Binomial)

# how many are ONLY variants, forms, etc.
hardOnes <- targetable %>%
  filter(!is.na(INFRASPECIFIC_EPITHET) |
           !is.na(CULTIVAR)) %>%
  filter(!Binomial %in% easyOnes$Binomial)

# case by case basis here we come!
# what I'm going to do is eliminate the ones I don't want to be in the pool
# then pull from the undesirables when the randomization occurs

eliminateAccsHard <- c("2163-65*A", # the one V. carlesii cultivar
                       
                       # V. nudum 'Bulk' has no leaf changes (berry only) but 'Winterthur' does
                       "565-2016*C", "565-2016*B", "565-2016*E", "565-2016*F", "141-2019*A", 
                       "565-2016*A", "565-2016*G", "228-2014*B", "228-2014*A", 
                       
                       # E. fortunei 'Coloratus' is somewhat considered a variant, so I'm keeping just that
                       "6841-1*A",
                       
                       # I'm going to ignore the A. altissima in the AA and just use wild or greenhouse ones
                       "695-80*B", "203-35*B"
)

eliminateAccs <- targetable %>%
  filter(!is.na(INFRASPECIFIC_EPITHET) |
           !is.na(CULTIVAR)) %>%
  filter(Binomial %in% easyOnes$Binomial) %>%
  pull(ACC_NUM_AND_QUAL) %>%
  c(eliminateAccsHard)

# the moment we've all been waiting for!
# selection of the actual plants for the project

set.seed(42325)

samplePlants <- targetable %>%
  filter(!ACC_NUM_AND_QUAL %in% eliminateAccs) %>%
  arrange(Binomial, ACC_NUM_AND_QUAL) %>% 
  group_by(Binomial) %>%
  slice_sample(n = 1) %>%
  select(id = ACC_NUM_AND_QUAL, Family, Genus, 
         Species, Binomial, colorgroup,
         geometry) %>%
  arrange(Family, Genus, Species) %>%
  st_as_sf()

# remove the plants I'm sampling from sf of all plants
plantAll.sf <- plantAll.sf[!plantAll.sf$ACC_NUM_AND_QUAL %in% samplePlants$id, ]

# make the grid ####
allbox <- st_bbox(samplePlants)

# add buffer
allbox <- allbox + c(-0.0004, -0.0004, 0.0004, 0.0004)

# I've ultimately decided 5 boxes wide and 8 tall is the best vibes
xgrid <- seq(allbox[1], allbox[3], length.out = 5)
ygrid <- seq(allbox[2], allbox[4], length.out = 8)

# get box widths
xit <- xgrid[2] - xgrid[1]
yit <- ygrid[2] - ygrid[1]

# make the grid polygon
polygons <- list()
id <- 1

for (i in seq_len(length(xgrid) - 1)) {
  for (j in seq_len(length(ygrid) - 1)) {
    coords <- matrix(c(
      xgrid[i],     ygrid[j],
      xgrid[i + 1], ygrid[j],
      xgrid[i + 1], ygrid[j + 1],
      xgrid[i],     ygrid[j + 1],
      xgrid[i],     ygrid[j]        # close the polygon
    ), ncol = 2, byrow = TRUE)
    
    poly <- st_polygon(list(coords))
    polygons[[id]] <- poly
    id <- id + 1
  }
}

# Create an sf object with the polygons
grid_sf <- st_sf(
  geometry = st_sfc(polygons),
  crs = 4326  # You can change this to match your map's CRS
)

# now put the grid number in with the plants on the sheet

hasPlant <- st_intersects(grid_sf, samplePlants) %>%
  sapply(length) > 0

numPlant <- st_intersects(grid_sf, samplePlants) %>%
  sapply(length)

grid_sf$numPlant <- numPlant

grid_sf <- grid_sf[hasPlant, ]

grid_sf$grid_id <- 1:nrow(grid_sf)

# make spreadsheet

whichPlants <- st_intersects(grid_sf, samplePlants)

eachGrid <- lapply(whichPlants, FUN = function(w){
  return(samplePlants[w,])
})

plantSheet <- eachGrid %>%
  bind_rows(.id = "gridNum") %>%
  as.data.frame() %>%
  select(gridNum, id, Binomial, Family)

write.csv(plantSheet, "plantSelection/selection/plantSheet.csv", row.names = F)

plantSheet[rep(1:nrow(plantSheet), each = 5), ] %>%
  mutate(leafNum = rep(1:5, nrow(plantSheet))) %>%
  write.csv("plantSelection/selection/plantSheetLeaves.csv", row.names = F)

save(grid_sf, plantAll.sf, samplePlants,
     xit, yit, 
     file = "plantSelection/selection/selectionObjects.rdata")



