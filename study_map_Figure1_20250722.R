# Clean the environment
rm(list = ls())

#summarize each phyto sample, getting richness, diversity, evenness
#load libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(ggspatial)
library(sf)

#setwd
setwd("C:/Users/ibishop/OneDrive - DOI/science/gc_metabolism/manuscript/gc_tailwaters map/")

#load powell shape
powell <- st_read("../../../gc_algal_composition/rawdata/lakepowell_shapefile/Shape/NHDWaterbody.shp")

#truncate to Lake Powell polygons only
truncated_powell <- powell %>% 
  filter(gnis_name %in% c("Lake Powell"))

#reduce resolution
simplified_powell <- st_simplify(truncated_powell, dTolerance = 10) %>% st_transform(crs = 4326)

#load mainstem, paria and state boundaries
mainstem <- st_read("../gc_tailwaters map/CO3.shp") %>% filter(NAME=="Colorado River") %>% st_transform(crs = 4326)
paria <- st_read("../gc_tailwaters map/Major River, Main Stem, Primary & Secondary Tribs/CO4.shp") %>% filter(NAME=="Paria River") %>% st_transform(crs = 4326)
state_boundaries <- st_read("Colorado_River_Basin_US_States/Colorado_River_Basin_US_States.shp") %>% st_transform(crs = 4326)

#set bbox for truncating mainstem shape upstream of dam
bbox <- st_as_sfc(st_bbox(c(xmin = -111.62, xmax = -111.488, ymin = 36.82, ymax = 36.94), crs = st_crs(mainstem)))

# Filter mainstem to include only features within the bounding box
mainstem_filtered <- mainstem[st_intersects(mainstem, bbox, sparse = FALSE)[,1], ]

#zoom region of interest
ROI <- c(-111.62,-111.43,36.82,36.97)

#base map
base_map <- ggplot() +
  geom_sf(data = mainstem_filtered, col="black", linewidth=1.5) +
  geom_sf(data = simplified_powell, fill="black", linewidth=0) +
  geom_sf(data = paria, col="black", linewidth=1) +
  coord_sf(xlim = c(ROI[1], ROI[2]), ylim = c(ROI[3], ROI[4])) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Removes major gridlines
    panel.grid.minor = element_blank(),  # Removes minor gridlines
    panel.border = element_rect(colour = "black", fill=NA, size=1),  # Adds a border
    axis.text.x = element_blank(),  # Removes x-axis labels
    axis.text.y = element_blank(),  # Removes y-axis labels
    axis.ticks = element_blank()    # Removes axis ticks
  )
map <- base_map + annotation_scale(location = "tl", width_hint = 0.3)
map
ggsave("./map2.png", map, height=10, width=10, units="cm")
# 
state_boundaries <- st_read("Colorado_River_Basin_US_States/Colorado_River_Basin_US_States.shp") %>%
  st_transform(crs = 4326) %>%
  filter(NAME=="Arizona")

ROI2 <- c(-117,-108,31,38)

tribs <- st_read("../gc_tailwaters map/Major River, Main Stem, Primary & Secondary Tribs/CO4.shp") %>% filter(NAME %in% c("Paria River", "Kanab Creek", "Havasu Creek", "Shinumo Creek", "Diamond Creek","Little Colorado River")) %>% st_transform(crs = 4326)

az_inset <- ggplot() +
  geom_sf(data = state_boundaries, col="black", fill=NA, linewidth=2) +
  geom_sf(data = mainstem, col="black", linewidth=1) +
  geom_sf(data = simplified_powell, fill="black", linewidth=0) +
  geom_sf(data = tribs, col="black", linewidth=0.5) +
  coord_sf(xlim = c(ROI2[1], ROI2[2]), ylim = c(ROI2[3], ROI2[4])) +
  theme_void()
az_inset
