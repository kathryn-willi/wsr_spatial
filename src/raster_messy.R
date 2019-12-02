library(stars)
library(tidyverse)
library(sf)  #Spatial datasets
library(mapview) #interactive mapping
library(raster) #Raster analysis
library(elevatr) #National Elevation Dataset downloader. 
library(feather)
library(rgdal)
library(whitebox)

# WHITEBOX:: https://jblindsay.github.io/wbt_book/preface.html

raw_dem <- raster('data/raw_dem.tif')

dem_fill <- raster('data/dem_fill.tif')

flow_acc <-raster('data/flowacc.tif')

flow_direction <- raster('data/flow_direction.tif')

wsr_end_point <- st_read("data/dem_snapped_end_points.shp") %>%
  st_transform(4326)

watersheds <- st_read('data/watersheds.shp')%>%
  st_transform(4326)

wq_table <- read_feather('data/unique_site_inventory.feather') %>%
  filter(source == 'WQP')

wq_spatial <- wq_table %>%
  st_as_sf(.,coords=c('long','lat'), 
           remove = FALSE,
           crs=4326) 
wq_within <- wq_spatial[watersheds,] 


# downslope_flowpath_length() with upstream wsr point used as the watershed outlet, 
# then extract values from the downslope_flowpath_length where the wqp_sites overlap to get distance to wsr
# would require iterating over all wsr watershed upstream points.?!
