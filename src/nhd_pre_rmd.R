library(tidyverse)
library(nhdplusTools)
library(sf)
library(feather)
library(mapview)

knitr::opts_knit$set(root.dir='../')

#reading in wsr watersheds
wsr_watersheds <- st_read('data/wsr_watersheds.shp')%>%
  st_transform(4326) 

#reading in wsr starting points, already snapped to the nhd
wsr_start_point <- st_read('data/nhd_snapped_upstream_points.shp') %>%
  st_transform(4326) %>%
  mutate(wqp_sites_5mi="")

mapview(list(wsr_start_point,wq_within))

#reading in wqp site data, as table and as xy points
wq_table <- read_feather('data/unique_site_inventory.feather') %>%
  filter(source == 'WQP')

wq_spatial <- wq_table %>%
  st_as_sf(.,coords=c('long','lat'), 
           remove = FALSE,
           crs=4326) 

#select wqp sites within wsr watersheds
wq_within <- wq_spatial[wsr_watersheds,] %>%
  st_zm()  #for future snapping function 
  left_join(., wq_table, by='SiteID')


#reading in nhd plus v2.1 flowlines

flowlines <- read_sf('D:/WSR_Project/data/clipped_nhdplusv2.shp') %>%
  st_zm() %>%
  st_transform(4326)

flowlines_points <- flowlines %>%
  st_coordinates() %>%
  as_tibble() %>%
  st_as_sf(.,coords=c('X','Y'), 
           remove = FALSE,
           crs=4326) %>%
    st_join(., flowlines, join=st_nearest_feature, left=TRUE)

#join wqp sites to nearest nhd plus v2.1 flowline feature, remove unnecessary columns (DISTANCE COULD BE FAR AWAY..) 
nearest_nhd_to_wq <- wq_within %>%
  st_join(., flowlines_points, join=st_nearest_feature, left=TRUE) %>%
  group_by(SiteID)

#... so must include distance between the snapped flowline and the wqp site point. Must figure out a way to calculate the distance 
#between these two points iterativelly and save that distance within "nearest_nhd_to_wq"...
relate_nhd <- wq_within %>%
  st_join(., flowlines_points, join=st_nearest_feature, left=TRUE) %>%
  st_set_geometry(NULL)

spatial_related_nhd <- relate_nhd %>%
  st_as_sf(.,coords=c('X','Y'), 
           remove = FALSE,
           crs=4326) %>%
  group_by(SiteID)

mapview(list(nearest_nhd_to_wq,spatial_related_nhd))

#found on https://stackoverflow.com/questions/54887209/compute-pointwise-distance-by-group-in-r-with-sf-dplyr but doesn't work?!
nearest_nhd_to_wq %>%
  mutate(dst=map2_dbl(SiteID, geometry,
                      ~ st_distance(.y, spatial_related_nhd %>% filter(SiteID == .x) %>% pull(geometry))
  ))

#join wsr start points to nhd plus v2.1 flowlines feature (SHOULD NOT BE FAR AWAY, SINCE ALREADY SNAPPED TO NHD FLOWLINES)
wsr_start_nhd_join <- st_join(wsr_start_point, flowlines, join = st_nearest_feature, left=TRUE) %>%
  select(., 'WSR', 'REACHCODE', 'COMID', 'LENGTHKM')

#identify flowline reachcodes that are up to 5 km upstream of wsr start point
upstream_test <- get_UT(flowlines, 6097391, distance = 5) %>%
  as_tibble() %>%
  rename(COMID = value) %>%
  inner_join(.,nearest_nhd_to_wq, by='COMID') %>%
  filter(LENGTHKM + added_distance_column <=  5) %>%  #filter out wqp sites that are beyond 5 km away including added distance from snapped flowine. 
  nest(., wsr_start_point)#figure out a way to nest the list of wqp sites within the wsr start point dataframe

#try to turn into a function that iterates "get_UT" for every wsr_start point
upstream <- function(x, y) {

  for(i in 1:length(x))
  upstream_list <- upstream_test <- get_UT(y, x$COMID[i], distance = 5) %>%
    as.tibble()%>%
    rename(COMID=value) %>%
    inner_join(.,wsr_start_point, by='COMID') %>%
    nest(-upstream_list[i], )
  return(upstream_list)
  
}

#... do this for 5, 10, and 25, then beyond 25 km upstream...

#DO FOR DOWNSTREAM....
get_DM(network, comid, distance = NULL, sort = FALSE, include = TRUE)

#DO WITHIN
upstream_test <- get_UT(flowlines, 6097391, distance = 5) %>%
  as_tibble() %>%
  rename(COMID = value) %>%
  inner_join(.,nearest_nhd_to_wq, by='COMID') %>%
  filter(LENGTHKM + added_distance_column <=  5) %>%  #filter out wqp sites that are beyond 5 km away including added distance from snapped flowine. 
  nest(., wsr_start_point)#figure out a way to nest the list of wqp sites within the wsr start point dataframe


joiner <- function(df1,df2,qname1,qname2){
  df <- inner_join(df1 %>%
                     select(Date,q_cfs),
                   df2 %>%
                     select(Date,q_cfs),by="Date") %>%
    rename(!!qname1 := 2, #Crazy rlang::tidyeval stuff I didn't want to get into
           !!qname2 := 3) %>% #Same
    as_tibble()
  return(df)
}



