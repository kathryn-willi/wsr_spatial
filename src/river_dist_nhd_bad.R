library(tidyverse)
library(sf)
library(nhdplusTools)
library(feather)
library(lwgeom) #for
library(maptools)
library(rgdal)
library(mapview)
library(riverdist)

knitr::opts_knit$set(root.dir='../')

#read in WSR watersheds:
wsr_watersheds <- st_read('data/wsr_watersheds.shp')%>%
  st_transform(4326)%>%
  st_zm() #for future snapping function

#read in WSR end points:
wsr_end_point <- st_read("data/nhd_snapped_end_points.shp") %>%
  st_transform(4326) %>%
  st_zm() #for future snapping function

read_feather('data/unique_site_inventory.feather') %>%
  write_csv('data/wq.csv')

#reading in wq sites, filter sites within WSR watershed:
wq_all <- read_feather('data/unique_site_inventory.feather') %>%
  st_as_sf(.,coords=c('long','lat'), 
           remove = FALSE,
           crs=4326) 

wq_within <- wq_all[wsr_watersheds,] %>%
  st_zm() #for future snapping function

wq_within$X <- st_coordinates(wq_within)[,1]
wq_within$Y <- st_coordinates(wq_within)[,2]


wq_wsr <- st_join(wq_within, wsr_watersheds) #how to join one to many???



##########################OR############################################################################################
##########################OR############################################################################################
##########################OR############################################################################################

raw_nhdplusv2 <- read_sf("data/nhdplusv2.shp") %>%
  st_transform(4326) %>%
  st_zm() #for future snapping function 

clipped_nhdplusv2 <- st_intersection(raw_nhdplusv2, wsr_watersheds)

st_write(clipped_nhdplusv2, 'data/clipped_nhdplusv2.shp')

clipped_nhdplusv2 <- read_sf('data/clipped_nhdplusv2.shp')




##########################OR############################################################################################
##########################OR############################################################################################
##########################OR############################################################################################
nhd_riverdist <- riverdist::line2network(path='D:/WSR_Project/data',
                                layer="clipped_nhdplusv2", 
                                reproject="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

snapped_wq_within <- xy2segvert(x=wq_wsr$X, y=wq_wsr$Y, rivers=rivs)
head(snapped_wq_within)

hist(snapped_wq_within$snapdist, breaks=50,main="EQ Site Point Snapping distance", col="skyblue3", xlab="Snapping Distance (m)", family="Roboto Condensed")


##########################OR############################################################################################
##########################OR############################################################################################
##########################OR############################################################################################
nhdplus_path("D:/WSR_Project/data/NHDPlusV21_National_Seamless.gdb")
flowlines <- read_sf(nhdplus_path(), "NHDFlowline_Network")

##########################OR############################################################################################
##########################OR############################################################################################
##########################OR############################################################################################

#SNAPPING WQ SITES TO NHD:
#from https://stackoverflow.com/questions/51292952/snap-a-point-to-the-closest-point-on-a-line-segment-using-sf:
st_snap_points <- function(x, y, max_dist = 100) {
  
  if (inherits(x, "sf")) n <- nrow(x)
  if (inherits(x, "sfc")) n <- length(x)
  
  out = do.call(c,
                lapply(seq(n), function(i) {
                  nearest <- st_nearest_points(st_geometry(x)[i], y)
                  nearest_length <- sf::st_length(nearest)
                  nearest_min <- which.min(nearest_length)
                  if (as.vector(nearest_length[nearest_min]) > max_dist) return(st_geometry(x)[i])
                  return(st_cast(nearest[nearest_min], "POINT")[2])
                })
  )
  return(out)
}

wqp_snapped_nhd <-  st_snap_points(wq_within, clipped_nhdplusv2, 500) %>%
  

mapview(list(raw_flowlines,wqp_snapped_nhd))


wq_snapped_tibble <- wqp_snapped_nhd %>%
  as.tibble 
snap_points <- mutate(wq_snapped_tibble, coordinates=gsub("[\\c|\\(|\\)]","",wq_snapped_tibble$geometry))


#####################################################
#####################################################

split_wsr <- st_split(clipped_nhdplusv2, wsr_end_point)
split_nhd <- st_split(splir_wsr, wqp_snapped_nhd)

nhd_aligned <- split_wsr %>%
  align_nhdplus_names() %>%
  prepare_nhdplus(0, 0, min_path_size = 0, 
                  purge_non_dendritic = FALSE, #should that stay TRUE?
                  warn = TRUE, error = FALSE)

#Linking NHD feature name to WSR endpoint
wsr_end_nhd_join <- st_join(wsr_end_point, split_wsr, join = st_nearest_feature, left=TRUE)

wqp_nhd_join <- wsr_nhd_join <- st_join(snapped_nhd_wqp, split_nhd, join = st_nearest_feature, left=TRUE) %>%
  filter(GNIS_NAME == "Eightmile River")

#upstream_test <- nhdplusTools::get_UT(nhd_aligned, 206912, distance=2) #this stopped working too... but even if it did
#how can I iterate over every WSR point to every wq_point within its watershed? (changing the )

#AFTERWARDS....
#create some sort of nested wsr point object

st_length(x, y, ..., dist_fun, by_element = FALSE,
          which = ifelse(isTRUE(st_is_longlat(x)), "Great Circle", "Euclidean"),
          par = 0, tolerance = 0)

##############################
###IS THIS A WAY TO CREATE CHARACTERS WITHIN R FUNCTIONS ? 
#where df1 is site number, qname is site name...

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

joined_df <- joiner(lincoln_q,elc_q,'lincoln_q','elc_q')


