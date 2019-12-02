library(tidyverse)
library(sf)
library(feather)
library(maptools)
library(mapview)
library(riverdist)
library(rgdal)

list.files('data/', pattern='\\.shp$')

watersheds <- st_read('data/watersheds.shp')%>%
  st_transform(4269)%>%
  st_zm()

raw_flowlines <- read_sf("data/nhdplusv2.shp") %>%
  st_transform(4269)%>%
  st_zm()

rivs2 <- st_intersection(raw_flowlines,watersheds)

mapview(rivs2)

st_write(rivs2, 'data/croppednhd.shp')

rivs <- riverdist::line2network(path='C:/Users/kwilli/Documents/WR674/ClassDirectories/wsr_r_spatial_project/data',
                                layer="croppednhd", reproject="+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")

rivs_fixed <- cleanup(rivers = rivs)

save(rivs_fixed, file = "data/rivs2_fixed.rda")

nload("data/rivs2_fixed.rda")

# now plot the old vs. the new
par(mfrow=c(1,2))
topologydots(rivers=rivs)
graphics::title("Raw River Topology", family="Roboto Condensed")
topologydots(rivs_fixed)
graphics::title("Clean River Topology", family="Roboto Condensed")

df_locs <- read_feather('data/unique_site_inventory.feather') %>%
  st_as_sf(.,coords=c('long','lat'), 
           remove = FALSE,
           crs=4326) 

df_locs_watersheds <- st_intersection(df_locs, watersheds)

plot(st_geometry(watersheds), lwd=2, axes=F)
plot(st_geometry(rivs2), col="blue", add=T)
plot(st_geometry(df_locs_watersheds), add=T, pch=21, bg = "orange", cex = 1.5)
graphics::title("Selected WQ Sites in NE Watersheds", family="Roboto Condensed")

df_locs_watersheds <- df_locs_watersheds %>% 
  st_transform(crs = 102003) # convert to Albers Equal Area

# add COORDS in projected form (UTM)
df_locs_watersheds$X <- st_coordinates(df_locs_watersheds)[,1]
df_locs_watersheds$Y <- st_coordinates(df_locs_watersheds)[,2]

# run this to snap points to line (and see distrib)
cdec_riv <- xy2segvert(x=df_locs_watersheds$X, y=df_locs_watersheds$Y, rivers=rivs_fixed)
head(cdec_riv)

hist(cdec_riv$snapdist, breaks=50,main="Point Snapping distance (CDEC to Flowline)", col="skyblue3", xlab="Snapping Distance (m)", family="Roboto Condensed")

# add vertices
df_locs_watersheds <- bind_cols(df_locs_watersheds, cdec_riv)

# MAP IT
plot(x=rivs_fixed, color = FALSE)
points(df_locs_watersheds$X, df_locs_watersheds$Y, pch=16, col="red") # raw coords
riverpoints(seg=cdec_riv$seg, vert=cdec_riv$vert, rivers=rivs_fixed, pch=22, cex=1.5,
            bg="forestgreen") # snapped coords
text(df_locs_watersheds$X, df_locs_watersheds$Y, labels=df_locs_watersheds$vert, pos = 4, family="Roboto Condensed")

# DETECTING ROUTES
detectroute(start=189, end=6, rivers=rivs_fixed)

# then use that information to pick the starting/end segments, and the vertices of interest.
riverdistance(startseg=189, startvert=185, endseg=6, endvert=1,
              rivers=rivs_fixed, map=TRUE)

# CREATE MATRIX OF DISTS
dmat <- riverdistancemat(df_locs_watersheds$seg, df_locs_watersheds$vert, rivs_fixed, ID = df_locs_watersheds$Locality ) %>% as.matrix
dmat <- dmat/1000 # convert to km
head(dmat)
