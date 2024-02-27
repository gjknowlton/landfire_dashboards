
## Code Notes -----
# Greasy tries to make a BpS map using rlandfire package, and code from Myles and Garrett
# Effort started February 19, 2024
# Issue will most likely be making the map with only top 5 or 6 BpSs, plus aestetics

## Load dependencies, set up organization

# load packages
library(rlandfire)
library(foreign)
library(sf)
library(terra)
library(tidyverse)

# create directories
#dir.create('inputs')
# paste shapefile in data directory (example will be central UP assessment area)
#dir.create('outputs')

# load shapes
shp <- st_read("inputs/cup_final.shp") %>% 
  st_transform(crs = 5070) %>%
  st_union() %>%
  st_sf()

vect(shp)
plot(shp)


mi_shp <- st_read("inputs/mi_cntys.shp") %>% 
  st_transform(crs = 5070) %>%
  st_union() %>%
  st_sf()

vect(mi_shp)
plot(mi_shp)



## Get LANDFIRE BpS data using rlandfire package -----

# set up
aoi <- getAOI(shp)
products <-  c("220BPS")
projection <- 5070
resolution <- 30


# R specific arguments
files_go_here <- "outputs"
save_file <- tempfile(fileext = ".zip", tmpdir = files_go_here)

# call API
ncal <- landfireAPI(products, 
                    aoi, 
                    projection, 
                    resolution, 
                    path = save_file)
# unzip
utils::unzip(zipfile = 'outputs/file222032a3185.zip', exdir = files_go_here)


## GIS work -----

# code mostly from Myles Walimaa

# read in BpS raster
bps_full <- rast('outputs/jae3582c37a9348adad6fa11cc4ceb321.tif')

# crop and mask raster to shape
bps_aoi <- bps_full %>%
  crop(shp) %>%
  mask(shp)# 

# read in master CONUS attribute table for BpS
bps_conus_atts <- read.csv("inputs/LF20_BPS_220.csv")

# set levels and active catagory
levels(bps_aoi)[[1]] <- bps_conus_atts
activeCat(bps_aoi) <- "VALUE"

# build attribute table for AOI
bps_aoi_atts <- values(bps_aoi, dataframe = T, na.rm = T) %>%
  table(dnn = "VALUE") %>%
  as.data.frame() %>%
  mutate_all(as.character) %>%
  mutate_all(as.integer) %>%
  left_join(cats(bps_aoi)[[1]], by = "VALUE") %>%
  filter(Freq != 0) %>%
  mutate(ACRES = round((Freq * 900 / 4046.86), 0),
         REL_PERCENT = round((Freq / sum(Freq)), 3) * 100) %>%
  arrange(desc(REL_PERCENT)) %>%
  mutate(great_bpss = if_else(REL_PERCENT < 5.0, 'Other', as.character(REL_PERCENT) ,as.character(BPS_NAME)))

# make vector of bpss to set to NA (delete?)  
bpss_to_delete <- bps_aoi_atts$VALUE[bps_aoi_atts$REL_PERCENT < 5]

# subset the raster
bps_aoi_5per <- bps_aoi

bps_aoi_5per[bps_aoi_5per %in% bpss_to_delete] <- NA

# set category for map
activeCat(bps_aoi_5per) <- "BPS_NAME"

# jank map
plot(bps_aoi_5per)


