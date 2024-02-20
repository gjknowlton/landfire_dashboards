library(tidyverse)
library(sf)
library(raster)
library(tmap)
tmap_mode("view")
##################################
# read data
EVT_PHYS <- raster("./OUTPUTS/evt_phys.tif")

ECOREGION <- st_read("./DATA/ecoregion_conus/ecoregion_conus.shp") %>%
  st_transform(crs(EVT_PHYS))

# only CONUS
CONUS <- st_read("./DATA/us_state_bounds_2017/tl_2017_us_state.shp") %>%
  st_transform(crs(EVT_PHYS)) %>% # transform to evt projection 
  st_simplify(preserveTopology = T, dTolerance = 1000) %>%
  dplyr::select(NAME, STUSPS) %>% # keep region and name attributes
  anti_join(data.frame(STUSPS = c("AK", "AS", "MP", "GU", "HI", "PR", "VI")))

##########################
# create ecoregion shp

eco <- read.csv("./OUTPUTS/ecoregion_join.csv")
megajoin <- read.csv("./OUTPUTS/mega_join.csv") %>%
  mutate(protect = if_else(GAP_VAL == 4, 0, 1)) %>%
  filter(protect == 1) %>%
  na.omit()

megajoin_sums <- megajoin %>%
  group_by(ECO_NAME) %>%
  summarise(TOTAL_PRO = sum(COUNT),
            GAP1 = sum(COUNT[GAP_VAL == 1]),
            GAP2 = sum(COUNT[GAP_VAL == 2]),
            GAP3 = sum(COUNT[GAP_VAL == 3]))

eco_sums <- eco %>%
  group_by(ECO_NAME) %>%
  summarise(TOTAL = sum(COUNT))

ECO_PRO_JOINED <- left_join(megajoin_sums, eco_sums) %>%
  mutate(Protected = round((TOTAL_PRO/TOTAL), 3) *100,
         GAP1 = round((GAP1 / TOTAL), 3) *100,
         GAP2 = round((GAP2 / TOTAL), 3) *100,
         GAP3 = round((GAP3 / TOTAL), 3) *100)

ECOREGION <- left_join(ECOREGION, ECO_PRO_JOINED) %>%
  dplyr::select(ECO_NAME, Protected, GAP1, GAP2, GAP3) %>%
  st_simplify(preserveTopology = T, dTolerance = 1000)

#######################
# binary layers

bin123 <- ECOREGION %>%
  mutate(bin123 = as.factor(if_else(Protected >= 30, 1, 0)),
         Difference = Protected - 30) %>%
  dplyr::select(ECO_NAME, bin123, Difference, Protected)

bin12 <- ECOREGION %>%
  mutate(bin12 = as.factor(if_else((GAP1+GAP2) >= 30, 1, 0)),
         Protected = (GAP1+GAP2),
         Difference = Protected - 30 ) %>%
  dplyr::select(ECO_NAME, bin12, Difference, Protected)


##############################
# interactive map


c3 <- 
  tm_shape(EVT_PHYS, name = "Ecosystems") +
    tm_raster(col = "evt_phys",
              palette = c("#6EBB00","#285601", "#8E9316", "#B62754", 
                          "#AC59D5", "#746BCC", "#528DAB", "#C6C45E",
                          "#B8B792", "#EABDEB", "#64566C", "#C17912",
                          "#E6D335"),
              title = "EVT_PHYS") +
  tm_layout(legend.height = -0.6, legend.width = -0.6) +
  tm_shape(bin123, name = "GAP 1, 2, and 3") +
    tm_polygons(col = "bin123", palette = "Greys",
                title = "Met 30% under GAP 1, 2, and 3",
                popup.vars = c("Protected", "Difference")) +
  tm_shape(bin12, name = "GAP 1 and 2") +
    tm_polygons(col = "bin12", palette = "Greys",
                title = "Met 30% under GAP 1 and 2",
                popup.vars = c("Protected", "Difference")) +
  tm_shape(ECOREGION, name = "Ecoregion Boundaries") +
   tm_borders(lty = "dotdash", lwd = 1.4, col = "black") +
  tm_shape(ECOREGION, name = "Ecoregion GAP") +
    tm_polygons(col = "Protected", palette = "Greens",
                title = "Ecoregion protected (%)",
                style = "fixed", breaks = c(0, 5, 10, 30, 50, 70, 100),
                popup.vars = c("Protected", "GAP1", "GAP2", "GAP3")) +
  tm_shape(CONUS, name = "State Boundaries")  +
    tm_borders(lty = "dashed", lwd = 0.8, col = "black") +
  tm_basemap("")

c3


tmap_save(c3, "./OUTPUTS/c3.html")
