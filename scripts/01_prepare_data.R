# ============================================================
# Roadkill risk
# part 1: Data prep
# ============================================================

library(dplyr)
library(terra)
library(sf)

# create raster template
rast <- rast(ext = ext(120, 122.1, 21.8, 25.4), resolution = 0.0009, crs = "EPSG:4326")  # WGS84 CRS

# road data
road.vec <- st_read("data_input/highway/highway.shp") %>% 
  st_transform(crs = 4326) %>% 
  rasterize(rast, touches = TRUE)
plot(road)
writeRaster(road, "data/road.tif")

## process land cover data
# create raster of taiwan island
taiwan.vec <- st_read("data_input/Taiwan_diss/Taiwan_diss.shp") %>% st_transform(crs = 4326)
taiwan.vec$value <- 1
taiwan <- vect(taiwan.vec) %>% buffer(width = 100) %>%
  rasterize(rast, field = "value", background = NA)
plot(taiwan)

# process vegetation data
vegetation.vec <- st_read("data_input/vegetation/ACO0301000011021.shp") %>% st_transform(crs = 4326)
veg_levels <- unique(vegetation.vec$FORMATION)[c(23,19,10,9,35,17,16,38,20,7,26,37,24,15,8,4,14,32,5,29,
                                                 27,18,25,30,28,12,6,33,21,13,36,1,3,22,2,11,31,34)]
formation_mapping <- setNames(12:(11 + length(veg_levels)), veg_levels)
vegetation.vec$Formation_int <- formation_mapping[vegetation.vec$FORMATION]
vegetation <- rasterize(vegetation.vec, rast, field = "Formation_int", background = NA)
plot(vegetation)
write.csv(formation_mapping, "data/var_name.csv")

# process WorldCover data
worldcover <- merge(rast("data_input/landcover/ESA_WorldCover_10m_2021_v200_N21E117_Map.tif"),
                    rast("data_input/landcover/ESA_WorldCover_10m_2021_v200_N21E120_Map.tif"),
                    rast("data_input/landcover/ESA_WorldCover_10m_2021_v200_N24E120_Map.tif")) %>%
  project(rast, method = "near")
worldcover <- worldcover * taiwan # extract land cover data only within the island
worldcover_mapping <- setNames(1:11,  c(10,20,30,40,50,60,70,80,90,95,100))
values(worldcover) <- worldcover_mapping[as.character(values(worldcover))]
plot(worldcover)

# combine two land cover data
unique(c(values(vegetation), values(worldcover)))
land <- worldcover
idx <- which(!is.na(values(vegetation)))
values(land)[idx] <- values(vegetation)[idx]
plot(land)
writeRaster(land, "data/landcover.tif")

## compute distance to water for road pixels
water <- land == 8
values(water)[which(is.na(values(water)))] <- FALSE
plot(water)

roadwater <- water
values(roadwater)[which(values(road) == 1)] <- NA
dist_to_water <- distance(roadwater, target = NA, exclude = FALSE) # 3 mins
# plot(dist_to_water)
plot(crop(dist_to_water, ext(120.3, 120.5, 23.6, 23.8))) # zoom in

writeRaster(dist_to_water, "data/dist_to_water.tif")
