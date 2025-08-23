library(dplyr)
library(terra)
library(sf)
library(ggplot2)

library(ENMeval)
# library(dismo)
# library(rJava)
library(mapview)
library(leaflet)
library(viridis)

# create raster template
rast <- rast(ext = ext(120, 122.1, 21.8, 25.4), resolution = 0.0009, crs = "EPSG:4326")  # WGS84 CRS

## import processed data
# target grids
road <- rast("data/road.tif")

# predictors
pred.dWater <- rast("data/dist_to_water.tif")
pred.land <- rast("data/landcover.tif")
values(pred.land)[which(is.na(values(road)))] <- NA
values(pred.dWater)[which(is.na(values(road)))] <- NA
pred <- c(pred.land, pred.dWater)
names(pred) <- c("landcover", "dis_to_water")
pred$landcover <- as.factor(pred$landcover)
# plot(pred)

## prepare roadkill data
roadkill_raw <- read.csv("data_input/roadkill.csv") %>% 
  filter(class %in% c("Squamata", "Amphibia")) %>%
  rename(lon = decimalLongitude, lat = decimalLatitude) %>%
  select(lon, lat)

on_target <- apply(!is.na(extract(pred, roadkill_raw)), 1, all)
roadkill <- roadkill_raw[on_target, ]


# remove data outside of the target grids
roadkill <- read.csv("data_input/roadkill.csv") %>% 
  rename(lon = decimalLongitude, lat = decimalLatitude)

extract.target <- function(occur, pred) {
  on_target <- apply(!is.na(extract(pred, occur)), 1, all)
  out <- occur[on_target, ]
  return(out)
}

# prepare dataset for different species groups
allsp <- roadkill %>% dplyr::select(lon, lat) %>%
  extract.target(pred)

herpet <- roadkill %>% filter(class %in% c("Squamata", "Amphibia")) %>%
  dplyr::select(lon, lat) %>%
  extract.target(pred)

bird <- roadkill %>% filter(class %in% c("Aves")) %>%
  dplyr::select(lon, lat) %>%
  extract.target(pred)

mammal <- roadkill %>% filter(class %in% c("Mammalia")) %>%
  dplyr::select(lon, lat) %>%
  extract.target(pred)

## build maxent model, using herpets (reptiles and amphibians) as example
set.seed(123)
m.herpet <- ENMevaluate(
  occs = roadkill,
  envs = pred,
  partitions = "none",
  algorithm = "maxnet",
  categoricals = "landcover",
  tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:5),
  n.bg = 10000,
  parallel = TRUE
)
terra::nlyr(pred); names(pred); terra::levels(pred$landcover)
head(roadkill)
terra::ext(pred); terra::crs(pred)

m.herpet <- maxent(x = pred, p = herpet, nbg = 100000, factors = "landcover", removeDuplicates = FALSE)
m.herpet <- maxent(x = pred, p = herpet, nbg = 100000, factors = "landcover", removeDuplicates = TRUE)
# m.herpet <- maxent(x = pred, p = herpet, nbg = 10000, factors = "landcover")

response(m.all)
response(m.herpet, var="landcover")
response(m.bird)
response(m.mammal, var="landcover")

## speed limit data
speed <- read.csv("data/speed_limit.csv")
pts_speed <- st_as_sf(speed, coords = c("lon", "lat"), crs = 4326)

## plot results as raster
# risk <- predict(pred, m.herpet)
# dt_occur <- herpet
# points <- st_as_sf(dt_occur, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
# 
# mapviewOptions(basemaps = c("CartoDB.DarkMatter", "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"))
# mapview(risk, maxpixels = 9332000, layer.name = "路殺風險", na.color = "transparent", col.regions = magma) +
#   mapview(points, layer.name = "路殺紀錄 (兩爬)", cex = 5, alpha = .1, col.regions = "white")


## plot risk as vector object
get_smooth_vect <- function(risk) {
  # extract risk values along road vectors from risk raster 
  coords <- st_coordinates(road.vec)
  risk.points <- extract(risk, coords[, c("X", "Y")])
  
  # compute risk values of segments (between points)
  risk.seg <- (risk.points[-length(risk.points)] + risk.points[-1]) / 2
  valid <- (coords[-nrow(coords), "L1"] == coords[-1, "L1"]) & (coords[-nrow(coords), "L2"] == coords[-1, "L2"])
  risk.seg <- risk.seg[valid] # exclude "segments" between two different lines
  
  # create line segments
  coords_from <- coords[-nrow(coords), ][valid, ]
  coords_to <- coords[-1, ][valid, ]
  segments <- lapply(seq_along(risk.seg), function(i) {
    st_linestring(rbind(coords_from[i, c("X", "Y")], coords_to[i, c("X", "Y")]))
  })
  
  # compute moving average 
  moving_average <- function(values, window) stats::filter(values, rep(1 / window, window), sides = 2) %>% as.numeric
  risk.seg.smooth <- moving_average(risk.seg, window = 100)
  risk.seg.smooth[is.na(risk.seg.smooth)] <- risk.seg[is.na(risk.seg.smooth)]
  risk_smooth_sf <- st_sf(geometry = st_sfc(segments, crs = st_crs(road.vec)), mean_value = risk.seg.smooth)
  
  return(risk_smooth_sf)
  
  #### try changing the moving average to be based on distance rather than number of points
}

points_herpet <- st_as_sf(herpet, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
points_mammal <- st_as_sf(mammal, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

risk_smooth_herpet <- get_smooth_vect(predict(pred, m.herpet))
risk_smooth_mammal <- get_smooth_vect(predict(pred, m.mammal))

mapviewOptions(basemaps = c("CartoDB.DarkMatter", "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"))
mapview(risk_smooth_herpet, zcol = "mean_value", layer.name = "路殺風險 (兩爬)") +
  mapview(points_herpet, layer.name = "路殺紀錄 (兩爬)", cex = 5, col.regions = "white") +
  mapview(pts_speed, layer.name = "速限牌", zcol = "content", cex = 5, col.regions = rev(turbo(13)))

# # Access the leaflet map
# leaflet.herpet <- map.herpet@map
# leaflet.herpet <- leaflet.herpet %>%
#   addLayersControl(baseGroups = c("深色地圖" = "CartoDB.DarkMatter",
#                                   "標準地圖" = "OpenStreetMap",
#                                   "衛星地圖" = "Esri.WorldImagery",
#                                   "地形圖" = "OpenTopoMap"),
#                    overlayGroups = c("路殺風險", "速限牌"), 
#                    options = layersControlOptions(collapsed = FALSE))
# leaflet.herpet

mapview(risk_smooth_mammal, zcol = "mean_value", layer.name = "路殺風險 (哺乳類)") +
  mapview(points_mammal, layer.name = "路殺紀錄 (哺乳類)", cex = 5, col.regions = "white") +
  mapview(pts_speed, layer.name = "速限牌", zcol = "content", cex = 5, col.regions = rev(turbo(13)))
# colorRampPalette(c("white","black"))(13)
