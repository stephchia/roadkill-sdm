# ============================================================
# Roadkill risk
# part 3: Visualize risk prediction map
# ============================================================

library(ggplot2)
library(sf)
library(mapview) # interactive map

# ------------------------------------------------------------------
# Interactive map + presence points (raster)
# ------------------------------------------------------------------
rk_points <- st_as_sf(rk_herpet, coords = c("lon", "lat"), crs = 4326)

mapviewOptions(basemaps = c("CartoDB.DarkMatter", "OpenStreetMap", "Esri.WorldImagery", "OpenTopoMap"))
mapview(risk, maxpixels = 9332000, layer.name = "Roadkill risk", na.color = "transparent") +
  mapview(rk_points, layer.name = "Roadkill records (herpet)", cex = 5, alpha = .1, col.regions = "white")

# ------------------------------------------------------------------
# Interactive map + presence points (vectorized)
# ------------------------------------------------------------------
# create road vector
road.vec <- st_read("data/input/highway/highway.shp") %>% st_transform(st_crs(risk))

get_smooth_vect <- function(risk) {
  # extract risk values along road vectors (at vertices) from risk raster 
  coords <- st_coordinates(road.vec)
  risk.points <- extract(risk, coords[, c("X", "Y")])
  
  # compute risk values of segments (midpoint average between consecutive vertices)
  risk.seg <- (risk.points[-length(risk.points)] + risk.points[-1]) / 2
  valid <- (coords[-nrow(coords), "L1"] == coords[-1, "L1"]) & 
    (coords[-nrow(coords), "L2"] == coords[-1, "L2"])
  risk.seg <- risk.seg[valid]
  
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
  
  # return sf object
  risk_smooth_sf <- st_sf(geometry = st_sfc(segments, crs = st_crs(road.vec)), 
                          mean_value = risk.seg.smooth)
  return(risk_smooth_sf)
}

risk_smooth_herpet <- get_smooth_vect(predict(pred, m.herpet))

mapview(risk_smooth_herpet, zcol = "mean_value", layer.name = "Roadkill risk (herpet)") +
  mapview(rk_points, layer.name = "Roadkill records (herpet)", cex = 5, col.regions = "white")