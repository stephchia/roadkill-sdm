# ============================================================
# Roadkill risk
# part 2: Model fitting
# ============================================================

library(dplyr)
library(raster)
library(dismo) # Maxent
library(rJava) # Maxent

# --------------------------------------
# Load predictors (rasters)
# --------------------------------------
#  NOTE: dismo::maxent uses sp classes
road <- raster("data/road.tif")
pred.dWater <- raster("data/dist_to_water.tif")
pred.land <- raster("data/landcover.tif")

# keep only grid cells on road
pred <- stack(pred.land, pred.dWater) %>% mask(road)
names(pred) <- c("landcover", "dis_to_water")

# --------------------------------------
# Presence data
# --------------------------------------
rk_herpet_raw <- read.csv("data_input/roadkill.csv") %>% 
  filter(class %in% c("Squamata", "Amphibia")) %>%
  rename(lon = decimalLongitude, lat = decimalLatitude) %>%
  select(lon, lat)

# keep only points that fall on valid predictor cells
on_target <- apply(!is.na(extract(pred, rk_herpet_raw)), 1, all)
rk_herpet <- rk_herpet_raw[on_target, ]

# --------------------------------------
# Fit MaxEnt model and make prediction
# --------------------------------------
# fit model
set.seed(123)
m.herpet <- maxent(x = pred, p = rk_herpet, nbg = 100000, factors = "landcover", removeDuplicates = FALSE)
m.herpet # print summary (open as an HTML page)
saveRDS(m.herpet, "output/m_herpet.rds")

# check fitted response
response(m.herpet)

# predict roadkill risk
risk <- predict(pred, m.herpet)
plot(risk)

