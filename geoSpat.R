#GeoSpatial Work

library(adehabitatHR)
library(leaflet)
library(mapview)
library(tidyverse)
library(sf)
library(lubridate)
library(terra)

files <- list.files(path = "C:\\Users\\Jawor\\Desktop\\Research\\LagoD_WD\\EXTRACT_15_WINDOW_120",
                    pattern = "*.csv",
                    full.names = TRUE)

combined_df <- files |> 
  map_dfr(read_csv)

write.csv(combined_df, file = "C:\\Users\\Jawor\\Desktop\\TBS_2026\\groupD_2014to2018.csv")

combined_df <- combined_df |>
  select(mean_x_proj, mean_y_proj, mean_latitude, mean_longitude, mean_alt)

trails <- st_read("https://raw.githubusercontent.com/NicoJaws23/creative-data-visualization/refs/heads/main/TBS_Trails.geojson") |>
  st_transform(32718)
river <- st_read("https://raw.githubusercontent.com/NicoJaws23/creative-data-visualization/refs/heads/main/rio_tiputini.geojson") |>
  st_transform(32718)

viewHR <- function(df, trailsDF, riverDF, groupID, HRcolor, elevation = c("Y", "N")){
  pts <- st_as_sf(df, coords = c("mean_x_proj", "mean_y_proj"), crs = 32718)
  pts_sp <- as(pts, "Spatial")
  proj4string(pts_sp) <- CRS("+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs")
  pts_simple <- SpatialPoints(pts_sp@coords, proj4string = CRS("+proj=utm +zone=18 +south +datum=WGS84"))
  khr <- kernelUD(pts_simple, h = "href")
  hr <- getverticeshr(khr, percent = 95)
  hr_sf <- st_as_sf(hr)
  hr_layer <- mapview(hr_sf, col.regions = HRcolor, color = HRcolor, alpha.regions = 0.4, layer.name = paste(groupID, "Home Range"))
  trail_layer <- mapview(trailsDF, color = "orange", layer.name = "Trails", lwd = 2)
  river_layer <- mapview(riverDF, color = "blue", layer.name = "River", lwd = 2)
  map <- hr_layer + trail_layer + river_layer
  return(list(map = map, homerange = hr_sf))
}



cHR <- viewHR(combined_df, trails, river, "D", "tan4")
cHR$map
st_write(cHR$homerange, "C:\\Users\\Jawor\\Desktop\\TBS_2026\\hr95_LagothrixD_2014to2018.geojson", driver = "GeoJSON")

create_hr_grid <- function(hr_sf, interval = 250) {
  
  hr_bbox <- st_bbox(hr_sf)
  
  x_coords <- seq(from = hr_bbox["xmin"], to = hr_bbox["xmax"], by = interval)
  y_coords <- seq(from = hr_bbox["ymin"], to = hr_bbox["ymax"], by = interval)
  
  grid_coords <- expand.grid(x = x_coords, y = y_coords)
  
  grid_pts <- st_as_sf(grid_coords, coords = c("x", "y"), crs = 32718)
  
  grid_pts_clipped <- grid_pts[st_within(grid_pts, hr_sf, sparse = FALSE), ]
  
  grid_pts_clipped <- grid_pts_clipped |>
    mutate(
      point_id = row_number(),
      utm_x = st_coordinates(geometry)[, 1],
      utm_y = st_coordinates(geometry)[, 2]
    ) |>
    select(point_id, utm_x, utm_y, geometry)
  
  grid_df <- grid_pts_clipped |>
    st_drop_geometry() |>
    select(point_id, utm_x, utm_y)
  
  return(list(spatial = grid_pts_clipped, dataframe = grid_df))
}

hr_grid <- create_hr_grid(cHR$homerange, interval = 250)

hr_grid_sf <- hr_grid$spatial
hr_grid_df <- hr_grid$dataframe

mapview(cHR$homerange, col.regions = "tan4", alpha.regions = 0.3) +
  mapview(hr_grid_sf, cex = 2, color = "black", col.regions = "black", layer.name = "250m Grid Points")

st_write(hr_grid_sf, "C:\\Users\\Jawor\\Desktop\\TBS_2026\\hr_grid_250m_LagothrixD.geojson", driver = "GeoJSON")

write.csv(hr_grid_df, "C:\\Users\\Jawor\\Desktop\\TBS_2026\\hr_grid_250m_LagothrixD_coords.csv", row.names = FALSE)
