#Camera Trap Analysis#

library(tidyverse)
library(hms)
library(ggplot2)
library(sf)
library(leaflet)
library(leaflet.extras)
library(htmltools)

#Reading in number of sightings data, waypoint data, joinging####
f <- "https://raw.githubusercontent.com/NicoJaws23/TBS2026/refs/heads/main/RecordOfSightings.csv"
df <- read.csv(f)

g <- "https://raw.githubusercontent.com/NicoJaws23/TBS2026/refs/heads/main/RealMonWaypoints.csv"
wp <- read.csv(g)

df <- df %>%
  filter(if_any(-NeedsReview, ~ !is.na(.x) & .x != ""))

df$Time <- as_hms(paste0(df$Time, ":00"))

df <- df|>
  mutate(NumberOfIndividuals = as.numeric(NumberOfIndividuals)) |>
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

df_full <- left_join(df, wp, by = "MonitorID")

dfSpeciesSplit <- df_full %>%
  mutate(ScientificName = na_if(ScientificName, "N/A")) %>%
  separate(
    ScientificName,
    into = c("Genus", "Species"),
    sep = " ",
    remove = FALSE,
    extra = "merge",
    fill = "right"
  )  
trails <- st_read("https://raw.githubusercontent.com/NicoJaws23/creative-data-visualization/refs/heads/main/TBS_Trails.geojson") |>
  st_transform(4326)
river <- st_read("https://raw.githubusercontent.com/NicoJaws23/creative-data-visualization/refs/heads/main/rio_tiputini.geojson") |>
  st_transform(4326)

north_arrow_html <- tags$div(
  style = "padding: 5px; background: rgba(255, 255, 255, 0.8); border-radius: 4px; text-align: center;",
  tags$div(style = "font-size: 20px; font-weight: bold; line-height: 1;", "▲"),
  tags$div(style = "font-size: 12px; font-weight: bold; font-family: sans-serif;", "N")
)

#Clean ScientificName

df$ScientificName <- trimws(df$ScientificName)  # removes leading/trailing spaces (fixes Leptotilla)

df$ScientificName <- recode(df$ScientificName,
                            # Capitalisation fixes
                            "Leptotilla Rufaxilla"     = "Leptotilla rufaxilla",
                            "Psophia Leucoptera"       = "Psophia leucoptera",
                            "Puma Concolor"            = "Puma concolor",
                            "Eira Barbara"             = "Eira barbara",
                            "Nasua Nasua"              = "Nasua nasua",
                            "Dasypus Novemcinctus"     = "Dasypus novemcinctus",
                            
                            # Typo fixes
                            "Lepardus paradalis"       = "Leopardus pardalis",
                            "Tianmus guttatus"         = "Tinamus guttatus",
                            "Mazama american"          = "Mazama americana",
                            "Myopracta pratti"         = "Myoprocta pratti",
                            "Cryptullerus soui"        = "Crypturellus soui"
)

df_species <- df[df$ScientificName != "N/A", ]

dfSpeciesSplit <- df_species %>%
  mutate(ScientificName = na_if(ScientificName, "N/A")) %>%
  separate(
    ScientificName,
    into = c("Genus", "Species"),
    sep = " ",
    remove = FALSE,
    extra = "merge",
    fill = "right"
  )  


df_full <- left_join(dfSpeciesSplit, wp, by = "MonitorID")

###Summarise total number of detections seen across ALL VIDEOS by species name####
species_summary <- df_full %>%
  group_by(MonitorID, Genus) %>%
  summarise(TotalDetections = n(), .groups = "drop")

species_summary <- left_join(species_summary, wp, by = "MonitorID")

ggplot(species_summary, mapping = aes(x = Genus, y = TotalDetections)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Heatmaps with leaflet#

expand_for_heatmap_1 <- function(data) {
  data[rep(seq_len(nrow(data)), times = data$TotalDetections), ]
}

species_list <- sort(unique(species_summary$Genus))

color_palette <- colorFactor(
  palette = "Set3",       # try also "Set1", "Set2", "Dark2", or "Paired"
  domain  = species_list
)

map_1Title<- tags$div(
  tag.map.title <- tags$style(HTML(".leaflet-control.map-title {
                                   background: rgba(255, 255, 255, 0.9);
                                   padding: 10px;
                                   font-weight: bold;
                                   font_size: 20px
                                   border-radius: 5px;
                                   }")),
  HTML("Total Number of Detections Per Species")
)
map_1 <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addProviderTiles(providers$OpenStreetMap,     group = "Street Map") |>
  
  # ── Static layers ────────────────────────────────────────────────────────────
  addPolylines(
    data   = trails,
    color  = "black",
    weight = 2,
    group  = "Trails",
    label  = "Trails"
  ) |>
  addPolylines(
    data   = river,
    color  = "blue",
    weight = 2,
    group  = "River",
    label  = "River"
  ) |>
  addControl(
    html = map_1Title,
    position = "topright",
    className = "map-title"
  ) |>
  addControl(
    html = north_arrow_html,
    position = "topright"
  ) |>
  addScaleBar(
    position = "bottomleft",
    options = scaleBarOptions(metric = TRUE, imperial = FALSE)
  ) |>
  addLayersControl(
    baseGroups    = c("Satellite", "Street Map"),
    overlayGroups = c("Trails", "River", species_list),
    options       = layersControlOptions(collapsed = TRUE)
  )

for (sp in species_list) {
  
  sp_data     <- species_summary %>% filter(Genus == sp)
  sp_expanded <- expand_for_heatmap_1(sp_data)
  sp_color    <- color_palette(sp)   # unique color for this species
  
  map_1 <- map_1 %>%
    addHeatmap(
      data   = sp_expanded,
      lat    = ~lat,
      lng    = ~lon,
      blur   = 30,
      max    = 1,
      radius = 25,
      group  = sp
    ) %>%
    addCircleMarkers(
      data        = sp_data,
      lat         = ~lat,
      lng         = ~lon,
      radius      = ~scales::rescale(TotalDetections, to = c(3, 12)),
      color       = "white",
      fillColor   = sp_color,
      fillOpacity = 0.8,
      weight      = 1,
      popup       = ~paste0(
        "<b>", Genus, "</b><br>",
        "Monitor: ", MonitorID, "<br>",
        "Detections: ", TotalDetections
      ),
      group = sp
    )
}

map_1 <- map_1 %>% hideGroup(species_list)

map_1

#Sitings of number of individuals of species by day by monitor####
df_uniqueSite <- df_full |>
  distinct(MonitorID, Date, ScientificName, .keep_all = TRUE) |>
  filter(ScientificName != "N/A")

uniqueSiteSummary <- df_uniqueSite |>
  group_by(MonitorID, ScientificName) |>
  summarise(TotalDetectionDays = n(), .groups = "drop")

uniqueSiteSummary <- left_join(uniqueSiteSummary, wp, by = "MonitorID")

ggplot(uniqueSiteSummary, mapping = aes(x = ScientificName, y = TotalDetectionDays)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

species_list_2 <- sort(unique(uniqueSiteSummary$ScientificName))

color_palette <- colorFactor(
  palette = "Set3",       # try also "Set1", "Set2", "Dark2", or "Paired"
  domain  = species_list_2
)

expand_for_heatmap_2 <- function(data) {
  data[rep(seq_len(nrow(data)), times = data$TotalDetectionDays), ]
}

map_2Title<- tags$div(
  tag.map.title <- tags$style(HTML(".leaflet-control.map-title {
                                   background: rgba(255, 255, 255, 0.9);
                                   padding: 10px;
                                   font-weight: bold;
                                   font_size: 20px
                                   border-radius: 5px;
                                   }")),
  HTML("Total Number of Detections Per Species Per Day Per Monitor")
)

map_2 <- leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addProviderTiles(providers$OpenStreetMap,     group = "Street Map") |>
  
  # ── Static layers ────────────────────────────────────────────────────────────
  addPolylines(
    data   = trails,
    color  = "black",
    weight = 2,
    group  = "Trails",
    label  = "Trails"
  ) |>
  addPolylines(
    data   = river,
    color  = "blue",
    weight = 2,
    group  = "River",
    label  = "River"
  ) |>
  addControl(
    html = map_2Title,
    position = "topright",
    className = "map-title"
  ) |>
  addControl(
    html = north_arrow_html,
    position = "topright"
  ) |>
  addScaleBar(
    position = "bottomleft",
    options = scaleBarOptions(metric = TRUE, imperial = FALSE)
  ) |>
  addLayersControl(
    baseGroups    = c("Satellite", "Street Map"),
    overlayGroups = c("Trails", "River", species_list),
    options       = layersControlOptions(collapsed = TRUE)
  )

for (sp in species_list) {
  
  sp_data     <- uniqueSiteSummary %>% filter(ScientificName == sp)
  sp_expanded <- expand_for_heatmap_2(sp_data)
  sp_color    <- color_palette(sp)   # unique color for this species
  
  map_2 <- map_2 %>%
    addHeatmap(
      data   = sp_expanded,
      lat    = ~lat,
      lng    = ~lon,
      blur   = 30,
      max    = 1,
      radius = 25,
      group  = sp
    ) %>%
    addCircleMarkers(
      data        = sp_data,
      lat         = ~lat,
      lng         = ~lon,
      radius      = ~scales::rescale(TotalDetectionDays, to = c(3, 12)),
      color       = "white",
      fillColor   = sp_color,
      fillOpacity = 0.8,
      weight      = 1,
      popup       = ~paste0(
        "<b>", ScientificName, "</b><br>",
        "Monitor: ", MonitorID, "<br>",
        "Detection Days: ", TotalDetectionDays
      ),
      group = sp
    )
}

map_2 <- map_2 %>% hideGroup(species_list)

map_2

#Sitings of number of individuals of species by monitor by day by hour####
df_hourly <- df_species |>
  mutate(Hour = format(as.POSIXct(paste("2000-01-01", Time), format = "%Y-%m-%d %H:%M"), "%H")) |>
  distinct(MonitorID, ScientificName, Date, Hour, .keep_all = TRUE)

hourly_summary <- df_hourly |>
  group_by(MonitorID, ScientificName, Date) |>
  summarise(TotalDetections = n(), .groups = "drop")

hourly_map_summary <- hourly_summary |>
  group_by(MonitorID, ScientificName) |>
  summarise(TotalDetections = sum(TotalDetections), .groups = "drop")

hourly_map_summary <- left_join(hourly_map_summary, wp, by = "MonitorID")

ggplot(hourly_summary, mapping = aes(x = ScientificName, y = TotalDetections)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

species_list_3 <- sort(unique(hourly_summary$ScientificName))

color_palette <- colorFactor(
  palette = "Set3",       # try also "Set1", "Set2", "Dark2", or "Paired"
  domain  = species_list_3
)

map_3Title<- tags$div(
  tag.map.title <- tags$style(HTML(".leaflet-control.map-title {
                                   background: rgba(255, 255, 255, 0.9);
                                   padding: 10px;
                                   font-weight: bold;
                                   font_size: 20px
                                   border-radius: 5px;
                                   }")),
  HTML("Total Number of Detections Per Species Per Day Per Hour Per Monitor")
)

map_3 <- leaflet() |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
  addProviderTiles(providers$OpenStreetMap,     group = "Street Map") |>
  
  # ── Static layers ────────────────────────────────────────────────────────────
  addPolylines(
    data   = trails,
    color  = "black",
    weight = 2,
    group  = "Trails",
    label  = "Trails"
  ) |>
  addPolylines(
    data   = river,
    color  = "blue",
    weight = 2,
    group  = "River",
    label  = "River"
  ) |>
  addControl(
    html = map_3Title,
    position = "topright",
    className = "map-title"
  ) |>
  addControl(
    html = north_arrow_html,
    position = "topright"
  ) |>
  addScaleBar(
    position = "bottomleft",
    options = scaleBarOptions(metric = TRUE, imperial = FALSE)
  ) |>
  addLayersControl(
    baseGroups    = c("Satellite", "Street Map"),
    overlayGroups = c("Trails", "River", species_list),
    options       = layersControlOptions(collapsed = TRUE)
  )

for (sp in species_list_3) {   # also fix: use species_list_3, not species_list
  
  sp_data     <- hourly_map_summary %>% filter(ScientificName == sp)
  sp_expanded <- sp_data[rep(seq_len(nrow(sp_data)), times = sp_data$TotalDetections), ]
  sp_color    <- color_palette(sp)
  
  map_3 <- map_3 %>%
    addHeatmap(
      data   = sp_expanded,
      lat    = ~lat,
      lng    = ~lon,
      blur   = 30,
      max    = 1,
      radius = 25,
      group  = sp
    ) %>%
    addCircleMarkers(
      data        = sp_data,
      lat         = ~lat,
      lng         = ~lon,
      radius      = ~scales::rescale(TotalDetections, to = c(3, 12)),
      color       = "white",
      fillColor   = sp_color,
      fillOpacity = 0.8,
      weight      = 1,
      popup       = ~paste0(
        "<b>", ScientificName, "</b><br>",
        "Monitor: ", MonitorID, "<br>",
        "Detections: ", TotalDetections
      ),
      group = sp
    )
}

map_3 <- map_3 %>% hideGroup(species_list)

map_3

