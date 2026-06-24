#Camera Trap Analysis#

library(tidyverse)
library(hms)
library(ggplot2)
library(sf)
library(leaflet)
library(leaflet.extras)

#Reading in number of sightings data, waypoint data, joinging####
df <- read.csv(file.choose())

wp <- read.csv(file.choose())
wp <- wp|>
  rename(MonitorID = WaypointID)

df$Time <- as_hms(paste0(df$Time, ":00"))

df <- df|>
  mutate(NumberOfIndividuals = as.numeric(NumberOfIndividuals)) |>
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

df_full <- left_join(df, wp, by = "MonitorID")

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

df_full <- left_join(df, wp, by = "MonitorID")

###Summarise total number of detections seen across ALL VIDEOS by species name####
species_summary <- df_species %>%
  group_by(MonitorID, ScientificName) %>%
  summarise(TotalDetections = n(), .groups = "drop")

species_summary <- left_join(species_summary, wp, by = "MonitorID")

ggplot(species_summary, mapping = aes(x = ScientificName, y = TotalDetections)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Heatmaps with leaflet#

expand_for_heatmap <- function(data) {
  data[rep(seq_len(nrow(data)), times = data$TotalDetections), ]
}

species_list <- sort(unique(species_summary$ScientificName))

color_palette <- colorFactor(
  palette = "Set3",       # try also "Set1", "Set2", "Dark2", or "Paired"
  domain  = species_list
)

map <- leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addProviderTiles(providers$OpenStreetMap,     group = "Street Map") %>%
  addLayersControl(
    baseGroups    = c("Satellite", "Street Map"),
    overlayGroups = species_list,
    options       = layersControlOptions(collapsed = TRUE)
  )

for (sp in species_list) {
  
  sp_data     <- species_summary %>% filter(ScientificName == sp)
  sp_expanded <- expand_for_heatmap(sp_data)
  sp_color    <- color_palette(sp)   # unique color for this species
  
  map <- map %>%
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

map <- map %>% hideGroup(species_list)

map

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
