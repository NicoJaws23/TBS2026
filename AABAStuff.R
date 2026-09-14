#AABA Stuff#

library(tidyverse)
library(fs)
library(lubridate)
library(leaflet)

df <- read.csv("OS_AV_FS_FD.csv")
head(df)
names(df)
lagothrix_d <- df %>%
  filter(Group == "Lagothrix D") %>%
  distinct(Avistaje.ID, .keep_all = TRUE)

#Geting D files####
# 1. Pull the AV numbers you need
av_ids <- lagothrix_d %>% pull(Avistaje.ID) %>% unique()

# 2. Set up source and destination
gps_root <- "D:/Proyecto Primates Data/GPS Data 2026"
dest_dir <- "GPS_Avistajes_LagothrixD"
dir_create(dest_dir)

# 3. Find every file inside any folder whose path contains "Avistaje" (case-insensitive)
all_files <- dir_ls(gps_root, recurse = TRUE, type = "file")

avistaje_files <- all_files %>%
  keep(~ str_detect(.x, regex("avistaje", ignore_case = TRUE)))

# 4. Match AV number as a whole token, bounded by dashes
av_pattern <- str_c("-(", str_c(av_ids, collapse = "|"), ")-")

matched_files <- avistaje_files %>%
  keep(~ str_detect(path_file(.x), av_pattern))

# 5. Copy them over
file_copy(matched_files, path(dest_dir, path_file(matched_files)), overwrite = TRUE)

# sanity check: did every AV ID get a match?
found_ids <- str_extract(path_file(matched_files), str_c(av_ids, collapse = "|"))
setdiff(av_ids, found_ids)  # should be empty (or length 0) if nothing's missing



#Averaging and Mapping
process_file <- function(filepath) {
  fname <- path_file(filepath)
  date_val <- str_extract(fname, "^\\d{4}-\\d{2}-\\d{2}")
  av_id    <- str_extract(fname, "AV\\d+")
  
  # pull observer name out of the "GPS Data <Name> 2026" folder in the path
  path_parts <- path_split(filepath)[[1]]
  observer_folder <- path_parts[str_detect(path_parts, regex("GPS Data", ignore_case = TRUE))][1]
  observer <- observer_folder %>%
    str_remove(regex("GPS Data", ignore_case = TRUE)) %>%
    str_remove("2026") %>%
    str_trim()
  
  df <- read_tsv(filepath, col_types = cols(.default = "c"), show_col_types = FALSE)
  
  numeric_cols <- c("Latitude", "Longitude", "y_proj", "x_proj", "altitude", "depth", "temp")
  other_cols   <- setdiff(names(df), c(numeric_cols, "time", "ltime"))
  
  df <- df %>%
    mutate(
      across(all_of(numeric_cols), as.numeric),
      time_parsed = ymd_hms(str_remove(time, "\\+\\d+$"), tz = "UTC"),
      bin = floor_date(time_parsed, "5 minutes")
    )
  
  df %>%
    group_by(bin) %>%
    summarize(
      across(all_of(numeric_cols), ~ mean(.x, na.rm = TRUE)),
      across(all_of(other_cols), ~ first(.x)),
      ltime = first(ltime),
      .groups = "drop"
    ) %>%
    rename(time = bin) %>%
    mutate(Observer = observer, Avistaje_ID = av_id, Date = date_val)
}

combined_smoothed <- map_dfr(matched_files, process_file)

write_csv(combined_smoothed, "GPS_Smoothed_5min_Combined.csv")

# color palette, one color per Avistaje
pal <- colorFactor(palette = "Dark2", domain = combined_smoothed$Avistaje_ID)

leaflet(combined_smoothed) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addCircleMarkers(
    lng = ~Longitude,
    lat = ~Latitude,
    color = ~pal(Avistaje_ID),
    radius = 4,
    stroke = FALSE,
    fillOpacity = 0.8,
    popup = ~paste0(
      "<b>Avistaje:</b> ", Avistaje_ID, "<br>",
      "<b>Observer:</b> ", Observer, "<br>",
      "<b>Date:</b> ", Date, "<br>",
      "<b>Time:</b> ", format(time, "%H:%M:%S"), "<br>",
      "<b>Altitude:</b> ", round(altitude, 1)
    )
  ) %>%
  addLegend(position = "bottomright", pal = pal, values = ~Avistaje_ID, title = "Avistaje ID")
