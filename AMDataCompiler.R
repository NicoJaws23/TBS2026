library(tidyverse)

# Set this to the folder containing your CSV files
csv_folder <- "C:\\Users\\Jawor\\Desktop\\Research\\BirdNet\\outputs\\BirdNet_AMTest\\MON01_DP1\\20260607"

csv_files <- list.files(csv_folder, pattern = "\\.csv$", full.names = TRUE)

lapply(csv_files, function(f) {
  df <- read_csv(f, col_types = col_spec)
  problems(df)
}) %>% keep(~ nrow(.x) > 0)

col_spec <- cols(
  `Start (s)` = col_double(),
  `End (s)` = col_double(),
  `Scientific name` = col_character(),
  `Common name` = col_character(),
  Confidence = col_double(),
  File = col_character()
)

combined_df <- csv_files %>%
  set_names() %>%
  map_dfr(~ read_csv(.x, col_types = col_spec), .id = "OriginalFile") %>%
  mutate(OriginalFile = basename(OriginalFile))

write_csv(combined_df, "combined_output.csv")