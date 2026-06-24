#Misc Code#

library(tidyverse)

df <- read.csv(file.choose())
unique(df$MonitorID)

counts_df <- df |>
  group_by(MonitorID) |>
  summarise(Count = n())

print(counts_df)

#File naming for cameratrap videos#

# Camera Trap File Renamer
# Renames MP4 files to include their parent folder name as a prefix

library(fs)  # Optional but helpful; base R works too

# ── Configuration ──────────────────────────────────────────────────────────────

# Set your root deployment folder (use forward slashes or double backslashes)
root_dir <- "D:/TBS_MonitorFiles_Deployment2"

# File extension to target (case-insensitive)
target_ext <- "\\.mp4$"

# Set to FALSE to do a dry run first (no files renamed, just previewed)
dry_run <- FALSE

# ── Find all matching files ────────────────────────────────────────────────────

mp4_files <- list.files(
  path       = root_dir,
  pattern    = target_ext,
  recursive  = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)

cat(sprintf("Found %d MP4 file(s) to process.\n\n", length(mp4_files)))

# ── Rename logic ───────────────────────────────────────────────────────────────

results <- lapply(mp4_files, function(filepath) {
  
  parent_folder <- basename(dirname(filepath))  # e.g. MON01_DP20260607TO20260614_CameraTrap_NWJ20
  old_name      <- basename(filepath)           # e.g. DSCF0027.MP4
  new_name      <- paste0(parent_folder, "_", old_name)  # e.g. MON01_..._DSCF0027.MP4
  new_path      <- file.path(dirname(filepath), new_name)
  
  # Skip if the file has already been renamed (prefix already present)
  if (startsWith(old_name, parent_folder)) {
    cat(sprintf("  [SKIP]    %s  (already renamed)\n", old_name))
    return(data.frame(status = "skipped", old = filepath, new = new_path))
  }
  
  if (dry_run) {
    cat(sprintf("  [DRY RUN] %s\n            --> %s\n", old_name, new_name))
    return(data.frame(status = "dry_run", old = filepath, new = new_path))
  }
  
  # Attempt rename
  success <- tryCatch({
    file.rename(filepath, new_path)
  }, error = function(e) FALSE)
  
  if (success) {
    cat(sprintf("  [OK]      %s --> %s\n", old_name, new_name))
    return(data.frame(status = "renamed", old = filepath, new = new_path))
  } else {
    cat(sprintf("  [ERROR]   Could not rename: %s\n", old_name))
    return(data.frame(status = "error", old = filepath, new = new_path))
  }
})

# ── Summary ────────────────────────────────────────────────────────────────────

results_df <- do.call(rbind, results)

cat("\n── Summary ───────────────────────────────────────────────────────────────────\n")
print(table(results_df$status))

if (dry_run) {
  cat("\nDry run complete — no files were renamed.\n")
  cat("Set  dry_run <- FALSE  to apply changes.\n")
}

# Camera Trap Metadata Extractor
# Parses renamed MP4 filenames and extracts Date/Time from embedded MP4 metadata
# Uses a single batch exiftool call for performance

# ── Dependencies ───────────────────────────────────────────────────────────────

# install.packages(c("exifr", "stringr", "dplyr"))
library(stringr)
library(dplyr)
library(exifr)

# exiftool must be installed on your system: https://exiftool.org

# ── Configuration ──────────────────────────────────────────────────────────────

root_dir <- "D:/TBS_MonitorFiles_Deployment2"

utc_offset_hours <- -5   # Ecuador = UTC-5, adjust here if ever needed

# ── Find all renamed MP4 files ─────────────────────────────────────────────────

mp4_files <- list.files(
  path        = root_dir,
  pattern     = "\\.mp4$",
  recursive   = TRUE,
  full.names  = TRUE,
  ignore.case = TRUE
)

cat(sprintf("Found %d MP4 file(s).\n\n", length(mp4_files)))

# ── Read ALL EXIF data in a single exiftool call ───────────────────────────────

cat("Reading EXIF metadata for all files (this may take a moment)...\n")

all_exif <- read_exif(
  mp4_files,
  tags = c("SourceFile", "CreateDate", "MediaCreateDate", "TrackCreateDate")
)

cat("Done. Parsing filenames...\n\n")

# ── Parse each file ────────────────────────────────────────────────────────────

records <- lapply(mp4_files, function(filepath) {
  
  filename <- tools::file_path_sans_ext(basename(filepath))  # strip .MP4
  
  # Expected pattern:
  # MON01 _ DP20260607TO20260614 _ CameraTrap _ NWJ20 _ DSCF0027
  # [1]     [2]                    [3]           [4]     [5]
  parts <- str_split(filename, "_", simplify = TRUE)
  
  if (length(parts) < 5) {
    warning(sprintf("Unexpected filename format, skipping: %s", filename))
    return(NULL)
  }
  
  monitor_id    <- parts[1]   # MON01
  deployment_id <- parts[2]   # DP20260607TO20260614
  # parts[3] is "CameraTrap" — label only, not stored
  camera_id     <- parts[4]   # NWJ20
  video_id      <- parts[5]   # DSCF0027
  
  # Pull this file's EXIF row from the batch result
  exif_row <- all_exif[all_exif$SourceFile == filepath, ]
  
  # Work through fallback priority
  dt_raw <- NA_character_
  for (tag in c("CreateDate", "MediaCreateDate", "TrackCreateDate")) {
    if (tag %in% names(exif_row) && !is.na(exif_row[[tag]]) && nchar(exif_row[[tag]]) > 0) {
      dt_raw <- exif_row[[tag]]
      break
    }
  }
  
  # Camera stores UTC; subtract offset to get Ecuador local time
  dt <- if (!is.na(dt_raw)) {
    parsed <- as.POSIXct(dt_raw, format = "%Y:%m:%d %H:%M:%S", tz = "UTC")
    local  <- parsed + utc_offset_hours * 3600
    list(date = format(local, "%Y-%m-%d"), time = format(local, "%H:%M"))
  } else {
    list(date = NA_character_, time = NA_character_)
  }
  
  data.frame(
    MonitorID          = monitor_id,
    CameraID           = camera_id,
    DeploymentPeriodID = deployment_id,
    VideoID            = video_id,
    Date               = dt$date,
    Time               = dt$time,
    stringsAsFactors   = FALSE
  )
})

# ── Assemble dataframe ─────────────────────────────────────────────────────────

records   <- Filter(Negate(is.null), records)   # drop any skipped files
camera_df <- bind_rows(records)

# ── Preview & flag missing datetimes ──────────────────────────────────────────

n_missing <- sum(is.na(camera_df$Date))

cat(sprintf("Built dataframe with %d rows and %d columns.\n", nrow(camera_df), ncol(camera_df)))

if (n_missing > 0) {
  cat(sprintf("  Warning: %d file(s) had no readable EXIF datetime.\n", n_missing))
  cat("  Affected files:\n")
  print(camera_df[is.na(camera_df$Date), c("MonitorID", "CameraID", "VideoID")])
}

cat("\n")
print(head(camera_df, 10))

# ── Export ─────────────────────────────────────────────────────────────────────

# Uncomment to save:
write.csv(camera_df, "D:/camera_trap_metadata_DP2.csv", row.names = FALSE)

# Replace with the full path of one file you know the correct time for
test_file <- "D:/TBS_MonitorFiles_Deployment1/MON01/CameraTrapFiles/MON01_DP20260607TO20260614_CameraTrap_NWJ20/MON01_DP20260607TO20260614_CameraTrap_NWJ20_DSCF0027.MP4"

# Pull every time-related tag exiftool can find
test_exif <- read_exif(test_file, tags = c(
  "SourceFile",
  "CreateDate",
  "MediaCreateDate", 
  "TrackCreateDate",
  "DateTimeOriginal",
  "ModifyDate",
  "TrackModifyDate",
  "MediaModifyDate",
  "TimeZone",
  "TimeZoneOffset"
))

# Print all columns and their values
t(test_exif)
