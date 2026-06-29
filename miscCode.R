#Misc Code#

library(tidyverse)

df <- read.csv(file.choose())
unique(df$MonitorID)

counts_df <- df |>
  group_by(MonitorID) |>
  summarise(Count = n())

print(counts_df)

#File naming for cameratrap videos####

# Camera Trap File Renamer
# Renames MP4 files to include their parent folder name as a prefix

library(fs)  # Optional but helpful; base R works too

# ── Configuration ──────────────────────────────────────────────────────────────

# Set your root deployment folder (use forward slashes or double backslashes)
root_dir <- "E:/TBS_MonitorFiles_Deployment3"

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

root_dir <- "E:/TBS_MonitorFiles_Deployment3"

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
write.csv(camera_df, "E:/camera_trap_metadata_D3.csv", row.names = FALSE)

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


#File naming and metadata extraction for audiomoths####

# ══════════════════════════════════════════════════════════════════════════════
# AudioMoth File Renamer + Metadata Extractor
# ══════════════════════════════════════════════════════════════════════════════
#
# Dependencies:
#   install.packages(c("stringr", "dplyr", "av"))
#   av requires FFmpeg — on Windows, it bundles it automatically on install.
# ══════════════════════════════════════════════════════════════════════════════

library(stringr)
library(dplyr)
library(av)

# ── Configuration ──────────────────────────────────────────────────────────────

root_dir  <- "D:/TBS_MonitorFiles_Deployment2"
dry_run   <- FALSE
csv_out   <- "D:/audiomoth_metadata_DP2.csv"

# NOTE: utc_offset_hours removed — AudioMoth is already configured to local time

# ── Helper: format seconds → "MM:SS" ──────────────────────────────────────────

fmt_mmss <- function(total_secs) {
  total_secs <- round(total_secs)
  sprintf("%02d:%02d", total_secs %/% 60L, total_secs %% 60L)
}

# ── Step 1: Find all WAV files ─────────────────────────────────────────────────

wav_files <- list.files(
  path        = root_dir,
  pattern     = "\\.wav$",
  recursive   = TRUE,
  full.names  = TRUE,
  ignore.case = TRUE
)

cat(sprintf("Found %d WAV file(s).\n\n", length(wav_files)))

# ── Step 2: Rename files ───────────────────────────────────────────────────────

cat("── Renaming ──────────────────────────────────────────────────────────────────\n")

renamed_paths <- character(length(wav_files))

for (i in seq_along(wav_files)) {
  
  filepath      <- wav_files[i]
  old_name      <- basename(filepath)
  dir_path      <- dirname(filepath)
  deploy_folder <- basename(dirname(dir_path))   # two levels up = deployment folder
  
  parts <- str_split(tools::file_path_sans_ext(old_name), "_", simplify = TRUE)
  
  if (ncol(parts) < 3) {
    cat(sprintf("  [SKIP – unexpected format] %s\n", old_name))
    renamed_paths[i] <- filepath
    next
  }
  
  date_block <- parts[2]   # 20260607
  time_block <- parts[3]   # 080430
  new_name   <- paste0(deploy_folder, "_", date_block, "_", time_block, ".wav")
  new_path   <- file.path(dir_path, new_name)
  
  if (startsWith(old_name, deploy_folder)) {
    cat(sprintf("  [SKIP – already renamed] %s\n", old_name))
    renamed_paths[i] <- filepath
    next
  }
  
  if (dry_run) {
    cat(sprintf("  [DRY RUN] %s\n            --> %s\n", old_name, new_name))
    renamed_paths[i] <- new_path
    next
  }
  
  success <- tryCatch(file.rename(filepath, new_path), error = function(e) FALSE)
  
  if (success) {
    cat(sprintf("  [OK]  %s\n    --> %s\n", old_name, new_name))
    renamed_paths[i] <- new_path
  } else {
    cat(sprintf("  [ERROR] Could not rename: %s\n", old_name))
    renamed_paths[i] <- filepath
  }
}

cat("\n")

# ── Step 3: Extract metadata ───────────────────────────────────────────────────

cat("── Extracting metadata ───────────────────────────────────────────────────────\n")

records <- lapply(seq_along(renamed_paths), function(i) {
  
  filepath   <- renamed_paths[i]
  filename   <- basename(filepath)
  name_noext <- tools::file_path_sans_ext(filename)
  parts      <- str_split(name_noext, "_", simplify = TRUE)
  
  if (ncol(parts) < 6) {
    warning(sprintf("Cannot parse metadata, skipping: %s", filename))
    return(NULL)
  }
  
  monitor_id   <- parts[1]   # MON01
  deploy_id    <- parts[2]   # DP20260607TO20260614
  # parts[3]   = "AudioMoth" — label only
  audiomoth_id <- parts[4]   # NWJ01
  date_raw     <- parts[5]   # 20260607
  time_raw     <- parts[6]   # 080430
  
  # Date/time already in local Ecuador time — no offset needed
  dt <- as.POSIXct(
    paste(date_raw, time_raw),
    format = "%Y%m%d %H%M%S",
    tz     = "UTC"   # tz="UTC" just prevents R adding a local system offset
  )
  date_str <- format(dt, "%Y-%m-%d")
  time_str <- format(dt, "%H:%M")
  
  # ── Duration via av (handles AudioMoth's non-standard WAV headers) ──────────
  duration_mmss <- tryCatch({
    if (!file.exists(filepath)) stop("File not found: ", filepath)
    info <- av_media_info(filepath)
    fmt_mmss(info$duration)
  }, error = function(e) {
    message(sprintf("  [DURATION ERROR] %s\n    %s", filename, e$message))
    NA_character_
  })
  
  cat(sprintf("  [OK] %s  |  %s  %s  |  %s\n",
              filename, date_str, time_str, duration_mmss))
  
  data.frame(
    MonitorID          = monitor_id,
    AudioMothID        = audiomoth_id,
    DeploymentPeriodID = deploy_id,
    FileName           = filename,
    Date               = date_str,
    Time               = time_str,
    Duration_MMSS      = duration_mmss,
    stringsAsFactors   = FALSE
  )
})

# ── Step 4: Assemble & export ──────────────────────────────────────────────────

records <- Filter(Negate(is.null), records)
meta_df <- bind_rows(records)

cat(sprintf("\nBuilt dataframe: %d rows × %d columns.\n", nrow(meta_df), ncol(meta_df)))

n_missing <- sum(is.na(meta_df$Duration_MMSS))
if (n_missing > 0) {
  cat(sprintf("  Warning: %d file(s) had unreadable durations.\n", n_missing))
  print(meta_df[is.na(meta_df$Duration_MMSS), c("MonitorID", "AudioMothID", "FileName")])
}

cat("\n")
print(head(meta_df, 10))

if (!dry_run) {
  write.csv(meta_df, csv_out, row.names = FALSE)
  cat(sprintf("\nCSV saved to: %s\n", csv_out))
} else {
  cat("\nDry run — CSV not written. Set dry_run <- FALSE to apply changes.\n")
}

# Point this at any one of your renamed WAV files
test_file <- "D:/TBS_MonitorFiles_Deployment1/MON01/AudioMothFiles/MON01_DP20260607TO20260614_AudioMoth_NWJ01/20260607/24F319046488BEF9_20260607_080430.wav"

info <- av_media_info(test_file)
print(info)

#Deleting non-sighting videos####
# =============================================================================
# Camera Trap Video Filter
# =============================================================================
# Reads a CSV of MonitorID (folder path) + VideoID (filename without extension)
# and DELETES all .mp4 files in those folders that are NOT in the list.
#
# CSV format expected:
#   MonitorID  - full path to the folder containing the video files
#   VideoID    - filename WITHOUT extension (e.g. "DSCF0008")
#
# SAFETY FEATURES:
#   - Dry-run mode (default ON): prints what would be deleted without deleting
#   - Summary printed before any deletion occurs
#   - Only touches folders that appear in the CSV (no wider filesystem access)
#   - Only targets .mp4 files (case-insensitive)
# =============================================================================

# ── USER SETTINGS ─────────────────────────────────────────────────────────────

DRY_RUN <- FALSE   # Set to FALSE to actually delete files

# ── LOAD CSV ──────────────────────────────────────────────────────────────────

library(tidyverse)

# Opens a file browser dialog to select your CSV
sightings <- read_csv(file.choose())

# Validate expected columns
required_cols <- c("MonitorID", "VideoID")
missing_cols  <- setdiff(required_cols, names(sightings))
if (length(missing_cols) > 0) {
  stop("CSV is missing required columns: ", paste(missing_cols, collapse = ", "))
}

cat("=== Camera Trap Video Filter ===\n")
cat("CSV loaded:", nrow(sightings), "rows\n\n")

# ── BUILD KEEP LIST ───────────────────────────────────────────────────────────
# Key: normalised folder path → set of VideoIDs to keep (without extension)

# Normalise folder paths (consistent separators, no trailing slash)
sightings$folder_norm <- normalizePath(sightings$MonitorID,
                                       winslash = "/",
                                       mustWork = FALSE)

keep_list <- split(sightings$VideoID, sightings$folder_norm)
# For each folder, store as a lower-cased set for case-insensitive matching
keep_list <- lapply(keep_list, tolower)

unique_folders <- names(keep_list)
cat("Unique folders in CSV:", length(unique_folders), "\n\n")

# ── SCAN FOLDERS AND IDENTIFY FILES TO DELETE ─────────────────────────────────

to_delete  <- character(0)
to_keep    <- character(0)
not_found  <- character(0)   # folders in CSV that don't exist on disk

for (folder in unique_folders) {
  
  if (!dir.exists(folder)) {
    warning("Folder not found on disk (skipping): ", folder)
    not_found <- c(not_found, folder)
    next
  }
  
  # List all .mp4 files in the folder (non-recursive; adjust if needed)
  mp4_files <- list.files(folder,
                          pattern     = "\\.mp4$",
                          ignore.case = TRUE,
                          full.names  = TRUE,
                          recursive   = FALSE)
  
  if (length(mp4_files) == 0) {
    cat("  [no .mp4 files found]", folder, "\n")
    next
  }
  
  # Extract bare filename without extension for comparison
  basenames <- tolower(tools::file_path_sans_ext(basename(mp4_files)))
  
  keep_ids  <- keep_list[[folder]]
  
  for (i in seq_along(mp4_files)) {
    if (basenames[i] %in% keep_ids) {
      to_keep <- c(to_keep, mp4_files[i])
    } else {
      to_delete <- c(to_delete, mp4_files[i])
    }
  }
}

# ── SUMMARY ───────────────────────────────────────────────────────────────────

cat("\n--- Summary ---\n")
cat("Files to KEEP  :", length(to_keep),  "\n")
cat("Files to DELETE:", length(to_delete), "\n")
if (length(not_found) > 0) {
  cat("Folders missing on disk:", length(not_found), "\n")
  cat(paste(" ", not_found, collapse = "\n"), "\n")
}

if (length(to_delete) > 0) {
  cat("\nFiles that will be deleted:\n")
  cat(paste(" ", to_delete, collapse = "\n"), "\n")
}

# ── DELETE (or dry-run) ────────────────────────────────────────────────────────

if (DRY_RUN) {
  cat("\n[DRY RUN] No files were deleted.\n")
  cat("Set DRY_RUN <- FALSE at the top of the script to perform actual deletion.\n")
} else {
  if (length(to_delete) == 0) {
    cat("\nNothing to delete — all .mp4 files in listed folders are on the keep list.\n")
  } else {
    cat("\nDeleting", length(to_delete), "files...\n")
    results <- file.remove(to_delete)
    n_ok    <- sum(results)
    n_fail  <- sum(!results)
    cat("  Deleted successfully:", n_ok, "\n")
    if (n_fail > 0) {
      cat("  FAILED to delete:", n_fail, "\n")
      cat(paste(" ", to_delete[!results], collapse = "\n"), "\n")
    }
  }
}
