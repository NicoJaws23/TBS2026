
# ==========================================================================
# Batch-clip Raven selections into folders by vocalization type
#
# Assumes:
#   - One folder of audio recordings (.wav)
#   - One folder of matching Raven selection tables (.csv or .txt),
#     where each table's filename shares a common prefix/stem with its
#     matching audio file (e.g. "Site1_Day3.wav" <-> "Site1_Day3.Table.1.selections.csv")
#   - Selection tables have at minimum: Begin Time (s), End Time (s),
#     and a column with your 3-letter call code (edit CALL_COL below)
#   - Output folders (VAB, VCH, VTR, etc.) already exist inside OUTPUT_DIR
# ==========================================================================

library(tuneR)

# ---- USER SETTINGS -------------------------------------------------------
AUDIO_DIR   <- "C:\\Users\\Jawor\\Desktop\\TBS_2026\\VXX_Recordings\\2026-07-11"          # folder with .wav files
SELTAB_DIR  <- "C:\\Users\\Jawor\\Desktop\\TBS_2026\\VXX_Recordings\\2026-07-11\\selectionTables"    # folder with Raven selection tables
OUTPUT_DIR  <- "C:\\Users\\Jawor\\Desktop\\Research\\BirdNet\\TrainingData"              # parent folder containing VAB/, VCH/, VTR/ etc.
CALL_COL    <- "Annotation"                   # name of your 3-letter code column
SEL_DELIM   <- "\t"                          # "\t" for Raven's default tab-delimited .txt export, "," if you ever use CSV
NORMALIZE   <- TRUE                          # TRUE = peak-normalize every clip after cutting
NORM_BITS   <- "16"                          # target bit depth for normalization ("16", "24", "32", or "1" for float [-1,1])

NORMALIZE_ANALYSIS_AUDIO <- TRUE             # TRUE = also normalize the full recordings you'll run BirdNET on
ANALYSIS_AUDIO_DIR        <- "C:\\Users\\Jawor\\Desktop\\Research\\BirdNet\\wavFilesForTrainingDataPt2"        # folder with your original 9-minute recordings
ANALYSIS_AUDIO_OUTPUT_DIR <- "C:\\Users\\Jawor\\Desktop\\Research\\BirdNet\\wavFilesForTrainingDataPt2Normalized"  # normalized copies go here -- originals are left untouched
# ---------------------------------------------------------------------------

# Match each selection table to its recording by shared filename stem
audio_files  <- list.files(AUDIO_DIR, pattern = "\\.wav$", full.names = TRUE, ignore.case = TRUE)
seltab_files <- list.files(SELTAB_DIR, pattern = "\\.(csv|txt)$", full.names = TRUE, ignore.case = TRUE)

get_stem <- function(path) {
  # Takes everything before the FIRST dot in the filename, so it works
  # regardless of how the selection table names the rest
  # (e.g. "ZOOM0077.WAV" -> "ZOOM0077",
  #  "ZOOM0077.BirdNET.selection.table.txt" -> "ZOOM0077",
  #  "ZOOM0073.Table.1.selections.txt" -> "ZOOM0073")
  base <- basename(path)
  sub("\\..*$", "", base)
}

audio_stems  <- sapply(audio_files, get_stem)
seltab_stems <- sapply(seltab_files, get_stem)

cat("Found", length(audio_files), "audio files and", length(seltab_files), "selection tables.\n")

for (i in seq_along(seltab_files)) {
  
  stem <- seltab_stems[i]
  match_idx <- which(audio_stems == stem)
  
  if (length(match_idx) == 0) {
    warning("No matching audio file for selection table: ", seltab_files[i])
    next
  }
  
  wav_path <- audio_files[match_idx[1]]
  sel_path <- seltab_files[i]
  
  cat("\nProcessing:", basename(wav_path), "with", basename(sel_path), "\n")
  
  sel <- read.delim(sel_path, sep = SEL_DELIM, stringsAsFactors = FALSE, check.names = FALSE)
  
  # Basic column sanity check
  needed_cols <- c("Begin Time (s)", "End Time (s)", CALL_COL)
  missing_cols <- setdiff(needed_cols, names(sel))
  if (length(missing_cols) > 0) {
    warning("Missing columns in ", basename(sel_path), ": ", paste(missing_cols, collapse = ", "))
    next
  }
  
  wav <- readWave(wav_path)
  sr  <- wav@samp.rate
  
  for (j in seq_len(nrow(sel))) {
    
    begin_s <- sel[["Begin Time (s)"]][j]
    end_s   <- sel[["End Time (s)"]][j]
    call    <- trimws(sel[[CALL_COL]][j])
    
    # Fix known typo: some selections were mislabeled VBA instead of VAB
    if (call == "VBA") call <- "VAB"
    
    if (is.na(begin_s) || is.na(end_s) || call == "") {
      warning("Skipping row ", j, " in ", basename(sel_path), " (missing time or call code)")
      next
    }
    
    out_folder <- file.path(OUTPUT_DIR, call)
    if (!dir.exists(out_folder)) {
      warning("Output folder for call type '", call, "' does not exist -- skipping row ", j)
      next
    }
    
    start_sample <- max(1, round(begin_s * sr))
    end_sample   <- min(length(wav@left), round(end_s * sr))
    
    clip <- extractWave(wav, from = start_sample, to = end_sample, xunit = "samples")
    
    out_name <- sprintf("%s_%s_sel%02d.wav", stem, call, j)
    out_path <- file.path(out_folder, out_name)
    
    writeWave(clip, out_path)
    cat("  Saved:", out_path, "\n")
  }
}

cat("\nClipping complete.\n")

# ==========================================================================
# Post-processing: peak-normalize every clip in every call-type folder
# ==========================================================================
if (NORMALIZE) {
  
  cat("\nNormalizing clips in all output folders...\n")
  
  call_folders <- list.dirs(OUTPUT_DIR, recursive = FALSE)
  
  for (folder in call_folders) {
    
    clip_files <- list.files(folder, pattern = "\\.wav$", full.names = TRUE, ignore.case = TRUE)
    
    if (length(clip_files) == 0) next
    
    cat("\n  Folder:", basename(folder), "(", length(clip_files), "clips )\n")
    
    for (clip_path in clip_files) {
      
      clip <- tryCatch(readWave(clip_path), error = function(e) NULL)
      
      if (is.null(clip)) {
        warning("Could not read clip, skipping: ", clip_path)
        next
      }
      
      norm_clip <- normalize(clip, unit = NORM_BITS)
      
      writeWave(norm_clip, clip_path)
      cat("    Normalized:", basename(clip_path), "\n")
    }
  }
  
  cat("\nNormalization complete.\n")
}

# ==========================================================================
# Post-processing: peak-normalize the full recordings you'll run through
# BirdNET for analysis (kept separate from training clips, and never
# overwrites your original field recordings)
# ==========================================================================
if (NORMALIZE_ANALYSIS_AUDIO) {
  
  cat("\nNormalizing full analysis recordings...\n")
  
  if (!dir.exists(ANALYSIS_AUDIO_OUTPUT_DIR)) {
    dir.create(ANALYSIS_AUDIO_OUTPUT_DIR, recursive = TRUE)
  }
  
  analysis_files <- list.files(ANALYSIS_AUDIO_DIR, pattern = "\\.wav$", full.names = TRUE, ignore.case = TRUE)
  
  if (length(analysis_files) == 0) {
    warning("No .wav files found in ANALYSIS_AUDIO_DIR: ", ANALYSIS_AUDIO_DIR)
  }
  
  for (rec_path in analysis_files) {
    
    rec <- tryCatch(readWave(rec_path), error = function(e) NULL)
    
    if (is.null(rec)) {
      warning("Could not read recording, skipping: ", rec_path)
      next
    }
    
    norm_rec <- normalize(rec, unit = NORM_BITS)
    
    out_path <- file.path(ANALYSIS_AUDIO_OUTPUT_DIR, basename(rec_path))
    writeWave(norm_rec, out_path)
    cat("  Normalized:", basename(rec_path), "->", out_path, "\n")
  }
  
  cat("\nAnalysis recording normalization complete. Originals in ", ANALYSIS_AUDIO_DIR,
      " were left untouched -- use the copies in ", ANALYSIS_AUDIO_OUTPUT_DIR, " for BirdNET.\n", sep = "")
}

cat("\nDone.\n")
