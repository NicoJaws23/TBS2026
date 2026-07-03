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
AUDIO_DIR   <- "C:\\Users\\Jawor\\Desktop\\TBS_2026\\VXX_Recordings"          # folder with .wav files
SELTAB_DIR  <- "C:\\Users\\Jawor\\Desktop\\TBS_2026\\SelectionTables"    # folder with Raven selection tables
OUTPUT_DIR  <- "C:\\Users\\Jawor\\Desktop\\Research\\BirdNet\\TrainingData"              # parent folder containing VAB/, VCH/, VTR/ etc.
CALL_COL    <- "Annotation"                   # name of your 3-letter code column
SEL_DELIM   <- "\t"                          # "\t" for Raven's default tab-delimited .txt export, "," if you ever use CSV
NORMALIZE   <- TRUE                          # TRUE = peak-normalize every clip after cutting
NORM_BITS   <- "16"                          # target bit depth for normalization ("16", "24", "32", or "1" for float [-1,1])
# ---------------------------------------------------------------------------

# Match each selection table to its recording by shared filename stem
audio_files  <- list.files(AUDIO_DIR, pattern = "\\.wav$", full.names = TRUE, ignore.case = TRUE)
seltab_files <- list.files(SELTAB_DIR, pattern = "\\.(csv|txt)$", full.names = TRUE, ignore.case = TRUE)

get_stem <- function(path) {
  # Grabs the recording name portion before Raven's ".Table.1.selections" suffix,
  # adjust the regex if your naming convention differs
  base <- tools::file_path_sans_ext(basename(path))
  sub("\\.Table\\..*$", "", base)
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

cat("\nDone.\n")
