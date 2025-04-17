#' Clean a Space Fortress raw file and output a cleaned file with selected columns.
#'
#' @param input_file Path to the raw Space Fortress file (txt, tab-separated).
#' @param output_file Path to write the cleaned file.
#' @return Invisibly returns the cleaned data.frame.
#'
#' The output file will have columns: system_time, e1, e2, e3, Point, TotalScore

clean_sf_file <- function(input_file, output_file) {
  # Read file
  df <- read.table(input_file, header=TRUE, sep="\t", dec=".", fill=TRUE, stringsAsFactors=FALSE)
  # Remove empty rows and keep only relevant events
  df <- subset(df, (e3 != "user" & e1 != "release" & system_time != "" & system_time != "n" & system_time != "y"),
               select = c("system_time", "e1", "e2", "e3"))
  df <- df[complete.cases(df), ]
  # Convert system_time to numeric and round
  df$system_time <- round(as.numeric(as.character(df$system_time)), digits=3)
  # Trim to game start/end
  row_begin <- which(df$e1 == "game" & df$e2 == "start")
  row_end <- which(df$e1 == "game" & df$e2 == "over")
  if (length(row_begin) > 0 && length(row_end) > 0) {
    df <- df[c(row_begin:row_end), ]
  }
  # Remove pause periods
  if (any(df$e2 == "pause")) {
    row_pause <- which(df$e2 == "pause")
    row_unpause <- which(df$e2 == "unpause")
    if (length(row_pause) > 0 && length(row_unpause) > 0) {
      time_pause <- df$system_time[row_pause]
      time_unpause <- df$system_time[row_unpause]
      pause_duration <- time_unpause - time_pause + 1
      df <- df[-c(row_pause:row_unpause), ]
      df[df$system_time >= time_unpause, "system_time"] <- df[df$system_time >= time_unpause, "system_time"] - pause_duration
    }
  }
  # Compute Point column
  df$Point <- 0
  # Ship collision
  df$Point[df$e1 == "collide" & (df$e2 == "shell" | df$e2 == "mine_0")] <- -50
  # Ship destruction
  df$Point[df$e1 == "destroyed" & df$e2 == "ship"] <- -100
  # Border crossing
  df$Point[df$e1 == "warp"] <- -35
  # Fortress collision
  df$Point[df$e1 == "collide" & df$e2 == "small_hex" & df$e3 == "ship"] <- -35
  # Fortress destruction
  df$Point[df$e1 == "destroyed" & df$e2 == "fortress"] <- 250
  # Friends Mines destruction
  df$Point[df$e1 == "collide" & df$e2 == "friend_mine"] <- 50
  # Foes Mines destruction
  df$Point[df$e1 == "collide" & df$e2 == "tagged_foe_mine"] <- 60
  # Mine disappears
  df$Point[df$e1 == "timeout" & df$e2 == "mine"] <- -50
  # Points Bonus capture
  df$Point[df$e1 == "pnts_bonus_capture"] <- 100
  # Shots Bonus capture
  df$Point[df$e1 == "shots_bonus_capture"] <- 50
  # Bonus failure
  df$Point[df$e1 == "pnts_bonus_failure" | df$e1 == "shots_bonus_failure"] <- -50
  # New Mine
  df$Point[df$e1 == "new_mine"] <- 0
  # New Bonus
  df$Point[df$e1 == "bonus_available"] <- 0
  # Missile collide Fortress
  df$Point[df$e1 == "collide" & df$e3 == "fortress"] <- 0
  # TotalScore: cumulative sum
  df$TotalScore <- cumsum(df$Point)
  # Select and reorder columns
  df <- df[, c("system_time", "e1", "e2", "e3", "Point", "TotalScore")]
  # Write cleaned file
  write.table(df, file=output_file, sep="\t", dec=".", row.names=FALSE, col.names=TRUE, quote=FALSE)
  invisible(df)
}

# Test with your specific files
clean_sf_file(
  "E:/OneDrive - ISAE-SUPAERO/3_Post-doc_ISAE/Articles/AppliedCognitivePsychology/github/data/sub-P001/ses-1/behavior/SpaceFortress-5.1.0_beta1_P001_2022-2-21_11-13-17.txt",
  "E:/OneDrive - ISAE-SUPAERO/3_Post-doc_ISAE/Articles/AppliedCognitivePsychology/github/tmp_data/sub-P001/ses-1/behavior/P001_SF_2022-2-21_11-13-17.txt"
)
