#### MAIN FILE FOR SPACE FORTRESS DATA EXTRACTION AND BASIC VISUALITION


#librairies
library("stringr")
library("foreach")
library("bestNormalize")
library("SciViews")
library("ggplot2")
library("ggpubr")
library("dplyr")
library("broom")

# Get script location - more robust method
script_path <- tryCatch({
    # First try rstudioapi as it's more reliable
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        dirname(rstudioapi::getSourceEditorContext()$path)
    } else {
        # Try getting the path from sys.frame
        path <- dirname(sys.frame(1)$ofile)
        if (is.null(path)) stop("Not running from file")
        path
    }
}, error = function(e) {
    # If all else fails, use the current working directory
    getwd()
})

if (!requireNamespace("rstudioapi", quietly = TRUE)) {
    warning("rstudioapi package not available - falling back to working directory")
}

# Standardize path separators
root_dir <- normalizePath(file.path(script_path, "..", "..", ".."))
data_dir <- normalizePath(file.path(root_dir, "data"))  # Remove extra "tests" folder
tmp_dir <- normalizePath(file.path(root_dir, "tmp_data"))
results_dir <- normalizePath(file.path(root_dir, "results", "combined_data", "behavior"))

##CLEANING SF TXT FILES & POINT COMPUTATION
date <- function(file_name){
  return(as.Date(substr(file_name, 34, 43), format="%Y-%m-%d"))
}

#' Date Hour
#'
#' @param file_name the file from which date and hour are extracted
#' @param clean false if the data are raw and true if cleaned
#'
#' @return the date and hour when the game session was played
#' @export
#'
#' @examples date_hour("SpaceFortress_AC2302_D02P2.txt" TRUE)
date_hour <- function(file_name, clean=FALSE) {
  if(clean==TRUE) {
    parsed_date = as.POSIXct(str_sub(file_name,22,-8), format="%Y-%m-%d_%H-%M-%S")
    print(paste("Clean file date parsed:", format(parsed_date, "%Y-%m-%d_%H-%M-%S")))
    return(parsed_date)
  } else {
    # Extract the full date pattern including full year
    date_pattern <- "\\d{4}-\\d{1,2}-\\d{1,2}_\\d{1,2}-\\d{1,2}-\\d{1,2}"
    date_str <- str_extract(file_name, date_pattern)
    parsed_date = as.POSIXct(date_str, format="%Y-%m-%d_%H-%M-%S")
    return(parsed_date)
  }
}

ID<- function(file_name,clean=FALSE){
  if(clean==TRUE){
    return(toupper(substr(file_name, 15,18)))
  }else{
    return(toupper(substr(file_name, 27, 30)))
  }
}

clean_file <- function(file_to_clean){ 
  #remove all empty rows
  clean_file=subset(file_to_clean,(e3!="user"&e1!="release"&system_time!=""&system_time!="n"&system_time!="y"),select=c("system_time","e1","e2","e3"))
  #clean_file=subset(file_to_clean,file_to_clean$e1=="display_game"|file_to_clean$e1=="game"|file_to_clean$e1=="press"|file_to_clean$e1=="collide"|file_to_clean$e1=="destroyed"|file_to_clean$e1=="warp"|file_to_clean$e1=="score+"|file_to_clean$e1=="timeout"|file_to_clean$e1=="pnts_bonus_capture"|file_to_clean$e1=="pnts_bonus_failure"|file_to_clean$e1=="shots_bonus_capture"|file_to_clean$e1=="shots_bonus_failure",select=c("system_time","e1","e2","e3"))
  clean_file=clean_file[complete.cases(clean_file),]
  #time trunc
  #clean_file$system_time=trunc(as.numeric(as.character(clean_file$system_time)))
  clean_file$system_time=round(as.numeric(as.character(clean_file$system_time)),digits=3)
  #file begins at game start and ends game over
  row_begin=which(clean_file$e1 == "game"& clean_file$e2=="start", arr.ind=TRUE)
  row_end=which(clean_file$e1 == "game"& clean_file$e2=="over", arr.ind=TRUE)
  clean_file=clean_file[c(row_begin:row_end),]
  #PAUSE DELETION
  if(any(clean_file$e2=="pause")){
    row_pause=which(clean_file$e2=="pause", arr.ind=TRUE)
    row_unpause=which(clean_file$e2=="unpause", arr.ind=TRUE)
    time_pause=clean_file$system_time[row_pause]
    time_unpause=clean_file$system_time[row_unpause]
    pause_duration=time_unpause-time_pause+1
    clean_file=subset(clean_file[-c(row_pause:row_unpause),])
    clean_file[(clean_file$system_time)>=time_unpause,"system_time"]=clean_file[(clean_file$system_time)>=time_unpause,"system_time"]-pause_duration
  }
  return(clean_file)
}

#write files containing the clean dataframe of each session of each participant 
write_file <- function(file, data_dir, tmp_dir) {
    if (!file.exists(file.path(data_dir, file))) {
        stop("Input file does not exist: ", file)
    }
    if (!dir.exists(tmp_dir)) {
        stop("Temporary directory does not exist: ", tmp_dir)
    }
    # Extract subject ID and session from file path
    parts = unlist(strsplit(file, "/"))
    sub_idx = which(grepl("sub-", parts))
    ses_idx = which(grepl("ses-", parts))
    
    sub_id = parts[sub_idx]
    ses_id = parts[ses_idx]
    
    # Create output directory
    out_dir = file.path(tmp_dir, sub_id, ses_id, "behavior")
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Construct full input path properly
    full_input_path = file.path(data_dir, file)
    
    # Read and process file
    file_to_write = read.table(full_input_path, header=TRUE, sep="\t", dec=".", fill=TRUE)
    id = ID(basename(file))
    dat = date_hour(basename(file))
    datetime_str = format(dat, format="%Y-%m-%d_%H-%M-%S")
    
    file_to_save = clean_file(file_to_write)
    file_to_save = compute_points(file_to_save)
    
    # Write to new location
    write.csv(file_to_save, 
          file.path(out_dir, paste0(id, "_SF_", datetime_str, ".csv")),
          row.names = TRUE)
    return(id)
}

#compute points of one session of one participant and return a dataframe (or the points only if point_only=TRUE)
compute_points <- function(file_to_read, point_only=FALSE, press=FALSE, prct=TRUE) {
    e1=file_to_read$e1
    dp=subset(file_to_read,e1=="press"|e1=="new_mine"|e1=="bonus_available"|e1=="collide"|e1=="destroyed"|e1=="warp"|e1=="score+"|e1=="timeout"|e1=="pnts_bonus_capture"|e1=="pnts_bonus_failure"|e1=="shots_bonus_capture"|e1=="shots_bonus_failure",select=c("system_time","e1","e2","e3"))
    dp$Type=NA
    dp$Point=0
    dp$Group=NA
    #Ship collision
    dp$Type[dp$e1=="collide"& (dp$e2=="shell"|dp$e2=="mine_0")]="ShipDamage"
    dp$Point[dp$e1=="collide"& (dp$e2=="shell"|dp$e2=="mine_0")]=-50
    #Ship destruction
    dp$Type[dp$e1=="destroyed"& dp$e2=="ship"]="ShipDestruction"
    dp$Point[dp$e1=="destroyed"& dp$e2=="ship"]=-100
    #Border crossing
    dp$Type[dp$e1=="warp"]="BorderCrossing"
    dp$Point[dp$e1=="warp"]=-35
    #Fortress collision
    dp$Type[dp$e1=="collide"& dp$e2=="small_hex" & dp$e3=="ship"]="FortressCollision"
    dp$Point[dp$e1=="collide"& dp$e2=="small_hex" & dp$e3=="ship"]=-35
    #Fortress destruction
    dp$Type[dp$e1=="destroyed"&dp$e2=="fortress"]="FortressDestruction"
    dp$Point[dp$e1=="destroyed"&dp$e2=="fortress"]=250
    # #Friends Mines destruction (false version)
    # dp$Type[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="50"]="FriendMineDestruction"
    # dp$Point[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="50"]=60
    # #Foes Mines destruction
    # dp$Type[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="60"]="FoeMineDestruction"
    # dp$Point[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="60"]=50 #INVERSION?
    # #Mine disappears
    # dp$Type[dp$e1=="timeout"&dp$e2=="mine"]="MineExtinction"
    # dp$Point[dp$e1=="timeout"&dp$e2=="mine"]=-50
    #Friends Mines destruction
    dp$Type[dp$e1=="collide"&dp$e2=="friend_mine"]="FriendMineDestruction"
    dp$Point[dp$e1=="collide"&dp$e2=="friend_mine"]=50
    #Foes Mines destruction
    dp$Type[dp$e1=="collide"&dp$e2=="tagged_foe_mine"]="FoeMineDestruction"
    dp$Point[dp$e1=="collide"&dp$e2=="tagged_foe_mine"]=60 
    #Mine disappears
    dp$Type[dp$e1=="timeout"&dp$e2=="mine"]="MineExtinction"
    dp$Point[dp$e1=="timeout"&dp$e2=="mine"]=-50
    # #Collision with a mine
    # dp$Type[dp$e1=="collide"& (dp$e2=="shell"|dp$e2=="mine_0")]="MineCollision"
    # dp$Point
    #Points Bonus capture
    dp$Type[dp$e1=="pnts_bonus_capture"]="PointsBonusCapture"
    dp$Point[dp$e1=="pnts_bonus_capture"]=100
    #Shots Bonus capture
    dp$Type[dp$e1=="shots_bonus_capture"]="ShotsBonusCapture"
    dp$Point[dp$e1=="shots_bonus_capture"]=50
    #Bonus failure
    dp$Type[dp$e1=="pnts_bonus_failure"|dp$e1=="shots_bonus_failure"]="BonusFailure"
    dp$Point[dp$e1=="pnts_bonus_failure"|dp$e1=="shots_bonus_failure"]=-50
    #New Mine
    dp$Type[dp$e1=="new_mine"]="NewMine"
    dp$Point[dp$e1=="new_mine"]=0
    #New Bonus
    dp$Type[dp$e1=="bonus_available"]="NewBonus"
    dp$Point[dp$e1=="bonus_available"]=0
    #Missile collide Fortress
    dp$Type[dp$e1=="collide"&dp$e3=="fortress"]="FortressShot"
    dp$Point[dp$e1=="collide"&dp$e3=="fortress"]=0
    #Group of Scores
    dp$Group[dp$Type=="ShipDamage"|dp$Type=="ShipDestruction"|dp$Type=="BorderCrossing"|dp$Type=="FortressCollision"]="Flight"
    dp$Group[dp$Type=="FortressDestruction"]="Fortress"  # Remove FortressShot from here
    dp$Group[dp$Type=="FriendMineDestruction"|dp$Type=="FoeMineDestruction"|dp$Type=="MineExtinction"]="Mine"
    dp$Group[dp$Type=="PointsBonusCapture"|dp$Type=="ShotsBonusCapture"|dp$Type=="BonusFailure"]="Bonus"
    dp$Group[dp$Type=="FortressShot"]="Fortress"  # Give FortressShot its own assignment
    dp$Group[dp$e1=="press"]="Press"
    dp$Type[dp$e1=="press"]="Press"
    
    # Only assign GameEvent group to new_mine and bonus_available
    dp$Group[dp$e1=="new_mine"|dp$e1=="bonus_available"]="GameEvent"
    
    # First filter out NA Types
    dp = subset(dp, !is.na(Type), select=c("system_time","e1","e2","e3","Type","Point","Group"))
    
    # Then calculate TotalScore after all points and groups are properly assigned
    dp$TotalScore = cumsum(dp$Point)
    
    if(point_only==TRUE&press==FALSE){
        return(subset(dp,e1!="press",select=c("Group","Point","TotalScore")))
    }else if(prct==TRUE){
        return(dp)  # Return without adding Difficulty column
    }else{
        return(subset(dp,e1!="new_mine"|e1!="bonus_available"))
    }
}

read_data_score <- function(files_data, tmp_dir, add_press=FALSE) {
  df_data = foreach(i=1:length(files_data), .combine=rbind) %do% {
    file_d = read.csv(files_data[i], header=TRUE, stringsAsFactors=FALSE)
    if(!add_press) {
      file_d = subset(file_d, Type!="Press")
    }
    c(list(file_d$system_time),
      list(file_d$Point),
      list(file_d$Point[file_d$Group=="Flight"]),
      list(file_d$Point[file_d$Group=="Bonus"]),
      list(file_d$Point[file_d$Group=="Mine"]),
      list(file_d$Point[file_d$Group=="Fortress"]),
      list(as.numeric(cumsum(file_d$Point))))
  }
  colnames(df_data) = c("System_time", "Point", "FlightPoint", "BonusPoint", 
                        "MinePoint", "FortressPoint", "CumulScore")
  rownames(df_data) = 1:nrow(df_data)
  return(as.data.frame(df_data))
}

#' Read and summarize final scores from Space Fortress data files
#'
#' @param files_data Vector of file paths to process
#' @param tmp_dir Temporary directory path
#' @param results_dir Results directory path
#' @return data.frame containing summarized scores
#' @export
read_final_Score <- function(files_data, tmp_dir, results_dir) {
    # Input validation
    if (length(files_data) == 0) {
        stop("No files provided to process")
    }
    
    # Process each file and combine results
    df_data <- foreach(file_path = files_data, .combine = rbind) %do% {
        tryCatch({
            # Read file
            file <- read.csv(file_path, 
                            header = TRUE,
                            stringsAsFactors = FALSE)
            
            # Basic validation
            if (nrow(file) == 0) {
                warning(sprintf("Empty file: %s", basename(file_path)))
                return(NULL)
            }
            
            # Extract date from filename
            date_str <- str_extract(basename(file_path), 
                                  "\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}")
            
            # Count events
            num_bonus <- sum(file$Type == "NewBonus", na.rm = TRUE)
            num_mine <- sum(file$Type == "NewMine", na.rm = TRUE)
            
            # Calculate success percentages
            bonus_captures <- sum(file$Type %in% c("ShotsBonusCapture", "PointsBonusCapture"), na.rm = TRUE)
            mine_destructions <- sum(file$Type %in% c("FriendMineDestruction", "FoeMineDestruction"), na.rm = TRUE)
            
            prct_bonus <- ifelse(num_bonus > 0, (bonus_captures * 100) / num_bonus, NA)
            prct_mine <- ifelse(num_mine > 0, (mine_destructions * 100) / num_mine, NA)
            
            # Determine game type
            difficulty <- ifelse(num_bonus == 0 && num_mine == 0, "monotask", "multitask")
            
            # Count actions
            num_shots <- sum(file$e2 == "fire", na.rm = TRUE)
            num_actions <- sum(file$e1 == "press", na.rm = TRUE)
            
            # Create summary row
            data.frame(
                Participant = gsub("^P", "", substr(basename(file_path), 1, 4)),
                Date = date_str,
                Difficulty = difficulty,
                TotalScore = sum(file$Point, na.rm = TRUE),
                Flight = sum(file$Point[file$Group == "Flight"], na.rm = TRUE),
                Bonus = sum(file$Point[file$Group == "Bonus"], na.rm = TRUE),
                Mine = sum(file$Point[file$Group == "Mine"], na.rm = TRUE),
                Fortress = sum(file$Point[file$Group == "Fortress"], na.rm = TRUE),
                NumberofBonus = num_bonus,
                NumberofMine = num_mine,
                Bonus_Prct = prct_bonus,
                Mine_Prct = prct_mine,
                FortressShot = sum(file$Type == "FortressShot", na.rm = TRUE),
                NumberOfShots = num_shots,
                NumberOfActions = num_actions,
                stringsAsFactors = FALSE
            )
        }, error = function(e) {
            warning(sprintf("Error processing file %s: %s", basename(file_path), e$message))
            return(NULL)
        })
    }
    
    # Check if any data was processed successfully
    if (is.null(df_data) || nrow(df_data) == 0) {
        stop("No data could be processed from any of the input files")
    }
    
    # Write summary to file
    output_file <- file.path(results_dir, "SF_summary.csv")
    tryCatch({
        write.csv(df_data,
                file = output_file,
                row.names = FALSE)
    }, error = function(e) {
        warning(sprintf("Could not write summary file: %s", e$message))
    })
    
    return(df_data)
}

write_summary_file <- function(file, tmp_dir) {
    # Extract filename from full path
    filename = basename(file)
    
    # Get directory path from full file path
    file_dir = dirname(file)
    
    # Read the clean file directly using the full path
    clean_data = read.csv(file, header=TRUE, stringsAsFactors=FALSE)
    
    # Calculate summary statistics
    prct_bonus = (sum(clean_data$Type=="ShotsBonusCapture"|clean_data$Type=="PointsBonusCapture")*100)/sum(clean_data$Type=="NewBonus")
    prct_mine = (sum(clean_data$Type=="FriendMineDestruction"|clean_data$Type=="FoeMineDestruction")*100)/sum(clean_data$Type=="NewMine")
    
    # Calculate number of shots and actions
    num_shots = sum(clean_data$e2=="fire")
    num_actions = sum(clean_data$e1=="press")
    
    # Create summary dataframe
    summary_data = data.frame(
        TotalScore = sum(clean_data$Point),
        Flight = sum(clean_data$Point[clean_data$Group=="Flight"]),
        Bonus = sum(clean_data$Point[clean_data$Group=="Bonus"]),
        Mine = sum(clean_data$Point[clean_data$Group=="Mine"]),
        Fortress = sum(clean_data$Point[clean_data$Group=="Fortress"]),
        NumberofBonus = sum(clean_data$Type=="NewBonus"),
        NumberofMine = sum(clean_data$Type=="NewMine"),
        Bonus_Prct = prct_bonus,
        Mine_Prct = prct_mine,
        FortressShot = sum(clean_data$Type=="FortressShot"),
        NumberOfShots = num_shots,
        NumberOfActions = num_actions
    )
    
    # Create summary filename
    summary_filename = sub("_SF_", "_SF_summary_", filename)
    summary_filename = sub("\\.txt$", ".csv", summary_filename)
    write.csv(summary_data, 
            file.path(file_dir, summary_filename),
            row.names = FALSE)
}

#' Process Space Fortress Data
#' 
#' @description 
#' Main function to process Space Fortress data files:
#' 1. Creates necessary directories
#' 2. Processes raw files
#' 3. Generates individual summaries
#' 4. Creates group summary
#'
#' @param data_dir Path to raw data directory
#' @param tmp_dir Path for temporary processed files
#' @param results_dir Path for final results
#' @param bar_width Width of progress bar (default: 50)
#' @return data.frame containing the group summary
#' @export
process_SF <- function(data_dir = NULL, tmp_dir = NULL, results_dir = NULL, bar_width = 50) {
    # Get script location if paths not provided
    if (is.null(data_dir) || is.null(tmp_dir) || is.null(results_dir)) {
        script_path <- tryCatch({
            if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
                dirname(rstudioapi::getSourceEditorContext()$path)
            } else {
                path <- dirname(sys.frame(1)$ofile)
                if (is.null(path)) stop("Not running from file")
                path
            }
        }, error = function(e) {
            getwd()
        })
        
        # Set default paths relative to script location
        root_dir <- normalizePath(file.path(script_path, "..", "..", ".."))
        if (is.null(data_dir)) data_dir <- normalizePath(file.path(root_dir, "data"))
        if (is.null(tmp_dir)) tmp_dir <- normalizePath(file.path(root_dir, "tmp_data"))
        if (is.null(results_dir)) results_dir <- normalizePath(file.path(root_dir, "results", "combined_data", "behavior"))
    }
    
    # Create directories if needed
    for (dir in c(data_dir, tmp_dir, results_dir)) {
        if (!dir.exists(dir)) {
            dir.create(dir, recursive = TRUE)
            message(sprintf("Created directory: %s", dir))
        }
    }
    
    # Find all SF files
    files <- list.files(path = data_dir, 
                       pattern = "^SpaceFortress-5.1.0.*\\.txt$",
                       recursive = TRUE,
                       full.names = FALSE)
    
    if (length(files) == 0) {
        stop("No Space Fortress files found in ", data_dir)
    }
    
    # Process raw files
    cat("Processing raw files...\n")
    total_files <- length(files)
    
    for(i in seq_along(files)) {
        tryCatch({
            write_file(files[i], data_dir=data_dir, tmp_dir=tmp_dir)
        }, error = function(e) {
            cat(sprintf("Error processing file: %s\nError message: %s\n", files[i], e$message))
            # Optionally, add file to a list of failed files
        })
        
        # Show progress
        progress <- round((i/total_files) * 100)
        filled <- round((i/total_files) * bar_width)
        empty <- bar_width - filled
        cat(sprintf("\rProcessing files: %d/%d [%s%s] %d%%", 
                    i, total_files,
                    paste(rep("=", filled), collapse=""),
                    paste(rep(" ", empty), collapse=""),
                    progress))
        flush.console()
    }
    
    cat("\nPreprocessing complete!\n\n")
    
    # Get cleaned files
    cleaned_files <- list.files(path = tmp_dir,
                               pattern = "^P\\d{3}_SF_\\d{4}.*\\.csv$",
                               recursive = TRUE,
                               full.names = TRUE)
    
    if (length(cleaned_files) == 0) {
        stop("No cleaned files generated in ", tmp_dir)
    }
    
    # Generate individual summaries
    cat("Generating individual summaries...\n")
    total_summaries <- length(cleaned_files)
    
    for(i in seq_along(cleaned_files)) {
        tryCatch({
            write_summary_file(cleaned_files[i], tmp_dir=tmp_dir)
        }, error = function(e) {
            cat(sprintf("Error processing file: %s\nError message: %s\n", cleaned_files[i], e$message))
            # Optionally, add file to a list of failed files
        })
        
        progress <- round((i/total_summaries) * 100)
        filled <- round((i/total_summaries) * bar_width)
        empty <- bar_width - filled
        cat(sprintf("\rGenerating summaries: %d/%d [%s%s] %d%%", 
                    i, total_summaries,
                    paste(rep("=", filled), collapse=""),
                    paste(rep(" ", empty), collapse=""),
                    progress))
        flush.console()
    }
    
    cat("\nIndividual summaries complete!\n\n")
    
    # Generate group summary
    cat("Generating group summary...\n")
    group_summary <- read_final_Score(cleaned_files, tmp_dir=tmp_dir, results_dir=results_dir)
    cat("Group summary complete!\n")
    
    return(group_summary)
}