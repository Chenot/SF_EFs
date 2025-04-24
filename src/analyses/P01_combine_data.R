## P01_combine_data.R
# Author: Quentin Chenot
# Date: 2023-06-26
# Description: This script combine the results of all executive functions tasks and demographics data into one dataframe.
# Dependencies: dplyr
# Inputs : processed behavioral data from the executive functions tasks stored in a '/results/combined_data' directory
# Outputs : one dataframe saved in a 'results/combined_data' directory


## LOAD LIBRARIES
# Function to check if each required package is installed, and install it if not
required_packages <- c("dplyr") # List of packages required for this script
install_if_not_present <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
}
lapply(required_packages, install_if_not_present) # Apply the function to each required package

# Load the libraries
library(dplyr) 


## PATH MANAGEMENT
# Get the directory and path to this file
this_file <- rstudioapi::getSourceEditorContext()$path  # if using RStudio
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir)) # Get the project directory
data_path <- file.path(project_dir, "results" , "combined_data","behavior") # Define the relative path to the data and results

# Load data from demographics and executive functions tasks
df_demographics <- read.csv(file.path(project_dir,"data", "participants_demographics.csv"))
df_antisaccade <- read.csv(file.path(data_path, "antisaccade.csv"))
df_categoryswitch <- read.csv(file.path(data_path, "categoryswitch.csv"))
df_colorshape <- read.csv(file.path(data_path, "colorshape.csv"))
df_dualnback <- read.csv(file.path(data_path, "dualnback.csv"))
df_keeptrack <- read.csv(file.path(data_path, "KeepTrack.csv"))
df_lettermemory <- read.csv(file.path(data_path, "lettermemory.csv"))
df_stopsignal <- read.csv(file.path(data_path, "StopSignal.csv"))
df_stroop <- read.csv(file.path(data_path, "stroop.csv"))
df_numberletter <- read.csv(file.path(data_path, "numberletter.csv"))
df_SF <- read.csv(file.path(data_path, "SF_summary.csv"))



# Select Dependent variable
df_demographics <- select(df_demographics, -Comment)
df_antisaccade <- select(df_antisaccade, -score_med_corrected, -score_med_corrected, -score_mean_raw, -score_med_raw, -percentage_resp_correct, -percentage_excluded_data)
df_categoryswitch <- select(df_categoryswitch, -mean_latency_switch, -mean_latency_noswitch, -percentage_resp_correct, -percentage_excluded_data)
df_colorshape <- select(df_colorshape, -mean_latency_switch, -mean_latency_noswitch, -percentage_resp_correct, -percentage_excluded_data)
df_numberletter <- select(df_numberletter, -mean_latency_switch, -mean_latency_noswitch, -percentage_resp_correct, -percentage_excluded_data)
df_stopsignal <- select(df_stopsignal, -prob_stop, -ssd, -stop_rt, -nonstop_rt, -ssrt_mean)
df_stroop <- select(df_stroop, -mean_latency_congruent, -mean_latency_noncongruent, -mean_latency_control, -percentage_resp_correct, -percentage_excluded_data, -block)


# Select Dependent variable (SF games)
df_SF_all <- df_SF[order(df_SF$Participant, df_SF$Date), ] # Add game number column based on order within each Participant and Difficulty
df_SF_all$GameNum <- ave(df_SF_all$Participant, 
                     list(df_SF_all$Participant, df_SF_all$Difficulty), 
                     FUN = seq_along)
df_SF_all$GameID <- paste0("SF_",  # Create new column names based on condition and game number
                       ifelse(df_SF_all$Difficulty == "monotask", "mono_", "multi_"),
                       sprintf("%02d", df_SF_all$GameNum))
df_SF_wide <- reshape(df_SF_all[, c("Participant", "GameID", "TotalScore")],
                  idvar = "Participant",
                  timevar = "GameID",
                  direction = "wide")
names(df_SF_wide) <- gsub("TotalScore.", "", names(df_SF_wide)) # Rename columns to remove "TotalScore." prefix
df_SF_wide <- select(df_SF_wide, -SF_multi_06) # Remove potential game 6
df_SF_wide <- as.data.frame(df_SF_wide[, names(df_SF_wide)])

df_SF <- df_SF[df_SF$Difficulty == "multitask", ] # Select best SF score for multitask games
df_SF <- aggregate(TotalScore ~ Participant, data = df_SF, FUN = max)
df_demographics$Participant <- gsub("P", "", df_demographics$Participant)
df_demographics$Participant <- as.numeric(df_demographics$Participant)


# Rename the columns
names(df_antisaccade)[names(df_antisaccade) == 'score_mean_corrected'] <- 'antisaccade'
names(df_categoryswitch)[names(df_categoryswitch) == 'switchCost'] <- 'categoryswitch'
names(df_colorshape)[names(df_colorshape) == 'switchCost'] <- 'colorshape'
names(df_dualnback)[names(df_dualnback) == 'propCorrect_overall'] <- 'dualnback'
names(df_keeptrack)[names(df_keeptrack) == 'corrected_score'] <- 'keeptrack'
names(df_lettermemory)[names(df_lettermemory) == 'propCorrect'] <- 'lettermemory'
names(df_stopsignal)[names(df_stopsignal) == 'ssrt_integration'] <- 'stopsignal'
names(df_stroop)[names(df_stroop) == 'InhibitionCost'] <- 'stroop'
names(df_numberletter)[names(df_numberletter) == 'switchCost'] <- 'numberletter'
names(df_SF)[names(df_SF) == 'TotalScore'] <- 'SF'

# Merge all dataframes into one dataframe, df_final
df_list <- list(df_demographics, df_antisaccade, df_categoryswitch, df_colorshape, df_dualnback, df_keeptrack, df_lettermemory, df_stopsignal, df_stroop, df_numberletter, df_SF, df_SF_wide)
df_final <- Reduce(function(x, y) merge(x, y, by = "Participant", all = TRUE), df_list)
df_final <- df_final[complete.cases(df_final), ]

# Save the final dataframe into a CSV file
results_file <- file.path(project_dir, "results", "combined_data", "data.csv")
write.csv(df_final, results_file, row.names = FALSE)


