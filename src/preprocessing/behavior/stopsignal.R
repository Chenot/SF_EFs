# Read and process data files for the Stop Signal task
process_stopsignal <- function(data_path, results_path) {
  
# List all relevant data files in the folder
file_paths <- list.files(path = data_path, pattern = "stopsignaltask2019_summary_P\\d+_.*\\.iqdat", full.names = TRUE, recursive = TRUE)

# Function to process each data file
process_file <- function(file) {
  
  # Read the data file
  sub <- read.delim(file, fileEncoding = "UTF-8")
  
  # Extract values of interest
  subject <- sub$subject[1]
  prob_stop <- sub$p_rs[1]
  ssd <- sub$ssd[1]
  stop_rt <- sub$sr_rt[1] 
  nonstop_rt <- sub$ns_rt[1] 
  ssrt_mean <- sub$ssrt_Mean[1] 
  ssrt_integration <- sub$SSRT_integration[1] 
  
  # Combine the summary statistics into a single row dataframe
  SS_sub <- data.frame(subject, prob_stop, ssd, stop_rt, nonstop_rt, ssrt_mean, ssrt_integration)
  
  # Change the name of the participant column to match with other dataframes.
  names(SS_sub)[names(SS_sub) == 'subject'] <- 'Participant'
  SS_sub$Participant <- gsub("P", "", SS_sub$Participant)
  
  # Extract participant ID from file name
  participant_id <- sub("^.*(P\\d+).*", "\\1", basename(file))
  
  # Replace "P" with empty string in participant id
  new_participant_id <- str_replace(participant_id, "P", '')
  
  # Define participant specific path with 'behavior' subfolder
  participant_folder = file.path(results_path, paste('sub-', new_participant_id, sep=''), 'behavior')
  
  # Create the participant specific directory if not exist
  if (!dir.exists(participant_folder)) {
    dir.create(participant_folder, recursive = TRUE)
  }
  
  # Save participant specific results
  results_file <- file.path(participant_folder, paste('sub-', new_participant_id, '_stopsignal_summary.csv', sep=''))
  write.csv(SS_sub, results_file, row.names = FALSE)
  
  return(SS_sub)
}


# Initialize an empty dataframe to store results for all participants
SS_all <- data.frame()

# Loop through each file, process it, and append the results to AS_all
for (file in file_paths) {
  SS_sub <- process_file(file)
  SS_all <- rbind(SS_all, SS_sub)
}

# Convert columns to numeric
SS_all <- SS_all %>%
  mutate(across(-Participant, ~ as.numeric(gsub(",", ".", .))))

# Save results
parent_directory <- dirname(results_path)
new_results_path <- file.path(parent_directory, "results", "combined_data","behavior")
# Create directory if not exist
if (!dir.exists(new_results_path)) {
  dir.create(new_results_path, recursive = TRUE)
}

results_file <- file.path(new_results_path, "stopsignal.csv")
write.csv(SS_all, results_file, row.names = FALSE)

# Print a completion message
message(paste(Sys.time(), ": STOP SIGNAL PROCESSING FINISHED. Results saved to: ", results_file))

}




