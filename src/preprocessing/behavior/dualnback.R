# Read and process data files for the Dual N-Back task
process_dualnback <- function(data_path, results_path) {
  
# List all relevant data files in the folder
file_paths <- list.files(path = data_path, pattern = "dualnbacktask_summary_P\\d+_.*\\.iqdat", full.names = TRUE, recursive = TRUE)

# Function to process each data file
process_file <- function(file) {
  
  # Read the data file
  sub <- read.delim(file, fileEncoding = "UTF-8")
  
  # Extract values of interest
  subject <- sub$subjectid[1]
  propCorrect_overall <- sub$propCorrect_overall[1]
  
  # Combine the summary statistics into a single row dataframe
  DN_sub <- data.frame(subject, propCorrect_overall)
  
  # Change the name of the participant column to match with other dataframes.
  names(DN_sub)[names(DN_sub) == 'subject'] <- 'Participant'
  DN_sub$Participant <- gsub("P", "", DN_sub$Participant)
  
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
  results_file <- file.path(participant_folder, paste('sub-', new_participant_id, '_dualnback_summary.csv', sep=''))
  write.csv(DN_sub, results_file, row.names = FALSE)
  
  
  return(DN_sub)
}


# Initialize an empty dataframe to store results for all participants
DN_all <- data.frame()

# Loop through each file, process it, and append the results to AS_all
for (file in file_paths) {
  DN_sub <- process_file(file)
  DN_all <- rbind(DN_all, DN_sub)
}

# Convert into numerical variable
DN_all$propCorrect_overall <- as.numeric(gsub(",", ".", DN_all$propCorrect_overall))

# Save results
parent_directory <- dirname(results_path)
new_results_path <- file.path(parent_directory, "results", "combined_data","behavior")
# Create directory if not exist
if (!dir.exists(new_results_path)) {
  dir.create(new_results_path, recursive = TRUE)
}

results_file <- file.path(new_results_path, "dualnback.csv")
write.csv(DN_all, results_file, row.names = FALSE)

# Print a completion message
message(paste(Sys.time(), ": DUAL N-BACK PROCESSING FINISHED. Results saved to: ", results_file))

}

