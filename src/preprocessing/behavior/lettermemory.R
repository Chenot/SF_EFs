# Read and process data files for the Letter Memory task
process_lettermemory <- function(data_path, results_path) {
  
# List all relevant data files in the folder
file_paths <- list.files(path = data_path, pattern = "lettermemorytask_summary_P\\d+_.*\\.iqdat", full.names = TRUE, recursive = TRUE)

# Function to process each data file
process_file <- function(file) {
  
  # Read the data file
  sub <- read.delim(file, fileEncoding = "UTF-8")
  
  # Extract values of interest
  subject <- sub$subjectid[1]
  propCorrect <- sub$propCorrect[1]
  
  # Combine the summary statistics into a single row dataframe
  LM_sub <- data.frame(subject, propCorrect)
  
  # Change the name of the participant column to match with other dataframes.
  names(LM_sub)[names(LM_sub) == 'subject'] <- 'Participant'
  LM_sub$Participant <- gsub("P", "", LM_sub$Participant)
  
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
  results_file <- file.path(participant_folder, paste('sub-', new_participant_id, '_lettermemory_summary.csv', sep=''))
  write.csv(LM_sub, results_file, row.names = FALSE)
  
  return(LM_sub)
}


# Initialize an empty dataframe to store results for all participants
LM_all <- data.frame()

# Loop through each file, process it, and append the results to AS_all
for (file in file_paths) {
  LM_sub <- process_file(file)
  LM_all <- rbind(LM_all, LM_sub)
}

# Convert into numerical variable
LM_all$propCorrect <- as.numeric(gsub(",", ".", LM_all$propCorrect))

# Save results
parent_directory <- dirname(results_path)
new_results_path <- file.path(parent_directory, "results", "combined_data","behavior")
# Create directory if not exist
if (!dir.exists(new_results_path)) {
  dir.create(new_results_path, recursive = TRUE)
}

results_file <- file.path(new_results_path, "lettermemory.csv")
write.csv(LM_all, results_file, row.names = FALSE)

# Print a completion message
message(paste(Sys.time(), ": LETTER-MEMORY PROCESSING FINISHED. Results saved to: ", results_file))

}


