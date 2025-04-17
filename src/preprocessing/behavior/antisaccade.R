# Read and process data files for the Antisaccade task
process_antisaccade <- function(data_path, results_path) {
  
# Create a list of all relevant data files in the directory.
file_paths <- list.files(path = data_path, pattern = "antisaccadetask_keyboard_raw_P\\d+_.*\\.iqdat", full.names = TRUE, recursive = TRUE)

# Define a function to process each individual data file.
process_file <- function(file) {
  # Read the data file.
  sub <- read.delim(file, fileEncoding = "UTF-8")
  
  # Filter the data to only include the test block.
  AS_test <- sub %>% filter(blockcode == "Antisaccade")
  
  # Further filter the data to only include correct responses.
  AS <- AS_test %>% filter(correct == 1)
  
  # Compute the median and MAD (Median Absolute Deviation) of the latency.
  med <- median(AS$latency)
  mad <- 3.32 * mad(AS$latency)
  medmad_pos <- med + mad
  min <- 200
  
  # Filter the latency based on computed median and MAD values.
  latency_corrected <- AS %>% filter(latency <= medmad_pos) %>% filter(latency >= min)
  
  # Compute subject-level summary statistics.
  subject <- AS$subject[1]
  score_mean_corrected <- mean(latency_corrected$latency)
  score_med_corrected <- median(latency_corrected$latency)
  score_mean_raw <- mean(AS$latency)
  score_med_raw <- median(AS$latency)
  percentage_resp_correct <- nrow(AS) / nrow(AS_test)
  percentage_excluded_data <- (1 - nrow(latency_corrected) / nrow(AS))*100
  
  # Combine the summary statistics into a single row data frame.
  AS_sub <- data.frame(subject, score_mean_corrected, score_med_corrected, score_mean_raw, score_med_raw, percentage_resp_correct, percentage_excluded_data)
  
  # Extract participant ID from file name
  participant_id <- sub("^.*(P\\d+).*", "\\1", basename(file))
  
  # Replace "P" with empty string in participant id
  new_participant_id <- gsub("P", "", participant_id)
  
  # Define participant specific path
  participant_folder = file.path(results_path, paste('sub-', new_participant_id, sep=''), 'behavior')
  
  # Create the participant specific directory if not exist
  if (!dir.exists(participant_folder)) {
    dir.create(participant_folder, recursive = TRUE)
  }
  
  # Combine the summary statistics into a single row data frame.
  AS_sub <- data.frame(subject, score_mean_corrected, score_med_corrected, score_mean_raw, score_med_raw, percentage_resp_correct, percentage_excluded_data)
  
  # Change the name of the participant column to match with other dataframes.
  names(AS_sub)[names(AS_sub) == 'subject'] <- 'Participant'
  AS_sub$Participant <- gsub("P", "", AS_sub$Participant)
  
  # Save participant specific results
  results_file <- file.path(participant_folder, paste('sub-', new_participant_id, '_antisaccade_summary.csv', sep=''))
  write.csv(AS_sub, results_file, row.names = FALSE)
  
  # Return AS_sub for further processing if needed
  return(AS_sub)
}

# Create an empty data frame to store the results for all participants.
AS_all <- data.frame()

# Loop through each file, process it, and append the results to AS_all.
for (file in file_paths) {
  AS_sub <- process_file(file)
  AS_all <- rbind(AS_all, AS_sub)
}

# Save results
parent_directory <- dirname(results_path)
new_results_path <- file.path(parent_directory, "results", "combined_data","behavior")

# Create directory if not exist
if (!dir.exists(new_results_path)) {
  dir.create(new_results_path, recursive = TRUE)
}
overall_results_file <- file.path(new_results_path, "antisaccade.csv")
write.csv(AS_all, overall_results_file, row.names = FALSE)

# Print a completion message
message(paste(Sys.time(), ": ANTISACCADE PROCESSING FINISHED. Results saved to: ", overall_results_file))

}