# Read and process data files for the Number Letter task
process_numberletter <- function(data_path, results_path) {
  
# List all relevant data files in the folder
file_paths <- list.files(path = data_path, pattern = "switch_numberlettertask_raw_P\\d+_.*\\.iqdat", full.names = TRUE, recursive = TRUE)

# Function to process each data file
process_file <- function(file) {
  
  # Read the data file
  sub <- read.delim(file, fileEncoding = "UTF-8")
  
  # Filter rows with "cst" in the "blocknum" column
  test <- sub %>% filter(blockcode == "combinedTask")
  
  # Create a new dataframe with specific columns from 'test'
  NL <- test %>% select(subject, blocknum, switch, congruence, correct, latency)
  
  
  # Filter correct responses
  NL_correct <- NL %>%
    filter(correct == 1)
  
  # Compute median and MAD
  med <- median(NL_correct$latency)
  mad <- 3.32 * mad(NL_correct$latency)
  medmad_pos <- med + mad
  min <- 200
  
  # Filter latency based on computed values
  latency_corrected <- NL_correct %>%
    filter(latency <= medmad_pos) %>%
    filter(latency >= min)
  
  # Collect subject-level summary statistics
  subject <- latency_corrected$subject[1]
  mean_latency_switch <- latency_corrected %>% filter(switch == 1) %>% summarize(mean_latency = mean(latency)) %>% pull(mean_latency)
  mean_latency_noswitch <- latency_corrected %>% filter(switch == 0) %>% summarize(mean_latency = mean(latency)) %>% pull(mean_latency)
  switchCost <- mean_latency_switch-mean_latency_noswitch
  percentage_resp_correct <- nrow(NL_correct) / nrow(NL)
  percentage_excluded_data <- (1 - nrow(latency_corrected) / nrow(NL_correct))*100
  
  # Combine the summary statistics into a single row dataframe
  NL_sub <- data.frame(subject, switchCost, mean_latency_switch, mean_latency_noswitch, percentage_resp_correct, percentage_excluded_data)

  # Change the name of the participant column to match with other dataframes.
  names(NL_sub)[names(NL_sub) == 'subject'] <- 'Participant'
  NL_sub$Participant <- gsub("P", "", NL_sub$Participant)
  
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
  results_file <- file.path(participant_folder, paste('sub-', new_participant_id, '_numberletter_summary.csv', sep=''))
  write.csv(NL_sub, results_file, row.names = FALSE)
  
  return(NL_sub)
}

# Initialize an empty dataframe to store results for all participants
NL_all <- data.frame()

# Loop through each file, process it, and append the results to AS_all
for (file in file_paths) {
  NL_sub <- process_file(file)
  NL_all <- rbind(NL_all, NL_sub)
}

# Save results
parent_directory <- dirname(results_path)
new_results_path <- file.path(parent_directory, "results", "combined_data","behavior")
# Create directory if not exist
if (!dir.exists(new_results_path)) {
  dir.create(new_results_path, recursive = TRUE)
}

results_file <- file.path(new_results_path, "numberletter.csv")
write.csv(NL_all, results_file, row.names = FALSE)

# Print a completion message
message(paste(Sys.time(), ": NUMBER-LETTER PROCESSING FINISHED. Results saved to: ", results_file))

}


