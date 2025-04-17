# Read and process data files for the Category Switch task
process_categoryswitch <- function(data_path, results_path) {

# List all relevant data files in the folder using the specific pattern
file_paths <- list.files(path = data_path, pattern = "categoryswitchtask_raw_P\\d+_.*\\.iqdat", full.names = TRUE, recursive = TRUE)

# Define function to process each data file
process_file <- function(file) {
  # Read the data file
  sub <- read.delim(file, fileEncoding = "UTF-8")
  
  # Filter the data for 'combinedTest' block only
  test <- sub %>% filter(blockcode == "combinedTest")
  
  # Extract specific columns from 'test' dataframe
  CS <- test %>% select(subject, blocknum, switch, congruence, correct, latency, target)
  
  # Remove the "truite" as there is an error in the feedback (a trout is not smaller than a basketball)
  CS <- CS[CS$target != "truite", ]
  
  # Filter for correct responses
  CS_correct <- CS %>% filter(correct == 1)
  
  # Compute median and Median Absolute Deviation (MAD)
  med <- median(CS_correct$latency)
  mad <- 3.32 * mad(CS_correct$latency)
  medmad_pos <- med + mad
  min <- 200
  
  # Filter latency based on computed values
  latency_corrected <- CS_correct %>%
    filter(latency <= medmad_pos) %>%
    filter(latency >= min)
  
  # Extract subject-level summary statistics
  subject <- latency_corrected$subject[1]
  mean_latency_switch <- latency_corrected %>% filter(switch == 1) %>% summarize(mean_latency = mean(latency)) %>% pull(mean_latency)
  mean_latency_noswitch <- latency_corrected %>% filter(switch == 0) %>% summarize(mean_latency = mean(latency)) %>% pull(mean_latency)
  switchCost <- mean_latency_switch-mean_latency_noswitch
  percentage_resp_correct <- nrow(CS_correct) / nrow(CS)
  percentage_excluded_data <- (1 - nrow(latency_corrected) / nrow(CS_correct))*100
  
  # Combine summary statistics into a single row dataframe
  CS_sub <- data.frame(subject, switchCost, mean_latency_switch, mean_latency_noswitch, percentage_resp_correct, percentage_excluded_data)
  
  # Change the name of the participant column to match with other dataframes.
  names(CS_sub)[names(CS_sub) == 'subject'] <- 'Participant'
  CS_sub$Participant <- gsub("P", "", CS_sub$Participant)
  
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
  results_file <- file.path(participant_folder, paste('sub-', new_participant_id, '_categoryswitch_summary.csv', sep=''))
  write.csv(CS_sub, results_file, row.names = FALSE)
  
  return(CS_sub)
}

# Initialize an empty dataframe to store results for all participants
CS_all <- data.frame()

# Loop through each file, process it, and append the results to CS_all
for (file in file_paths) {
  CS_sub <- process_file(file)
  CS_all <- rbind(CS_all, CS_sub)
}

# Save results
parent_directory <- dirname(results_path)
new_results_path <- file.path(parent_directory, "results", "combined_data","behavior")
# Create directory if not exist
if (!dir.exists(new_results_path)) {
  dir.create(new_results_path, recursive = TRUE)
}
results_file <- file.path(new_results_path, "categoryswitch.csv")
write.csv(CS_all, results_file, row.names = FALSE)

# Print a completion message
message(paste(Sys.time(), ": CATEGORY SWITCH PROCESSING FINISHED. Results saved to: ", results_file))

}
