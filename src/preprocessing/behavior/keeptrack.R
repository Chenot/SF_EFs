# Read and process data files for the Keep Track task
process_keeptrack <- function(data_path, results_path) {
  
# List all relevant data files in the folder
file_paths <- list.files(path = data_path, pattern = "keeptracktask_raw_P\\d+_.*\\.iqdat", full.names = TRUE, recursive = TRUE)

# Define a custom function to merge non-NULL values
merge_non_null <- function(x, y) {
  if (is.null(x)) {
    return(y)
  } else {
    return(x)
  }
}

# Function to process a single file and return a processed dataframe
process_file <- function(file) {
  # Read the file
  sub_P <- read.delim(file, fileEncoding = "UTF-8")
  
  # Filter, reshape and process the data
  KTa <- sub_P %>%
    filter(blockcode == "test", grepl("^test_recall", trialcode), 
           grepl("[a-zA-Z]", response)) %>%
    mutate(trial_type = ifelse(grepl("^test_recall3", trialcode), 
                               "group_3", "group_4")) %>%
    separate(trialcode, into = c("trial_code_prefix", "trial_number", 
                                 "trial_code_suffix"), sep = "_") %>%
    select(subject, trial_type, trial_number, response, 
           contains("Category"), ) %>%
    select(-"targetCategory5", -"Category5_last", -"correctCategory5", 
           -"targetCategory6", -"Category6_last", -"correctCategory6")%>%
    pivot_wider(names_from = trial_number, values_from = response, 
                names_prefix = "response_")

  
  # Merge the response_recall3 and response_recall4 columns using the custom function merge_non_null()
  KTb <- KTa %>%
    mutate(response_recall = map2(response_recall3, response_recall4, merge_non_null)) %>%
    unnest_wider(response_recall, names_sep = "_")
  
  # Create a data frame of given responses by selecting the subject and recall columns and gathering the response columns
  given_response <- KTb %>%
    select(subject, trial_type, response_recall_1, response_recall_2, response_recall_3, response_recall_4) %>%
    gather(key = "recall", value = "given_response", response_recall_1:response_recall_4) %>%
    select(-recall)
  
  # Create a data frame of expected responses by selecting the subject and recall columns and gathering the category columns
  expected_response <- KTb %>%
    select(subject, Category1_last, Category2_last, Category3_last, Category4_last) %>%
    gather(key = "recall", value = "expected_response", Category1_last:Category4_last) %>%
    select(-recall, -subject)
  
  # Create a data frame of scores by selecting the subject and recall columns and gathering the correct category columns
  score <- KTb %>%
    select(subject, correctCategory1, correctCategory2, correctCategory3, correctCategory4) %>%
    gather(key = "recall", value = "score", correctCategory1:correctCategory4) %>%
    select(-recall, -subject)
  
  # Combine the given_response, expected_response, and score data frames into a single data frame
  KT <- cbind(given_response, expected_response, score)
  
  return(KT)
}

# Correct the score taking into account the spelling mistakes
# Initialize an empty dataframe to store results for all participants
KT_all <- data.frame()

# Loop through each file, process it, and append the results to KT_all
for (file in file_paths) {
  KT <- process_file(file)
  KT_all <- rbind(KT_all, KT)
}


# Remove accents and upper cases
KT_all$given_response <- iconv(KT_all$given_response, to = "ASCII//TRANSLIT")
KT_all$expected_response <- iconv(KT_all$expected_response, to = "ASCII//TRANSLIT")
KT_all$given_response <- tolower(KT_all$given_response)
KT_all$expected_response <- tolower(KT_all$expected_response)

## Correct spelling mistakes
lookup_table <- c("abricot" = "abricot",
                  "abricots" = "abricot",
                  "australie" = "australie",
                  "australie " = "australie",
                  "banane" = "banane",
                  "banana" = "banane",
                  "banane " = "banane",
                  "bleu" = "bleu",
                  "bleue" = "bleu",
                  "blanc" = "blanc",
                  "blanche" = "blanc",
                  "bouche" = "bouche",
                  "bouche " = "bouche",
                  "bras" = "bras",
                  "bresil" = "bresil",
                  "bresil " = "bresil",
                  "bresile" = "bresil",
                  "brezil" = "bresil",
                  "cheval" = "cheval",
                  "chevale" = "cheval",
                  "chine" = "chine",
                  "chien" = "chien",
                  "coude" = "coude",
                  "coud" = "coude",
                  "coude " = "coude",
                  "frere" = "frere",
                  "fere" = "frere",
                  "frere " = "frere",
                  "fraise" = "fraise",
                  "frais" = "fraise",
                  "inde" = "inde",
                  "italie" = "italie",
                  "jaune" = "jaune",
                  "jaune " = "jaune",
                  "japon" = "japon",
                  "jappn" = "japon",
                  "jambe" = "jambe",
                  "jamb" = "jambe",
                  "jambee" = "jambe",
                  "jambre" = "jambe",
                  "lapin" = "lapin",
                  "lain" = "lapin",
                  "lion" = "lion",
                  "lion " = "lion",
                  "loup" = "loup",
                  "loups" = "loup",
                  "main" = "main",
                  "mere" = "mere",
                  "mere " = "mere",
                  "melon" = "melon",
                  "oncle" = "oncle",
                  "onlce" = "oncle",
                  "ours" = "ours",
                  "ourse" = "ours",
                  "pere" = "pere",
                  "pere " = "pere",
                  "pied" = "pied",
                  "pieds" = "pied",
                  "poire" = "poire",
                  "poire " = "poire",
                  "pomme" = "pomme",
                  "rouge" = "rouge",
                  "soeur" = "soeur",
                  "soeur " = "soeur",
                  "tante" = "tante",
                  "vert" = "vert",
                  "vert " = "vert",
                  "violet" = "violet")


# Use ifelse() to correct the given_response column
KT_all_corrected <- KT_all %>%
  mutate(given_response_corrected = ifelse(given_response %in% names(lookup_table), 
                                           lookup_table[given_response], 
                                           given_response))

## Verify the answer, to see if there are spelling mistakes.
word_count_g <- KT_all_corrected %>%
  count(given_response, sort = TRUE)
word_count_g_c <- KT_all_corrected %>%
  count(given_response_corrected, sort = TRUE)
word_count_e <- KT_all_corrected %>%
  count(expected_response, sort = TRUE)

## Create a new column with the corrected score, taking into account spelling mistakes.
KT_all_corrected <- KT_all_corrected %>%
  mutate(corrected_score = ifelse(given_response_corrected == expected_response, 1, 0),
         score = as.numeric(score))


## Create a dataframe with the score per participant, depending on trial type (three vs four letter to memorize).
KT_scores <- KT_all_corrected %>%
  group_by(subject, trial_type) %>%
  summarize(score = sum(score, na.rm = TRUE),
            corrected_score = sum(corrected_score, na.rm = TRUE))

## Create a dataframe with the total score per participant.
KT_scores_grouped <- KT_all_corrected %>%
  group_by(subject) %>%
  summarize(corrected_score = sum(corrected_score, na.rm = TRUE))
KT_scores_grouped$corrected_score <- KT_scores_grouped$corrected_score/28

# Change the name of the participant column to match with other dataframes.
names(KT_scores_grouped)[names(KT_scores_grouped) == 'subject'] <- 'Participant'
KT_scores_grouped$Participant <- gsub("P", "", KT_scores_grouped$Participant)

# Save individual results
# Split KT_scores_grouped into a list of dataframes by participant_id
list_of_participants <- split(KT_scores_grouped, KT_scores_grouped$Participant)

# Define function to save each participant's data to a file
save_individual_files <- function(df, participant_id, results_path) {
  # Extract new participant ID by removing 'P' from the original participant_id
  new_participant_id <- sub("P", '', participant_id)
  
  # Define participant specific path
  participant_folder = file.path(results_path, paste0('sub-', new_participant_id, '/behavior'))
  
  # Create the participant specific directory if not exist
  if (!dir.exists(participant_folder)) {
    dir.create(participant_folder, recursive = TRUE)
  }
  
  # Define the results file path
  results_file <- file.path(participant_folder, paste0('sub-', new_participant_id, '_keeptrack_summary.csv'))
  
  # Write the dataframe to the file
  write.csv(df, results_file, row.names = FALSE)
}

# Loop over the list of dataframes and apply the save_individual_files function to each
for (participant_id in names(list_of_participants)) {
  save_individual_files(list_of_participants[[participant_id]], participant_id, results_path)
}


# Save overall results
parent_directory <- dirname(results_path)
new_results_path <- file.path(parent_directory, "results", "combined_data","behavior")
# Create directory if not exist
if (!dir.exists(new_results_path)) {
  dir.create(new_results_path, recursive = TRUE)
}

results_file <- file.path(new_results_path, "keeptrack.csv")
write.csv(KT_scores_grouped, results_file, row.names = FALSE)


# Print a completion message
message(paste(Sys.time(), ": KEEP-TRACK PROCESSING FINISHED. Results saved to: ", results_file))

}


