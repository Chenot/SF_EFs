## _main.R
# Author: Quentin Chenot
# Date: 2023-06-21
# Description: This script sources task-specific R scripts and runs them to process executive function task data.
# Dependencies: Requires the following R packages - hms, tidyverse, and the RStudio API. 
# Inputs : raw behavioral data from the executive functions tasks stored in a '/data' directory
# Outputs : processed behavioral data  of the executive functions tasks saved in a '/derived_data' directory


## LOAD LIBRARIES
# Function to check if each required package is installed, and install it if not
required_packages <- c("hms", "tidyverse") # List of packages required for this script
install_if_not_present <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
  }
}
lapply(required_packages, install_if_not_present) # Apply the function to each required package

# Load the libraries
library(hms)
library(tidyverse)

## PATH MANAGEMENT
# Get the directory and path to this file
this_file <- rstudioapi::getSourceEditorContext()$path  # if using RStudio
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(dirname(this_dir))) # Get the project directory
data_path <- file.path(project_dir, "data") # Define the relative path to the input data
tmp_path <- file.path(project_dir, "tmp_data") # Define the relative path to the temporary data
results_path <- file.path(project_dir, "derived_data") # Define the relative path to the output data

# Create results directories
dirs_to_create <- c(
  "derived_data",
  "results",
  "results/combined_data",
  "tmp_data"
)

# Create each directory
for (dir in dirs_to_create) {
  full_dir <- file.path(project_dir, dir)
  if (!dir.exists(full_dir)) {
    dir.create(full_dir, recursive = TRUE, showWarnings = FALSE)
  }
}

## PREPROCESS THE BEHAVIORAL DATA
# Source to get the functions
source("antisaccade.R")
source("categoryswitch.R")
source("colorshape.R")
source("dualnback.R")
source("keeptrack.R")
source("lettermemory.R")
source("numberletter.R")
source("stopsignal.R")
source("stroop.R")
source("SF.R")


# Run the functions to create the .csv files with the results from all tasks and all participants
# process_antisaccade(data_path, results_path)
# process_categoryswitch(data_path, results_path)
# process_colorshape(data_path, results_path)
# process_dualnback(data_path, results_path)
# process_keeptrack(data_path, results_path)
# process_lettermemory(data_path, results_path)
# process_numberletter(data_path, results_path)
# process_stopsignal(data_path, results_path)
# process_stroop(data_path, results_path)
process_SF()
