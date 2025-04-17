### Script to extract SF psychometrics

## PATH MANAGEMENT
# Get the directory and path to this file
this_file <- rstudioapi::getSourceEditorContext()$path  # if using RStudio
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(this_dir) # Get the project directory

# Data & Figure paths
data_path <- file.path(project_dir, "data") # Define the relative path to the data files
df_demographics <- read.csv(file.path(data_path,"participants_demographics.csv"))
df_sf_fidelity <- read.csv(file.path(data_path,"SF.csv"))
df_final <- read.csv(file.path(data_path,"data.csv"))

figure_path <- file.path(project_dir, "results" , "figures") # Define the relative path to the data and results


# Remove outliers
df_sf_fidelity <- df_sf_fidelity %>%
  inner_join(df_demographics %>% filter(Inclusion == 1), by = "Participant")
df_final <- df_final %>%
  select(-Age)
df_final <- df_final %>%
  inner_join(df_demographics %>% filter(Inclusion == 1), by = "Participant")

## Demographics
# Calculate summary statistics for each variable
summary_stats <- df_final %>%
  summarise(
    MeanAge = mean(Age, na.rm = TRUE),
    SdAge = sd(Age, na.rm = TRUE),
    MinAge = min(Age, na.rm = TRUE),
    MaxAge = max(Age, na.rm = TRUE),
    MeanEL = mean(EducationLevel, na.rm = TRUE),
    SdEL = sd(EducationLevel, na.rm = TRUE),
    MinEL = min(EducationLevel, na.rm = TRUE),
    MaxEL = max(EducationLevel, na.rm = TRUE),
    MeanVGexp = mean(VGexp, na.rm = TRUE),
    SdVGexp = sd(VGexp, na.rm = TRUE),
    MinVGexp = min(VGexp, na.rm = TRUE),
    MaxVGexp = max(VGexp, na.rm = TRUE)
  )

# Convert the summary statistics into a long format
long_stats <- summary_stats %>%
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value")

# Create a table structure for display
# Separate statistics into their respective categories
stats_table <- tibble(
  Statistic = c("Mean", "SD", "Min", "Max"),
  Age = NA_real_, # Initialize columns with NA of type double
  EducationLevel = NA_real_,
  VGexp = NA_real_
)

# Populate the table with values
stats_table$Age <- c(
  long_stats$Value[long_stats$Statistic == "MeanAge"],
  long_stats$Value[long_stats$Statistic == "SdAge"],
  long_stats$Value[long_stats$Statistic == "MinAge"],
  long_stats$Value[long_stats$Statistic == "MaxAge"]
)
stats_table$EducationLevel <- c(
  long_stats$Value[long_stats$Statistic == "MeanEL"],
  long_stats$Value[long_stats$Statistic == "SdEL"],
  long_stats$Value[long_stats$Statistic == "MinEL"],
  long_stats$Value[long_stats$Statistic == "MaxEL"]
)
stats_table$VGexp <- c(
  long_stats$Value[long_stats$Statistic == "MeanVGexp"],
  long_stats$Value[long_stats$Statistic == "SdVGexp"],
  long_stats$Value[long_stats$Statistic == "MinVGexp"],
  long_stats$Value[long_stats$Statistic == "MaxVGexp"]
)

# count sex and handedness
sex_count <- df_final %>%
  summarise(
    CountMen = sum(Sex == 'man', na.rm = TRUE),
    CountWomen = sum(Sex == 'woman', na.rm = TRUE)
  )

handedness_count <- df_final %>%
  summarise(
    CountLeftHandeed = sum(Handedness == 'left-handed', na.rm = TRUE),
    CountRightHanded = sum(Handedness == 'right-handed', na.rm = TRUE)
  )

# Display the results
print(sex_count)
print(handedness_count)
print(stats_table)


