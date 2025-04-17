### Script to extract SF psychometrics

## LIBRARY MANAGEMENT
library(dplyr)

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

#################
## SENSITIVITY ##
#################

# Select maximum score
total_scores <- df_final$TotalScore

# Calculate skewness, kurtosis, normality (Shapiro-Wilk)
skewness_val <- skewness(total_scores, type = 2)
kurtosis_val <- kurtosis(total_scores, type = 2)
shapiro_p_val <- shapiro.test(total_scores)$p.value

# Display the results
cat("Skewness: ", skewness_val, "\n")
cat("Kurtosis: ", kurtosis_val, "\n")
cat("Shapiro-Wilk p-value: ", shapiro_p_val, "\n")

# Generating plots
# QQ-plot using ggplot2 and ggpubr
qq_plot <- ggplot(data.frame(sample = total_scores), aes(sample = sample)) +
  stat_qq() +
  stat_qq_line(colour = "red") +
  ggtitle("QQ-plot for TotalScore") +
  theme_pubr() +
  print(qq_plot) # Display the QQ-plot

# Distribution plot with a rug using ggplot2
SF_sensitivity <- ggplot(data.frame(TotalScore = total_scores), aes(x = TotalScore)) +
  geom_density(color = "black", fill = "grey", alpha = .5) +
  geom_rug() +
  theme_pubr() +
  ylab("Density")+
  xlab("Space Fortress score")
print(SF_sensitivity) # Display the distribution plot

filename <- paste0(plot_directory, "/SF_sensitivity.pdf")
ggsave(filename, plot = SF_sensitivity, width = 7, height = 5, units = "in")

## SF fidelity 
icc_result <- ICC(df_sf_fidelity[, 2:6]) # Exclude the Participant column for the calculation
print(icc_result) # Print the ICC result

# If you want to extract just the ICC value
icc_value <- icc_result$ICC
print(icc_value)

