### Script to extract SF psychometrics
## LIBRARY MANAGEMENT
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(e1071)
library(nortest)
library(psych)

## PATH MANAGEMENT
# Get the directory and path to this file
this_file <- rstudioapi::getSourceEditorContext()$path  # if using RStudio
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir)) # Get the project directory

# Data & Figure paths
data_path <- file.path(project_dir, "data") # Define the relative path to the data files
df_final <- read.csv(file.path(project_dir,"results", "combined_data", "data.csv"))
df_final <- df_final[df_final$Inclusion == 1, ]
figure_path <- file.path(project_dir, "results" , "figures") # Define the relative path to the data and results

#################
## SENSITIVITY ##
#################

# Calculate skewness, kurtosis
skewness_val <- skewness(df_final$SF, type = 2)
kurtosis_val <- kurtosis(df_final$SF, type = 2)

# Calculate normality (Kolmogorov-Smirnov)
df_final$SF_jittered <- df_final$SF + rnorm(length(df_final$SF), mean = 0, sd = 1e-5) # Add jitter to the data
ks_test_jittered <- ks.test(df_final$SF_jittered, "pnorm", mean = mean(df_final$SF_jittered), sd = sd(df_final$SF_jittered))

# Display the results
cat("Skewness: ", skewness_val, "\n")
cat("Kurtosis: ", kurtosis_val, "\n")
cat("Kolmogorov-Smirnov test p-value (jittered): ", ks_test_jittered$p.value, "\n")

# Generating plots
# Distribution QQ-plot
qqplot <- ggqqplot(df_final$SF, 
                   xlab = "Theoretical Quantiles", ylab = "Sample Quantiles")

SF_sensitivity <- ggplot(data = df_final, aes(x = SF)) +
  geom_density(color = "black", fill = "grey", alpha = .5) +
  geom_rug() +
  theme_pubr() +
  ylab("Density")+
  xlab("Space Fortress score")

# Combine the two plots into a single 1x2 plot
combined_plot <- ggarrange(SF_sensitivity, qqplot, ncol = 2, nrow = 1)

# Print the combined plot
print(combined_plot)

# Save plot
filename <- paste0(figure_path, "/SF_sensitivity.pdf")
ggsave(filename, plot = combined_plot, width = 10, height = 4, units = "in")

#################
## RELIABILITY ##
#################
# Select multi games columns
df_icc <- df_final[, c("SF_multi_01", "SF_multi_02", "SF_multi_03", "SF_multi_04", "SF_multi_05")]

# Calculate ICC
icc_result <- ICC(df_icc)
print(icc_result)