# Load the libraries
library(dplyr)
library(ggplot2)
library(ggpubr)
library(gridExtra)
library(cowplot)
library(rstudioapi)
library(e1071)
library(cowplot)
library(xtable)
library(stats)

## PATH MANAGEMENT
# Get the directory and path to this file
this_file <- rstudioapi::getSourceEditorContext()$path  # if using RStudio
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir)) # Get the project directory
figure_path <- file.path(project_dir, "results" , "figures") # Define the relative path where the figures will be saved

# Data & Figure paths
data_path <- file.path(project_dir, "results" , "combined_data") # Define the relative path to your data and results

# Load data
file_path <- file.path(data_path, "data_zscored.csv")
df_final <- read.csv(file_path)

# RAW DATA DISTRIBUTION GRAPH
# Inhibition
id1 <- ggplot(df_final, aes(x=antisaccade)) +
  geom_histogram(aes(y=..density..), binwidth=20, colour="black", fill="#DC267F", alpha=.5) +
  geom_density() + 
  geom_rug() +
  labs(title = "Antisaccade", x = "Mean response time (ms)") +
  theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5))


id2 <- ggplot(df_final, aes(x=stopsignal)) +
  geom_histogram(aes(y=..density..),  binwidth=15, colour="black", fill="#DC267F", alpha=.5) +
  geom_density() + 
  geom_rug() +
  labs(title = "Stop Signal", x = "Stop Signal response time (ms)") +
  theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5))

id3 <- ggplot(df_final, aes(x=stroop)) +
  geom_histogram(aes(y=..density..),  binwidth=33, colour="black", fill="#DC267F", alpha=.5) +
  geom_density() + 
  geom_rug() +
  labs(title = "Stroop", x = "Inhibition cost (ms)") +
  theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5))

# Updating
ud1 <- ggplot(df_final, aes(x=dualnback)) +
  geom_histogram(aes(y=..density..), binwidth=0.025, colour="black", fill="#648FFF", alpha=.5) +
  geom_density() + 
  geom_rug() +
  labs(title = "Dual n-back", x = "Percentage of correct responses") +
  theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5))

ud2 <- ggplot(df_final, aes(x=lettermemory)) +
  geom_histogram(aes(y=..density..), binwidth=0.083333, colour="black", fill="#648FFF", alpha=.5) +
  geom_density() + 
  geom_rug() +
  labs(title = "Letter Memory", x = "Percentage of correct responses") +
  theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5))

ud3 <- ggplot(df_final, aes(x=keeptrack)) +
  geom_histogram(aes(y=..density..), binwidth=0.0357, colour="black", fill="#648FFF", alpha=.5) +
  geom_density() + 
  geom_rug() +
  labs(title = "Keep Track", x = "Percentage of correct responses") +
  theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5))


# Shifting
sd1 <- ggplot(df_final, aes(x=categoryswitch)) +
  geom_histogram(aes(y=..density..),  binwidth=33, colour="black", fill="#FFB000", alpha=.5) +
  geom_density() + 
  geom_rug() +
  labs(title = "Category Switch", x = "Switching cost (ms)") +
  theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5))

sd2 <- ggplot(df_final, aes(x=colorshape)) +
  geom_histogram(aes(y=..density..),  binwidth=40, colour="black", fill="#FFB000", alpha=.5) +
  geom_density() + 
  geom_rug() +
  labs(title = "Color Shape", x = "Switching cost (ms)") +
  theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5))

sd3 <- ggplot(df_final, aes(x=numberletter)) +
  geom_histogram(aes(y=..density..),  binwidth=66, colour="black", fill="#FFB000", alpha=.5) +
  geom_density() + 
  geom_rug() +
  labs(title = "Number Letter", x = "Switching cost (ms)") +
  theme_pubr()+
  theme(plot.title = element_text(hjust = 0.5))

# Arrange all the plots in a grid
EFd <- plot_grid(id1, id2, id3, ud1, ud2, ud3, sd1, sd2, sd3, ncol = 3, nrow = 3)

# Add a title to the plot with a specific size and color
EFd <- EFd + ggtitle("Histogram and density plot for executive functions tasks") +
  theme(plot.title = element_text(size = 20, face = "bold", color = "black", hjust = 0.5))

# Display the final plot
EFd

# Save as PDF
pdf_filename <- file.path(figure_path, "SuppFig_EF_score_distribution.pdf")
ggsave(pdf_filename, EFd, width = 14, height = 10)



## RAW DATA DESCRIPTIVE STATISTICS
# Function to calculate mean, SD, skewness and kurtosis for EF tasks #######
calculate_metrics <- function(df, column) {
  
  # Determine the original column name by removing 'zscore_' prefix
  original_column <- sub("zscore_", "", column)
  
  # Calculate skewness and kurtosis for the z-scored column
  skewness_val <- round(skewness(df[[column]], na.rm = TRUE), 2)
  kurtosis_val <- round(kurtosis(df[[column]], na.rm = TRUE), 2)
  
  # Calculate mean and standard deviation for the original column
  mean_val <- mean(df[[original_column]], na.rm = TRUE)
  sd_val <- sd(df[[original_column]], na.rm = TRUE)
  
  # Multiply mean and standard deviation by 100 for specific tasks
  if (original_column %in% c("dualnback", "keeptrack", "lettermemory")) {
    mean_val <- mean_val * 100
    sd_val <- sd_val * 100
  }
  
  # Round the mean and standard deviation values
  mean_val <- round(mean_val, 2)
  sd_val <- round(sd_val, 2)
  
  # Create a dataframe to store results
  result <- data.frame(
    task = original_column,
    mean = mean_val,
    sd = sd_val,
    skewness = skewness_val,
    kurtosis = kurtosis_val
  )
  
  return(result)
}

# List of column names to calculate metrics for
column_names <- c("zscore_antisaccade", "zscore_stopsignal", "zscore_stroop",
                  "zscore_dualnback", "zscore_lettermemory", "zscore_keeptrack",
                  "zscore_categoryswitch", "zscore_colorshape", "zscore_numberletter")

# Calculate metrics for each column and combine results
results <- do.call(rbind, lapply(column_names, function(x) calculate_metrics(df_final, x)))

# Convert the dataframe to LaTeX
latex_demographics <- xtable(results)
latex_demographics



## Space Fortress correlations with demographics
################################################################################
## Hypothesis 3. The SF total score correlates with demographics:
## positively with Education level;
## negatively with age
## positively with video game experience
## Men will have a higher score than women. We tested this with a mixed-effect model.

# Function to format p-values correctly, ensuring correct display for very small values
format_p_value <- function(p_value) {
  if (p_value < .001) {
    "p < .001"
  } else {
    formatted_p = sprintf("%.3f", p_value)  # Format with three decimal places
    formatted_p = sub("^0\\.", ".", formatted_p)  # Remove leading zero for numbers less than 1
    paste("p =", formatted_p)
  }
}
format_p_value2 <- function(p) {
  if (p < 0.001) {
    "$<$ .001"
  } else {
    sprintf("= %.3f", p)  # Keeping three decimal places for consistency
  }
}

# Function to calculate and format the 95% CI of a correlation coefficient
format_ci <- function(cor_object) {
  se <- sqrt((1 - cor_object$estimate^2) / (cor_object$parameter))
  lower <- cor_object$estimate - qt(0.975, cor_object$parameter) * se
  upper <- cor_object$estimate + qt(0.975, cor_object$parameter) * se
  paste("[", round(lower, 2), ",", round(upper, 2), "]", sep = "")
}

# Format correlation results into a string
format_correlation <- function(cor_test, hypothesis) {
  r_value <- cor_test$estimate
  p_value <- cor_test$p.value
  r_squared <- r_value^2
  ci <- format_ci(cor_test)
  formatted_string <- paste("r =", sub("^0\\.", ".", format(r_value, digits = 2)),
                            "(95\\% CI", ci, ", p", format_p_value2(p_value), 
                            ", R² =", sub("^0\\.", ".", format(round(r_squared, 3), digits = 3)),
                            ") for the", hypothesis)
  formatted_string
}

# SF & NET
cor_test_NET <- cor.test(df_final$zscore_SF, df_final$EducationLevel)
r_value <- round(cor_test_NET$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_NET$p.value))

# Plot
NET_SF <- ggplot(df_final, aes(x=EducationLevel, y= zscore_SF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = 12, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Education Level")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# SF & Age
cor_test_Age <- cor.test(df_final$zscore_SF, df_final$Age)
r_value <- round(cor_test_Age$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_Age$p.value))

# Plot
age_SF <- ggplot(df_final, aes(x=Age, y=zscore_SF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = 22, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Age")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# SF & VGexp
cor_test_VGexp <- cor.test(df_final$zscore_SF, df_final$VGexp)
r_value <- round(cor_test_VGexp$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_VGexp$p.value))

# Plot
VG_SF <- ggplot(df_final, aes(x=VGexp, y=zscore_SF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = 2, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Video Game Experience")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# Plot GRID with demographics
Demographics_SF <- plot_grid(VG_SF, NET_SF, age_SF, ncol = 3, nrow = 1)

# Save plot
filename <- paste0(figure_path, "/SF_demographics.pdf")
ggsave(filename, plot = Demographics_SF, width = 12, height = 4, units = "in")
