library(ggplot2)
library(ggpubr)
library(ggExtra)
library(dplyr)
library(cowplot)
library(broom)
library(xtable)
library(reshape2)
library(viridis)

## PATH MANAGEMENT
# Get the directory and path to this file
this_file <- rstudioapi::getSourceEditorContext()$path  # if using RStudio
this_dir <- dirname(this_file)
setwd(this_dir)
project_dir <- dirname(dirname(this_dir)) # Get the project directory

# Data & Figure paths
data_path <- file.path(project_dir, "data") # Define the relative path to the data files
df_final <- read.csv(file.path(project_dir,"results", "combined_data", "data_zscored.csv"))
figure_path <- file.path(project_dir, "results" , "figures") # Define the relative path to the data and results

# p value formating
format_p_value <- function(p_value) {
  if (p_value < .001) {
    "p < .001"
  } else {
    formatted_p = sprintf("%.3f", p_value)  # Format with three decimal places
    formatted_p = sub("^0\\.", ".", formatted_p)  # Remove leading zero for numbers less than 1
    paste("p =", formatted_p)
  }
}

################################################################################
## Hypothesis 1. The EF composite score highly correlates with the maximum score obtained in SF.
# Calculate correlation and format the p-value
cor_test <- cor.test(df_final$zscore_SF, df_final$zscore_EF)
r_value <- round(cor_test$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test$p.value))

# Plot
EF_SF <- ggplot(df_final, aes(y = zscore_SF, x = zscore_EF)) +
  geom_point() +
  geom_smooth(method = lm, color = "black", fill = "lightgray", se = TRUE) +
  annotate("text", x = -1, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr() +
  xlab("Executive Functions (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))

# Save plot
filename <- paste0(figure_path, "/SF_EF.pdf")
ggsave(filename, plot = EF_SF, width = 5, height = 4, units = "in")

################################################################################
## Hypothesis 2. The SF total score has a higher correlation degree with the updating score than the shifting score, than the inhibition score.
# Calculate correlation and format the p-value

# Updating
cor_test_updating <- cor.test(df_final$zscore_SF, df_final$zscore_WM)
r_value <- round(cor_test_updating$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_updating$p.value))

#Plot
SF_updating <- ggplot(df_final, aes(y=zscore_SF, x=zscore_WM))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Updating (z-score)")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# Shifting
cor_test_shifting <- cor.test(df_final$zscore_SF, df_final$zscore_shifting)
r_value <- round(cor_test_shifting$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_shifting$p.value))

# Plot
SF_shifting <- ggplot(df_final, aes(y=zscore_SF, x=zscore_shifting))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Shifting (z-score)")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# Inhibition
cor_test_inhibition <- cor.test(df_final$zscore_SF, df_final$zscore_inhibition)
r_value <- round(cor_test_inhibition$estimate, 2)
r_value_formatted <- sub("^0\\.", ".", r_value)
r_squared <- round(r_value^2, 2)
r_squared_formatted <- sub("^0\\.", ".", r_squared)
cor_label <- paste("r =", r_value_formatted, ", R² =", r_squared_formatted, ",", format_p_value(cor_test_inhibition$p.value))

#Plot
SF_inhibition <- ggplot(df_final, aes(y=zscore_SF, x=zscore_inhibition))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  annotate("text", x = -1.5, y = 2.5, label = cor_label, hjust = 0) +  # Add custom label
  theme_pubr()+
  xlab("Inhibition (z-score)")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# Correlations results
cor_test_updating
cor_test_shifting
cor_test_inhibition

# Plot GRID with EF sub-scores
EFsubscores_SF <- plot_grid(SF_updating,SF_shifting,SF_inhibition, ncol = 3, nrow = 1)

# Save plot
filename <- paste0(figure_path, "/SF_EFsubscores.pdf")
ggsave(filename, plot = EFsubscores_SF, width = 12, height = 4, units = "in")


# Function to format p-values correctly, ensuring correct display for very small values
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

# Calculate formatted strings
updating_result <- format_correlation(cor_test_updating, "updating (hypothesis 2a)")
shifting_result <- format_correlation(cor_test_shifting, "shifting (hypothesis 2b)")
inhibition_result <- format_correlation(cor_test_inhibition, "Inhibition (hypothesis 2c)")

# Create the paragraph
paragraph <- paste("The correlations between the EF sub-scores and SF score were",
                   updating_result, ";",
                   shifting_result, ";",
                   inhibition_result, ". These results are presented in the Figure \\ref{fig:EFsSFcorr}.")

# Print the corrected paragraph
print(paragraph)

################################################################################
## Hypothesis 3. The SF total score correlates with demographics:
## positively with Education level;
## negatively with age
## positively with video game experience
## Men will have a higher score than women. We tested this with a mixed-effect model.

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

#SF & Gender
t_test_result <- t.test(zscore_SF ~ Sex, data = df_final) # Perform the Student's t-test
mean_sd <- df_final %>%       # Calculate means and standard deviations for each group
  group_by(Sex) %>%
  summarise(
    mean_zscore_SF = mean(zscore_SF, na.rm = TRUE),
    sd_zscore_SF = sd(zscore_SF, na.rm = TRUE)
  )

# Print the results
print(t_test_result)
print(mean_sd)

# Creating the plot
SF_gender <- ggplot(df_final, aes(x=zscore_EF, y=zscore_SF, color=Sex)) +
  geom_point() +
  geom_smooth(method="lm", se=TRUE) +
  theme_pubr() +
  xlab("Executive Functions (z-score)") +
  ylab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1)) +
  scale_color_manual(name = "Gender", 
                     labels = c("Men", "Women"),
                     values = c("man" = "#00aeef", "woman" = "#ef00ae")) # Replace "color1" and "color2" with your desired colors

# Adding marginal histograms
SF_gender <- ggMarginal(SF_gender, groupColour = TRUE, groupFill = TRUE)
SF_gender

# Save plot
filename <- paste0(figure_path, "/SF_gender.pdf")
ggsave(filename, plot = SF_gender, width = 6, height = 6, units = "in")

# Function to calculate 95% CI for correlation coefficient
correlation_ci <- function(r, n, conf_level = 0.95) {
  z <- atanh(r)  # Fisher transformation
  se <- 1 / sqrt(n - 3)
  alpha <- 1 - conf_level
  z_crit <- qnorm(1 - alpha / 2)
  ci_lower <- tanh(z - z_crit * se)
  ci_upper <- tanh(z + z_crit * se)
  return(c(ci_lower, ci_upper))
}

# Calculate correlation results with correct CI for correlation coefficient
results <- df_final %>%
  group_by(Sex) %>%
  do({
    cor_test <- cor.test(.$zscore_SF, .$zscore_EF)
    lm_test <- lm(zscore_EF ~ zscore_SF, data = .)
    r_squared <- summary(lm_test)$r.squared
    
    # Calculate 95% CI for correlation coefficient
    ci <- correlation_ci(cor_test$estimate, nrow(.))
    
    # Returning a combined data frame
    data.frame(
      Sex = unique(.$Sex),
      r_value = cor_test$estimate,
      p_value = cor_test$p.value,
      r_squared = r_squared,
      CI_lower = ci[1],
      CI_upper = ci[2]
    )
  }) %>%
  ungroup()

# View results
print(results)

# GLM SF & demographics
model1 <- lm(formula = zscore_SF ~ zscore_EF + Age + EducationLevel + VGexp + Sex, data = df_final)
summary(model1)

# Convert the model summary to an xtable
model1_table <- xtable(summary(model1))

# Print the xtable with LaTeX formatting
print(model1_table, type = "latex", include.rownames = FALSE)


################################################################################
## Hypothesis 4. The SF sub-scores correlates positively with specific EF. Specifically,
## (a) Mine scores with updating;
## (b) Fortress score with inhibition;
## (c) Bonus score with updating;
## (d) Flying score with updating. 
# Select Space Fortress scores
space_fortress_scores <- df_final %>%
  select(SF, Flight, Bonus, Mine, Fortress)

# Select cognitive task z-scores
cognitive_task_scores <- df_final %>%
  select(zscore_EF, zscore_inhibition, zscore_WM, zscore_shifting)

# Initialize matrices to store correlations and p-values
cor_matrix <- matrix(nrow = ncol(cognitive_task_scores), ncol = ncol(space_fortress_scores))
p_matrix <- matrix(nrow = ncol(cognitive_task_scores), ncol = ncol(space_fortress_scores))

# Compute correlations and p-values
for (i in 1:ncol(cognitive_task_scores)) {
  for (j in 1:ncol(space_fortress_scores)) {
    cor_test <- cor.test(cognitive_task_scores[[i]], space_fortress_scores[[j]], use = "complete.obs")
    cor_matrix[i, j] <- cor_test$estimate
    p_matrix[i, j] <- cor_test$p.value
  }
}

# Apply Holm-Bonferroni correction to p-values
p_adjusted <- p.adjust(p_matrix, method = "holm")
p_adjusted_matrix <- matrix(p_adjusted, nrow = nrow(p_matrix), ncol = ncol(p_matrix))

# Assign row and column names to the matrices
rownames(cor_matrix) <- c("EF", "Inhibition", "Updating", "Shifting")
colnames(cor_matrix) <- c("SF", "Flight", "Bonus", "Mine", "Fortress")
rownames(p_adjusted_matrix) <- rownames(cor_matrix)
colnames(p_adjusted_matrix) <- colnames(cor_matrix)

# Convert the correlation matrix to a dataframe in long format
cor_df <- melt(cor_matrix)
p_df <- melt(p_adjusted_matrix)

# Combine correlation coefficients and adjusted p-values
cor_df$p_adjusted <- p_df$value

# Determine significance
cor_df$significance <- ifelse(cor_df$p_adjusted < 0.05, "bold", "plain")

# Rename columns for clarity
colnames(cor_df) <- c("CognitiveTask", "SpaceFortressScore", "Correlation", "p_adjusted", "significance")

# Plot
EFSF_matrix <- ggplot(cor_df, aes(x = SpaceFortressScore, y = CognitiveTask, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sub("^(-?)0\\.", "\\1.", sprintf("%.2f", Correlation)), 
            fontface = significance), color = "white", size = 3) +
  scale_fill_viridis(option = "magma", 
                     limits = c(.1, .6),
                     direction = -1,  # Add this line to invert the colors
                     name = "Pearson\nCorrelation") +
  theme_minimal() +
  labs(x = "SF Scores", y = "EF composite scores") +
  coord_fixed()
EFSF_matrix

# Save plot
filename <- paste0(figure_path, "/SF_EF_matrix.pdf")
ggsave(filename, plot = EFSF_matrix, width = 5, height = 4, units = "in")

