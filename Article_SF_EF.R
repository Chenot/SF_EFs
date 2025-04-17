# Library Loading
library(ggplot2)
library(cowplot)
library(lme4)
library(nlme)
library(tidyverse)
library(ez)
library(pwr)
library(gghalves)
library(dplyr)
library(bayestestR)
library(ggridges)
library(stargazer)
library(kableExtra)
library(data.table)
library(psych)
library(ggpubr)
library(ggExtra)
library(readxl)
library(GGally)
library(gridExtra)
library(xtable)
library(psych)
library(reshape2)
library(e1071)

# Load SF data
# # Extract n=150 (RR)
# source("D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\Code\\Extract_Behavior_data\\Clean_SF.R")
# path_clean="D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\SF\\clean_data"
# fil_clean=list.files(path=path_clean,recursive = T) #load the clean files
# data=read_final_Score(fil_clean,path_clean,detailed = TRUE) #Create the data
# data <- na.omit(data) # Remove NaN (monotask)
# data$Points = data$TotalScore # Rename to the same name
# df_SF2_max_scores <- data %>%
#   arrange(Pseudo, desc(TotalScore)) %>%
#   group_by(Pseudo) %>%
#   slice(1) %>%
#   ungroup()
# 
# # Merge database from the 2 studies
# df_SF2_max_scores[] <- lapply(df_SF2_max_scores, function(x) gsub("_2", "", x)) # Replace _2 by nothing
# 
# # Compute zscore
# df_SF2_max_scores$Points <- as.numeric(df_SF2_max_scores$Points)
# df_SF2_max_scores$zscore_SF <- scale(df_SF2_max_scores$Points)
# df_SF2_max_scores <- df_SF2_max_scores %>% rename(Participant  = Pseudo)
# 
# write.csv(df_SF2_max_scores,"D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\SF_all.csv", row.names = FALSE)
# 
# 
# ## Generate final table
# SF <- read.csv("D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\SF_all.csv")
# EF <- read.csv("D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\EF.csv")
# EF_raw <- read.csv("D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\EF_raw.csv")
# subj <- read.csv("D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\participants.csv")
# 
# df_final <- merge(SF, EF, by ="Participant", all = TRUE)
# df_final <- merge(df_final, EF_raw, by ="Participant", all = TRUE)
# df_final <- merge(df_final, subj, by ="Participant", all = TRUE)
# 
# write.csv(df_final,"D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\data_final_all.csv", row.names = FALSE)
# 
# # Fidelity dataframe
# data <- data %>%
#   mutate(SessionNumber = case_when(
#     Session == "G02" ~ "TotalScore_1",
#     Session == "G04" ~ "TotalScore_2",
#     Session == "G06" ~ "TotalScore_3",
#     Session == "G08" ~ "TotalScore_4",
#     Session == "G10" ~ "TotalScore_5",
#   ))
# 
# # Now, pivot the data to have one row per participant and separate columns for each session's total score
# scores_per_participant <- data %>%
#   select(Pseudo, SessionNumber, TotalScore) %>%
#   spread(key = SessionNumber, value = TotalScore)
# 
# # Rename the first column to "Pseudo" if it's not already named that due to the pivot
# colnames(scores_per_participant)[1] <- "Pseudo"
# scores_per_participant <- scores_per_participant %>%
#   rename(Participant = Pseudo) %>%
#   mutate(Participant = gsub("_2$", "", Participant)) # Removes '_2' from the end of each participant identifier
# scores_per_participant <- scores_per_participant[, -ncol(scores_per_participant)]
# 
# # View the structure of the updated dataframe
# str(scores_per_participant)
# write.csv(scores_per_participant,"D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\SF_fidelity.csv", row.names = FALSE)

# Save APM
# df_final <- read.csv("D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\data_final_all.csv")
# df_sf_fidelity <- read.csv("D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\SF_fidelity.csv")
# SF_APM <- read.csv("D:/Google Drive/Professionnel/3_Post-doc_ISAE/RR_MicroStates/tmp_results/behavioral/SF_APM.csv")
# Merging df_final and SF_APM by Participant and Session
# merged_df <- merge(df_final, SF_APM, by = c("Participant", "Session"))

# write.csv(merged_df,"D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\data_final_all_APM.csv", row.names = FALSE)


###################################################
###################################################
################### SF analyses ###################
###################################################

## Load data and directory
plot_directory <- "D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\Article\\SF_EF\\Figures"

df_final <- read.csv("D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\data_final_all.csv")
df_sf_fidelity <- read.csv("D:\\Google Drive\\Professionnel\\3_Post-doc_ISAE\\RR_MicroStates\\tmp_results\\behavioral\\SF_fidelity.csv")

# WITHOUT OUTLIERS
df_final <- df_final[-c(10, 16, 17, 28, 31, 32, 34, 38, 48, 58, 62, 66, 79, 87, 88, 97, 104, 105, 106,114, 116, 141, 147, 163, 181, (nrow(df_final)-9):nrow(df_final)),]   # P010 => pro-gamer
df_sf_fidelity <- df_sf_fidelity[-c(10, 16, 17, 28, 31, 32, 34, 38, 48, 58, 62, 66, 79, 87, 88, 97, 104, 105, 106,114, 116, 141, 147, 163),]   # P010 => pro-gamer

#df_final <- df_final[-c(10, 31, 32, 34, 48, 58, 88, 97, 104, 106, (nrow(df_final)-9):nrow(df_final)),]   # P010 => pro-gamer
#df_final <- df_final[-c((nrow(df_final)-9):nrow(df_final)),]   # P010 => pro-gamer


## Demographics
# Calculate summary statistics for each variable
summary_stats <- df_final %>%
  summarise(
    MeanAge = mean(Age, na.rm = TRUE),
    SdAge = sd(Age, na.rm = TRUE),
    MinAge = min(Age, na.rm = TRUE),
    MaxAge = max(Age, na.rm = TRUE),
    MeanNET = mean(NiveauEtudes, na.rm = TRUE),
    SdNET = sd(NiveauEtudes, na.rm = TRUE),
    MinNET = min(NiveauEtudes, na.rm = TRUE),
    MaxNET = max(NiveauEtudes, na.rm = TRUE),
    MeanVGexp = mean(VGexp, na.rm = TRUE),
    SdVGexp = sd(VGexp, na.rm = TRUE),
    MinVGexp = min(VGexp, na.rm = TRUE),
    MaxVGexp = max(VGexp, na.rm = TRUE)
  )

# Convert the summary statistics into a long format
long_stats <- summary_stats %>%
  pivot_longer(cols = everything(), names_to = "Statistic", values_to = "Value")

# Create a more simplified table structure for display
# Separate statistics into their respective categories
stats_table <- tibble(
  Statistic = c("Mean", "SD", "Min", "Max"),
  Age = NA_real_, # Initialize columns with NA of type double
  NET = NA_real_,
  VGexp = NA_real_
)

# Populate the table with values
stats_table$Age <- c(
  long_stats$Value[long_stats$Statistic == "MeanAge"],
  long_stats$Value[long_stats$Statistic == "SdAge"],
  long_stats$Value[long_stats$Statistic == "MinAge"],
  long_stats$Value[long_stats$Statistic == "MaxAge"]
)
stats_table$NET <- c(
  long_stats$Value[long_stats$Statistic == "MeanNET"],
  long_stats$Value[long_stats$Statistic == "SdNET"],
  long_stats$Value[long_stats$Statistic == "MinNET"],
  long_stats$Value[long_stats$Statistic == "MaxNET"]
)
stats_table$VGexp <- c(
  long_stats$Value[long_stats$Statistic == "MeanVGexp"],
  long_stats$Value[long_stats$Statistic == "SdVGexp"],
  long_stats$Value[long_stats$Statistic == "MinVGexp"],
  long_stats$Value[long_stats$Statistic == "MaxVGexp"]
)

# Since Gender does not fit this format, we handle it separately
gender_count <- df_final %>%
  summarise(
    CountMen = sum(Genre == 'H', na.rm = TRUE),
    CountWomen = sum(Genre == 'F', na.rm = TRUE)
  )

# Display the tables
print(stats_table)
print(gender_count)


##Correlations between EFs and SF
# Generate Graph correlations
a <- ggplot(df_final, aes(y=zscore_SF, x=zscore_EF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  stat_cor(label.x = -1, label.y =2.5) +
  theme_pubr()+
  xlab("Executive Functions (z-score)")+
  ylab("Space Fortress (z-score)")+
  #ggtitle("n = 71") +
  theme(plot.title = element_text(hjust = 0.1))

b <- ggplot(df_final, aes(y=zscore_SF, x=zscore_shifting))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  stat_cor(label.x = -1.2, label.y = 2.5) +
  theme_pubr()+
  xlab("Shifting (z-score)")+
  ylab("Space Fortress (z-score)")+
  #ggtitle("Switching by SF") +
  theme(plot.title = element_text(hjust = 0.1))

c <- ggplot(df_final, aes(y=zscore_SF, x=zscore_inhibition))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  stat_cor(label.x = -1.6, label.y = 2.5) +
  theme_pubr()+
  xlab("Inhibition (z-score)")+
  ylab("Space Fortress (z-score)")+
  #ggtitle("Inhibition by SF") +
  theme(plot.title = element_text(hjust = 0.1))

d <- ggplot(df_final, aes(y=zscore_SF, x=zscore_WM))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  stat_cor(label.x = -2, label.y = 2.5) +
  theme_pubr()+
  xlab("Updating (z-score)")+
  ylab("Space Fortress (z-score)")+
  #ggtitle("WM by SF") +
  theme(plot.title = element_text(hjust = 0.1))

# Plot correlations between SF scores and EFs tests (global, switching, inhib, WM)
EF_SF <- plot_grid(a,b,c,d, ncol = 2, nrow = 2)
EFs_SF <- plot_grid(b,c,d, ncol = 3, nrow = 1)

filename <- paste0(plot_directory, "/EF_SF.pdf")
ggsave(filename, plot = a, width = 5, height = 4, units = "in")
filename <- paste0(plot_directory, "/EFs_SF.pdf")
ggsave(filename, plot = EFs_SF, width = 10, height = 3, units = "in")


#Correlation matrix for all EF tasks
# Select Space Fortress scores
space_fortress_scores <- df_final %>%
  select(TotalScore, Flight, Bonus, Mine, Fortress)

# Select cognitive task z-scores
cognitive_task_scores <- df_final %>%
  select(zscore_stroop, zscore_stopsignal, zscore_antisaccade, zscore_keeptrack, 
         zscore_lettermemory, zscore_dualnback, zscore_numberletter, zscore_categoryswitch, 
         zscore_colorshape)

# Initialize an empty matrix to store correlations
cor_matrix <- matrix(nrow = ncol(cognitive_task_scores), ncol = ncol(space_fortress_scores))

# Compute correlations
for (i in 1:ncol(cognitive_task_scores)) {
  for (j in 1:ncol(space_fortress_scores)) {
    cor_matrix[i, j] <- cor.test(cognitive_task_scores[[i]], space_fortress_scores[[j]], 
                                 use = "complete.obs")$estimate
  }
}

# Assign row and column names to the matrix
rownames(cor_matrix) <- colnames(cognitive_task_scores)
colnames(cor_matrix) <- colnames(space_fortress_scores)

# View the correlation matrix
cor_matrix

# Convert the matrix to a dataframe in long format
cor_df <- melt(cor_matrix)

# Rename columns for clarity
colnames(cor_df) <- c("CognitiveTask", "SpaceFortressScore", "Correlation")

# Plot
ggplot(cor_df, aes(x = SpaceFortressScore, y = CognitiveTask, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 3) +  # Add correlation coefficients
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Space Fortress Scores", y = "Cognitive Task Z-Scores", title = "Correlation between Cognitive Task Z-Scores and Space Fortress Scores") +
  coord_fixed()





#Correlation matrix for all EF tasks
# Select Space Fortress scores
space_fortress_scores <- df_final %>%
  select(TotalScore, Flight, Bonus, Mine, Fortress)

# Select cognitive task z-scores
cognitive_task_scores <- df_final %>%
  select(zscore_EF, zscore_inhibition, zscore_WM, zscore_shifting)

# Initialize an empty matrix to store correlations
cor_matrix <- matrix(nrow = ncol(cognitive_task_scores), ncol = ncol(space_fortress_scores))

# Compute correlations
for (i in 1:ncol(cognitive_task_scores)) {
  for (j in 1:ncol(space_fortress_scores)) {
    cor_matrix[i, j] <- cor.test(cognitive_task_scores[[i]], space_fortress_scores[[j]], 
                                 use = "complete.obs")$estimate
  }
}

# Assign row and column names to the matrix
rownames(cor_matrix) <- c("EF", "Inhibition", "Updating", "Shifting")
colnames(cor_matrix) <- c("SF", "Flight", "Bonus", "Mine", "Fortress")

# View the correlation matrix
cor_matrix

# Convert the matrix to a dataframe in long format
cor_df <- melt(cor_matrix)

# Rename columns for clarity
colnames(cor_df) <- c("CognitiveTask", "SpaceFortressScore", "Correlation")

# Plot
EFSF_matrix <- ggplot(cor_df, aes(x = SpaceFortressScore, y = CognitiveTask, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", Correlation)), color = "black", size = 3) +  # Add correlation coefficients
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), space = "Lab", name="Pearson\nCorrelation") +
  theme_minimal() +
  labs(x = "SF Scores", y = "EF composite scores") +
  coord_fixed()


filename <- paste0(plot_directory, "/EFSF_matrix.pdf")
ggsave(filename, plot = EFSF_matrix, width = 6, height = 4, units = "in")

##Demographics

#### TMP data's
# Generate Graph correlations with size of VG as covariate
ggplot(df_final, aes(x=zscore_SF, y=zscore_EF))+
  geom_point(aes(size = VGexp))+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  stat_cor(label.x = 1, label.y = -1) +
  theme_pubr()+
  ggtitle("EFs by SF") +
  theme(plot.title = element_text(hjust = 0.1))

##Age
# EF & Age
age_EF <- ggplot(df_final, aes(x=Age, y=zscore_EF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  stat_cor(label.x = 20, label.y = 1) +
  theme_pubr()+
  xlab("Age")+
  ylab("Executive Functions (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# SF & Age
age_SF <- ggplot(df_final, aes(x=Age, y=zscore_SF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  stat_cor(label.x = 20, label.y = 2.5) +
  theme_pubr()+
  xlab("Age")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))
plot_grid(age_EF,age_SF, ncol = 2, nrow = 1)


## Video Game Questionnaire
# EF & VG
EF_VG <- ggplot(df_final, aes(x=VGexp, y=zscore_EF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  stat_cor(label.x = 1, label.y = 1) +
  theme_pubr()+
  xlab("Video Game Experience")+
  ylab("Executive Functions (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# SF & VG
SF_VG <- ggplot(df_final, aes(x=VGexp, y=zscore_SF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  stat_cor(label.x = 1, label.y = 2.5) +
  theme_pubr()+
  xlab("Video Game Experience")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))
plot_grid(EF_VG,SF_VG, ncol = 2, nrow = 1)

# Education Level
# EF & NET
NET_EF <- ggplot(df_final, aes(x=NiveauEtudes, y=zscore_EF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  stat_cor(label.x = 1, label.y =1) +
  theme_pubr()+
  xlab("Education Level")+
  ylab("Executive Functions (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))

# SF & NET
NET_SF <- ggplot(df_final, aes(x=NiveauEtudes, y= zscore_SF))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  stat_cor(label.x = 1, label.y = 2.5) +
  theme_pubr()+
  xlab("Education Level")+
  ylab("Space Fortress (z-score)")+
  theme(plot.title = element_text(hjust = 0.1))
plot_grid(NET_EF,NET_SF, ncol = 2, nrow = 1)


#SF demographics plots
SF_demographics <- plot_grid(age_SF,NET_SF,SF_VG, ncol = 3, nrow = 1)
filename <- paste0(plot_directory, "/SF_demographics.pdf")
ggsave(filename, plot = SF_demographics, width = 10, height = 3, units = "in")

# SF & EFs + Genre
df_final <- df_final %>%
  filter(Genre != "Autre")
SF_sex <- ggplot(df_final, aes(x=zscore_SF, y=zscore_EF, color=Genre))+
  geom_point()+
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  theme_pubr()+
  ylab("Executive Functions (z-score)")+
  xlab("Space Fortress (z-score)")+
  #ggtitle("n = 71") +
  theme(plot.title = element_text(hjust = 0.1))
SF_sex <- ggMarginal(SF_sex, groupColour = TRUE, groupFill = TRUE)
SF_sex
filename <- paste0(plot_directory, "/SF_sex.pdf")
ggsave(filename, plot = SF_sex, width = 5, height = 5, units = "in")


# SF & EFs + correlation by Genre
p <- ggplot(df_final, aes(x=zscore_SF, y=zscore_EF, color=Genre)) +
  geom_point() +
  geom_smooth(method=lm , color="black", fill="lightgray", se=TRUE) +
  geom_smooth(data = subset(df_final, Genre == "H"), method = "lm", se = TRUE, aes(group = Genre)) +
  geom_smooth(data = subset(df_final, Genre == "F"), method = "lm", se = TRUE, aes(group = Genre)) +
  theme_pubr() +
  ylab("Executive Functions (z-score)") +
  xlab("Space Fortress (z-score)") +
  theme(plot.title = element_text(hjust = 0.1))
p3 <- ggMarginal(p, groupColour = TRUE, groupFill = TRUE)
p3

# Means
df_final %>%
  group_by(Genre) %>%
  summarise(
    mean_zscore_EF = mean(zscore_EF, na.rm = TRUE),
    mean_zscore_SF = mean(zscore_SF, na.rm = TRUE),
    mean_VGexp = mean(VGexp, na.rm = TRUE),
    mean_Age = mean(Age, na.rm = TRUE),
    mean_NiveauEtudes = mean(VGexp, na.rm = TRUE)
  )

# Explication score SF
model1 <- lm(formula = zscore_SF ~ zscore_EF + Age + NiveauEtudes + VGexp + Genre, data = df_final)
summary(model1)

# Convert the model summary to an xtable
model1_table <- xtable(summary(model1))

# Print the xtable with LaTeX formatting
print(model1_table, type = "latex", include.rownames = FALSE)
# Explication score FE
model2 <- lm(formula = zscore_EF ~ Age + NiveauEtudes + Genre, data = df_final)
summary(model2)

###############################
####### SF psychometrics ######
###############################

## SF sensitivity 
# Assuming df_final is your dataframe and TotalScore is the column of interest
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
# 
# ##Supplementary analyses : zscores
# # Compute z-scores for specified columns and convert them to vectors
# df_final$zscore_Flight <- as.vector(scale(df_final$Flight, center = TRUE, scale = TRUE))
# df_final$zscore_Bonus <- as.vector(scale(df_final$Bonus, center = TRUE, scale = TRUE))
# df_final$zscore_Mine <- as.vector(scale(df_final$Mine, center = TRUE, scale = TRUE))
# df_final$zscore_Fortress <- as.vector(scale(df_final$Fortress, center = TRUE, scale = TRUE))
# 
# # Compute the average z-score of the subtasks
# df_final$zscore_SF_subtasks <- rowMeans(df_final[, c("zscore_Flight", "zscore_Bonus", "zscore_Mine", "zscore_Fortress")], na.rm = TRUE)
# 
# #Density plots
# # Plot for zscore_SF_subtasks
# ggplot(df_final, aes(x = zscore_SF_subtasks)) +
#   geom_density(color = "black", fill = "grey", alpha = .5) +
#   geom_rug() +
#   theme_pubr() +
#   ylab("Density") +
#   xlab("SF Subtasks Z-Score")
# 
# # Plot for zscore_Flight
# ggplot(df_final, aes(x = zscore_Flight)) +
#   geom_density(color = "black", fill = "grey", alpha = .5) +
#   geom_rug() +
#   theme_pubr() +
#   ylab("Density") +
#   xlab("Flight Z-Score")
# 
# # Plot for zscore_Bonus
# ggplot(df_final, aes(x = zscore_Bonus)) +
#   geom_density(color = "black", fill = "grey", alpha = .5) +
#   geom_rug() +
#   theme_pubr() +
#   ylab("Density") +
#   xlab("Bonus Z-Score")
# 
# # Plot for zscore_Mine
# ggplot(df_final, aes(x = zscore_Mine)) +
#   geom_density(color = "black", fill = "grey", alpha = .5) +
#   geom_rug() +
#   theme_pubr() +
#   ylab("Density") +
#   xlab("Mine Z-Score")
# 
# 
# # correlations
# # Plot for zscore_SF vs. zscore_SF_subtasks
# ggplot(df_final, aes(x=zscore_SF, y=zscore_SF_subtasks)) +
#   geom_point() +
#   geom_smooth(method="lm", color="black", fill="lightgray", se=TRUE) +
#   stat_cor(method = "pearson", label.x = -2, label.y = 1) +
#   theme_pubr() +
#   xlab("SF Z-Score") +
#   ylab("SF Subtasks Z-Score") +
#   ggtitle("Correlation between SF Z-Score and SF Subtasks Z-Score") +
#   theme(plot.title = element_text(hjust = 0.5))
# 
# # Plot for zscore_EF vs. zscore_SF_subtasks
# ggplot(df_final, aes(x=zscore_EF, y=zscore_SF_subtasks)) +
#   geom_point() +
#   geom_smooth(method="lm", color="black", fill="lightgray", se=TRUE) +
#   stat_cor(method = "pearson", label.x = -1, label.y = 1.5) +
#   theme_pubr() +
#   xlab("EF Z-Score") +
#   ylab("SF Subtasks Z-Score") +
#   ggtitle("Correlation between EF Z-Score and SF Subtasks Z-Score") +
#   theme(plot.title = element_text(hjust = 0.5))
