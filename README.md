# Ryann-Lubeck-s-repo
Repository for testing my Git/GitHub setup


library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
library(tidyverse)
library(caret) 
library(xgboost)
library(pROC)
library(reshape2)
library(gridExtra)
session=list()
for(i in 1:18){
  session[[i]]=readRDS(paste('/Users/ryannlubeck/Downloads/Data/session',i,'.rds',sep=''))
}

# Section 2: Exploratory Data Analysis
## Data Structure
session_summary <- map_df(1:18, function(i) {
  num_trials <- length(session[[i]]$spks)
  num_neurons <- if(num_trials > 0) nrow(session[[i]]$spks[[1]]) else 0
  tibble(Session = i, 
         Number_of_Trials = num_trials, 
         Number_of_Neurons = num_neurons)
})
print(session_summary)
summary_info <- lapply(session, function(s) {
  data.frame(
    Mouse = s$mouse_name,
    Mean_Contrast_Left = mean(s$contrast_left),
    Mean_Contrast_Right = mean(s$contrast_right),
    Num_Success = sum(s$feedback_type == 1),
    Num_Failure = sum(s$feedback_type == -1)
  )
})
summary_df <- bind_rows(summary_info)
print(summary_df)

all_brain_areas <- unique(unlist(lapply(session, function(s) {
  if (!is.null(s$brain_area) && length(s$brain_area) > 0) {
    return(unique(s$brain_area))
  } else {
    return(NULL)
  }
})))
brain_areas_by_session <- lapply(session, function(s) {
  if (!is.null(s$brain_area) && length(s$brain_area) > 0) {
    area_counts <- table(s$brain_area)
    area_counts <- setNames(as.numeric(area_counts), names(area_counts))
    all_areas_count <- setNames(vector("numeric", length(all_brain_areas)), all_brain_areas)
    all_areas_count[names(area_counts)] <- area_counts
    return(all_areas_count)
  } else {
    return(setNames(rep(0, length(all_brain_areas)), all_brain_areas))
  }
})
brain_area_df <- data.frame(matrix(unlist(brain_areas_by_session), nrow = length(brain_areas_by_session), byrow = TRUE), stringsAsFactors = FALSE)
colnames(brain_area_df) <- all_brain_areas
brain_area_df$Session <- 1:nrow(brain_area_df)
brain_area_long <- tidyr::pivot_longer(brain_area_df, -Session, names_to = "Brain_Area", values_to = "Count")
brain_area_long$Count <- as.numeric(brain_area_long$Count)
filtered_brain_area_long <- brain_area_long %>%
  filter(Count > 0)
ggplot(filtered_brain_area_long, aes(x = factor(Session), y = Count, fill = Brain_Area)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  theme_minimal() +
  labs(title = "Brain Area Distribution Across Sessions (Non-zero Counts)", x = "Session", y = "Count") +
  scale_fill_brewer(palette = "Set3")
  
feedback_types <- map_dfr(session, ~as.data.frame(table(.x$feedback_type)), .id = 'session')
ggplot(feedback_types, aes(x = Var1, y = Freq, fill = session)) + 
  geom_bar(stat = 'identity') +
  labs(x = 'Feedback Type', y = 'Frequency') +
  theme_minimal()
  
session_list = list()
success_rates = data.frame(Mouse=character(), Session=integer(), TotalTrials=integer(), Successes=integer(), SuccessRate=numeric(), stringsAsFactors=FALSE)
for(i in 1:18){
    session_data <- readRDS(paste('/Users/ryannlubeck/Downloads/Data/session',i,'.rds',sep=''))
    session_list[[i]] <- session_data
    total_trials <- length(session_data$feedback_type)
    successes <- sum(session_data$feedback_type == 1)
    success_rate <- successes / total_trials
    success_rates <- rbind(success_rates, data.frame(Mouse=session_data$mouse_name, 
                                                     Session=i, 
                                                     TotalTrials=total_trials, 
                                                     Successes=successes, 
                                                     SuccessRate=success_rate))
}
print(success_rates)
session_list = list()
success_rates = data.frame(Mouse=character(), Session=integer(), TotalTrials=integer(), Successes=integer(), SuccessRate=numeric(), stringsAsFactors=FALSE)
for(i in 1:18){
    session_data <- readRDS(paste('/Users/ryannlubeck/Downloads/Data/session',i,'.rds',sep=''))
    session_list[[i]] <- session_data
    total_trials <- length(session_data$feedback_type)
    successes <- sum(session_data$feedback_type == 1)
    success_rate <- successes / total_trials
    success_rates <- rbind(success_rates, data.frame(Mouse=session_data$mouse_name, 
                                                     Session=i, 
                                                     TotalTrials=total_trials, 
                                                     Successes=successes, 
                                                     SuccessRate=success_rate))
}
success_rates_by_mouse <- success_rates %>%
  group_by(Mouse) %>%
  summarise(TotalTrials = sum(TotalTrials),
            TotalSuccesses = sum(Successes),
            AverageSuccessRate = mean(SuccessRate))
print(success_rates_by_mouse)

## Neural Activity for each Trial

all_sessions_avg_activity <- lapply(1:length(session), function(i) {
  if(length(session[[i]]$spks) > 0) {
    apply(session[[i]]$spks[[1]], 2, mean) 
  } else {
    rep(NA, ncol(session[[1]]$spks[[1]])) 
  }
})
activity_matrix <- do.call(cbind, all_sessions_avg_activity)
matplot(t(activity_matrix), type = 'l', main = 'Average Neural Activity Across Sessions', xlab = 'Time Bins', ylab = 'Average Activity', col = 1:18)
legend("topright", legend = paste('Session', 1:18), col = 1:18, lty = 1, cex = 0.5)

brain_area_counts <- do.call(rbind, lapply(brain_areas_by_session, function(x) {
    as.data.frame(t(x))
}))
colnames(brain_area_counts) <- all_brain_areas
brain_area_counts$Session <- 1:nrow(brain_area_counts)
brain_area_long <- tidyr::pivot_longer(brain_area_counts, -Session, names_to = "Brain_Area", values_to = "Neuron_Count")
filtered_brain_area_long <- brain_area_long %>%
  group_by(Brain_Area) %>%
  filter(sum(Neuron_Count) > 0) %>%
  ungroup()
ggplot(filtered_brain_area_long, aes(x = factor(Session), y = reorder(Brain_Area, Neuron_Count), fill = Neuron_Count)) +
  geom_tile() + 
  scale_fill_gradient(low = "white", high = "blue") + 
  labs(title = "Neuron Distribution Across Sessions by Brain Area (Filtered)", x = "Session", y = "Brain Area") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
## Changes Across Trials
set.seed(123)  
num_sessions <- 18  
num_trials_per_session <- 500  
sessions <- list()
for (i in 1:num_sessions) {
  feedback_type <- sample(c(1, -1), num_trials_per_session, replace = TRUE)
  session_df <- data.frame(trial_id = 1:num_trials_per_session, feedback_type = feedback_type)
  sessions[[i]] <- session_df
}
plot_success_rate_binned <- function(session_data, session_number) {
  session_data$trial_bin <- ceiling(row_number(session_data) / 25)
  summary_data <- session_data %>%
    group_by(trial_bin) %>%
    summarize(success_rate = mean(feedback_type == 1, na.rm = TRUE))
  p <- ggplot(summary_data, aes(x = as.factor(trial_bin), y = success_rate)) +
    geom_bar(stat = "identity") +
    ylim(0, 1) +  
    labs(title = paste("Session", session_number), x = "Trial Bin (Each bin = 25 trials)", y = "Success Rate") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  
  return(p)
}
plot_list <- lapply(1:length(sessions), function(i) plot_success_rate_binned(sessions[[i]], i))
do.call(gridExtra::grid.arrange, c(plot_list[1:6], ncol = 3))  
do.call(gridExtra::grid.arrange, c(plot_list[7:12], ncol = 3))  
do.call(gridExtra::grid.arrange, c(plot_list[13:18], ncol = 3)) 

plot_list <- list()
for (i in 1:18) {
    mean_spike_rates <- data.frame(trial_id = integer(), mean_spike = numeric())
    for (j in 1:length(session[[i]]$spks)) {
        trial_spike_sum <- rowSums(session[[i]]$spks[[j]])  
        trial_mean_spike_rate <- mean(trial_spike_sum) / ncol(session[[i]]$spks[[j]])  
        mean_spike_rates <- rbind(mean_spike_rates, data.frame(trial_id = j, mean_spike = trial_mean_spike_rate))
    }
    p <- ggplot(mean_spike_rates, aes(x = trial_id, y = mean_spike)) +
        geom_line() +  
        geom_smooth(method = 'loess', color = "blue", se = FALSE) +  
        theme_minimal() +
        ggtitle(paste("Session", i)) +  
        labs(x = "Trial ID", y = "Mean Spike Rate (spikes/bin)")  
    plot_list[[i]] <- p
}
do.call(gridExtra::grid.arrange, c(plot_list[1:6], ncol = 3))  
do.call(gridExtra::grid.arrange, c(plot_list[7:12], ncol = 3))  
do.call(gridExtra::grid.arrange, c(plot_list[13:18], ncol = 3)) 

## Homogeneity & Heterogeneity 
contrast_differences = numeric()
for(session in session_list) {
    for(i in 1:length(session$contrast_left)) {
        diff = abs(session$contrast_left[i] - session$contrast_right[i])
        rounded_diff = round(diff * 4) / 4 
        contrast_differences = c(contrast_differences, rounded_diff)
    }
}
unique_differences = unique(contrast_differences)
contrast_diff_df = data.frame('ContrastDifference' = sort(unique_differences), 'Frequency' = integer(length(unique_differences)))
for(i in 1:length(unique_differences)) {
    contrast_diff_df$Frequency[i] = sum(contrast_differences == contrast_diff_df$ContrastDifference[i])
}
print(contrast_diff_df)
contrast_differences <- numeric()
successes <- numeric()
for(session in session_list) {
    for(i in 1:length(session$contrast_left)) {
        diff = abs(session$contrast_left[i] - session$contrast_right[i])
        rounded_diff = round(diff * 4) / 4 
        success = ifelse(session$feedback_type[i] == 1, 1, 0) 
        contrast_differences <- c(contrast_differences, rounded_diff)
        successes <- c(successes, success)
    }
}
data <- data.frame(ContrastDifference = contrast_differences, Success = successes)
success_rates_by_contrast <- data %>%
  group_by(ContrastDifference) %>%
  summarise(TotalTrials = n(),
            Successes = sum(Success),
            SuccessRate = mean(Success) * 100) %>% 
  arrange(ContrastDifference)
print(success_rates_by_contrast)

# Section 3: Data Integration
num_sessions <- 18
sessions <- list()
for (i in 1:num_sessions) {
  sessions[[i]] <- readRDS(paste('/Users/ryannlubeck/Downloads/Data/session',i,'.rds',sep=''))
  sessions[[i]]$session_id <- rep(i, length(sessions[[i]]$spks))
}
preprocess_spikes <- function(spikes) {
  max_spikes <- max(spikes)
  min_spikes <- min(spikes)
  norm_spikes <- (spikes - min_spikes) / (max_spikes - min_spikes)
  return(norm_spikes)
}
combined_data <- list()
for (i in 1:num_sessions) {
  session_data <- sessions[[i]]
  for (j in 1:length(session_data$spks)) {
    trial_data <- list(
      feedback_type = session_data$feedback_type[j],
      contrast_left = session_data$contrast_left[j],
      contrast_right = session_data$contrast_right[j],
      brain_area = session_data$brain_area,
      session_id = i,
      trial_id = j
    )
    combined_data[[length(combined_data) + 1]] <- trial_data
  }
}
combined_df <- map_df(combined_data, ~{
  tibble(
    feedback_type = .x$feedback_type,
    contrast_left = .x$contrast_left,
    contrast_right = .x$contrast_right,
    brain_area = .x$brain_area,
    session_id = .x$session_id,
    trial_id = .x$trial_id,
  )
})
combined_df <- combined_df %>%
  mutate(contrast_difference = contrast_left - contrast_right)
combined_df

# Section 4: Predictive Model
predictive_dat <- combined_df %>% 
  mutate(feedback_type = ifelse(feedback_type == -1, 0, 1))
predictive_dat$brain_area <- as.numeric(as.factor(predictive_dat$brain_area))
label <- predictive_dat$feedback_type
X <- predictive_dat %>% select(-feedback_type) %>% as.matrix()
set.seed(123) # for reproducibility
trainIndex <- createDataPartition(label, p = .8, list = FALSE, times = 1)
train_df <- predictive_dat[trainIndex, ]
test_df <- predictive_dat[-trainIndex, ]
train_X <- X[trainIndex,]
test_X <- X[-trainIndex,]
train_label <- label[trainIndex]
test_label <- label[-trainIndex]
xgb_model <- xgboost(data = train_X, label = train_label, objective = "binary:logistic", nrounds = 10, verbose = 0)
predictions <- predict(xgb_model, newdata = test_X)
predicted_labels <- ifelse(predictions > 0.5, 1, 0)
accuracy <- mean(predicted_labels == test_label)
print(paste("Accuracy:", accuracy))
conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(test_label))
print(conf_matrix$table)
auroc <- roc(response = as.numeric(test_label), predictor = predictions)
print(auroc)

# Section 5: Prediction Performance on the Test Sets
num_test_sessions <- 2 
test_sessions <- list()
for (i in 1:num_test_sessions) {
  test_sessions[[i]] <- readRDS(paste('/Users/ryannlubeck/Downloads/tests/test', i, '.rds', sep=''))
  test_sessions[[i]]$session_id <- rep(i, length(test_sessions[[i]]$spks))
}
test_combined_data <- list()
for (i in 1:num_test_sessions) {
  session_data <- test_sessions[[i]]
  for (j in 1:length(session_data$spks)) {
    trial_data <- list(
      feedback_type = session_data$feedback_type[j],  
      contrast_left = session_data$contrast_left[j],
      contrast_right = session_data$contrast_right[j],
      brain_area = session_data$brain_area,
      session_id = i,
      trial_id = j
    )
    test_combined_data[[length(test_combined_data) + 1]] <- trial_data
  }
}
test_combined_df <- map_df(test_combined_data, ~{
  tibble(
    feedback_type = .x$feedback_type, 
    contrast_left = .x$contrast_left,
    contrast_right = .x$contrast_right,
    brain_area = .x$brain_area,
    session_id = .x$session_id,
    trial_id = .x$trial_id,
  )
})
test_combined_df <- test_combined_df %>%
  mutate(contrast_difference = contrast_left - contrast_right)
test_combined_df
predictive_dat <- test_combined_df %>% 
  mutate(feedback_type = ifelse(feedback_type == -1, 0, 1))
predictive_dat$brain_area <- as.numeric(as.factor(predictive_dat$brain_area))
label <- predictive_dat$feedback_type
X <- predictive_dat %>% select(-feedback_type) %>% as.matrix()
set.seed(123) 
trainIndex <- createDataPartition(label, p = .8, list = FALSE, times = 1)
train_df <- predictive_dat[trainIndex, ]
test_df <- predictive_dat[-trainIndex, ]
train_X <- X[trainIndex,]
test_X <- X[-trainIndex,]
train_label <- label[trainIndex]
test_label <- label[-trainIndex]
xgb_model <- xgboost(data = train_X, label = train_label, objective = "binary:logistic", nrounds = 10, verbose = 0)
predictions <- predict(xgb_model, newdata = test_X)
predicted_labels <- ifelse(predictions > 0.5, 1, 0)
accuracy <- mean(predicted_labels == test_label)
print(paste("Accuracy:", accuracy))
conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(test_label))
print(conf_matrix$table)
auroc <- roc(response = as.numeric(test_label), predictor = predictions)
print(auroc)
  
  
  
  
