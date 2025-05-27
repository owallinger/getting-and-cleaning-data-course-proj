library(data.table)
library(dplyr)
library(tidyr)
data_dir <- "UCI HAR Dataset"
zip_file <- "dataFiles.zip"
if (!file.exists(data_dir)) {
  download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", zip_file)
  unzip(zip_file)
}
features <- fread(file.path(data_dir, "features.txt"), col.names = c("index", "name"))
activity_labels <- fread(file.path(data_dir, "activity_labels.txt"), col.names = c("code", "activity"))
selected_features <- grep("(mean|std)\\(\\)", features$name)
feature_names <- gsub("[()]", "", features$name[selected_features])
load_dataset <- function(type) {
  x <- fread(file.path(data_dir, type, paste0("X_", type, ".txt")))[, ..selected_features]
  setnames(x, feature_names)
  y <- fread(file.path(data_dir, type, paste0("Y_", type, ".txt")), col.names = "Activity")
  subject <- fread(file.path(data_dir, type, paste0("subject_", type, ".txt")), col.names = "Subject")
  bind_cols(subject, y, x)
}
train_data <- load_dataset("train")
test_data <- load_dataset("test")
full_data <- bind_rows(train_data, test_data)
full_data <- full_data %>%
  mutate(Activity = factor(Activity, levels = activity_labels$code, labels = activity_labels$activity),
         Subject = as.factor(Subject))
tidy_data <- full_data %>%
  pivot_longer(-c(Subject, Activity), names_to = "Variable", values_to = "Value") %>%
  group_by(Subject, Activity, Variable) %>%
  summarise(Average = mean(Value), .groups = "drop") %>%
  pivot_wider(names_from = "Variable", values_from = "Average")
fwrite(tidy_data, "tidyData.txt", quote = FALSE)
