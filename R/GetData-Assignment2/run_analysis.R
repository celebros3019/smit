unzip("~/getdata-projectfiles-UCI HAR Dataset.zip")

library(readr)
library(tidyr)

read_table("features_info.txt", col_names = F) -> features_info
read_table("features.txt", col_names = F) -> features
read_table("activity_labels.txt", col_names = F) -> activity_labels
read_table("train/X_train.txt", col_names = F) -> training_set
read_table("train/y_train.txt", col_names = F) -> training_labels
read_table("test/X_test.txt", col_names = F) -> test_set
read_table("test/y_test.txt", col_names = F) -> test_labels
read_table("train/subject_train.txt", col_names = F) -> subject_train
read_table("test/subject_test.txt", col_names = F) -> subject_test

colnames(test_set) <- features[[1]]
colnames(training_set) <- features[[1]]

rbind(test_set,training_set) -> set
rbind(subject_test,subject_train) -> subjects
rbind(test_labels,training_labels) -> labels
colnames(subjects) <- c("subject")
colnames(labels) <- c("activity")

cbind(subjects,labels,set) -> data1

require(dplyr)
data1 -> dataset

select(dataset, c(1:2)) -> subject_activity
select(dataset, contains ("-mean()")) -> mean
select(dataset, contains ("-std()")) -> stdev

cbind(subject_activity,mean,stdev) -> dataset2

dataset2[,2][which(dataset2[,2] == 1)] <- "Walk"
dataset2[,2][which(dataset2[,2] == 2)] <- "Walk_Upstairs"
dataset2[,2][which(dataset2[,2] == 3)] <- "Walk_Downstairs"
dataset2[,2][which(dataset2[,2] == 4)] <- "Sit"
dataset2[,2][which(dataset2[,2] == 5)] <- "Stand"
dataset2[,2][which(dataset2[,2] == 6)] <- "Lay_Down"

final_data <- dataset2



mutate(final_data, variable = paste(final_data[,1], final_data[,2], sep = "; ")) -> final_data


set <- NULL
for (i in unique(final_data$variable)) {
  mm <- final_data[which(final_data$variable == i),]
  mm2 <- colMeans(mm[3:68])
  set <- rbind(set,mm2)}

toset <- cbind(unique(final_data$variable), unique(set))
as.vector(toset[,1])
as.vector(toset[,1]) -> labels
strsplit(labels, "; ") -> t
unlist(t) -> labels
rownames(toset) <- NULL
asdf <- as.integer(labels)
subject <- labels[which(complete.cases(asdf) == T)]
activity <- labels[which(complete.cases(asdf) == F)]
cbind(subject, activity,set) -> data
rownames(data) <- NULL
as.data.frame(data) -> data
write.table(data, "~/data.txt", row.name = F)