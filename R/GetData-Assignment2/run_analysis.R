unzip("~/getdata-projectfiles-UCI HAR Dataset.zip") -> files

  library(readr)
  library(tidyr)
  library(dplyr)

    read_delim(files[1], delim = "/n", col_names = FALSE) -> activity_labels
    read_delim(files[2], delim = "/n", col_names = FALSE) -> features
    read_delim(files[3], delim = "/n", col_names = FALSE) -> features_info
    read_delim(files[14], delim = "/n", col_names = FALSE) -> subject_test
    read_delim(files[16], delim = "/n", col_names = FALSE) -> test_labels
    read_delim(files[26], delim = "/n", col_names = FALSE) -> subject_train
    read_delim(files[28], delim = "/n", col_names = FALSE) -> training_labels

    read_table(files[15], col_names = F) -> test_set
    read_table(files[27], col_names = F) -> training_set

      colnames(test_set) <- features[[1]]
      colnames(training_set) <- features[[1]]
        rbind(test_set,training_set) -> set
        rbind(subject_test,subject_train) -> subjects
        rbind(test_labels,training_labels) -> labels

      colnames(subjects) <- c("subject")
      colnames(labels) <- c("activity")
        cbind(subjects,labels,set) -> dataset

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
  unique(final_data$variable) -> var

set <- as.data.frame(NULL)
for (i in 1:length(var)) {
  mm <- final_data[which(final_data$variable == var[i]),]
  mm2 <- colMeans(mm[3:68])
  set <- rbind(mm2, set)}

  names(set) <- names(final_data[3:68])

toset <- cbind(unique(final_data$variable), unique(set))
  as.vector(toset[,1]) -> labels
    strsplit(labels, "; ") -> t
    unlist(t) -> labels
    rownames(toset) <- NULL
  intlb <- as.integer(labels)
    subject <- labels[which(complete.cases(intlb) == T)]
    activity <- labels[which(complete.cases(intlb) == F)]
      cbind(subject, activity,set) -> data
      rownames(data) <- NULL
      as.data.frame(data) -> data
      as.vector(names(data)) -> nms
    strsplit(nms, " ") -> t
    unlist(t) -> nmk
      intnm <- as.integer(nmk)
      names <- nmk[which(complete.cases(intnm) == F)]
        names(data) <- names

write.table(data, "~/data.txt", row.name = F, quote = FALSE)
