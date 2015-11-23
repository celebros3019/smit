###This code takes the activity-tracking dataset provided from Samsung in the Getting and Cleaning Data course
###and identifies the mean for each of 33 measures, for each of 30 subjects, for each of 6 activities.


Order:
  
* We start out by unzipping the zipfile.
* We load readr and tidyr packages
* We read in the necessary tables using read_table

* We name the sets with their measurement names, indicated in the features set

* We bind together the test and training sets for the data, subject data, and label data
* We name the unnamed columns
* We bind all of the data together into one set

* We use dplyr to make a neater, easier-to-read set using tbl_df
* We single out the subject and activity data, the means, and the standard deviations, and then bind them together
* We change the activity codes into appropriate activity names

* We read the data into a table called final_data and remove all the now-unnecessary data tables from the environment.


* Now we move on to creating a clean data set:
  - Using mutate, we create a column concatenating the subject and activity so that we can find our desired unique primary index
* We use a "for 'i' in" loop to subset the variables that identify with each index variable set, calculate the column means, and bind them together
* We bind the unique variable with that mean
* Vectorize that index and split it back into its individual parts, setting them as subject and activity
* Bind those back to the set
* Translate it back into a data frame and write it to a txt file






###Dictionary:

* data1 is a preliminary dataset with the bound subjects, labels, and datasets
* dataset is a more finalized version of data1

* subject_activity is a subselected table of dataset, containing just the subject and label information
* mean contains all the columns that identified means of a particular measure
* stdev contains all the columns that identified standard deviations of a particular measure
* dataset2 is the set the contains the information we actually want, excluding all the columns we aren't using
* final_data is the finalized version of dataset2, with the activities renamed

* set contains the means for each measure for each subject and activity pair
* toset is the unique version of set, eliminating duplicates
* data is the final data set to be written