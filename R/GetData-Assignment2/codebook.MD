###This code takes the activity-tracking dataset provided from Samsung in the Getting and Cleaning Data course and identifies the mean for each of 33 measures, for each of 30 subjects, for each of 6 activities.



####Dictionary:

* dataset is a preliminary dataset with the bound subjects, labels, and datasets

* subject_activity is a subselected table of dataset, containing just the subject and label information
* mean contains all the columns that identified means of a particular measure
* stdev contains all the columns that identified standard deviations of a particular measure
* dataset2 is the set the contains the information we actually want, excluding all the columns we aren't using
* final_data is the finalized version of dataset2, with the activities renamed

* set contains the means for each measure for each subject and activity pair
* toset takes those means and binds them to the unique subject-activity pairs, eliminating duplicates
* data is the final data set to be written


####Order:
  
* We start out by unzipping the zipfile.
* We load readr, dplyr, and tidyr packages
* We read in the necessary tables using read\_delim and read\_table
* We name the sets with their measurement names, indicated in the features set
* We bind together the test and training sets for the data, subject data, and label data
* We name the unnamed columns
* We bind all of the data together into one set
* We use dplyr to single out the subject and activity data, the means, and the standard deviations, and then bind them together
* We change the activity codes into appropriate activity names
* We read the data into a table called final_data and remove all the now-unnecessary data tables from the environment.
* Using mutate, we create a column concatenating the subject and activity so that we can find our desired unique primary index, then identify the number of unique variables
* We use a "for 'i' in" loop to subset the variables that identify with each index variable set and calculate the column means
* We reset the names appropriately
* We bind the unique variable (subject-activity set) with its associated mean
* Vectorize that index and split it back into its individual parts, setting them as subject and activity, by splitting on ";" and identifying which side translates to integer (subject)
* Bind those back to the set
* Remove the initializing number and space preceding the activity name
* Translate it back into a data frame and write it to a txt file