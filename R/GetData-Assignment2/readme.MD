###This code takes the activity-tracking dataset provided from Samsung in the Getting and Cleaning Data course and identifies the mean for each of 33 measures, for each of 30 subjects, for each of 6 activities.

_Start with the zipped folder in your home directory._
_This uses readr, tidyr, and dplyr packages, and all their dependencies._
_The file can be run as one, with an output of the txt file that I submitted for part one, or can be run in two sets: one to create the first bit (until final_data has been created) and the second half of the code to create the tidied data set._


####Instructions:
  
  
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
