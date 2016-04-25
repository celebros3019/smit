###This code takes the activity-tracking dataset provided from Samsung in the Getting and Cleaning Data course and identifies the mean for each of 33 measures, for each of 30 subjects, for each of 6 activities.

_Start with the zipped folder in your home directory._
_This uses readr, tidyr, and dplyr packages, and all their dependencies._
_The file can be run as one, with an output of the txt file that I submitted for part one, or can be run in two sets: one to create the first bit (until the final data set has been created) and the second half of the code to create the tidied data set._


####Instructions:
  
  
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

#### Now we move on to creating a clean data set:

* Using mutate, we create a column concatenating the subject and activity so that we can find our desired unique primary index, then identify the number of unique variables

* We use a "for 'i' in" loop to subset the variables that identify with each index variable set and calculate the column means

* We reset the names appropriately

* We bind the unique variable (subject-activity set) with its associated mean

* Vectorize that index and split it back into its individual parts, setting them as subject and activity, by splitting on ";" and identifying which side translates to integer (subject)

* Bind those back to the set

* Remove the initializing number and space preceding the activity name

* Translate it back into a data frame and write it to a txt file