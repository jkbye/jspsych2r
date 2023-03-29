# jsPsych demo for the In_Data files labeled s23 (Spring 2023 semester demo)
# run demo here: https://run.pavlovia.org/jkbye/epsy5123_wk9/

source("jsPsych_to_R_functions.R")

# Step 0: load in example data file to check out
ex_data <- read.csv("In_Data/epsy5123_s23_jspsych_demo_0.csv")
ex_data # check it out!

# Step 1: notice the plugin types used:
ex_data$trial_type
#   pavlovia
#   html-keyboard-response
#   image-slide-response
#   sketchpad
#   survey-likert
#   survey-text

# these are all built in to the parse_study() function 
#   *except* sketchpad (so far) which we'll ignore for now :)

# if your own work uses multi-select, please see the s22 demo for examples


# Step 2: test out on ex_data
parse_study(ex_data) # this works (the warnings are no prefix set, so not unique names)
parse_study(ex_data, multi_list, prefix = TRUE, rt = TRUE) # or with prefix and RT


# Step 3: now we can apply to all relevant files

files <- list.files("In_Data", pattern = "s23.*csv$", full.names = TRUE) 
# above we select just those w/ "s23" in the filename and ending in "csv"
# fullnames allows us to preserve the In_Data/ relative path
# note: if your In_Data folder only has relevant files, this is unnecessary

parse_df <- NULL # initialization of empty df
for (f in files) { # for each file f
  cur_df <- read.csv(f) # read in f
  # parse_study below adapts settings we want from above
  cur_parse <- parse_study(cur_df, multi_list, prefix = TRUE, rt = TRUE)
  # then bind rows to the ongoing df
  parse_df <- bind_rows(parse_df, cur_parse)
}

# Step 4: check the result
parse_df 