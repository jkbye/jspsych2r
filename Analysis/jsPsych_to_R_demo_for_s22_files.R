# jsPsych demo for the In_Data files labeled s22 (Spring 2022 semester demo)
# rund demo here: https://run.pavlovia.org/jkbye/epsy5123_week10

source("jsPsych_to_R_functions.R")

# Step 0: load in example data file to check out
ex_data <- read.csv("In_Data/epsy5123_s22_jspsych_demo_0.csv")
ex_data # check it out!

# Step 1: notice the plugin types used:
#   html-keyboard-response
#   survey-likert
#   survey-multi-select

# these are all built in to the parse_study() function,
#   except multi-select is a bit complicated because
#   in jsPsych, it doesn't record *un*selected items

# so here, we copy-paste-adapt arrays from the javascript into R vectors:
color_options <- c("Red", "Yellow", "Green", "Blue", "Black")
food_options <- c("Apples", "Bananas", "Carrots", "Donuts", "Eggplant")
lang_options <- c("R", "JavaScript", "HTML", "CSS", "Python")
# then put them into a list, where the names (lefthand side of =)
#   match the item names in the data file's JSON
multi_list <- list(
  Colors = color_options, # note Color, Foods, Lang match json attributes
  Foods = food_options, 
  Lang = lang_options
)

# Step 2: test out on ex_data
# parse_study(ex_data) # notice this would give an error, need to include multi_list!
parse_study(ex_data, multi_list) # this works
parse_study(ex_data, multi_list, prefix = TRUE, rt = TRUE) # or with prefix and RT


# Step 3: now we can apply to all relevant files

files <- list.files("In_Data", pattern = "s22.*csv$", full.names = TRUE) 
# above we select just those w/ "s22" in the filename and ending in "csv"
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