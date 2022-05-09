# jsPsych demo

source("jsPsych_to_R_functions.R")
round
parse_study

ex_data <- read.csv("In_Data/epsy5123_jspsych_demo.csv")
ex_data

color_options <- c("Red", "Yellow", "Green", "Blue", "Black")
food_options <- c("Apples", "Bananas", "Carrots", "Donuts", "Eggplant")
lang_options <- c("R", "JavaScript", "HTML", "CSS", "Python")

multi_list <- list(Colors = color_options, Foods = food_options, Lang = lang_options)

files <- list.files("In_Data", full.names = TRUE)

parse_df <- NULL # initialization

list.files("In_data", full.names = TRUE, pattern = "*.csv")

# for (i in files) {
#   print(i)
# }

# use a blank DF + 

#files will be a vector, and f will take the value of each value in the vector
for (f in files) {
  cur_data <- read.csv(f)
  cur_parse <- parse_study(cur_data, multi_list, prefix = TRUE, rt = TRUE)
  print(cur_parse)
  parse_df <- bind_rows(parse_df, cur_parse)
}
parse_df

