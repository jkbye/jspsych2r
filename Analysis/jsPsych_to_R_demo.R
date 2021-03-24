# jsPsych demo

source("jsPsych_to_R_functions.R")
round
parse_study

ex_data <- read.csv("In_Data/epsy5200_jspsych_demo.csv")
ex_data

coffee_options <- c('sugar', "cream", 'honey', 'nondairy milk', 'dairy milk', 'more coffee')
food_options <- c("Apples", "Bananas", "Carrots", "Donuts", "Eggplant")

multi_list <- list(coffee_options, food_options)

files <- list.files("In_Data", full.names = TRUE)

parse_df <- NULL # initialization

# for (i in files) {
#   print(i)
# }

for (f in files) {
  cur_data <- read.csv(f)
  cur_parse <- parse_study(cur_data, multi_list, prefix = TRUE, rt = TRUE)
  parse_df <- bind_rows(parse_df, cur_parse)
}
parse_df

