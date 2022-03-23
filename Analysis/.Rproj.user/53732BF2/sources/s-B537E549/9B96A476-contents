# jsPsych demo

source("jspsych2r/Analysis/jsPsych_to_R_functions.R")
round
parse_study

ex_data <- read.csv("jspsych2r/Analysis/In_Data/epsy5200_jspsych_demo.csv")
ex_data

coffee_options <- c('sugar', "cream", 'honey', 'nondairy milk', 'dairy milk', 'more coffee')
food_options <- c("Apples", "Bananas", "Carrots", "Donuts", "Eggplant")

multi_list <- list(coffee_options, food_options)

parsed <- ex_data %>% 
  parse_study(multi_list)
# oops, something is broken