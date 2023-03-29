library(tidyverse)
source("jsPsych_to_R_functions.R")

ex_data <- read.csv("In_Data/epsy5123_s23_jspsych_demo_0.csv")
ex_data

parse_study(ex_data)
pi
round(3.4)
round()
parse_study

ex <- parse_study(ex_data, prefix = TRUE, rt = TRUE)

files <- list.files("In_Data", pattern = "s23", full.names = TRUE)
files

parse_df <- NULL # initialization
for (f in files) {
  cur_df <- read.csv(f)
  cur_parse <- parse_study(cur_df, prefix = TRUE, rt = TRUE)
  parse_df <- bind_rows(parse_df, cur_parse)
}
parse_df
