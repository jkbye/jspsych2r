# UMN EPSY 5200: Programming Methods for Psychological Research
# Jeffrey K. Bye

# HELPER FUNCTIONS to process a folder of .csv files with JSON
#     exported from jsPsych studies run on Pavlovia.org

## NOTE: these have not all been error-tested yet with jsPsych 6.3

#### IMPORTANT
# This file just defines the functions I've written.
# I don't recommend editing it unless you'd like to change the functions.
# Instead, edit the example jsPsych_to_R_demo.r script that I've included.
# That script *calls* this one via source() to load these functions.
# By keeping the function definitions separate in this file,
#   it's easier for you to focus on your own data processing in the other file.

# NOTE: These functions are a working draft -- if you run into a tricky error,
#   please let me know and I will work to fix it!




# Load required packages --------------------------------------------------

# uncomment install.packages() if necessary
#install.packages("tidyverse")
#install.packages("RJSONIO")
library(tidyverse) # need to load tidyverse packages to use this script
library(RJSONIO) # also load the RJSONIO package to deal with JSON data input (here, to handle the curly brace cells)


# Define useful functions to be used  --------------------------------


# Core functions ----------------------------------------------------------

## grab_cell: function to grab a cell by index and col name
#   Inputs: df_json (a data.frame imported from a single .csv file from jsPsych & Pavlovia)
#           trial_ind (the integer in the trial_index column) to index the correct row of df_json
#           col_name (character string) to select the column with the single value
#   Output: the contents of the cell
# Note: this is a 'core' function that the user doesn't need to call directly
#   it is used by 'wrapper' functions, e.g., get_rt, grab_single_resp
grab_cell <- function(df_json, trial_ind, col_name) { 
  df_json %>% 
    filter(trial_index == trial_ind) %>% # find current trial index
    pull(col_name) %>% # pull the column
    return()
  # no further processing performed (just return the value)
}
# usage examples:
#   grab_cell(jsp_demo_data, 0, "rt")
# or the tidyverse way:
#   jsp_demo_data %>% 
#     grab_cell(0, "rt")


## parse_json: function to grab the JSON within a cell and process it
#   Inputs: df_json (a data.frame imported from a single .csv file from jsPsych & Pavlovia)
#           trial_ind (the integer in the trial_index column) to index the correct row of df_json
#           col_name (character string) to select the column with the single value
#   Output: the response as a character string
parse_json <- function(df_json, trial_ind, col_name) {
  df_json %>% 
    grab_cell(trial_ind, col_name) %>% # pass trial_ind and col_name 
    # parse JSON content in return
    as.character() %>% # convert JSON to charaacter (in case it's factored)
    fromJSON() # and use fromJSON (RJSONIO) to parse
}
# Note that this will return values for more than one item at a time
#     if you have multiple item on one page
# The way the JSON data are organized by jsPsych/Pavlovia puts the data for 
#     all questions on one "page" into one cell of the csv (as JSON)
# Thus, if you have a single page with multiple items, they're all put in 1 cell




# Wrapper functions -------------------------------------------------------

## get_rt: grab reaction time from the 'rt' column for the row with trial_ind
#   Inputs: df_json (a data.frame imported from a single .csv file from jsPsych & Pavlovia)
#           trial_ind (the integer in the trial_index column) to index the correct row of df_json
#   Output: the reaction time (in milliseconds) as a numeric
get_rt <- function(df_json, trial_ind) { 
  df_json %>% 
    grab_cell(trial_ind, "rt") %>% 
    # pass trial_ind and "rt" col name to grab_cell function
    return()
  # no further processing needed (RT cells are not JSON)
}
# usage examples:
#   get_rt(jsp_demo_data, 0)
# or the tidyverse way:
#   jsp_demo_data %>% 
#     get_rt(0)


## parse_instructions: grab instruction info for the row with trial_ind, parse it
#   Inputs: df_json (a data.frame imported from a single .csv file from jsPsych & Pavlovia)
#           trial_ind (the integer in the trial_index column) to index the correct row of df_json
#           prefix (optional, default false) whether to add trial # as prefix
#   Output: instruction rt
parse_instructions <- function(df_json, trial_ind, prefix = F) { 
  instr_df <- df_json %>% 
    get_rt(trial_ind) %>% 
    enframe() %>% # force rt to df
    select(instr_rt = value) # rename col and get rid of extra col
  if (prefix) { # if prefix to be added
    instr_df <- instr_df %>%  # add trial prefix i#
      rename_all(function(x) paste0("i", trial_ind, "_", x))
  }
  return(instr_df) # return parsed as df (columns)
}


## parse_likert: grab likert JSON for the row with trial_ind, parse it
#   Inputs: df_json (a data.frame imported from a single .csv file from jsPsych & Pavlovia)
#           trial_ind (the integer in the trial_index column) to index the correct row of df_json
#           rt (optional, default false) whether to also return RT
#           prefix (optional, default false) whether to add trial # as prefix
#   Output: likert scale value(s) as a df row (rt optional)
parse_likert <- function(df_json, trial_ind, rt = F, prefix = F) { 
  lik_df <- df_json %>% 
    parse_json(trial_ind, "response") %>% # pass to parse_json function
    enframe() %>% # force named vector to df (will be long)
    pivot_wider(names_from = name, values_from = value) # now make wide
  if (rt) { # if rt also requested
    lik_df <- lik_df %>%  # add the rt col
      bind_cols(tibble(rt = get_rt(df_json, trial_ind)))
  }
  if (prefix) { # if prefix to be added
    lik_df <- lik_df %>%  # add trial prefix i#
      rename_all(function(x) paste0("i", trial_ind, "_", x))
  }
  lik_df %>%
    return() # return parsed as df (columns)
}
## LATER: ADD RECODE OPTION



## parse_keyboard: grab keypress info for the row with trial_ind, parse it
#   Inputs: df_json (a data.frame imported from a single .csv file from jsPsych & Pavlovia)
#           trial_ind (the integer in the trial_index column) to index the correct row of df_json
#           rt (optional, default false) whether to also return RT
#           prefix (optional, default false) whether to add trial # as prefix
#   Output: the key as a character string in a df (rt col optional)
parse_keyboard <- function(df_json, trial_ind, rt = F, prefix = F) { 
  stim <- df_json %>% 
    grab_cell(trial_ind, "stimulus")
  key <- df_json %>% 
    grab_cell(trial_ind, "response") #%>% # grab contents of cell
    #as.raw() %>% # convert ASCII code to raw (hexadecimal)
    #rawToChar() # then to its equivalent character
  key_df <- data.frame(stim, key)
  if (rt) { # if rt also requested
    key_df <- key_df %>%  # add the rt col
      bind_cols(tibble(rt = get_rt(df_json, trial_ind)))
  }
  if (prefix) { # if prefix to be added
    key_df <- key_df %>%  # add trial prefix i#
      rename_all(function(x) paste0("i", trial_ind, "_", x))
  }
  return(key_df) # return parsed as df (columns)
}


## parse_slider: grab slider info for the row with trial_ind, parse it
#   Inputs: df_json (a data.frame imported from a single .csv file from jsPsych & Pavlovia)
#           trial_ind (the integer in the trial_index column) to index the correct row of df_json
#           rt (optional, default false) whether to also return RT
#           prefix (optional, default false) whether to add trial # as prefix
#   Output: slider value in a df cell (rt col optional)
parse_slider <- function(df_json, trial_ind, rt = F, prefix = F) { 
  stim <- df_json %>% 
    grab_cell(trial_ind, "stimulus")
  value <- df_json %>% 
    grab_cell(trial_ind, "response") # grab contents of cell
  val_df <- data.frame(stim, value)
  if (rt) { # if rt also requested
    val_df <- val_df %>%  # add the rt col
      bind_cols(tibble(rt = get_rt(df_json, trial_ind)))
  }
  if (prefix) { # if prefix to be added
    val_df <- val_df %>%  # add trial prefix i#
      rename_all(function(x) paste0("i", trial_ind, "_", x))
  }
  return(val_df) # return parsed as df (columns)
}


## parse_multi_choice: grab multi-choice JSON for the row with trial_ind, parse it
#   Inputs: df_json (a data.frame imported from a single .csv file from jsPsych & Pavlovia)
#           trial_ind (the integer in the trial_index column) to index the correct row of df_json
#           rt (optional, default false) whether to also return RT
#           prefix (optional, default false) whether to add trial # as prefix
#   Output: df row with choice text (rt optional)
parse_multi_choice <- function(df_json, trial_ind, rt = F, prefix = F) { 
  mc_df <- df_json %>% 
    parse_json(trial_ind, "response") %>% # pass to parse_json function
    enframe() %>% # force named vector to df (will be long)
    pivot_wider(names_from = name, values_from = value) # now make wide
  if (rt) { # if rt also requested
    mc_df <- mc_df %>%  # add the rt col
      bind_cols(tibble(rt = get_rt(df_json, trial_ind)))
  }
  if (prefix) { # if prefix to be added
    mc_df <- mc_df %>%  # add trial prefix i#
      rename_all(function(x) paste0("i", trial_ind, "_", x))
  }
  return(mc_df) # return parsed as df (columns)
}


### parse_multi_select: function to grab MULTIPLE responses (multi-select)
#   Inputs: df_json (a data.frame imported from a single .csv file from jsPsych & Pavlovia)
#           trial_ind (the integer in the trial_index column) to index the correct row of df_json
#           resp_opt_list (a list of vectors w/ all possible options user can select) to search for in responses
#           rt (optional, default false) whether to also return RT
#           prefix (optional, default false) whether to add trial # as prefix
#   Output: df row with cols of 1/0 for each possible selection (rt optional)
parse_multi_select <- function(df_json, trial_ind, resp_opt_list, rt = F, prefix = F) {
  # grab the JSON list of chosen responses
  resp_chosen_df <- df_json %>% 
    parse_json(trial_ind, "response") %>%  # pass to parse_json function 
    # note that if multiple on same page, resp_chosen is a named list, so
    enframe() # force named vector to df (will be one row per multi-select)
  
  ms_df <- NULL # initialize a null df for multi-select responses
  for (mi in 1:nrow(resp_chosen_df)) { # for each row (each multi-select item)
    
    # grab name of response option type, all options from input list, resps chosen
    resp_name <- resp_chosen_df[[mi,'name']]
    resp_chosen <- resp_chosen_df[[mi,"value"]] %>% unlist()
    resp_opts <- resp_opt_list[[resp_name]] # grab from input list
    
    for (ri in 1:length(resp_opts)) { # for all *possible* responses
      resp_col <- paste0(resp_name, "_", resp_opts[ri])
      if (resp_opts[ri] %in% resp_chosen) { # if current participant chose this response
        ms_df[resp_col] <- 1 # code as 1
      } else { # else, not chosen
        ms_df[resp_col] <- 0 # code as 0
      }
    }
  }
  ms_df <- ms_df %>% # convert to df
    enframe() %>% # force named vector to df (will be long)
    pivot_wider(names_from = name, values_from = value) # now make wide
  if (rt) { # if rt also requested
    # make the rt column name from all objects on that page
    rt_col_name <- paste0(paste(resp_chosen_df$name, collapse="_"), "_rt")
    ms_df[rt_col_name] <- get_rt(df_json, trial_ind)
  }
  if (prefix) { # if prefix to be added
    names(ms_df) <- paste0("i", trial_ind, "_", names(ms_df))
  }
  return(ms_df) # return the dataframe
}


## parse_iat: parse implicit association task (IAT) trial data
#   Inputs: df_json (a data.frame imported from a single .csv file from jsPsych & Pavlovia)
#           trial_ind (the integer in the trial_index column) to index the correct row of df_json
#           rt (optional, default true) whether to also return RT
#               note: since IAT is about RT, default is True here
#           prefix (optional, default false) whether to add trial # as prefix
#   Output: df row with IAT data: key pressed, correct (1/0), RT (default)
parse_iat <- function(df_json, trial_ind, rt = T, prefix = F) { 
  stim <- df_json %>% 
    grab_cell(trial_ind, "stimulus") # grab filename of image stim
  key <- df_json %>% 
    grab_cell(trial_ind, "key_press") %>% # grab contents of cell
    as.raw() %>% # convert ASCII code to raw (hexadecimal)
    rawToChar() # then to its equivalent character
  corr <- df_json %>% 
    grab_cell(trial_ind, "correct") %>% # grab logical (TRUE or FALSE)
    ifelse(yes = 1, no = 0) # convert to 1/0
  iat_df <- data.frame(key, corr)
  if (rt) { # if rt also requested (default true for IAT)
    iat_df <- iat_df %>%  # add the rt col
      bind_cols(tibble(rt = get_rt(df_json, trial_ind)))
  }
  iat_df <- iat_df %>%  # add trial prefix i#
    rename_all(function(x) paste0("iat_", stim, "_", x))
  if (prefix) { # if prefix to be added
    iat_df <- iat_df %>%  # add trial prefix i#
      rename_all(function(x) paste0("i", trial_ind, "_", x))
  }
  return(iat_df) # return parsed as df (columns)
}




# Master function ---------------------------------------------------------

## parse_study: parse relevant data from all trials, using trial type as key
#   Inputs: df_json (a data.frame imported from a single .csv file from jsPsych & Pavlovia)
#           resp_opt_list (optional, a list of vectors w/ all possible options user can select) to search for in responses
#           rt (optional, default false) whether to also return RT
#           prefix (optional, default false) whether to add trial # as prefix
#   Output: wide data.frame for given participant
parse_study <- function(df_json, resp_opt_list = NULL, rt = F, prefix = F) { 
  # grab trial numbers
  trial_nums <- df_json %>% 
    pull(trial_index)
  
  # initialize an empty df for current participant's data (wide format)
  out_data <- NULL
  
  # loop through all trials
  for (t in trial_nums) {
    
    # find current trial type to run relevant function
    cur_trial_type <- df_json %>% 
      filter(trial_index == t) %>% # trial t
      pull(trial_type) # get trial type
    
    # now run the appropriate function for this trial type
    if (cur_trial_type == "instructions") {
      new_df <- parse_instructions(
        df_json, t, prefix # parse as instructions (get RT)
      )
    } else if (cur_trial_type == "survey-likert") {
      new_df <- parse_likert(
        df_json, t, rt, prefix # parse as likert
      )
    } else if (cur_trial_type == "survey-multi-choice") {
      new_df <- parse_multi_choice(
        df_json, t, rt, prefix # parse as multi-choice
      )
    } else if (cur_trial_type == "survey-multi-select") {
      new_df <- parse_multi_select(
        df_json, t, resp_opt_list, rt, prefix # parse as multi-select
      )
    } else if (cur_trial_type == "html-keyboard-response" |
             cur_trial_type == "image-keyboard-response") {
      new_df <- parse_keyboard(
        df_json, t, rt, prefix # parse as keyboard response
      )
    } else if (cur_trial_type == "html-slider-response") {
      new_df <- parse_slider(
        df_json, t, rt, prefix # parse as slider response
      )
    } else if (cur_trial_type == "iat-image") {
      new_df <- parse_iat(
        df_json, t, prefix # parse as IAT response (assume RT=T, default)
      )
    }
    # bind cols
    out_data <- out_data %>% 
      bind_cols(new_df)
  }
  
 return(out_data) 
}



# List of future changes to make ------------------------------------------

# eliminate some redundancy in functions
# only pass 1 row to parse_ functions?
# add more functions for new item types
# move RT and Prefix functionality to modular fxns outside of custom ones
# add error-checking (NA returning)
# move more core fxns out (e.g., key char)
