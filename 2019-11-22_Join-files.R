
# join_files function takes one argument- file 
# Returns processed file, or list of files when used with lapply
# File should have: 
# 
# A column for each answered question
# Two timestamp (ts) columns, one adjusted for scoring session number (ts_adj)
# A column for key pressed (button)

######################
### Example of use ###
######################

# Set working directory to directory containing files you want to process

# Load packages needed

library(tidyverse)

# Define function

join_files <- function(file) {
  
  data_full <-
    read.delim(file, header = F) # full version to get session number
  
  data_ts <- read.delim(file,
                        skip = 24,
                        header = F,
                        sep = ",") %>%
    rename(ts = 1, button = 2) # version with just data
  
  data_ts$button <- as.character(data_ts$button) # key as character
  data_ts$button <- gsub('\\s+', '', data_ts$button) # get rid of leading space in key column
  
  questions <-
    slice(data_full, 15:20) # get rows with answers to questions
  answers <-
    str_split(str_sub(questions$V1, start = 10), boundary("word")) # pull out answers
  
  answers_dfs <-
    function(list = answers) {
      # function to make df of correct length for each answer
      as.data.frame(rep(list, times = length(data_ts$ts))) # returns a list of dfs
    }
  dfs <- lapply(answers, answers_dfs) # apply function to answers
  
  dfs_joined <-
    bind_cols(dfs) %>% # join answers in one df, rename appropriately
    rename(
      scorer = 1,
      family = 2,
      monkey = 3,
      ses = 4,
      test = 5,
      object = 6
    )
  
  full <-
    bind_cols(dfs_joined, data_ts) # join data df with data to df with answers
  
  ses_dur <- full %>% filter(button == "EOF") # extract session duration
  
  full_adj <- full %>% mutate(ts_adj =
                                case_when(
                                  ses == "1" ~ as.numeric(ts),
                                  # adjust for diff time lengths by session number
                                  ses == "2" ~ as.numeric(ts + ses_dur$ts),
                                  ses == "3" ~ as.numeric(ts + (ses_dur$ts * 2))
                                ))
  full_adj[, 1:6] <-
    sapply(full_adj[, 1:6], as.character) # answers as char to stop warnings
  return(full_adj)
}

all <- list.files(pattern = "*.dat") # Lists files ending with .dat in current directory
join <- lapply(all, join_files) # Applies function to this list of files, returns processed list
all_joined <- bind_rows(join) # Binds processed files together

fam3 <- all_joined %>% # If you want to save specific family
  filter(family == 3)

# Then can create .csv file 

write.csv(fam3, "2019-11-22_Family3.csv")

