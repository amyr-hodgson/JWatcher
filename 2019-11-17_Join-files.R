# Function to join JWatcher files when a scoring session has been split up into
# several blocks. Specifically, when Novel Object scoring has been split into 3
# x 10 minute scoring sessions. This function deals with the .dat files, which
# are needed to get the timestamps for each transition to a new state for each
# monkey.

# wd = Data_files in Primate_social/Novel_object

library(tidyverse)

# Function takes one argument- file or list of files

join_files <- function(file) {
  
  data_full <- read.delim(file, header = F) # full version to get session number
  data_ts <- read.delim(file,
                        skip = 24,
                        header = F,
                        sep = ",") %>%
    rename(ts = 1, key = 2) # version with just data
  
  data_ts$key <- as.character(data_ts$key) # key as character
  data_ts$key <- gsub('\\s+', '', data_ts$key) # get rid of leading space in key column
  
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
  
  ses_dur <- full %>% filter(key == "EOF") # extract session duration
  
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

all_monkeys <- list.files(pattern = "*.dat")
all_test <- lapply(all_monkeys, join_files)
all_test2 <- bind_rows(all_test)
write.csv(all_test2, "2019-11-17_Testdata.csv")

fam6 <- list.files(pattern = "*.dat")
f6join <- lapply(fam6, join_files)
f6join <- bind_rows(f6join) %>%
  filter(family == 6)
write.csv(f6join, "2019-11-20_fam6.csv")
