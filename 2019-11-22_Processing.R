
library(tidyverse)

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

fam3 <- all_joined %>% 
  filter(family == 3)

fam5 <- all_joined %>% 
  filter(family == 5)

fam6 <- all_joined %>% 
  filter(family == 6)

# Then can create .csv file 

write.csv(fam3, "2019-11-22_Family3.csv")
write.csv(fam5, "2019-11-22_Family5.csv")
write.csv(fam6, "2019-11-22_Family6.csv")

# Timeline version

timeline <- function(df, scale = 1000, type = 'button') {
  df$ts <-
    round(df$ts / as.numeric(scale), digits = 0) # adjusting timestamps according to scale specified
  df$ts_adj <-
    round(df$ts_adj / as.numeric(scale), digits = 0) # adjusting timestamps according to scale specified
  
  ses_no <-
    df %>% # longest session number- will multiply this by length of sessions to get full length
    filter(ses == max(ses)) %>%
    distinct(ses)
  
  ses_len <- df %>% # session length
    filter(button == "EOF") %>%
    distinct(ts)
  
  time <-
    as.data.frame(rep(0:(ses_len$ts * ses_no$ses))) %>% # creating timeline
    rename(ts_adj = 1)
  
  df <-
    rowid_to_column(df, var = "rowid") # stop spread() complaining
  
  df <- df %>% # coding on/off and location codes
    mutate(onoff = ifelse(button == "3", 0, 1)) %>%
    mutate(
      location_coded = case_when(
        button == "3" ~ 0,
        # away
        button == "2" ~ 1,
        # shelf
        button == "1" | # radius
          button == "4" ~ 2,
        button == "a" | # touch
          button == "s" ~ 3
      )
    )
  spread_df <-
    df %>% # creating column for each monkey and arranging by timestamp
    filter(button != "EOF") %>%
    spread(key = monkey, value = type) %>% # pick button or location coded
    arrange(ts_adj)
  
  joined <-
    full_join(time, spread_df, by = "ts_adj") %>% # joining timeline and spread data
    select(-ts,-rowid) %>%
    fill(-ts_adj, .direction = "down") %>% # fill in NAs downwards
    fill(-ts_adj, .direction = "up") %>% # fill in start of session NAs
    distinct(ts_adj, .keep_all = T) %>%
    arrange(ts_adj)
  
  return(joined)
}

df3 <- read_csv("2019-11-22_Family3.csv") %>% 
   select(-X1) 

df5 <- read_csv("2019-11-22_Family5.csv") %>% 
  select(-X1)

df6 <- read_csv("2019-11-22_Family6.csv") %>% 
  select(-X1)

fam3_timeline <- timeline(df = df3, scale = 1000, type = 'location_coded')

fam5_timeline <- timeline(df = df5, scale = 1000, type = 'location_coded')

fam6_timeline <- timeline(df = df6, scale = 1000, type = 'location_coded')

write.csv(fam3_timeline, "2019-11-22_Family3_timeline.csv")
write.csv(fam5_timeline, "2019-11-22_Family5_timeline.csv")
write.csv(fam6_timeline, "2019-11-22_Family6_timeline.csv")

# Adding DOB to joined version

f5 <- read_csv("2019-11-22_Family5.csv") %>%
  select(-X1)

f5 <- f5 %>%
  mutate(
    DOB =
      case_when(
        monkey == "Alderaan" ~ "2016-06-28",
        monkey == "Scout" ~ "2016-08-12",
        monkey == "Zinc" | monkey == "Zircon" ~ "2019-01-02",
        monkey == "Quantum" |
          monkey == "Quartz" ~ "2019-06-09"
      )
  ) %>%
  mutate(test_day = "2019-09-26")

write.csv(f5, "2019-11-24_Family5-DOB.csv")

f6 <- read_csv("2019-11-22_Family6.csv") %>%
  select(-X1)

f6 <- f6 %>%
  mutate(
    DOB =
      case_when(
        monkey == "Ackbar" ~ "2016-01-16",
        monkey == "Bouncer" ~ "2016-05-17",
        monkey == "Spaniel" ~ "2018-06-28",
        monkey == "Papillon"
        | monkey == "Poodle" ~ "2018-11-29",
        monkey == "Nugget" | monkey == "Ninja" ~ "2019-05-02"
      ),
    test_day = "2019-10-24"
  )

write.csv(f6, "2019-11-24_Family6-DOB.csv")
