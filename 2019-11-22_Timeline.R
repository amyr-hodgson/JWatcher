
# Timeline function takes output from join_files() and uses this to create a
# full record of every monkey's activity across the whole session, with a column
# for each monkey, for the desired time resolution (e.g. if scale = 1000, there
# will be a row for each second showing each monkey's location)

# timeline() takes 3 arguments:

# df: data you want to turn into a full timeline for each monkey. Should be
# output from join_files() processing of .dat files

# scale: number to divide milliseconds by. Choose 1000 for seconds, 100 for
# seconds/10

#type: whether you want just the button pressed ('button'), whether the monkey
# was on or off the shelf ('onoff') or the coded location ('coded_location'- look
# inside the function for details)

# Note: Ignore any column in the output not of the type you specified

######################
### Example of use ###
######################

# Set working directory to directory containing files you want to process

# Load packages needed

library(tidyverse)

# Define function

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

# Read in data you want to turn into full timeline

df <- read_csv("2019-11-22_Family3.csv") %>% 
  select(-X1) 

# Call function

fam3_timeline <-
  timeline(df = df, scale = 1000, type = 'location_coded')

# If want the data in long format (one column for all monkeys) for e.g. graphs

fam3_timeline_long <- fam3_timeline %>%
  gather(
    key = 'monkey',
    value = 'loc',
    Rafeky,
    Shiba,
    Mountain,
    Field,
    Chalk,
    Coal,
    Malachite
  )

# Can then write as csv

write.csv(fam3_timeline, "2019-11-22_Family3_timeline.csv")
