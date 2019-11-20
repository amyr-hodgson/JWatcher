# tweak timeline function for graphs


library(tidyverse)

dat <- read_csv("2019-11-17_Testdata.csv") %>% 
  select(-X1) %>% 
  rename(button = key) # spread() didn't like that this was called key

# function currently takes 3 arguments:
# df: data, processed .dat file by join_files()
# scale: number by which to divide milliseconds. 100 gives secs/10, 1000 gives seconds
# type: whether to give output as 'button' ie key in JWatcher, or 'location_coded' (see in function)

timeline_adj <- function(df, scale = 100, type = 'button') {
  
  df$ts <- round(df$ts/as.numeric(scale), digits = 0) # converting to secs/10
  df$ts_adj <- round(df$ts_adj/as.numeric(scale), digits = 0)
  
  ses_no <- df %>% # longest session number- multiply this by length of sessions
    filter(ses == max(ses)) %>% 
    distinct(ses)
  
  ses_len <- df %>% # session length
    filter(button == "EOF") %>% 
    distinct(ts)
  
  time <- as.data.frame(rep(1:(ses_len$ts*ses_no$ses))) %>% # creating timeline
    rename(ts_adj = 1) 
  
  df <- rowid_to_column(df, var = "rowid") # stop spread() complaining
  
  df <- df %>%
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
  
  spread_df <- df %>% 
    filter(button != "EOF") %>%
    spread(key = monkey, value = type) %>% # pick button or location coded
    arrange(ts_adj)
   
  joined <- full_join(time, spread_df, by = "ts_adj") %>% 
    select(-ts, -rowid) %>% 
    fill(-ts_adj, .direction = "down") %>%
    fill(-ts_adj, .direction = "up") %>%
    distinct(ts_adj, .keep_all = T)
  
  return(joined)
}


test <- timeline_adj(dat, scale = 1000, type = 'location_coded')

# lose resolution this way. would be better to do with 100 first, do overlap &
# first, then collapse to lower resolution for graphs
# or could just do seperately

# need long for graphs? gather now or before in function?
