# testing elements of function to get full time series of behaviour transitions
# from already processed .dat data

library(tidyverse)

dat <- read_csv("2019-11-17_Testdata.csv") %>% 
  select(-X1) %>% 
  rename(button = key) # spread() didn't like that this was called key

# dat$ts <- round(dat$ts/100, digits = 0) # converting to secs/10
# dat$ts_adj <- round(dat$ts_adj/100, digits = 0)
# 
# time <- as.data.frame(rep(1:18000)) %>% # filter EOF row and do $ts*tally(key) or similar
#   rename(ts_adj = 1) 
# 
# ses_no <- dat %>% # longest session number- multiply this by length of sessions
#   filter(ses == max(ses)) %>% 
#   distinct(ses)
# 
# ses_len <- dat %>% # session length
#   filter(key == "EOF") %>% 
#   distinct(ts)
# 
# tim <- as.data.frame(rep(1:(ses_len$ts*ses_no$ses)))
# 
# sp1 <- dat %>% 
#   filter(key != "EOF") %>%
#   spread(monkey, value = key) %>% 
#   arrange(ts_adj)
# 
# joined <- full_join(time, sp1, by = "ts_adj") %>% 
#   select(-ts) %>% 
#   fill(-ts_adj, .direction = "down") %>%
#   fill(-ts_adj, .direction = "up")


# function version - working


timeline <- function(df) {
  
  df$ts <- round(df$ts/100, digits = 0) # converting to secs/10
  df$ts_adj <- round(df$ts_adj/100, digits = 0)
  
  ses_no <- df %>% # longest session number- multiply this by length of sessions
    filter(ses == max(ses)) %>% 
    distinct(ses)
  
  ses_len <- df %>% # session length
    filter(button == "EOF") %>% 
    distinct(ts)
  
  time <- as.data.frame(rep(1:(ses_len$ts*ses_no$ses))) %>% # creating timeline
    rename(ts_adj = 1) 
  
  spread_df <- df %>% 
    filter(button != "EOF") %>%
    spread(key = monkey, value = button) %>% 
    arrange(ts_adj)
  
 # df <- rowid_to_column(df, var = "rowid")
  joined <- full_join(time, spread_df, by = "ts_adj") %>% 
    select(-ts) %>% 
    fill(-ts_adj, .direction = "down") %>%
    fill(-ts_adj, .direction = "up")
  
  return(joined)
}


test <- timeline(dat)



# latencies

latencies <- joined[!duplicated(joined$Ackbar),]

latencies <- joined %>% # dplyr version
  distinct(Ackbar, .keep_all = T)

get_lat <- function(df, monkey) {
  lat <- df[!duplicated(df[[monkey]]),]
  return(lat)
}

# can't get what I want working, but this does it

A <- get_lat(joined, 'Ackbar')
B <- get_lat(joined, 'Bouncer')
S <- get_lat(joined, 'Spaniel')

bind_rows("A" = A, "B" = B, "S" = S, .id = "id")

# overlaps

# if wanted to get e.g. overlap of Bouncer & Ackbar on shelf

AB <- test %>%
  mutate(AnB = ifelse(Ackbar == 2 & Bouncer == 2, 1, 0)) # when both on shelf

ABCount <- AB %>%
  tally(AnB == 1) # count() gets FALSE as well

ABsecs <- ABCount$n / 10 # convert to seconds

# who was first?

counting <- AB %>%
  mutate(
    first =
      case_when(
        AnB == 1 & # when both on shelf
          Ackbar == lag(Ackbar) & Bouncer != lag(Bouncer) ~ "AFirst", # and A matches the row above, and B doesn't
        AnB == 1 &
          Bouncer == lag(Bouncer) & Ackbar != lag(Ackbar) ~ "BFirst",
        AnB == 1 &
          Ackbar != lag(Ackbar) & Bouncer != lag(Bouncer) ~ "no_first", # when nobody is first
        TRUE ~ "no"
      )
  )

counting %>%
  group_by(first) %>%
  tally()
