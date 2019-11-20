# testing graphs with join_files output data, with tweaks

library(tidyverse)
library(plotly)
library(ggstance)

# df <- read_csv("2019-11-19_fam6.csv") %>% 
#   select(-X1) %>% 
#   rename(button = key) %>%
#   arrange(ts_adj) %>%
#   mutate(onoff = ifelse(button == "3", 0, 1)) %>%
#   mutate(
#     location_coded = case_when(
#       button == "3" ~ 0,
#       # away
#       button == "2" ~ 1,
#       # shelf
#       button == "1" | # radius
#         button == "4" ~ 2,
#       button == "a" | # touch
#         button == "s" ~ 3
#     )
#   )
# 
# df$monkey <- as.factor(df$monkey)
# 
# ggplot(data = df, aes(x = ts_adj, y = location_coded, fill = monkey, colour = monkey)) +
#   geom_point() + 
#   geom_line()

##########

df <- read_csv("2019-11-20_fam6.csv") %>% 
  select(-X1) %>% 
  rename(button = key)

timeline <- function(df, scale = 100, type = 'button') {
  
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


tl <- timeline(df, scale = 1000, type = 'location_coded') %>%
  arrange(ts_adj)

tll <- tl %>%
  gather(key = 'monkey', value = 'loc', Ackbar, Bouncer, Spaniel, Poodle, Papillon, Nugget, Ninja)

tll$monkey <- as.factor(tll$monkey)

adjusted <- adjusted %>%
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

adjusted$age <-
  difftime(as.Date(adjusted$test_day), as.Date(adjusted$DOB), units = "weeks") /
  52.25

adjusted <- adjusted %>%
  mutate(
    dev_stage =
      case_when(
        (age * 12) > 20 ~ "Adult",
        (age * 12) > 14 &
          (age * 12) < 20 ~ "Late Adolescent",
        (age * 12) < 14 & (age * 12) > 8 ~ "Early Adolescent",
        (age * 12) > 4 & (age * 12) < 8 ~ "Older Infant",
        (age * 12) < 4 ~ "Young Infant"
      )
  )


adjusted$dev_stage <- factor(
  adjusted$dev_stage,
  levels = c("Adult", "Late Adolescent", "Early Adolescent", "Older Infant", "Young Infant"),
  labels = c(
    "Adult",
    "Late Adol.",
    "Early Adol.",
    "Older Infant",
    "Young Infant"
  )
)

adjusted <- adjusted %>%
  mutate(loc_dodge = 
           case_when(
             monkey == "Ackbar" ~ loc + .3,
             monkey == "Bouncer" ~ loc + .2,
             monkey == "Spaniel" ~ loc + .1,
             monkey == "Papillon" ~ loc,
             monkey == "Poodle" ~ loc -.1,
             monkey == "Nugget" ~ loc -.2,
             monkey == "Ninja" ~ loc -.3
           ))


mycol <- c("#e6194B", "#f58231", "#ffe119", "#3cb44b", "#4363d8")

names(mycol) <- levels(adjusted$dev_stage)
colScale <- scale_color_manual(name = "dev_stage",values = mycol)

adjusted$loc1 <- factor(adjusted$loc, levels = c("3","2","1","0"))

adjusted$monkey1 <- factor(adjusted$monkey, levels = c("Ninja","Nugget","Poodle","Papillon","Spaniel","Bouncer","Ackbar"))
adjusted$monkey2 <- factor(adjusted$monkey, levels = c("Ackbar", "Bouncer", "Spaniel", "Papillon", "Poodle", "Nugget", "Ninja"))


p <-
  ggplot(data = adjusted, aes(
    x = monkey1,
    y = ts_adj,
    colour = dev_stage
  )) +
  geom_point(size = 1)# +, shape = 15
 # geom_line(size = 1, position = position_dodge(width = .1), alpha = .2)# +#, position = position_dodgev(height = .1)
 # coord_cartesian(xlim = c(0, 500)) +
  #scale_x_continuous(breaks = seq(0, 500, 100)) 

  
p + colScale  + 
  facet_grid(loc1~ .) + 
  coord_flip()

# could try facet by monkey?

adjusted$loc2 <- factor(adjusted$loc, levels = c("0","1","2","3"))

ggplot(data = adjusted, aes(
    x = loc,
    y = ts_adj,
    colour = dev_stage
  )) +
  geom_point(size = 1) +
 # geom_line() +
  colScale  + 
  facet_grid(monkey2~ .) + 
  coord_flip()

ggplot(data = adjusted, aes(
  x = ts_adj,
  y = loc,
  colour = dev_stage
)) +
  geom_point(size = .02) +
  geom_line(size = .5) +
  colScale +
  facet_grid(monkey2 ~.)

# monkey1ggplotly(p)

# ggplot(data = tll, aes(
#     x = ts_adj,
#     y = loc,
#     fill = monkey,
#     colour = monkey
#   )) +
#   geom_point(size = .5, position = position_dodge(width = .1)) +
#   geom_line(size = .5, position = position_dodge(width = .1)) +#, position = position_dodgev(height = .1)
#   coord_cartesian(xlim = c(0, 500)) +
#   scale_x_continuous(breaks = seq(0, 500, 100)) 

# may want to have dataset with onset and offset times rather than 
# value for each second? 