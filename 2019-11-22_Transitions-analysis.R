# Want to look at how many approaches made to the shelf, radius etc
# Also total duration on the shelf/radius etc

st_age <- function(df) {
  
  age <- df %>%
    mutate(age = difftime(as.Date(test_day), as.Date(DOB), units = "weeks") /
             52.25)
  
  dev <- age %>%
    mutate(Stage = case_when(
      (age * 12) > 20 ~ "Adult",
      (age * 12) > 14 &
        (age * 12) < 20 ~ "Late Adolescent",
      (age * 12) < 14 & (age * 12) > 8 ~ "Early Adolescent",
      (age * 12) > 4 & (age * 12) < 8 ~ "Older Infant",
      (age * 12) < 4 ~ "Young Infant"
    ))
  
  return(dev)
}


library(tidyverse)

f3 <- read_csv("2019-11-24_Family3-DOB.csv") %>%
  select(-X1) %>%
  filter(button != "EOF")

f5 <- read_csv("2019-11-24_Family5-DOB.csv") %>%
  select(-X1) %>%
  filter(button != "EOF")

f6 <- read_csv("2019-11-24_Family6-DOB.csv") %>%
  select(-X1) %>%
  filter(button != "EOF")

f3 <- st_age(f3)
f5 <- st_age(f5)
f6 <- st_age(f6)

# Want to be able to get transitions to shelf that are only preceded by 'away' -
# otherwise will count moving from radius to shelf as approach to shelf

returns <- function(df) {
  df <- df %>%
    mutate(return = 
             case_when(
               button == "2" & lag(button == "1") ~ "yes",
               button == "1" & lag(button == "a" | button == "s") ~ "yes",
               TRUE ~ "no"
             ))
  return(df)
}

f3 <- returns(f3)
f5 <- returns(f5)
f6 <- returns(f6)

full <- bind_rows(f3, f5, f6)

# for this family should all be no anyway

# count approaches

count_stage <- function(df) {
  counted <- df %>%
  group_by(Stage, button, monkey) %>%
  tally() 
  
  return(counted)
}

f3count <- count_stage(f3)
full_count <- count_stage(full)

# average approaches by stage

avg_stage <- function(df) {
  avgd <- df %>%
    filter(return == "no") %>% # only include rows that are not returns (eg from radius to shelf)
    group_by(monkey, button, Stage) %>%
    tally() %>%
    filter(button != "3") %>% # not interested in avg aways
    group_by(Stage, button) %>%
    summarise(avg = mean(n))
  
  return(avgd)
}

f3avg <- avg_stage(f3)
f5avg <- avg_stage(f5)
f6avg <- avg_stage(f6) 

full_avg <- avg_stage(full)

# basic graphs

ggplot(data = full_avg %>% filter(button != "4"), aes(x = button , y = avg, fill = Stage)) +
  geom_col(colour = "black", width = .5, position = position_dodge()) 

ggplot(data = full_count %>% filter(button != "4" & button != "3"), 
       aes(x = Stage, y = n, colour= Stage)) +
  geom_point() +
  facet_grid(~button)+ 
  theme(axis.text.x = element_text(angle = 90))

