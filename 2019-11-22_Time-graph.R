
library(tidyverse)

# Making time series graph

# Defining function to add age and dev stage

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

# Function for graphs

time_graph <- function(df) {
  
  abbrev_y <- c("away", "shelf", "radius", "touch")

  p <- ggplot(data = df, aes(
    x = ts_adj,
    y = loc,
    colour = Stage
  )) +
    geom_point(size = .02) +
    geom_line(size = .5) +
    colScale +
    facet_grid(monkey ~.)+
    coord_cartesian(ylim = c(0, 3.1)) +
    scale_y_continuous(breaks = seq(0, 3.1, 1), labels = abbrev_y) +
    theme(panel.spacing = unit(.5, "lines")) +
    labs(x = "Time (seconds)", y = "Location")
  
  return(p)
}
# to switch labels, scale_y_continuous(position = 'right') and facet_grid(switch = 'both')

rainbow <- function(df) {
  ggplot(data = df, aes(
    x = monkeyrev,
    y = ts_adj,
    colour = Stage
  )) +
    geom_point(size = 1) +
    colScale  + 
    facet_grid(locrev~ .) + 
    coord_flip() +
    labs(x = "Monkey", y = "Time (seconds)")
  
}

# Colour scheme

mycol <- c("#e6194B", "#f58231", "#ffe119", "#3cb44b", "#4363d8")

##### 

family3 <- read_csv("2019-11-22_Family3_timeline.csv") %>% 
  select(-X1)

family3 <- family3 %>%
  gather(
    key = 'monkey',
    value = 'loc',
    Rafeky, Shiba, Mountain, Field, Chalk, Coal, Malachite
  )

family3 <- family3 %>%
  mutate(
    DOB =
      case_when(
        monkey == "Rafeky" ~ "2010-10-09",
        monkey == "Shiba" ~ "2018-03-19",
        monkey == "Mountain" | monkey == "Field" ~ "2018-08-20",
        monkey == "Chalk" |
          monkey == "Coal" ~ "2019-01-25",
        monkey == "Malachite" ~ "2019-07-01"
      )
  ) %>%
  mutate(test_day = "2019-09-12")

family3 <- st_age(family3)

family3$Stage <- factor(
  family3$Stage,
  levels = c("Adult", "Late Adolescent", "Early Adolescent", "Older Infant", "Young Infant"),
  labels = c(
    "Adult",
    "Late Adol.",
    "Early Adol.",
    "Older Infant",
    "Young Infant"
  )
)

family3$monkey <-
  factor(
    family3$monkey,
    levels = c(
      "Rafeky",
      "Shiba",
      "Mountain",
      "Field",
      "Chalk",
      "Coal",
      "Malachite"
    )
  )
family3$monkeyrev <-
  factor(
    family3$monkey,
    levels = c(
      "Malachite",
      "Coal",
      "Chalk",
      "Field",
      "Mountain",
      "Shiba",
      "Rafeky"
    )
  )

family3$locrev <- factor(family3$loc, levels = c("2", "1", "0"), 
                         labels = c("Radius", "Shelf", "Away"))

names(mycol) <- levels(family3$Stage)
colScale <- scale_color_manual(name = "Stage",values = mycol)



#####

family5 <- read_csv("2019-11-22_Family5_timeline.csv") %>% 
  select(-X1)

family5 <- family5 %>%
  gather(
    key = 'monkey',
    value = 'loc',
    Alderaan:Zircon
  )

family5 <- family5 %>%
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

family5 <- st_age(family5)

family5$Stage <- factor(
  family5$Stage,
  levels = c("Adult", "Late Adolescent", "Early Adolescent", "Older Infant", "Young Infant"),
  labels = c(
    "Adult",
    "Late Adol.",
    "Early Adol.",
    "Older Infant",
    "Young Infant"
  )
)

family5$monkey <-
  factor(family5$monkey,
         levels = c("Alderaan", "Scout", "Zinc", "Zircon", "Quantum", "Quartz"))

family5$monkeyrev <-
  factor(family5$monkey,
         levels = c("Quartz", "Quantum", "Zircon", "Zinc", "Scout", "Alderaan"))

family5$locrev <- factor(family5$loc, levels = c("3", "2", "1", "0"), 
                         labels = c("Touch", "Radius", "Shelf", "Away"))

names(mycol) <- levels(family5$Stage)
colScale <- scale_color_manual(name = "Stage",values = mycol)

#####


family6 <- read_csv("2019-11-22_Family6_timeline.csv") %>% 
  select(-X1)

family6 <- family6 %>%
  gather(
    key = 'monkey',
    value = 'loc',
    Ackbar, Bouncer, Spaniel, Poodle, Papillon, Nugget, Ninja
  )

family6 <- family6 %>%
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

family6 <- st_age(family6)

family6$Stage <- factor(
  family6$Stage,
  levels = c("Adult", "Late Adolescent", "Early Adolescent", "Older Infant", "Young Infant"),
  labels = c(
    "Adult",
    "Late Adol.",
    "Early Adol.",
    "Older Infant",
    "Young Infant"
  )
)

family6$monkey <-
  factor(family6$monkey,
         levels = c("Ackbar", "Bouncer", "Spaniel", "Papillon", "Poodle", "Nugget", "Ninja"))

family6$monkeyrev <-
  factor(family6$monkey,
         levels = c("Ninja","Nugget","Poodle","Papillon","Spaniel","Bouncer","Ackbar"))

family6$locrev <- factor(family6$loc, levels = c("3", "2", "1", "0"), 
                         labels = c("Touch", "Radius", "Shelf", "Away"))

names(mycol) <- levels(family6$Stage)
colScale <- scale_color_manual(name = "Stage",values = mycol)


#####

time_graph(family3) + geom_vline(xintercept = 1)
time_graph(family5)
time_graph(family6) + geom_vline(xintercept = 241)

rainbow(family3)+ geom_hline(yintercept = 1)
rainbow(family5)
rainbow(family6)+ geom_hline(yintercept = 241)
