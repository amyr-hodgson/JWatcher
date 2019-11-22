
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
    theme(panel.spacing = unit(.55, "lines")) +
    labs(x = "Time (seconds)", y = "Location")
  
  return(p)
}


# Colour scheme

mycol <- c("#e6194B", "#f58231", "#ffe119", "#3cb44b", "#4363d8")

# Family3

##### 

family3 <- read_csv("2019-11-22_Family3_timeline.csv") %>% 
  select(-X1)

family3 <- family3 %>%
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

family3$locrev <- factor(family3$loc, levels = c("2", "1", "0"))

names(mycol) <- levels(family3$Stage)
colScale <- scale_color_manual(name = "Stage",values = mycol)

#####

# to switch labels, scale_y_continuous(position = 'right') and facet_grid(switch = 'both')

time_graph(family3)
