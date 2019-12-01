# cleaner version of counting approaches alone


library(tidyverse)

# UNIT IS HALF A SECOND

fam3_pilot_h <- read_csv("2019-11-27_Family3_pilot_TL_halfsec.csv") %>%
  select(-X1, -onoff)

# takes halfsec timeline data
# level means 1 = shelf, 2 = radius, 3 = touch

approach_alone <- function(data, column, monkey_name, split_into, level) {
  
  name <- as.character(monkey_name)
  column <- head(column, -1)
  
  indexed <- data %>% 
    filter(ts_adj != 3600) %>% # get rid of end second
    mutate(index = # creates a new column for grouping
             rep(1:as.numeric(split_into), each = 3600 / as.numeric(split_into)))
  
  alone_shelf <- indexed %>%
    select(-scorer, -button, -ses, -family, -test, - object, -button) %>%
    filter(column != lag(column) & column == as.numeric(level)) %>%
    filter_at(vars(-ts_adj, -name, -index), all_vars(. == 0)) %>%
    select(index, ts_adj)
  
  return(alone_shelf)
}



mountain <-
  approach_alone(
    fam3_pilot_h,
    column = fam3_pilot_h$Mountain,
    monkey_name = "Mountain",
    split_into = 8,
    level = 1
  )
malachite <-
  approach_alone(
    fam3_pilot_h,
    column = fam3_pilot_h$Malachite,
    monkey_name = "Malachite",
    split_into = 10,
    level = 1
  )
chalk <-
  approach_alone(
    fam3_pilot_h,
    column = fam3_pilot_h$Chalk,
    monkey_name = "Chalk",
    split_into = 10,
    level = 1
  )
coal <-
  approach_alone(
    fam3_pilot_h,
    column = fam3_pilot_h$Coal,
    monkey_name = "Coal",
    split_into = 10,
    level = 1
  )
field <-
  approach_alone(
    fam3_pilot_h,
    column = fam3_pilot_h$Field,
    monkey_name = "Field",
    split_into = 10,
    level = 1
  )
rafeky <-
  approach_alone(fam3_pilot_h,
                 column = fam3_pilot_h$Rafeky,
                 monkey_name = "Rafeky",
                 split_into = 10,level = 1
  )
shiba <-
  approach_alone(fam3_pilot_h,
                 column = fam3_pilot_h$Shiba,
                 monkey_name = "Shiba",
                 split_into = 10,level = 1
  )

all <-
  bind_rows("Malachite" = malachite,
            "Mountain" = mountain,
            "Field" = field,
            "Chalk" = chalk,
            "Coal" = coal,
            "Shiba" = shiba,
            "Rafeky" = rafeky,
            .id = "name")

all$name <- as.factor(all$name)
all$index <- as.factor(all$index)

all_tally <- all %>%
  group_by(index, name, .drop = F) %>%
  tally() %>%
  arrange(index)

#all$index <- as.numeric(all$index)

library(ggstance)

ggplot(data = all_tally, aes(x = n, y = index, group = name, color = name)) +
 # geom_point(position = position_dodge(width = .3)) + 
 # geom_line(position = position_jitter(.2)) +
  geom_line(position = position_dodgev(.2)) +
  coord_flip()

ggplot(data = all_tally, aes(x = index, y = n, group = name, color = name)) +
  geom_line(position = position_dodge(.2), size = 1, lineend = 'round') #+
  #geom_point(position = position_dodge(.2)) 

ggplot(data = all_tally, 
       aes(x = index, y = n, colour= name, fill = name)) +
  geom_col(position = position_dodge())
