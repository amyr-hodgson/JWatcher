# First should do total duration on the shelf by monkey/dev stage
# Counts are in transitions analysis script

library(tidyverse)

# UNIT IS HALF A SECOND

fam3_pilot <- read_csv("2019-11-27_Family3_pilot_TL_halfsec.csv") %>%
  select(-X1, -onoff)

fam3_pilot_long <- fam3_pilot %>%
  gather(
    key = 'monkey',
    value = 'loc',
    Rafeky, Shiba, Mountain, Field, Chalk, Coal, Malachite
  ) #  

fam3_test1 <- read_csv("2019-11-27_Family3_test1_TL_halfsec.csv") %>%
  select(-X1, -onoff)

fam3_test1_long <- fam3_test1 %>%
  gather(
    key = 'monkey',
    value = 'loc',
    Rafeky, Mountain, Field, Chalk, Coal, Malachite
  ) 

counts <- fam3_pilot_long %>%
  group_by(monkey) %>%
  tally(loc) %>%
  mutate(adj = n/(sum(fam3_pilot_long$loc)))

counts_1 <- fam3_test1_long %>%
  group_by(monkey) %>%
  tally(loc) %>%
  mutate(adj = n/(sum(fam3_test1_long$loc)))

# Time on the shelf alone vs with another

Chalk <- fam3_pilot %>%
  filter(Chalk != 0)

# Chalklong <- Chalk %>%
#   gather(key = 'monkey',
#          value = 'loc',
#          8:14)
#Chalk[, 9:14] == 0
# Chalkalone <- Chalklong %>%
#   filter(monkey != "Chalk") %>%
#   filter()

Chalkalone <- Chalk %>%
  filter(Coal == 0 &
           Rafeky == 0 &
           Malachite == 0 & Field == 0 & Mountain == 0 & Shiba == 0)

sum(Chalk$Chalk)/2 # total approach (seconds)
sum(Chalkalone$Chalk)/2 # total approach alone (seconds)
(sum(Chalkalone$Chalk)/2) / (sum(Chalk$Chalk)/2)

Field <- fam3_pilot %>%
  filter(Field != 0)

Fieldalone <- Field %>%
  filter(Coal == 0 &
           Rafeky == 0 &
           Malachite == 0 & Chalk == 0 & Mountain == 0 & Shiba == 0)

(sum(Fieldalone$Field)/2) / (sum(Field$Field)/2)

# Better way?
Fie <- fam3_pilot %>%
  filter(Field != 0)

Field <- fam3_pilot %>%
  filter(Field != 0)%>%
  summarise(tot = sum(Field))

Fieldtot <- Field %>%
  summarise(tot = sum(Field))

Fieldcut <- Fie %>%
  select(-ses, -button, -object, -test, -scorer, -family, -ts_adj)

Fieldlone <- Fieldcut %>%
  filter_at(vars(-Field), all_vars(. == 0)) %>%
  summarise(lon = sum(Field))

fi <- bind_cols(Fieldlone, Field) %>%
  mutate(prop = lon/tot)

# function for proportion of time alone 
# could weight if divided by seconds not weighted?

# not working

alone <- function(df, animal) {
  
  an <- df %>% filter(animal != 0)
  
  total <-
    df %>% filter(animal != 0) %>% summarise(tot = sum(animal))
  
  cut <-
    an %>% select(-ses,-button,-object,-test,-scorer,-family,-ts_adj)

  lone <- cut %>% filter_at(vars(-animal), all_vars(. == 0)) %>%
    summarise(lon = sum(animal))
  
  pr <- bind_cols(lone, total) %>%
    mutate(prop = lon / tot)
  
  return(pr)
}

# alone(df, "Field")
# alone(df, Field)
alone(df, df$Field)
# 
# alone(field)

#alone1 <- function(df, animal) {
  
  an <- df %>% filter(df$Chalk != 0)
  
  total <-
    df %>% filter(df$Chalk != 0) %>% summarise(tot = sum(df$Chalk))
 
  cut <-
    an %>% select(-ses,-button,-object,-test,-scorer,-family,-ts_adj)
  
   lone <- cut %>% 
     filter_at(vars(-df$Chalk), all_vars(. == 0)) 
   lone <- cut %>% 
     filter_at(vars(-animal), all_vars(. == 0)) 
   
   #%>%
   animal <- as.character('Chalk')
   animal <- 'Chalk'
     summarise(lone, lon = sum(lone$Chalk)) #you
     summarise(lone, lon = sum(lone[,'Chalk']))
   alone <-  summarise(lone, lon = sum(lone[,animal]))
  # 
   pr <- bind_cols(alone, total) %>%
     mutate(prop = lon / tot)
  # 
   return(total)
#}

alone1(df, df$Malachite)

# This one is subideal but works

alone2 <- function(data, column, name) {
  
  animal_name <- as.character(name)
  animal_nonzero <- data %>% filter(column != 0)
  
  total_approach <-
    data %>% filter(column != 0) %>% summarise(tot = sum(column))
  # could change sum(column) bit above to nrows(column) to allow weighting by app type?
  only_monkeys <-
    animal_nonzero %>% select(-ses,-button,-object,-test,-scorer,-family,-ts_adj)
  
  approach_alone <- only_monkeys %>% 
    filter_at(vars(-animal_name), all_vars(. == 0)) 
 
  alone_sum <-  summarise(approach_alone, lon = sum(approach_alone[,animal_name]))
   
  proportion <- bind_cols(alone_sum, total_approach) %>%
    mutate(prop = lon / tot)
   
  return(proportion)
}

chalk <- alone2(fam3_pilot, fam3_pilot$Chalk, 'Chalk')
mal <- alone2(fam3_pilot, fam3_pilot$Malachite, 'Malachite')
moun <- alone2(fam3_pilot, fam3_pilot$Mountain, 'Mountain')
field <- alone2(fam3_pilot, fam3_pilot$Field, 'Field')
shiba <- alone2(fam3_pilot, fam3_pilot$Shiba, 'Shiba')
rafeky <- alone2(fam3_pilot, fam3_pilot$Rafeky, 'Rafeky') # nothing to analyse
coal <- alone2(fam3_pilot, fam3_pilot$Coal, 'Coal')

alone_all <- bind_rows("Shiba" = shiba,
                       "Chalk" = chalk, 
                       "Coal" = coal,
                       "Malachite" = mal,
                       "Field" = field,
                       "Mountain" = moun, .id = "name")

chalk1 <- alone2(fam3_test1, fam3_test1$Chalk, 'Chalk')
mal1 <- alone2(fam3_test1, fam3_test1$Malachite, 'Malachite')
moun1 <- alone2(fam3_test1, fam3_test1$Mountain, 'Mountain')
field1 <- alone2(fam3_test1, fam3_test1$Field, 'Field')
rafeky1 <- alone2(fam3_test1, fam3_test1$Rafeky, 'Rafeky') # nothing to analyse
coal1 <- alone2(fam3_test1, fam3_test1$Coal, 'Coal')

alone_all1 <- bind_rows("Chalk" = chalk1, 
                       "Coal" = coal1,
                       "Malachite" = mal1,
                       "Field" = field1,
                       "Mountain" = moun1,
                       "Rafeky" = rafeky1, .id = "name")
# condensing bouts to have only change times (preserving locations for others)
# window that scrolls down, checks previous and ahead values (lag()). if different, new col 1.
# if same, 0