# dealing with .cd files

# wd = novel_Object/Results

join_summary <- function(file) {
  cd <-
    read.delim(
      file,
      skip = 15,
      header = T,
      sep = ",",
      stringsAsFactors = F
    ) %>% # works
    select(-Modifier,-Modifier.Name,-Event.N)
  
  cdfull <- read.delim(file, header = F)
  
  cd$Behavior <- gsub('\\s+', '', cd$Behavior) # get rid of leading space in column
  cd$Behavior.Name <- gsub('\\s+', '', cd$Behavior.Name) # get rid of leading space in column
  
  questions <- cdfull %>% slice(9:14)
  answers <-
    str_split(str_sub(questions$V1, start = 10), boundary("word")) # pull out answers
  
  answers_dfs <-
    function(list = answers) {
      # function to make df of correct length for each answer
      as.data.frame(rep(list, times = length(cd$Behavior.Name))) # returns a list of dfs
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
    bind_cols(dfs_joined, cd) # join data df with data to df with answers
  
  full[, 1:6] <-
    sapply(full[, 1:6], as.character) # answers as char to stop warnings
  
  return(full)
}

sum_monkeys <- list.files(pattern = "*.cd.res")
sum_test <- lapply(sum_monkeys, join_summary)
sum_test2 <- bind_rows(sum_test)

# need to figure out what to do with these columns (sum, avg whatever) based on what colnames actually mean

sum_by <- sum_test2 %>%
  group_by(monkey, Behavior.Name) %>%
  summarise_at(vars(...))
