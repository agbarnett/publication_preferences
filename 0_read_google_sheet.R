# 0_read_google_sheet.R
# read the google sheets with the preferences from the 
# Jan 2023 (update Dec 2023)
#library(gsheet) # did not work
library(janitor)
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# url = 'docs.google.com/spreadsheets/d/1BuVsRMAVMh3GhrlO-bTz0NY5NoKNZcyLK9c4x022wi4'
# downloaded on 31 Jan 2023
data = read_excel('data/Potential attributes for author preferences DCE.xlsx', sheet=1) %>%
  clean_names() %>%
  mutate(group = ifelse(group=='', NA, group)) %>% # change to missing for fill
  fill(group) # carry forward missing groups

# transform to long
long = pivot_longer(data = data, 
                    cols = -c(group, attribute),
                    names_to = 'author') %>%
  mutate(value = ifelse(value=='', FALSE, TRUE),
         value = ifelse(is.na(value), FALSE, value))

# remove excluded studies
long = filter(long,
              !str_detect(author, '^ioannidis|^foss|^dore|^chowdhury'))

# save
save(long, file='data/0_literature.RData')
