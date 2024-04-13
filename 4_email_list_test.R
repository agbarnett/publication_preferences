# 4_email_list_test.R
# used by 3_send_emails.R; test version
# March 2024
library(dplyr)
library(janitor)
library(stringr)

## get the personal links from Qualtrics, see 3a_make_email_list_test.R, links generated on 15-Mar-2024
# get the links
sample = read.csv(file='emails/export-EMD_pFO2m3j5lRv3U1H-2024-03-15T05-53-37-470Z.csv') %>%
  clean_names() %>%
  select(first_name, email, link) %>%
  mutate(
    approach = TRUE, # approach everyone
    email = str_squish(tolower(email))) %>% # for merge
  rename('name' = 'first_name')
