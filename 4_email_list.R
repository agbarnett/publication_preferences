# 4_email_list.R
# used by 4_send_emails.R
# March 2024
library(dplyr)
library(janitor)
library(stringr)
library(readxl)

## get the personal links from Qualtrics
# combine the links from the three blocks and two scenarios 
source('4_sent_emails_links_from_qualtrics.R')
sample = filter(sample, sample_number == this_sample_number) # `this_sample_number` from 4_send_emails.R

#
sample = rename(sample, 
                'name' = 'first_name') %>%
  mutate(approach = TRUE, # start by approaching all
         email = str_remove_all(email, pattern='e.mail:')) 
# quick check for duplicate emails
any(duplicated(sample$email))

## remove responders
load('data/5_AnalysisReady_v2.RData') # second version
data = select(data, recipient_email) %>%
  mutate(recipient_email = tolower(recipient_email))
sample = anti_join(sample, data, by=c('email' = 'recipient_email'))

## exclude dead, opted out and said they'd done
excluded1 = read_excel('emails/dead_emails.xlsx', sheet = 'dead or rejected')
excluded2 = read_excel('emails/dead_emails.xlsx', sheet = 'opt out')
excluded3 = read_excel('emails/dead_emails.xlsx', sheet = 'said_theyd_done')
excluded = bind_rows(excluded1, excluded2, excluded3) %>%
  mutate(email = tolower(email),
         email = str_squish(email))
#
N_before = nrow(sample)
sample = filter(sample, !email%in%excluded$email)
N_after = nrow(sample)
if(N_before - N_after != nrow(excluded)){cat('Error actually excluded ', N_before - N_after, ', should have been ', nrow(excluded), sep='')}

## add changed emails
changes = read_excel('emails/dead_emails.xlsx', sheet = 'changed') %>%
  mutate(old = str_squish(old),
         new = str_squish(new)) %>%
  rename('email' = 'old') # for merge
sample = left_join(sample, changes, by = 'email') %>%
  mutate(email = ifelse(!is.na(new), new, email)) # replace if there's a new email
