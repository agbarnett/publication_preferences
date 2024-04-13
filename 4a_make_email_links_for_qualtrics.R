# 4a_make_email_list.R
# make the email list for importing into Qualtrics
# see https://www.qualtrics.com/support/survey-platform/contacts/contact-list-overview/
# and https://www.qualtrics.com/support/survey-platform/distributions-module/email-distribution/personal-links/
# April 2024
library(stringr)
library(dplyr)
TeachingDemos::char2seed('wycombe')

### part 1 samples for pilot design ###

# from 3_find_authors_pubmed.R
load('emails/3_emails.RData')
# randomly allocate block and scenario
selected = mutate(selected,
                  email = str_replace_all(email, ',', '.') # remove one typo
  ) %>%
  mutate(
    block = sample(1:3, size=n(), replace=TRUE), # randomly allocate block
    scenario = sample(1:2, size=n(), replace=TRUE), # randomly allocate scenario
    random = runif(1:n())) %>% 
  arrange(random) %>% # randomly reorder to create a random sample
  select(-random) %>%
  mutate(id = 1:n())
# check:
# with(selected, table(block,scenario))

# 50 respondents inflated by 25% response rate
N_pilot = 50/(1/4) 
N_per_group = ceiling(N_pilot / 6) # number per group 
first_sample = group_by(selected, block, scenario) %>% # ensure balance by block and scenario
  slice(1:N_per_group) %>%
  ungroup()
with(first_sample, table(block,scenario)) # check for balance

# second pilot batch due to poor response rate (April 2024)
N_second = 50/(0.05) # inflate by response rate
N_per_group_second = ceiling(N_second / 6) # number per group 
sample = filter(selected, !email %in% first_sample$email) %>% # remove first sample (do not approach again)
  group_by(block, scenario) %>% # ensure balance by block and scenario
  slice(1:N_per_group_second) %>%
  ungroup()
with(sample, table(block,scenario)) # check for balance

### part 2 sample for updated design ###

# combine two email sources
load('emails/3_emails.RData')
pilot = selected
load('emails/3_emails_2024.RData')
selected = bind_rows(selected, pilot) 
# remove those selected in the pilot
selected = filter(selected, !email %in% first_sample$email)
selected = filter(selected, !email %in% sample$email)
# remove duplicate emails
selected = distinct(selected, email, .keep_all = TRUE)

# randomly allocate block
TeachingDemos::char2seed('fleetwood')
selected = mutate(selected,
                  email = str_replace_all(email, ',', '.') # remove one typo
) %>%
  mutate(
    block = sample(1:3, size=n(), replace=TRUE), # randomly allocate block
    scenario = sample(1:2, size=n(), replace=TRUE), # randomly allocate scenario
    random = runif(1:n())) %>% 
  arrange(random) %>% # randomly reorder to create a random sample
  select(-random) %>%
  mutate(id = 1:n())
#
N_third = (300 / 0.046) # inflate by response rate from pilot
N_per_group_updated = ceiling(N_third / 6) # number per group 
sample = group_by(selected, block, scenario) %>% # ensure balance by block and scenario
  slice(1:N_per_group_updated) %>%
  ungroup()
with(sample, table(block,scenario)) # check for balance

### part 3 export for Qualtrics, had to have quote = TRUE ###

for (this_block in 1:3){
  for (this_scenario in 1:2){
    file = paste('emails/third/list_for_qualtrics_block', this_block, '_scenario', this_scenario, '.csv', sep='')
    this_sample = filter(sample, block == this_block, scenario == this_scenario)
    # have to shorten address otherwise causes error in Qualtrics
    this_sample = mutate(this_sample, affiliation = str_sub(affiliation, 1, 200))
    # write to file
    write.csv(this_sample, file = file, quote=TRUE, row.names=FALSE)
  }
}

# keep a record of the original data
today = as.Date(Sys.time())
save(sample, today, file = 'data/4_updated_sample.RData')

# then generate personal links in Qualtrics, click on directories in Qualtrics then make a new contact list
# file read in using 4_email_list.R

