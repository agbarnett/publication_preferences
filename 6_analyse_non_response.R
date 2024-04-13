# 6_analyse_non_response.R
# look at non-response using the sampling frame
# April 2024
library(dplyr)
library(rentrez)
library(janitor)
library(stringr)
library(readxl)
library(tidyr)
library(rpart)
library(rpart.plot)
source('99_functions.R')

# get email data that has affiliation
load('emails/3_emails.RData') # data = `selected`
selected = mutate(selected, email = tolower(email))

# only get those that were sent
source('4_sent_emails.R') # data = 'sample'
selected = filter(selected, email %in% sample$email)
remove(sample) # clean up

# exclude dead emails
excluded = read_excel('emails/dead_emails.xlsx', sheet = 'dead or rejected') %>%
  mutate(email = tolower(email),
         email = str_squish(email))
selected = filter(selected, !email %in% excluded$email)
remove(excluded) # clean up

# get responders
load('data/5_AnalysisReady.RData') # data = `data`
data = select(data, recipient_email) %>%
  mutate(recipient_email = tolower(recipient_email),
         responded = 1)
selected = left_join(selected, data, by=c('email' = 'recipient_email')) %>%
  replace_na(list(responded=0)) # missing are non-responders
remove(data) # clean up

# extract information from email and affiliation
selected = separate(selected, col='email', into=c(NA, 'emails'), sep='@') %>% # exclude part before email
   mutate(
     affiliation = tolower(affiliation),
     university = str_detect(affiliation, '\\buniversity'),
     hospital = str_detect(affiliation, '\\bhospital'),
     dentist = str_detect(affiliation, '\\bdentist'),
     #
     emails = extract_country(emails) # function to extract key parts of email
   )
#group_by(selected, emails) %>% tally() %>% arrange(-n) %>% print(n=20)  # quick check

# extract publication date from pubmed?

## run classification tree
# decrease potential complexity of tree
my.control = rpart.control()
my.control$minbucket = 20
my.control$minsplit = 40
my.control$maxdepth = 5
# run tree
formula = 'responded ~ hospital + dentist + university + emails'
TeachingDemos::char2seed('fleetwood')
tree = rpart(formula, data = selected, control = my.control)
rsq.rpart(tree)
rpart.plot(tree)
