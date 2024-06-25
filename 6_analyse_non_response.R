# 6_analyse_non_response.R
# look at non-response using the sampling frame
# June 2024
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
load('emails/3_emails.RData') # data = `selected`, from 3_find_authors_pubmed.R
selected1 = selected
load('emails/3_emails_2024.RData') # second sample
selected = bind_rows(selected, selected1) %>% 
  mutate(email = tolower(email))

# only get those that were sent
source('5_emails_sent.R') # data = 'sample_sent'
selected = left_join(sample_sent, selected, by='email') %>%
  unique()
remove(sample_sent) # clean up

# exclude dead emails
excluded = read_excel('emails/dead_emails.xlsx', sheet = 'dead or rejected') %>%
  mutate(email = tolower(email),
         email = str_squish(email))
selected = filter(selected, !email %in% excluded$email)
cat('There were ', nrow(excluded), ' excluded.\n', sep='')
remove(excluded) # clean up

# remove duplicates - people were not emailed twice
selected = group_by(selected, email) %>% slice(1) %>% ungroup()

## get responders
#
load('data/5_AnalysisReady.RData') # data = `data`
data1 = select(data, recipient_email)
#
load('data/5_AnalysisReady_v2.RData') # data = `data`
data2 = select(data, recipient_email)
data = bind_rows(data1, data2)
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

# function to split labels
split.fun <- function(x, labs, digits, varlen, faclen)
{
  # replace commas with spaces (needed for strwrap)
  labs <- gsub(",", " ", labs)
  for(i in 1:length(labs)) {
    # split labs[i] into multiple lines
    labs[i] <- paste(strwrap(labs[i], width = 25), collapse = "\n")
  }
  labs
}

# export
jpeg('figures/6_tree_response.jpg', width=5, height = 5, units='in', res=600)
rpart.plot(tree, split.fun = split.fun)
dev.off()

# make table of predicted probabilities based on three leaves of the tree
numbers = mutate(selected, pred = predict(tree)) %>%
  group_by(emails, pred) %>%
  summarise(n = n()) 
for_table = group_by(numbers, pred) %>%
  summarise(
    N = sum(n),
    label = paste(emails, collapse = ', ')) %>%
  ungroup() %>%
  mutate(
    pred = round(pred*100)/100,
    percent = round(prop.table(N)*100)) %>%
  select(label, N, percent, pred)
write.table(for_table, file='results/6_tree_table.txt', quote = FALSE, row.names = FALSE)
      