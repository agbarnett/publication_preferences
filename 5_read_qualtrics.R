# 5_read_qualtrics.R
# read the data directly from qualtrics
# March 2024
library(dplyr)
library(qualtRics)
library(janitor)
library(readxl)
library(stringr)
library(tidyr) # for pivot

### Part 1: labels ### 

# get the variable names; from a qualtrics export to Excel with no data
names = read_excel('data/5_qualtrics_labels.xlsx', col_names = FALSE, n_max = 1) %>% 
  clean_names()
names = janitor::make_clean_names(names)  
# ... then get the labels
in_labels = read_excel('data/5_qualtrics_labels.xlsx', col_names = FALSE, skip=1, n_max = 1)
labs = as.character(matrix(in_labels))
# make a data frame of labels
not_needed = c('end_date', 'response_id', 'ip_address', 'location_latitude', 'location_longitude', 'user_language',
  'recipient', 'policy', 'sentiment', 'external_reference', 'distribution_channel', 'q1')
labels = bind_cols(names=names, labels=labs) %>%
  filter(!str_detect(names, paste(not_needed, collapse = '|'))) # drop variables that are not needed, some are qualtrics variables

# add labels for generated variables
new_labels = read.table(header=TRUE,sep=',', text='
names,labels
duration_in_mins,Duration (in minutes)
')
# just need one label
labels = bind_rows(labels, new_labels) %>% 
  unique() 

### Part 2: data on Qualtrics ###
## set up API access
source('99_my_qualtrics_key.R')

## list all available surveys
surveys = all_surveys() %>%
  filter(str_detect(name, pattern='^Journal DCE')) # just the DCE surveys
# Loop through multiple surveys (blocks and scenarios)
raw = NULL
for (k in 1:nrow(surveys)){
  ## get block and scenario number
  block = as.numeric(str_extract(surveys$name[k], pattern = '(?<=block )[0-9]'))
  scenario = as.numeric(str_extract(surveys$name[k], pattern = '(?<=scenario )[0-9]'))
  ## now get data from Qualtrics
  this_survey = fetch_survey(surveyID = surveys$id[k], 
                             label = TRUE, # use text for surveys
                             add_column_map = FALSE,
                             force_request = TRUE,
                             verbose = TRUE)
  if(nrow(this_survey) > 0){
    this_survey = clean_names(this_survey) %>%
      select(-q_data_policy_violations, -ip_address, -status, -user_language, -external_reference, -location_latitude, -location_longitude) %>% # variables not used
      mutate(block = block,
             scenario = scenario,
             id = paste(block, '.', scenario, '.', 1:n(), sep='')) %>% # make unique ID number
      mutate_at(vars(starts_with("q")), funs(as.character)) # change all questions to character, better for concatenating
    # concatenate
    raw = bind_rows(raw, this_survey)
  }
}
raw = select(raw, 'id', everything()) # move ID to first column

## removals ##

# remove those who did not consent
raw = filter(raw, q1=='Yes') %>% select(-q1)

# remove test responses prior to distribution
raw = filter(raw, distribution_channel == 'gl') %>% # must be a response from a generated link
  select(-distribution_channel)

# remove people with zero progress ... 
cat('There were ', nrow(filter(raw, progress==0)),' respondents with a zero progress.\n', sep='')
raw = filter(raw, progress > 0)
# ... and who did not answer any DCE questions
dce_questions = paste('q', c(seq(3,17,2),20,22), sep='')
#
selected_questions = select(raw, 'response_id', all_of(dce_questions)) %>% # 
  mutate_all(as.character) %>%
  pivot_longer(cols = -response_id) %>%
  mutate(missing = is.na(value)) 
# overall missing
all_missing = group_by(selected_questions, response_id) %>%
  summarise(n=n(), miss = sum(missing)) %>%
  filter(miss == n)
cat('There were ', nrow(all_missing),' respondents who completed no DCE questions\n', sep='')
raw = filter(raw, !response_id %in% all_missing$response_id)

## data edits
data = mutate(raw, 
              q25_8_text = str_squish(q25_8_text), # remove unwanted spaces from optional text
              q27 = as.numeric(q27), # years in research
              q28 = as.numeric(q28), # number of peer-reviewed papers in research
              q31 = str_squish(q31)) %>% 
  select(-recorded_date, # just use start_date
         -starts_with('q24_do'),
         -starts_with('q30_do'), 
         -starts_with('q40_do')) # do not need the randomised order for questions 24, 30 or 40

# question duration and progress
data = mutate(data,
              duration_mins = duration_in_seconds/60,
              progress_cat = cut(progress, c(-0.001,5,50,75,100)), # progress percent as categories
              start_date = as.Date(start_date)) %>% # do not need time (just date); time zone is AEST
  select(-duration_in_seconds)

### save ###
# main version
comment_questions = c('q25_8_text','q31')
data = select(data, -recipient_first_name, -recipient_last_name, -recipient_email) # not needed
save(data, labels, comment_questions, file='data/5_AnalysisReady.RData')
# export for Sameera, remove variables with commas
for_csv = select(data, -contains('recipient'), -'end_date', -'finished', -'progress', -'response_id', -all_of(comment_questions), -q25, -q26_4_text)
#write.table(for_csv, file='data/tab_data.txt', sep='\t', quote = FALSE, row.names = FALSE) # tab format
write.table(for_csv, file='data/tab_data.csv', sep=',', quote = FALSE, row.names = FALSE) # csv format
# export labels
source('99_functions.R')
labels = mutate(labels, label2 = nice_rename(names))
write.table(labels, file='data/labels.txt', sep='\t', quote = FALSE, row.names = FALSE)

