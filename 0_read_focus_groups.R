# 0_read_focus_groups.R
# read the focus group data; exported from Qualtrics
# August 2023
library(readxl)
library(janitor)
library(dplyr)

## clinical focus group on 1st August

## read - deleted second row of Excel sheets
# a) text answers
text = read_excel('data/focus_groups/Nominal+group+-+text_1+August+2023_13.18.xlsx') %>%
  clean_names() %>%
  mutate(duration = as.numeric(duration_in_seconds)/60) %>%
  select('duration', starts_with('q'), -'q_data_policy_violations')

# smallsets start ranks caption[Read in data from Excel, create duration, drop columns]

# b) ranks
ranks = read_excel('data/focus_groups/Nominal+group+–+ranking_1+August+2023_13.17.xlsx') %>%
  clean_names() %>%
  mutate(duration = as.numeric(duration_in_seconds)/60) %>%
  select('duration', starts_with('q'), -'q_data_policy_violations') 

# smallsets end

# labels for ranks (got from asking for Recode in Qualtrics)
labels = c(
  "Article processing charge",
"Impact Factor",
"Journal's reputation",
"Review process and the quality of peer review process",
"Ease of submission",
"Circulation/ Target audience/ Readership",
"Speed of decision",
"Journal ranking",
"Previous experience with journal",
"Suitability of journal to paper topic",
"Know and respect the editors",
"Publisher",
"Non-predatory journal",
"Chance of rejection",
"Word count, figure allowance, etc"
)
labels = data.frame(question =1:length(labels), labels=labels) %>%
  mutate(question=as.numeric(question))

# save
save(text, ranks, labels, file = 'data/0_focus_groups.RData')


## public health and health services on 11th August

## read - deleted second row of Excel sheets
# a) text answers
text = read_excel('data/focus_groups/Nominal+group+-+text_11+August+2023_20.06.xlsx') %>%
  clean_names() %>%
  mutate(duration = as.numeric(duration_in_seconds)/60) %>%
  select('duration', starts_with('q'), -'q_data_policy_violations')

# b) ranks
ranks = read_excel('data/focus_groups/Nominal+group+–+ranking_11+August+2023_20.07.xlsx') %>%
  clean_names() %>%
  mutate(duration = as.numeric(duration_in_seconds)/60) %>%
  select('duration', starts_with('q'), -'q_data_policy_violations') 

# labels for ranks (got from asking for Recode in Qualtrics)
labels = c(
  "Open access",
  "Access by policy makers",
  "Journal ranking/ reputation/ quality/ prestige",
  "Impact Factor",
  "Peer review process/ quality/ time for peer reviewing process",
  "Speed of decision/ publication time",
  "Citation/ Reuse by others",
  "Journal scope, recognition",
  "Journal's guideline/ word count/ tables/ figures",
  "Rejection rate",
  "Study purpose/ scope and findings",
  "Article publishing charge (APC)",
  "Wide readership"
)
labels = data.frame(question =1:length(labels), labels=labels) %>%
  mutate(question=as.numeric(question))

# save
save(text, ranks, labels, file = 'data/0_focus_groups_non_clinical.RData')

## part 2: read the survey data (both groups combined)
survey = read_excel('data/focus_groups/Nominal+group+-+pre-survey_11+August+2023_20.10.xlsx') %>%
  clean_names() %>%
  mutate(duration = as.numeric(duration_in_seconds)/60) %>%
  select('duration', starts_with('q'), -'q_data_policy_violations') %>%
  filter(!is.na(q2_1)) # filter missing response

save(survey, file = 'data/0_focus_groups_survey.RData')

