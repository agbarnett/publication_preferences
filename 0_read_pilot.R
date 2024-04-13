# 0_read_pilot.R
# read the pilot focus group data; exported from Qualtrics
# June 2023
library(readxl)
library(janitor)
library(dplyr)

## read
# a) text answers
text = read_excel('data\\pilot\\Nominal+group+-+text_23+June+2023_15.36.xlsx') %>%
  clean_names() %>%
  mutate(duration = as.numeric(duration_in_seconds)/60) %>%
  select('duration', starts_with('q'), -'q_data_policy_violations')

# b) ranks
ranks = read_excel('data/pilot/Nominal+group+–+ranking_23+June+2023_15.35.xlsx') %>%
  clean_names() %>%
  mutate(duration = as.numeric(duration_in_seconds)/60) %>%
  select('duration', starts_with('q'), -'q_data_policy_violations') 

# labels for ranks
labels = c(
  "speed of decision",
"cited in policy/clinical guideline influencing in policy",	
  "open access",
"journal that matches the paper's topic",	
"impact factor",	
"chances of rejection",
"journal reputation",	
"journal ranking",	
"indexing (e.g. pubmed)",	
"peer review process and its quality",
"ease of submission",	
"article processing charge (APC)",
"equity of access for low and middle income countries",
"clear authorship statement (who did what)",
"journal's preprint policy",
  "wide audience and readership",
"journal read by peers",
  "overall quality of paper"
)
labels = data.frame(question =1:length(labels), labels=labels) %>%
  mutate(question=as.numeric(question))

# save
save(text, ranks, labels, file = 'data/0_pilot.RData')
