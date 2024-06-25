# 7_prepare_journal_data.R
# prepare the journal data for the conditional logit
# called by 7_conditional_logit_apollo.R and 7_conditional_logit_logitr.R
# June 2024
library(janitor)
library(readxl)
library(stringr)

## part 1: get the design, using my slightly modified design from ngene ##

# second design launched in April 
design2 = read_excel('data/DCE design_2024.04.10.xlsx', sheet='adrian', skip=0) %>%
  clean_names() %>%
  select(-starts_with('x')) %>%
  arrange(block, choice_situation) %>%
  group_by(block) %>%
  mutate(qnumber = 1:n()) %>% # add question number (choice set number, nested in block)
  ungroup() 
# with(design2, table(block, qnumber)) # check, should be balanced

# first design launched in March
design1 = read_excel('data/DCE design_2024.03.13.xlsx', sheet='adrian', skip=0) %>%
  clean_names() %>%
  select(-starts_with('x')) %>%
  arrange(block, choice_situation) %>%
  group_by(block) %>%
  mutate(qnumber = 1:n()) %>% # add question number (choice set number, nested in block)
  ungroup() 
# with(design1, table(block, qnumber)) # check, should be balanced

# concatenate the designs
design = bind_rows(design1, design2, .id = 'design') %>%
  mutate(design = as.numeric(design))

## part 2: get the survey data ##

# first survey 
load('data/5_AnalysisReady.RData') # from 5_read_qualtrics.R
dce_questions = paste('q', c(seq(5,17,2),20), sep='') # without dominant and repeat
data1 = select(data, id, all_of(dce_questions))
# second survey
load('data/5_AnalysisReady_v2.RData') # from 5_read_qualtrics_v2.R
data2 = select(data, id, all_of(dce_questions))
data = bind_rows(data1, data2, .id = 'design') %>%
  mutate(design = as.numeric(design))

# transform into long format
long = select(data, design, id, all_of(dce_questions)) %>%
  group_by(design, id) %>%
  pivot_longer(all_of(dce_questions), names_to = 'question', values_to = 'choice') %>%
  ungroup() %>%
  separate(col = id, into=c('block','scenario',NA), sep='\\.', convert=TRUE, remove = FALSE) %>% # extract block and scenario
  mutate(qnumber = as.numeric(as.factor(as.numeric(str_remove(question,'q'))))) # convert text question to number

# combine design and answers
data = left_join(long, design, by=c('design','block','qnumber'))

# double answers
choice_one = select(data, id, design, block, qnumber, choice, contains('choice1_')) %>%
  rename_with(.fn = ~gsub("choice1_", "", .), .cols=everything()) # rename columns
choice_two = select(data, id, design, block, qnumber, choice, contains('choice2_')) %>%
  rename_with(.fn = ~gsub("choice2_", "", .), .cols=everything()) # rename columns
database = bind_rows(choice_one, choice_two, .id = 'CHOICE') %>%
  mutate(CHOICE = ifelse(CHOICE == 1, 'Journal A', 'Journal B'),
    strata = paste(id, '.', design, '.', qnumber, sep=''), # create unique strata for each choice per participant (needed for clogit)
         choicen = as.numeric(choice == CHOICE)) %>% # create binary choice
  select(-choice) %>%
  arrange(strata) # needed by apollo

# transform into a model matrix (see weights sheet of DCE design_2024.03.13 for reference)
database = mutate(database, 
              field1 = as.numeric(field==1), # ranked highest
              field2 = as.numeric(field==2), # moderate rank (ref = no rank)
              format = format - 1, # 1 = reference (major formatting)
              decision = 2 - decision, # 2 = reference (slow)
              review = 2 - review, # 2 = reference (unhelpful)
              editor = 2 - editor, # 2 = reference (cut)
              evidence = 2 - evidence) %>% # 1 = reference (not useful)
  select(-field)

# add scenario, back from block
database = separate(database, col = 'strata', into=c(NA,'scenario',NA,NA,NA), remove = FALSE)

remove(design) # otherwise gets confused with later paste
