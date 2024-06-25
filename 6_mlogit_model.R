# 6_mlogit_model.R
# run the multinomial logit model on the DCE data; currently using conditional logistic
# April 2024
#library(mlogit)
library(readxl)
library(survival) # for clogit
library(tidyr)
library(dplyr)
library(broom)
library(ggplot2)

## get the designs
# pilot
design = read_excel('data/DCE design_2024.03.13.xlsx', sheet='adrian', skip=0) %>%
  clean_names() %>%
  select(-starts_with('x')) %>%
  arrange(block, choice_situation) %>% # choices were ordered this way in Qualtrics
  group_by(block) %>%
  mutate(task_number = 1:n()) %>% # add task number for merging (choice set number, nested in block)
  ungroup() 
design_long = NULL
for (blocknum in 1:3){
  for (task in 1:8){
    this_set = filter(design, block == blocknum, task_number == task) # use one choice set
    # split design into numbers 1 and 2
    choice_one = select(this_set, block, task_number, contains('choice1')) %>%
      rename_with(.fn = ~gsub("choice1_", "", .), .cols=everything()) %>% # rename columns
      mutate(choicenum = 1)
    choice_two = select(this_set, block, task_number, contains('choice2')) %>%
      rename_with(.fn = ~gsub("choice2_", "", .), .cols=everything()) %>% # rename columns
      mutate(choicenum = 2)
    design_long = bind_rows(design_long, choice_one, choice_two)
  }
}

# main sample - to do
# "AuthorP_main_2024.04.9_NoASC_187166.xlsx" 

# get the qualtrics data and switch to long format
load('data/5_AnalysisReady.RData')
long_first = select(data, id, block, scenario, q5, q7, q9, q11, q13, q15, q17, q20) %>% # not the dominant or re-test
  group_by(id, block, scenario) %>%
  pivot_longer(cols = starts_with('q'), names_to='task', values_to='selection') %>%
  ungroup() %>%
  mutate(task_number = case_when( # for merging
    task == 'q5' ~ 1,
    task == 'q7' ~ 2,
    task == 'q9' ~ 3,
    task == 'q11' ~ 4,
    task == 'q13' ~ 5,
    task == 'q15' ~ 6,
    task == 'q17' ~ 7,
    task == 'q20' ~ 8
  ),
    strata = 1:n()) # each choice task per person is a strata
# get binary yes/no for both choices
long1 = mutate(long_first, choicenum = 1, choice = as.numeric(selection == 'Journal A'))
long2 = mutate(long_first, choicenum = 2, choice = as.numeric(selection == 'Journal B'))
long = bind_rows(long1, long2) 
# table(table(long$strata)) # check, should all be 2

# add the design to the qualtrics data
for_analysis = full_join(long, design_long, by=c('block','task_number','choicenum')) %>%
  filter(!is.na(choice)) %>% # can't use if choice is missing
  mutate(field = factor(field),
         field = relevel(field, ref = '3'), # reference is no ranking
         format = factor(format),
         format = relevel(format, ref = '1'), # reference is major formating
         decision = factor(decision),
         decision = relevel(decision, ref = '2'), # reference is slow
         review = factor(review),
         review = relevel(review, ref = '2'), # reference is unhelpful
         editor = factor(editor),
         editor = relevel(editor, ref = '2'), # reference is cut results
         evidence = factor(evidence),
         evidence = relevel(evidence, ref = '2')) # reference is not useful

# using conditional logistic regression from survival library#
formula = 'choice ~ 
  field + 
  format + 
  decision + 
  review + 
  editor + 
  evidence + 
  strata(strata)' 
# add interactions
formula.plus = paste(formula, '+ field:editor') 
clogit_model = clogit(as.formula(formula.plus), 
                      data = for_analysis)
summary(clogit_model)
car::vif(clogit_model) # check for colinearity

# vary by scenario
clogit_model1 = clogit(as.formula(formula), 
                       data = filter(for_analysis, scenario == 1))
clogit_model2 = clogit(as.formula(formula), 
                       data = filter(for_analysis, scenario == 2))
## plot
# for ordering x-axis
breaks = c('field1','field2','format2','decision1','review1','editor1','evidence1')
labels = c('Highest JIF','Moderate JIF','Major formatting','Fast decision','Helpful review','Minor editorial change','Useful for promotion')
# get estimates with intervals
e1 = tidy(clogit_model1, conf.int = TRUE)
e2 = tidy(clogit_model2, conf.int = TRUE)
for_plot = bind_rows(e1, e2, .id = 'scenario') %>%
  mutate(term = factor(term, levels = rev(breaks), labels=rev(labels))) # rev so that order in plot matches tables

# plot
plot = ggplot(data= for_plot, aes(x=term, y=estimate, ymin=conf.low, ymax=conf.high, col=factor(scenario)))+
  geom_hline(lty=2, yintercept=0, col='grey77')+
  geom_point(position=position_dodge(width=0.25), size=3)+
  geom_errorbar(width=0, position=position_dodge(width=0.25), linewidth=1.05)+
  scale_color_manual('Scenario', values=c('darkseagreen2','darkorange2'), labels=c('First submission','Desk rejected'))+
  xlab('')+
  ylab('Estimated effect')+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.minor = element_blank() )

plot
ggsave(filename = 'figures/clogit.jpg', plot = plot, dpi = 500, units='in', width=5.5, height=5)

