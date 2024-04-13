# 2_analyse_locate.R
# Analyse the LOCATE data as a practice of modelling the results of a DCE
# December 2023
library(dplyr)
library(janitor)
library(ggplot2)
source('99_functions.R')
library("mlogit") # for mixed logit model

# get the data
data = read.csv('sameera/Final_LOCATE_CLEANED_All.csv') %>% 
  clean_names() %>%
  mutate(
    costc = cost / 100, # scale to per 100
    choice_set = floor((1:n()+1)/2)) %>% # add unique choice set per id
  select(-cost) # drop old cost

# check correlation in design matrix
design = select(data, contains('_'), 'wait', 'costc') %>%
  select(-choice_set)
plot = check_cor(design)
plot

## set up data for mlogit
# first get data in wide format
one = filter(data, altij==1) %>% select(-altij, -cset)
two = filter(data, altij==2) %>% select(-altij, -cset)
wide = full_join(one, two, suffix = c('.1','.2'), by=c('id','age','sex','choice_set')) %>%
  mutate(decision = case_when(
    decision.1 == 1  ~ 1,
    decision.2 == 1  ~ 2
  )) %>%
  select(-decision.1, -decision.2) %>%
  select('id','age','sex','choice_set', 'decision', everything())

# just use factor versions of conduct and accuracy
wide = select(wide, 'id','age','sex','choice_set', 'decision', starts_with('con_'), starts_with('acc_'), starts_with('costc'), starts_with('wait'), starts_with('sou_'))  

#
mlogit_ready = dfidx(wide, 
                    idx = list(c("choice_set", "id")), #
                    varying = 6:27,
                    shape = 'wide',
                    sep='.', # separator for alternatives 1 and 2
                    choice = "decision") # choice that was made

# reference categories are: conduct = ?, accuracy = 75, cost is linear, wait is linear, source = x, 
mlogit_model = mlogit(decision ~ con_com + con_gp + acc_85 + acc_95 + costc + wait + sou_2 + sou_3| 0, 
                         data = mlogit_ready, 
                         rpar=c(con_com = 'n', con_gp = 'n', acc_85 = 'n', acc_95 = 'n', 
                                costc = 'n', wait= 'n', sou_2='n', sou_3='n'), # random parameters assumed normal
                         R = 100, halton = NA, panel = TRUE)
summary(mlogit_model)
#car::vif(mlogit_model)
