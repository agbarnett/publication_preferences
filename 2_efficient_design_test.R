# 2_efficient_design_test.R
# testing the efficient design for DCE made by Sameera in ngene
# January 2024
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(broom) # for tidy
library(ggplot2)
#library(lme4) # to adjust for clustering
library("mlogit") # for mixed logit model
library(stringr)
#library(support.CEs) # for make.dataset
library(survival) # for clogit
source('99_functions.R') # for sim_choice

# get the design
design = read_excel('data/DCE design_2024.01.18.xlsx', sheet=2, skip=0) %>%
  clean_names() %>%
  select(-starts_with('x')) %>%
  arrange(block, choice_situation) %>%
  group_by(block) %>%
  mutate(qnumber = 1:n()) %>% # add question number (choice set number, nested in block)
  ungroup() 
# with(design, table(block, qnumber)) # check

## simulate data
# logit effect sizes (first category is reference and is assumed zero); select names to match design
effects = list(
  e_field = c(0, -0.2, -0.4), # assume greater desire for higher ranking
  e_format = c(0, 0.2), # assume greater desire for easier submissions
  e_decision = c(0, -0.1), # preference for faster speeds
  e_review = c(0, -0.1), # preference for helpful reviews
  e_editor = c(0, 0), # editor`s changes --- assumed null as a check
  e_evidence = c(0, -0.1)) # preference for ?
# using the function
random = sim_choice(n_respondents = 200,
                    noise_sd = 0.1,
           design = design,
           effects = effects)
# extract the two formats of the data (wide and long)
simulated = random$simulated
long = random$long

## check correlation in design matrix
design = model.matrix(choice ~ factor(field) + factor(format) + factor(decision) + factor(review) + factor(editor) + factor(evidence), 
data = long)
plot = check_cor(design[,-1]) # not intercept
plot

# using conditional logistic regression from survival library
clogit_model = clogit(choice ~ factor(field) + factor(format) + factor(decision) + factor(review) + factor(editor) + factor(evidence) + strata(strata), 
                          data = long)
summary(clogit_model)
car::vif(clogit_model) # check for colinearity

# plot effects against estimates
eplot = plot_compare(model = clogit_model, effects = effects)
eplot


# set up simulation data for DCE, see https://cran.r-project.org/web/packages/mlogit/vignettes/c5.mxl.html
simulated = mutate(simulated, chid = 1:n()) # choice ID, one for every individual's choice
simulated_ready = dfidx(simulated, 
                        idx = list(c("chid", "participant")), #
                        varying = 2:17,
                        shape = 'wide',
                        sep='_', # separator for alternatives 1 and 2
                        choice = "choice") # choice that was made

## using mlogit
# need to make 3 categories into factors
#factor(ease) + factor(completeness) + factor(impf) + factor(reputation) + factor(speed) + factor(review) + factor(impact) + factor(open)
simulated_mxl = mlogit(choice ~ ease + completeness + impf + reputation + speed + review + impact + open | 0, 
                       data = simulated_ready, 
                       rpar=c(ease = 'n', completeness = 'n', impf = 'n', reputation = 'n', 
                              speed = 'n', review = 'n', impact = 'n', open = 'n'), 
                       R = 100, halton = NA, panel = TRUE)
summary(simulated_mxl)


