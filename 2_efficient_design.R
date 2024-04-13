# 2_efficient_design.R
# looking at balance of designs (so all levels appear same number of times)
# see book https://doi.org/10.1007/978-981-99-4562-7
# December 2023
library(dplyr)
library(janitor)
library(survival) # for conditional logistic regression
library(support.CEs) # for design tools
library(broom) # for tidy
library(stringr)
library(ggplot2)
source('99_functions.R') # for simulated DCE data


# number of choice sets for full factorial
(full_factorial = 2^5 * 3^1)
(full_factorial = 2^3 * 3^1)

#
unlabeled = rotation.design( attribute.names = list(
  ranking = c("high","moderate",'none'), 
  format = c("major","minor"), 
  speed = c("fast","slow"), 
  #reviews = c("help improve","contradictory"), 
  #editor = c("minor changes","cut analysis"), 
  evidence = c("next grant","policy")), 
  nalternatives = 2, 
  nblocks = 2, 
  row.renames = FALSE, 
  seed = 987, 
  randomize = FALSE)

#

# try full factorial design for smaller design with just three attributes
full.fac = Lma.design( attribute.names = list(
  ranking = c("high","moderate",'none'), 
  format = c("major","minor"), 
  evidence = c("next grant","policy")), 
  nalternatives = 2, 
  nblocks = 1)
## format as per design matrix from Sameera's example
# rename choices
choice_one = full.fac$alternatives$alt.1 %>%
  rename_with(.fn = ~gsub("^", "choice1_", .), -(1:3)) # all but first three columns
choice_two = full.fac$alternatives$alt.2 %>%
  rename_with(.fn = ~gsub("^", "choice2_", .), -(1:3)) # all but first three columns
design = full_join(choice_one,
                   choice_two, by=c('BLOCK','QES')) %>%
  clean_names() %>%
  select(-starts_with('alt')) %>%
  rename('qnumber' = 'qes') 

# logit effect sizes (first category is reference and is assumed zero)
effects = list(
  e_ranking = c(0.2, 0.1, 0), # prefer higher ranking
  e_format = c(0, 0), # prefer easier submission
  e_evidence = c(0, 0.2)) # null

# create random data
random = sim_choice(n_respondents = 300,
                    noise_sd = 2, # standard deviation for noise 
                    design = design,
                    effects = effects)
# extract the two formats of the data (wide and long)
simulated = random$simulated
long = random$long

# using conditional logistic regression from survival library; each question is a strata
clogit_model = clogit(choice ~ factor(ranking) + factor(format) + factor(evidence) + strata(strata), 
                          data = long)
summary(clogit_model)
car::vif(clogit_model)

# plot effects against estimates
eplot = plot_compare(model = clogit_model, effects = effects)
eplot


# reconstruct probabilities?