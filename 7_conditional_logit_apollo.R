# 7_conditional_logit_apollo.R
# run the conditional logit models using the apollo package
# could not get to work
# May 2024
library(apollo)
library(dplyr)
library(tidyr)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName  ="MNL_journal",
  modelDescr ="Multinomial logit for journal preferences",
  indivID    ="strata",
  seed = TeachingDemos::char2seed('madrid'),
  outputDirectory = 'results'
)

## get the data (may need both) ##
source('7_prepare_journal_data.R')


# ####################################################### #
#### 3. Parameter definition                           ####
# ####################################################### #

### Vector of parameters, including any that are kept fixed 
### during estimation
apollo_beta = c(int = 0,
                e_field1  = 0,
                e_field2  = 0,
                e_format  = 0,
                e_decision = 0,
                e_review = 0,
                e_editor = 0,
                e_evidence = 0)

### Vector with names (in quotes) of parameters to be
###  kept fixed at their starting value in apollo_beta.
### Use apollo_beta_fixed = c() for no fixed parameters.
#apollo_fixed = c("asc_car")
apollo_beta_fixed = c()

# ####################################################### #
#### 4. Input validation                               ####
# ####################################################### #
apollo_inputs = apollo_validateInputs()


# ####################################################### #
#### 5. Likelihood definition                          ####
# ####################################################### #

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as
  ### in mnl_settings, order is irrelevant.
  V = list()
  V[['journal']] = int + e_field1 *field1 +
    e_field2 *field2 +
    e_format *format + 
    e_decision*decision +
    e_review*review +
    e_editor*editor +
    e_evidence*evidence
    
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(journala=1), 
#    avail         = list(car=av_car, bus=av_bus, 
#                         air=av_air, rail=av_rail), 
    choiceVar     = choicen,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ####################################################### #
#### 6. Model estimation and reporting                 ####
# ####################################################### #

model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs,
                        list(writeIter=FALSE))
