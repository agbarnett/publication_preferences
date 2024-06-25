################################################################################
## Karin Groothuis-Oudshoorn
## March 25th 2024
################################################################################

## ******** NOTE! REPLACE "PATH" IN LINE 38 WITH DATA FILE LOCATION ******** ##

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #


setwd("E:/nBox/QUT/AusHSI work/Literature/DCE/DCE in R")



### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(readr)
library(dplyr)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Conditional_logit_Chapter_Example",
  modelDescr      = "Conditional logit model on example article Patient",
  indivID         = "id",  
  #mixing          = TRUE,
  nCores          = 4,
  outputDirectory = "output"
)

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #

# load data into object database. Replace PATH with location of the data file
database = read_csv("dataAB_wide.csv")


# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta <- c(b_head1 = 1, 
                 b_head2 = 0.5, 
                 b_risk1 = 1,
                 b_risk2 = .5,
                 b_mode1 = 0.5,
                 b_mode2 = 0.5,
                 b_cost = -0.011)
### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_b_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[['alt1']]  = b_head1*head01 + b_head2*head11 + b_risk1*risk01 + b_risk2*risk0.51 + b_mode1*pill1 + b_mode2*inject1 + b_cost*cost1
  V[['alt2']]  = b_head1*head02 + b_head2*head12 + b_risk1*risk02 + b_risk2*risk0.52 + b_mode1*pill2 + b_mode2*inject2 + b_cost*cost2
    
    
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2), 
    avail         = list(alt1=1, alt2=1), 
    choiceVar     = choice,
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

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model, modelOutput_settings = list(printPVal = 2))

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model, saveOutput_settings = list(printPVal = 2))

