################################################################################
## Karin Groothuis-Oudshoorn
## March 25th 2024
################################################################################

## ******** NOTE! REPLACE "PATH" IN LINE 39 WITH DATA FILE LOCATION ******** ##

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)
library(readr)
library(dplyr)
library(stringr)
### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName       = "Mixed_logit_Chapter_Example", 
  modelDescr      = "Mixed logit model on example article Patient",
  indivID         = "id",  
  mixing          = TRUE,
  nCores          = 4,
  outputDirectory = "output", 
  seed = 24
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
apollo_beta <- c(m_b_head1 = 1, 
                 s_b_head1 = 0.1,
                 m_b_head2 = 0.5, 
                 s_b_head2 = 0.1,
                 m_b_risk1 = 1,
                 s_b_risk1 = 0.1,
                 m_b_risk2 = .5,
                 s_b_risk2 = 0.1,
                 m_b_mode1 = 0.5,
                 s_b_mode1 = 0.1,
                 m_b_mode2 = 0.5,
                 s_b_mode2 = 0.1,
                 m_b_cost = -0.011,
                 s_b_cost = 0.1)

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_b_fixed = c() if none
apollo_fixed = c()

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

draws_vector <- paste("draws", c("head1", "head2", "risk1", "risk2", "mode1", "mode2", "cost"), sep = "_")

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "mlhs",
  interNDraws    = 75,
  interUnifDraws = c(),
  interNormDraws = draws_vector,
  intraDrawsType = "",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){ 
  randcoeff = list()  
  randcoeff[["b_head1"]] = m_b_head1 + s_b_head1 * draws_head1
  randcoeff[["b_head2"]] = m_b_head2 + s_b_head2 * draws_head2
  randcoeff[["b_risk1"]] = m_b_risk1 + s_b_risk1 * draws_risk1 
  randcoeff[["b_risk2"]] = m_b_risk2 + s_b_risk2 * draws_risk2 
  randcoeff[["b_mode1"]] = m_b_mode1 + s_b_mode1 * draws_mode1 
  randcoeff[["b_mode2"]] = m_b_mode2 + s_b_mode2 * draws_mode2 
  randcoeff[["b_cost"]] = m_b_cost + s_b_cost * draws_cost 
  return(randcoeff)  
}


# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
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
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2), 
    avail         = list(alt1=1, alt2=1), 
    choiceVar     = choice,
    V             = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

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

apollo_saveOutput(model, saveOutput_settings = list(printPVal=2))
