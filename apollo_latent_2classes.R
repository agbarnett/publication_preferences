################################################################################
## Karin Groothuis-Oudshoorn
## March 25th 2024
################################################################################

## ******** NOTE! REPLACE "PATH" IN LINE 38 WITH DATA FILE LOCATION ******** ##

# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

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
  modelName       = "Latent_2classes_Chapter_Example",
  modelDescr      = "Latent class model on example article Patient",
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
apollo_beta <- rep(0,2*7 + 2)
apollo_beta <- c(rep(c(1,0.5,1,0.5,0.5,0.5,-0.011), each = 2), 0.5,0)

colname <- c("b_head1","b_head2","b_risk1","b_risk2","b_mode1","b_mode2","b_cost")
names(apollo_beta) <- paste0(rep(c(colname,"delta"), each = 2), rep(c("_a","_b"),times = 8),sep = "")

### Vector with names (in quotes) of parameters to be kept fixed at their starting value in apollo_beta, use apollo_beta_fixed = c() if none
apollo_fixed = c("delta_b")

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  
  lcpars[["b_head1"]] = list(b_head1_a, b_head1_b)
  lcpars[["b_head2"]] = list(b_head2_a, b_head2_b)
  lcpars[["b_risk1"]] = list(b_risk1_a, b_risk1_b)
  lcpars[["b_risk2"]] = list(b_risk2_a, b_risk2_b)
  lcpars[["b_mode1"]] = list(b_mode1_a, b_mode1_b)
  lcpars[["b_mode2"]] = list(b_mode2_a, b_mode2_b)
  lcpars[["b_cost"]] = list(b_cost_a, b_cost_b)
  
  ###
  V=list()
  V[["class_a"]] = delta_a
  V[["class_b"]] = delta_b

  mnl_settings = list(
    alternatives = c(class_a=1, class_b=2), 
    avail        = 1, 
    choiceVar    = NA, 
    V            = V
  )
  lcpars[["pi_values"]] = apollo_mnl(mnl_settings, functionality="raw")
  lcpars[["pi_values"]] = apollo_firstRow(lcpars[["pi_values"]], apollo_inputs)
  ###
  return(lcpars)
}

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
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2),
    avail        = list(alt1=1, alt2=1),
    choiceVar    = choice
  )
  
  ### Loop over classes
  for (s in 1:2){
    
    ### Compute class-specific utilities
    V=list()
    V[['alt1']]  = b_head1[[s]]*head01 + b_head2[[s]]*head11 + b_risk1[[s]]*risk01 + b_risk2[[s]]*risk0.51 + b_mode1[[s]]*pill1 + b_mode2[[s]]*inject1 + b_cost[[s]]*cost1
    V[['alt2']]  = b_head1[[s]]*head02 + b_head2[[s]]*head12 + b_risk1[[s]]*risk02 + b_risk2[[s]]*risk0.52 + b_mode1[[s]]*pill2 + b_mode2[[s]]*inject2 + b_cost[[s]]*cost2

    mnl_settings$V = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
  }
  
  ### Compute latent class model probabilities
  lc_settings   = list(inClassProb = P, classProb=pi_values)
  
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

### Optional starting values search
# apollo_beta=apollo_searchStart(apollo_beta, apollo_fixed,apollo_probabilities, apollo_inputs)

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)
