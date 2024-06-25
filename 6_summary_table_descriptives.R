# 6_summary_table_descriptives.R
# summary table 
# June 2024
library(tableone)
library(dplyr)

## get the data, from 5_read_qualtrics.R and 5_read_qualtrics_v2.R
load('data/5_AnalysisReady.RData')
data1 = rename(data, 'q30a' = 'q30', 'q32' = 'q31') # rename questions to match first version
labels1 = labels
load('data/5_AnalysisReady_v2.RData')
data2 = data
labels2 = labels
data = bind_rows(data1, data2, .id = 'sample')

# to here
#
results = mutate(results, has_orcid = ifelse(is.na(orcid), 'No', 'Yes'))
## Create a TableOne object
myVars = c('journal','role','matches','n_papers_cited','has_orcid')
nonnormal = c('matches','n_papers_cited')
catVars = c('journal','role','has_orcid')
tab1 <- CreateTableOne(vars = myVars, 
                       data = results, 
                       factorVars = catVars, 
                       strata = 'recommendation', 
                       test = FALSE, 
                       includeNA = TRUE,
                       addOverall = FALSE)
print(tab1, 
      #      nonnormal = nonnormal, # use median instead of mean
      catDigits = 1, # will no longer allow 0
      contDigits = 1, 
      explain = TRUE, 
      showAllLevels = TRUE,
      formatOptions = list(big.mark = ","))
