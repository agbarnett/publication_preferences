# 6_summary_table_descriptives.R
# summary table for paper
# June 2024
library(tableone)
library(dplyr)
library(xtable) # for latex

## get the data, from 5_read_qualtrics.R and 5_read_qualtrics_v2.R
load('data/5_AnalysisReady.RData')
data1 = rename(data, 'q30a' = 'q30', 'q32' = 'q31') # rename questions to match first version
labels1 = labels
load('data/5_AnalysisReady_v2.RData')
data2 = data
labels2 = labels
data = bind_rows(data1, data2, .id = 'sample')

# rename for table
data = rename(data,
              'difficulty' = 'q24',
              'field' = 'q25',
              'gender' = 'q26',
              'experience' = 'q27',
              'papers' = 'q28')
## Create a TableOne object
myVars = c('difficulty','field','gender','experience','papers')
nonnormal = c('experience','papers')
catVars = c('difficulty','field','gender')
tab1 <- CreateTableOne(vars = myVars, 
                       data = data, 
                       factorVars = catVars, 
                 #      strata = 'recommendation', 
                       test = FALSE, 
                       includeNA = FALSE,
                       addOverall = FALSE)
ptab = print(tab1, 
      nonnormal = nonnormal, # use median instead of mean
      catDigits = 1, # will no longer allow 0
      contDigits = 1, 
      explain = TRUE, 
      showAllLevels = TRUE,
      formatOptions = list(big.mark = ","))
ptab = bind_cols(rownames(ptab),ptab)
print(xtable(ptab), file = 'results/6_summary_table.tex', include.rownames = FALSE) # for latex
