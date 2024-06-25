# 6_impute_missing.R
# impute the missing choice tasks
# May 2024
library(dplyr)
library(mice)
library(openxlsx)

# from 5_read_qualtrics.R
load('data/5_AnalysisReady.RData')

# what proportion is missing, dictactes number of missing data sets
dce_questions = paste('q', c(seq(3,17,2),20,22), sep='')
dce_only = select(data, all_of(dce_questions))
dce_only = is.na(as.matrix(dce_only))
any_miss = rowSums(dce_only) > 1
n_impute = round(100*sum(any_miss) / nrow(data))
cat('Percent missing is ', n_impute, '%\n', sep='')

# prepare imputed data
to_impute = select(data, starts_with('q'), -contains('_text'), -'q31') %>% # remove text
  mutate_at(vars(everything(dce_questions)), .funs = as.factor) # imputation only works with factors

# now impute
TeachingDemos::char2seed('crawley')
imputed = mice(
  data = to_impute,
  m = n_impute,
  method = "rf")
View(complete(imputed, 1))

# 
data_to_bind = select(data, 'id', 'block', 'scenario')
imputed_list = list()
for (k in 1:n_impute){
  this_data = complete(imputed, k)
  this_data = bind_cols(data_to_bind, this_data)
  imputed_list[[k]] = this_data
}

## export to excel
hs1 <- createStyle(fgFill = "seagreen3", textDecoration = "Bold", fontColour = "white") # header style
wb <- createWorkbook("Adrian Barnett")
# Add 
for (k in 1:n_impute){
  sheet_name = paste('imputed', k)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, x=imputed_list[[k]], rowNames = FALSE, headerStyle = hs1)
  #setColWidths(wb, sheet = 1, cols = 1:6, widths = "auto")
}
saveWorkbook(wb, file = "data/dce_first_imputed.xlsx", overwrite = TRUE)