# 99_data_for_github.R
# data for github
# July 2024
library(dplyr)
library(openxlsx)

## get the data, from 5_read_qualtrics.R and 5_read_qualtrics_v2.R
load('data/5_AnalysisReady.RData')
data1 = rename(data, 'q30a' = 'q30', 'q32' = 'q31') # rename questions to match first version
labels1 = labels
load('data/5_AnalysisReady_v2.RData')
data2 = data
labels2 = labels
data = bind_rows(data1, data2, .id = 'sample')
# remove sensitive variables
data = select(data, -starts_with('recipient'), -contains('text'), -response_id, -end_date, -q32)

## get data dictionaries
labels = read.table('data/labels.txt', sep='\t',header = TRUE)
labels2 = read.table('data/labels_v2.txt', sep='\t',header = TRUE)

## export to Excel
filename = "rdata/analysis_ready.xlsx"
wb = createWorkbook(creator='Adrian Barnett')
addWorksheet(wb, sheetName = "data")
addWorksheet(wb, sheetName = 'dictionary, sample 1')
addWorksheet(wb, sheetName = 'dictionary, sample 2')
writeDataTable(wb, sheet = 1, x = data,
               colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = 2, x = labels,
               colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = 3, x = labels2,
               colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, filename, overwrite = TRUE)