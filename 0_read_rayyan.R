# 0_read_rayyan.R
# read in the systematic review data from rayyan
# Jan 2023
library(dplyr)
library(janitor)
library(stringr)
TeachingDemos::char2seed('barrow')
library(openxlsx)

#
rayyan = read.csv('rayyan/articles.csv', header=TRUE) %>%
  clean_names() %>%
  select(-key, -day, -issn, -language, -publisher, -location, -abstract, -keywords, -pubmed_id, -pmc_id, -url)

# included 
included = filter(rayyan, str_detect(notes, "Included")) # & str_detect(notes, "Adrian")) 

# randomly split 32
reviewer = c(rep('Adrian',32),rep('Rangi',32))
reviewer = sample(reviewer, replace = FALSE, size = 64) # randomly re-order
included = bind_cols(included, reviewer)
names(included[length(names(included))]) = 'reviewer'
included = select(included, -notes)
adrian = filter(included, reviewer == 'Adrian')
rangi = filter(included, reviewer == 'Rangi')

# export to Excel
filename = "rayyan/papers_to_read.xlsx"
wb = createWorkbook(creator='Adrian Barnett')
addWorksheet(wb, sheetName = "Adrian")
addWorksheet(wb, sheetName = "Rangi")
freezePane(wb, sheet = "Adrian", firstRow = TRUE) ## freeze first column
freezePane(wb, sheet = "Rangi", firstRow = TRUE) ## freeze first column
writeDataTable(wb, sheet = "Adrian", x = adrian,
               colNames = TRUE, rowNames = FALSE)
writeDataTable(wb, sheet = "Rangi", x = rangi,
               colNames = TRUE, rowNames = FALSE)
saveWorkbook(wb, filename, overwrite = TRUE)
