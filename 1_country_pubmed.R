# 1_country_pubmed.R
# get the country data from the pubmed affiliation
# Feb 2024
library(dplyr)
library(stringr)
library(openalexR)
options(openalexR.mailto = "a.barnett@qut.edu.au")
#options(openalexR.apikey = "EXAMPLE_APIKEY")

# from 0_find_authors_pubmed.R
load('data/0_emails.RData')
selected = mutate(selected, country='', type='', mesh='')

for (k in 1:N){ # loop through each row
  # make query
  query = paste("https://api.openalex.org/works/pmid:", selected$pmid[k], sep='')
  # get data based on query
  result = tryCatch(oa_request(query), error = function(e) { NULL })
#  cat(length(result),'\n')
  if(length(result$authorships)==0){next}
  anything = length(result$authorships[[1]]$countries) 
  if(anything == FALSE){next}
  country = result$authorships[[1]]$countries[[1]][1]
  selected$country[k] = country
  selected$type[k] = result$type
  if(length(result$mesh) == 0){this_mesh = NULL}
  if(length(result$mesh) >  0){this_mesh = result$mesh[[1]]$descriptor_name}
  selected$mesh = this_mesh
  result = NULL
  Sys.sleep(2)
}

# top countries
group_by(selected, country) %>%
  tally() %>%
  arrange(-n) %>%
  slice(1:5)

# save
save(selected, file = 'data/1_emails.RData')

