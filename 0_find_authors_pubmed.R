# 0_find_authors_pubmed.R
# find recent authors with emails using pubmed (adapted from career disruption)
# do not share (added to gitignore)
# Nov 2023
library(tidyr)
library(dplyr)
library(rentrez)
library(XML)
library(stringr)
source('0_my_pubmed_key.R')

## ready to run overnight

## Section 1: pubmed search 
# types of pubmed article to exclude:
source('0_publication_types.R') # for publication types
types_to_include = filter(pub_types, include==1) %>%
  pull(type)
# years to search
years = 2022:2023
years_search = paste(years, '[PDAT]', collapse=' OR ', sep='')
# loop through years and publication types (otherwise search results are too long)
search_results = NULL
for (type in types_to_include){
  search_term = paste('(', years_search ,') AND ', type, '[PTYP]', sep='')
  p.search <- entrez_search(db='pubmed', term = search_term, retmax=30000, api_key =my.api.key)
  this_search = data.frame(id = p.search$ids, type=type)
  search_results = bind_rows(search_results, this_search)
}
search_results = unique(search_results) # remove duplicates
N = nrow(search_results)
# randomly order search results because we won't get through them all, so stop when we have enough and this will be a random sample
search_results <- sample_frac(search_results, 1L)

## Section 2: Affiliation details for matching papers
data = NULL
start = 20384 # to restart after `502` time out
stop = nrow(search_results)
for (k in start:stop){ # takes a while
  recs <- entrez_fetch(db = "pubmed", id = search_results$id[k], rettype = "xml", parsed = TRUE, api_key =my.api.key)
  ForeName = xmlValue(getNodeSet(recs,'//AuthorList//Author//ForeName')) # name
  LastName = xmlValue(getNodeSet(recs,'//AuthorList//Author//LastName')) # name
  affiliations = xmlValue(getNodeSet(recs,'//AuthorList//AffiliationInfo//Affiliation'))
  index = grep('@', affiliations) # find email address
  if(length(index)>0){
    index = index[1] # chose first author with email
    chosen_affl = affiliations[[index]]
    splits = str_split(chosen_affl, pattern=' ')[[1]]
    email = splits[grep('@', splits)]
    name = paste(ForeName[index], LastName[index], sep= ' ')
    affl = str_remove(affiliations[index], email) # take email out of affiliation (used to get address)
    frame = data.frame(pmid = search_results$id[k], 
                       name = name, 
                       email = email, 
                       affiliation = affl)
    data = bind_rows(data, frame)
  }
  Sys.sleep(3) # short delay to prevent request errors
  if(k%%100 == 0){cat('Up to', k,'\r')}
  
} # End of big loop

## clean up emails
selected = mutate(data,
              email = tolower(email), # for consistency (important for removing duplicates)
              email = str_remove_all(email, pattern='^\\('),
              email = str_remove_all(email, pattern='\\.$'), # remove dot at end of email
              email = str_remove_all(email, pattern=',$'), # remove comma at end of email
              email = str_squish(email), # remove unwanted spaces
              nchar = nchar(name),
              nchare = nchar(email)) %>%
  filter(nchar <= 50,  # remove one that is not a name
         email != '', # remove empty emails
         nchare > 5, # remove unlikely very short emails
         name != 'NA NA',
         !str_detect(email, '^@') # exclude Twitter handles starting with @
         ) 

# select longest name per email (and remove duplicates)
selected = group_by(selected, email) %>%
  arrange(email, desc(nchar)) %>% 
  slice(1) %>% # 
  ungroup() %>%
  select(-starts_with('nchar')) # no longer needed

# save
today = as.Date(Sys.Date())
save(today, N, selected, file='data/0_emails.RData') # 

# check locations
check = separate(selected, email, sep='@', into=c('pre','place'))
tab = group_by(check, place) %>%
  tally() %>%
  arrange(-n)
head(tab, 10)
