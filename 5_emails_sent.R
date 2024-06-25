# 5_emails_sent.R
# calculate the number of participants approached in each round #
# used by 6_analyse_non_response.R and 6_summary_combined.Rmd
# May 2024
library(dplyr)

sample_sent = NULL

#
to_load = dir('emails', pattern='2024-03-16') # first pilot sample, links generated on 16-Mar-2024
n_sent = 0
for (file in to_load){
  this_file = paste('emails/', file, sep='')
  this = read.csv(file = this_file) %>%
    clean_names() %>%
    select(email) %>%
    mutate(sample_number = 1)
  sample_sent = bind_rows(sample_sent, this) 
  n_sent = n_sent + nrow(this)
}
frame1 = data.frame(sample = 'First pilot', date = as.Date('2024-03-16'), n_sent = n_sent)

#
to_load = dir('emails/second', pattern='2024-04-02') # second pilot sample, links generated on 02-Apr-2024
n_sent = 0
for (file in to_load){
  this_file = paste('emails/second/', file, sep='')
  this = read.csv(file = this_file) %>%
    clean_names() %>%
    select(email_address) %>%
    rename('email' = 'email_address') %>%
    mutate(sample_number = 2)
  sample_sent = bind_rows(sample_sent, this) 
  n_sent = n_sent + nrow(this)
}
frame2 = data.frame(sample = 'Second pilot', date = as.Date('2024-04-02'), n_sent = n_sent)

#
to_load = dir('emails/third', pattern='2024-04-13') # v2 of dce, links generated on 13-Apr-2024
n_sent = 0
for (file in to_load){
  this_file = paste('emails/third/', file, sep='')
  this = read.csv(file = this_file) %>%
    clean_names() %>%
    select(email) %>%
    mutate(sample_number = 3)
  sample_sent = bind_rows(sample_sent, this) 
  n_sent = n_sent + nrow(this)
}
frame3 = data.frame(sample = 'First main', date = as.Date('2024-04-13'), n_sent = n_sent)

# clean up few errors
sample_sent = mutate(sample_sent,
                     email = str_remove_all(email, pattern='\\)$|;$'),
                     email = str_squish(email))

# save
sent = bind_rows(frame1, frame2, frame3)
save(sample_sent, sent, file = 'data/5_sent.RData')

