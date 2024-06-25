# 4_send_emails.R
# send emails to potential participants
# use links generated in Qualtrics
library(janitor)
library(dplyr)
library(readxl)
library(stringr)
library(RDCOMClient) # for sending emails direct from R (installed from github)

## Section 1: get the sample ##
## get the personal links from Qualtrics, see 4a_make_email_list.R, links generated on 13-Feb-2023
#source('4_email_list_test.R') # test version
this_sample_number = 3 # 1 = first sample with pilot design of 200, 2 = second sample with pilot design of 1000, 3 = third sample with updated design of 6374
source('4_email_list.R')
N_sample = nrow(sample)

# check no missing emails
summary(nchar(sample$email))

## Section 2: create emails ##
# read in the email as one variable
approach = 4 # which approach number, this controls the email content
email = read.table(paste('emails/email',approach,'.txt',sep=''), sep='!', quote='')
email = email$V1
# extract the subject
subject = str_remove(email[1], '^Subject: ')
email = email[-1] # remove first row (subject)
n_email = length(email)

# make individual emails
link_to_picf = 'https://osf.io/p9guj/wiki/home/'
link_to_osf = 'https://osf.io/p9guj/'
n_responses = '355' # number of responses for reminder email (must be character)
email.body = list()
for (k in 1:N_sample){ # loop through participants
  email.body[[k]] = ''
  for (j in 1:n_email){ # loop through email lines
    text = email[j]
    text = str_replace(text, '\\[name\\]', sample$name[k]) # add name
    text = str_replace(text, '\\[here\\]', paste('<a href="', link_to_picf, '">here</a>', sep='')) # add link 
    text = str_replace(text, '\\[click here\\]', paste('<a href="', sample$link[k], '">click here</a>', sep='')) # add link 
    text = str_replace(text, '\\[this link\\]', paste('<a href="', sample$link[k], '">this link</a>', sep='')) # add link 
    text = str_replace(text, '\\[Open Science Framework\\]', paste('<a href="', link_to_osf, '">Open Science Framework</a>', sep='')) # add link 
    text = str_replace(text, '\\[n_responses\\]', n_responses) # add number of responses
    email.body[[k]] = paste(email.body[[k]], text, '<br><br>', sep='')
  }
}



#### Section 3: send emails ###
min.send = 101 # used to run in batches
#max.send = 100 # **** temporary for testing ****
max.send = nrow(sample)
for (k in min.send:max.send){ 
  # only create email if its okay to approach them
  if(sample$approach[k] == TRUE){
    # start the app
    OutApp <- COMCreate("Outlook.Application")
    # create an email 
    outMail = OutApp$CreateItem(0)
    outMail[["To"]] = sample$email[k] # comment out for testing ...
    #outMail[["To"]] = 'a.barnett@qut.edu.au' # ... for testing
    outMail[["subject"]] = subject
    outMail[["HTMLbody"]] = email.body[[k]]
    # send it                     
    outMail$Send()
    Sys.sleep(4) # short break
  } # end of if
}
