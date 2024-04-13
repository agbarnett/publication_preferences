# 4a_make_email_list_test.R
# make the email list for importing into Qualtrics - test version
# see https://www.qualtrics.com/support/survey-platform/contacts/contact-list-overview/
# and https://www.qualtrics.com/support/survey-platform/distributions-module/email-distribution/personal-links/
# March 2024
library(dplyr)

# get the participants names and emails
sample = read.table(header=TRUE, sep=',',text='
name,email
Sanj,sanjeewa.kularatna@duke-nus.edu.sg
Sameera,sameera.senanayake@duke-nus.edu.sg
David Brain,david.brain@qut.edu.au
Natalia Gonzalez Bohorquez,n.gonzalezbohorquez@qut.edu.au
Rangi Weerasuriya,sucharitha.weerasuriya@qut.edu.au
')

# export for qualtrics, had to have quote = TRUE
write.csv(sample, file = 'emails/test_list_for_qualtrics.csv', quote=TRUE, row.names=FALSE)

# then generate personal links in Qualtrics, click on directories in Qualtrics then make a new contact list
# file read in using 4_email_list_test.R

