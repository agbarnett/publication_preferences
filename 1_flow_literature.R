# 1_flow_literature.R
# flow chart for literature
# Jan 2023
library(diagram)
library(dplyr)

## get data from rayyan for abstract/title screening
rayyan = read.csv('rayyan/articles.csv', header=TRUE) %>%
  clean_names() %>%
  mutate(included = str_detect(notes, "Included")) %>%
  separate(notes, into=c('one','reason'), sep='RAYYAN-EXCLUSION-REASONS:') %>% # separate out exclusion reasons, can ignore warnings
  select(-key, -day, -issn, -language, -publisher, -location, -abstract, -keywords, -pubmed_id, -pmc_id, -url, -one)

# total number of papers
n_total = nrow(rayyan)
# total number of papers that made it through initial screening
included = filter(rayyan, included == TRUE) 
n_included = nrow(included)

# exclusion reasons at abstract screening
reasons = filter(rayyan, included == FALSE) %>%
  group_by(reason) %>%
  tally() %>%
  mutate(reason = str_squish(str_to_sentence(reason))) %>%
  ungroup()
# to here

# included 


# exclude studies at full text 
long = filter(long,
              !str_detect(author, '^ioannidis|^foss|^dore|^chowdhury'))


# labels, big N for institutions, little n for RIAs
l1 = paste('Number of papers after removing duplicates\n(N=', n_total, ')', sep='') # 
l2 = paste('RIAs uncontactable\n(N=', not_contactable, ')', sep='') # 
# 
l5 = paste('Institution did not\ngive emails (N=', inst_not_supplied, ')', sep='') # 
l6 = paste('Institution gave\nemails (N=', inst_supplied, ')', sep='') # 
#
l7 = paste('RIAs with email\n(n=', rias_with_email, ')', sep='') # 
l8 = paste('RIAs who responded\n(n=', rias_responded, ')', sep='') # 
l9 = paste('Email no longer valid\n(n=', invalid_email, ')', sep='') # 
labels = c(l1, l2, l3, l4, l5, l6, l7, l8, l9)
n.labels = length(labels)

#
### make data frame of box chars
# box.prop = length/width ratio, so > 1 = tall and thin
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.5	0.94	white	square	0.3	0.12
2	0.23	0.75	white	square	0.3	0.12
3	0.5	0.75	white	square	0.3	0.12
4	0.77	0.75	white	square	0.3	0.12
5	0.23	0.5	white	square	0.3	0.12
6	0.5	0.5	white	square	0.3	0.12
7	0.77	0.35	white	square	0.3	0.12
8	0.77	0.11	white	square	0.3	0.12
9	0.5	0.19	white	square	0.3	0.12')
# positions:
pos = as.matrix(subset(frame, select=c(x, y)))
# joins between boxes
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[2, 1] = "' '"
M[3, 1] = "' '"
M[4, 1] = "' '"
M[5, 3] = "' '"
M[6, 3] = "' '"
M[7, 4] = rias_first_pass
M[7, 6] = rias_second_pass
M[8, 7] = "' '"
M[9, 7] = "' '" # invalid email
# colours
tcol = rep('black', n.labels)

## make figure 
jpeg('figures/consort.flow.jpg', width=7.5, height=7, units='in', res=400, quality = 100)
par(mai=c(0,0.04,0.04,0.04))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = tcol)
# footnote:
text(x = 0.1, y = 0.04, cex = 0.8, labels = 'RIA = research integrity advisor; N = number of institutions, n = number of RIAs', adj=c(0,0.5))
dev.off()
