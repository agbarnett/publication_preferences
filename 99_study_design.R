# 99_study_design.R
# study design diagram for paper - not yet complete
# May 2024
library(diagram)
library(dplyr)

## Need final numbers for thinking aloud and main survey

# labels
l1 = 'Literature review\n64 papers, 77 attributes'
l2 = 'Focus groups: 16 participants\nInterviews: 2 participants'
l3 = 'Data analysis with\na distilling approach'
l4 = 'Thinking aloud interviews\n7 participants'
l5 = "Final attribute selection based\non participants' voices"
l6 = 'Internal pre-testing'
l7 = 'External pilot testing\n96 respondents'
l8 = 'Final design\n520 respondents'
labels = c(l1, l2, l3, l4, l5, l6, l7, l8)
n.labels = length(labels)

#
### make data frame of box chars
# box.prop = length/width ratio, so > 1 = tall and thin
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.25	0.83	grey92	square	0.22	0.20
2	0.25	0.65	grey92	square	0.22	0.20
3	0.25	0.47	grey92	square	0.22	0.20
4	0.25	0.28	grey92	square	0.22	0.20
5	0.25	0.10	grey92	square	0.22	0.20
6	0.75	0.83	grey92	square	0.22	0.20
7	0.75	0.47	grey92	square	0.22	0.20
8	0.75	0.10	grey92	square	0.22	0.20')
# positions:
pos = as.matrix(subset(frame, select=c(x, y)))
# joins between boxes
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[2, 1] = "' '"
M[3, 2] = "' '"
M[4, 3] = "' '"
M[5, 4] = "' '"
M[7, 6] = "' '"
M[8, 7] = "' '"
# colours
tcol = rep('black', n.labels)

## make figure 
jpeg('figures/99_study_design.jpg', width=6.5, height=5.5, units='in', res=500, quality = 100)
par(mai=c(0,0.03,0.03,0.03))
plotmat(M, pos = pos, name = labels, lwd = 1, shadow.size=0, curve=0,
        box.lwd = 2, cex.txt = 1, box.size = frame$box.size, box.col=frame$box.col,
        box.type = frame$box.type, box.prop = frame$box.prop, txt.col = tcol)
# headers
rect(xleft = 0.05, ybottom = 0.92, xright = 0.45, ytop = 0.99, col = 'grey50', border=NA)
rect(xleft = 0.55, ybottom = 0.92, xright = 0.95, ytop = 0.99, col = 'grey50', border=NA)
text(x = 0.25, y = 0.955, labels = 'Attribute development', col = 'white')
text(x = 0.75, y = 0.955, labels = 'Preference elicitation', col = 'white')
# boxes
rect(xleft = 0.05, ybottom = 0.05, xright = 0.45, ytop = 0.99)
rect(xleft = 0.55, ybottom = 0.05, xright = 0.95, ytop = 0.99)
dev.off()
