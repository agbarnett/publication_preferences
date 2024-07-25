# 99_study_design.R
# study design diagram for paper - not yet complete
# May 2024
library(diagram)
library(dplyr)

## Need final numbers for thinking aloud and main survey

# labels
l1 = 'Literature review\n64 papers, 77 attributes'
l2 = 'Focus group pilot: 9 participants'
l3 = 'Focus groups: 16 participants.\nIn-depth semi-structured\ninterviews: 2 participants.'
l4 = 'Data analysis with\na distilling approach\n(steps and attribute numbers)\n1) Attribute identification, 50\n2) Attribute generation, 88\n3) Attribute merging, 21\n4) Attributes of importance, 16\n5) Final selected attributes, 7'
l5 = 'Thinking aloud interviews\n10 participants'
l6 = "Final attribute selection\nusing participants' voices"
l7 = 'Internal pre-testing'
l8 = 'External pilot testing\n96 respondents'
l9 = 'Final design\n520 respondents'
labels = c(l1, l2, l3, l4, l5, l6, l7, l8, l9)
n.labels = length(labels)+2 # plus 2 for dummy arrow

#
### make data frame of box chars
# box.prop = length/width ratio, so > 1 = tall and thin
frame = read.table(sep='\t', stringsAsFactors=F, skip=0, header=T, text='
i	x	y	box.col	box.type	box.prop	box.size
1	0.25	0.85	grey92	square	0.22	0.20
2	0.25	0.68	grey92	square	0.22	0.20
3	0.25	0.50	grey92	square	0.28	0.20
4	0.25	0.195	grey92	square	0.62	0.20
5	0.75	0.84	grey92	square	0.22	0.20
6	0.75	0.67	grey92	square	0.22	0.20
7	0.75	0.43	grey92	square	0.22	0.20
8	0.75	0.27	grey92	square	0.22	0.20
9	0.75	0.10	grey92	square	0.22	0.20
10	0.45	0.24	grey92	square	0	0
11	0.55	0.84	grey92	square	0	0')
# positions:
pos = as.matrix(subset(frame, select=c(x, y)))
# joins between boxes
M = matrix(nrow = n.labels, ncol = n.labels, byrow = TRUE, data = 0)
M[2, 1] = "' '"
M[3, 2] = "' '"
M[4, 3] = "' '"
M[6, 5] = "' '"
M[8, 7] = "' '"
M[9, 8] = "' '"
M[11, 10] = "' '" # arrow from LHS to RHS
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
rect(xleft = 0.55, ybottom = 0.90, xright = 0.95, ytop = 0.99, col = 'grey50', border=NA) # 
rect(xleft = 0.55, ybottom = 0.53+0.04, xright = 0.95, ytop = 0.53-0.04, col = 'grey50', border=NA) # slightly bigger for longer text
text(x = 0.25, y = 0.955, labels = 'Attribute development', col = 'white')
text(x = 0.75, y = 0.945, labels = 'Attribute development\n(continued)', col = 'white')
text(x = 0.75, y = 0.53, labels = 'Preference elicitation\nusing surveys', col = 'white')
# boxes
rect(xleft = 0.05, ybottom = 0.05, xright = 0.45, ytop = 0.99)
rect(xleft = 0.55, ybottom = 0.05, xright = 0.95, ytop = 0.99)
dev.off()
