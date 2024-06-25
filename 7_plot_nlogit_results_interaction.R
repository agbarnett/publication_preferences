# 7_plot_nlogit_results_interaction.R
# plot and tabulate the nlogit results created by Sameera
# version for interaction results
# June 2024
library(dplyr)
library(gridExtra)
library(ggplot2)
library(readxl)
library(janitor)
library(stringr)
library(xtable)
source('99_functions.R')

# colours
interaction_colours = c("aquamarine3", "gold3")

#
column_names = c('term','est','se','z','p_value','lower','upper')
# labels without references
labels_table = c('Highest JIF', 'Moderate JIF', 'Minor formatting', 'Fast decision',
           'Helpful review', 'Changes in wording/format', 'Useful for promotion')
# labels with references
labels = c('Highest JIF\n(cf. no JIF)', 'Moderate JIF \n(cf. no JIF)', 'Minor formatting\n(cf. Major)', 'Fast decision\n(cf. Slow)',
           'Helpful review\n(cf. not helpful)', 'Changes in wording/format\n(cf. Cut table and analysis)', 'Useful for promotion\n(cf. Not useful)')

### part 1: get data ##


# interactions - main effects
interaction1 = process_results(sheet=8, range='A5:G16') # rank and editor
interaction2 = process_results(sheet=8, range='I5:O16') # rank and Formatting
interaction3 = process_results(sheet=8, range='Q5:W16') # rank and speed
interaction4 = process_results(sheet=8, range='Y5:AE16') # speed and review
interaction5 = process_results(sheet=8, range='AG5:AM16') # editor and review

# interactions - covariance
varcov1 = process_results(sheet=11, interaction = TRUE, range='B5:R21', name_range = 'B4:R4') # rank and editor
varcov2 = process_results(sheet=11, interaction = TRUE, range='B27:R43', name_range = 'B26:R26') # rank and editor
varcov3 = process_results(sheet=11, interaction = TRUE, range='B48:R64', name_range = 'B47:R47') # rank and editor
varcov4 = process_results(sheet=11, interaction = TRUE, range='B69:Q84', name_range = 'B68:Q68') # rank and editor
varcov5 = process_results(sheet=11, interaction = TRUE, range='B90:Q105', name_range = 'B89:Q89') # rank and editor

### part 2: plot interactions ###
#
int1 = plot_interaction(indata = interaction1, 
                        varcov = varcov1,
                        main = c('RANK_M','RANK_H','EDITOR'), # medium then high JIF
                        interaction = c('R2EDITOR|','R1EDITOR|'), # medium then high
                        ltitle = "Editor's\nrequest",
                        llabels = c('Cut table &\nanalysis','Format &\nWording'), 
                        xlab = 'Journal rank',
                        xlabels = c('None','Moderate','High'),
                        colours = interaction_colours)
#
int2 = plot_interaction(indata = interaction2, 
                        varcov = varcov2,
                        main = c('RANK_M','RANK_H','STY_MIN'), # medium then high JIF
                        interaction = c('R2_STYLE|','R1_STYLE|'), # medium then high
                        ltitle = "Formatting\nrequirements",
                        llabels = c('Major','Minor'), 
                        xlab = 'Journal rank',
                        xlabels = c('None','Moderate','High'),
                        colours = interaction_colours)
#
int3 = plot_interaction(indata = interaction3, 
                        varcov = varcov3,
                        main = c('RANK_M','RANK_H','SPEED'),
                        interaction = c('R2_SPEED|','R1_SPEED|'), # medium then high
                        ltitle = "Review speed",
                        llabels = c('Slow','Fast'), 
                        xlab = 'Journal rank',
                        xlabels = c('None','Moderate','High'),
                        colours = interaction_colours)
#
int4 = plot_interaction(indata = interaction4, 
                        varcov = varcov4,
                        main = c('SPEED','REVIEW'),
                        interaction = 'SPE_REV|',
                        ltitle = 'Helpful review', 
                        llabels = c('No','Yes'), 
                        xlab = 'Speed',
                        xlabels = c('Slow','Fast'),
                        colours = interaction_colours)
#
int5 = plot_interaction(indata = interaction5, 
                        varcov = varcov5,
                        main = c('EDITOR','REVIEW'),
                        interaction = 'EDIT_REV|',
                        ltitle = 'Helpful review',
                        llabels = c('No','Yes'), 
                        xlab = 'Editor requested',
                        xlabels = c('Cut table\nand analysis','Format and\nwording'),
                        colours = interaction_colours)

## make mega plot of interaction results
# remove y-axis labels for graphs in the right column:
int1 = int1 + theme(legend.box.margin	= margin(t=0, r=0, b=3, l=0)) # increase gap in legend because of carriage return in legend title
int2 = int2 + ylab(label = NULL) + theme(legend.box.margin	= margin(t=0, r=0, b=3, l=0)) # increase gap in legend because of carriage return in legend title
int3 = int3 + ylab(label = NULL) 
int4 = int4 + ylab(label = NULL) 
int5 = int5 + ylab(label = NULL) + theme(legend.box.margin	= margin(t=3, r=0, b=4, l=0)) # increase space so that it sits nicer compared with neighbouring plots
      
# layout matrix
lmatrix <- rbind(c(1,2,3), # repeat three for bottom of axis
                 c(4,5,3),
                 c(4,5,NA))
jpeg('figures/combined_plot_interactions.jpg', width=8, height=7, units='in', res=600, quality=100)
grid.arrange(int1, int2, int5, int3, int4, 
             layout_matrix = lmatrix,
             heights = c(1,0.04,1), # narrow number in middle for bottom of top-right panel
             widths = c(1.05,1,1))
dev.off()


### part 3: latex tables ##

# interactions
interaction = rbind(interaction1, interaction2, interaction3, interaction4, interaction5) %>%
  filter(str_detect(term, '\\|'))
latex_table = mutate(interaction, 
                     interaction = case_when(
                       term == 'R2EDITOR|' ~ "Moderate rank x Editor's request",
                       term == 'R1EDITOR|' ~ "High rank x Editor's request",
                       term == 'R2_STYLE|' ~ "Moderate rank x Formatting requirements",
                       term == 'R1_STYLE|' ~ "High rank x Formatting requirements",
                       term == 'R2_SPEED|' ~ "Moderate rank x Speed",
                       term == 'R1_SPEED|' ~ "High rank x Speed",
                       term == 'SPE_REV|' ~ "Helpful review x Speed",
                       term == 'EDIT_REV|' ~ "Editor's request x Helpful review"
                     ),
                     lower = round(lower*100)/100,
                     upper = round(upper*100)/100,
                     CI = paste(lower, 'to', upper)) %>%
  select(interaction, est, CI, p_value)
# convert p-values
for(k in 1:nrow(latex_table)){latex_table$p_value[k] = convert_pvalue(latex_table$p_value[k])}
#
print(xtable(latex_table), include.rownames=FALSE, hline.after=TRUE)

