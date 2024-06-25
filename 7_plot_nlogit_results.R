# 7_plot_nlogit_results.R
# plot and tabulate the nlogit results created by Sameera
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
one_colour = 'darkseagreen3'
two_colours = c("khaki3", "red2")
two_colours = c("orangered2", "steelblue3")
latent_colours = c("mediumvioletred", "lightseagreen", "orange3", 'black')

#
column_names = c('term','est','se','z','p_value','lower','upper')
# labels without references
labels_table = c('Highest JIF', 'Moderate JIF', 'Minor formatting', 'Fast decision',
           'Helpful review', 'Changes in wording/format', 'Useful for promotion')
# labels with references
labels = c('Highest JIF\n(cf. no JIF)', 'Moderate JIF \n(cf. no JIF)', 'Minor formatting\n(cf. Major)', 'Fast decision\n(cf. Slow)',
           'Helpful review\n(cf. not helpful)', 'Changes in wording/format\n(cf. Cut table and analysis)', 'Useful for promotion\n(cf. Not useful)')

### part 1: get data ##


# overall results
overall = process_results(sheet=2, range='A4:G10')
# scenario results 
scenario1 = process_results(sheet=3, range='B5:H11')
scenario2 = process_results(sheet=3, range='J5:P11')
scenario = bind_rows(scenario1, scenario2, .id = 'group')
# experience results 
experience1 = process_results(sheet=4, range='A5:G11')
experience2 = process_results(sheet=4, range='I5:O11')
experience = bind_rows(experience1, experience2, .id = 'group')
# publication results 
publication1 = process_results(sheet=5, range='A5:G11')
publication2 = process_results(sheet=5, range='I5:O11')
publications = bind_rows(publication1, publication2, .id = 'group')
# gender results 
gender1 = process_results(sheet=6, range='A5:G11')
gender2 = process_results(sheet=6, range='I5:O11')
gender = bind_rows(gender1, gender2, .id = 'group')
# target publications results 
target1 = process_results(sheet=7, range='B6:H12')
target2 = process_results(sheet=7, range='J6:P12')
target = bind_rows(target1, target2, .id = 'group')
# latent classes
latent1 = process_results(sheet=9, range='C4:I10') # 
latent2 = process_results(sheet=9, range='C13:I19') # 
latent3 = process_results(sheet=9, range='C22:I28') # 
latent4 = process_results(sheet=9, range='C31:I37') # 
latent_prop = process_results(sheet=9, range='C39:D42') %>% # proportions for facet headers
  mutate(group = as.character(1:4), # char for merge
         est = round(est*100), # convert to percentage
         gname = paste('Group = ', group, ' (', est, '%)', sep='')) %>%
  select(group, gname)
latent = bind_rows(latent1, latent2, latent3, latent4, .id = 'group') %>%
  left_join(latent_prop, 'group') %>%
  select(-group) %>%
  rename('group' = 'gname')


### part 2: main results plotted ##

overall_p = plot_function(overall, legend_title = 'Overall', colours = one_colour)
overall_p = overall_p + ggtitle('No stratification') + 
  #scale_y_log10(limits = c(NA,30.5))+ # to avoid '30' getting cropped
  theme(plot.title = element_text(size = 11, hjust=0, vjust=0)) # push title down
scenario_p = plot_function(scenario, legend_title = 'Prior rejections', legend_labels = c('0','2'), colours = two_colours)
experience_p = plot_function(experience, legend_title = 'Years experience', legend_labels = c('≤ 10','> 10'), colours = two_colours)
publications_p = plot_function(publications, legend_title = 'Publications', legend_labels = c('≤ 43','> 43'), colours = two_colours)
gender_p = plot_function(gender, legend_title = 'Female', legend_labels = c('Yes','No'), colours = two_colours) # 
target_p = plot_function(target, legend_title = 'Publication target', legend_labels = c('Yes','No'), colours = two_colours) # 
latent_p = plot_function(latent, legend_title = 'Latent class', legend_labels = 1:4, colours = latent_colours) # 

# remove y-labels for graphs in the right column; remove space outside plot region
experience_p = experience_p + scale_x_reverse(breaks = 1:7, labels=NULL)
publications_p = publications_p + scale_x_reverse(breaks = 1:7, labels=NULL)
gender_p = gender_p + scale_x_reverse(breaks = 1:7, labels=NULL) 
target_p = target_p + scale_x_reverse(breaks = 1:7, labels=NULL) +theme(legend.key.spacing.x = unit(0.5,'pt')) # reduce legend spacing to get wording in
# remove x-labels for top row (ylab due to coord_flip)
overall_p = overall_p + ylab(NULL)
experience_p = experience_p + ylab(NULL) +theme(legend.key.spacing.x = unit(0.5,'pt')) # reduce legend spacing to get wording in
gender_p = gender_p + ylab(NULL)

## make mega plot of stratified results
lmatrix = matrix(1:6, ncol=3)
jpeg('figures/combined_plot.jpg', width=9, height=8, units='in', res=600, quality=100)
grid.arrange(overall_p, scenario_p, experience_p, publications_p, gender_p, target_p,
             layout_matrix = lmatrix,
             heights = c(1, 1.05), # as top row has no x-label
             widths = c(1.5, 1, 1))
dev.off()



### part 3: latex tables ##

# main table
latex_table = mutate(overall, 
                     attribute = factor(termn, labels = labels_table),
                     lower = round(lower*100)/100,
                     upper = round(upper*100)/100,
                     CI = paste(lower, 'to', upper)) %>%
  select(attribute, est, CI)
print(xtable(latex_table), include.rownames=FALSE, hline.after=TRUE)


### part 4: alternative latent class plot ##
latent_alternative_p = latent_p + theme(legend.position = 'none')+
  facet_wrap(~group)
jpeg('figures/latent_facet.jpg', width=6, height=5.5, units='in', res=600, quality=100)
print(latent_alternative_p)
dev.off()
