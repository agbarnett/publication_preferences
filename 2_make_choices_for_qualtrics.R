# 2_make_choices_for_qualtrics.R
# make the images for qualtrics; also for latex; also create the scenario as a figure for easy re-use
# March 2024
library(readxl)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(ggplot2)
library(ragg) # for agg_png
library(flextable) # make table images using flextable
library(xtable) # for creating latex results for the protocol
set_flextable_defaults(font.family = "Times New Roman") # match whatever we use in Qualtrics

## Part 1: data ##
## a1) get the design from Sameera; 
# - `DCE design_2024.01.18` was previous version with 2 blocks
# - `DCE design_2024.03.13` was version with 3 blocks that was used in the pilot; use 'adrian' sheets where blocks are adjusted to get a good dot plot (see figure below)
design = read_excel('data/AuthorP_main_2024.04.9_NoASC_187166.xlsx', sheet='adrian', skip=0) %>%
  clean_names() %>%
  arrange(block, choice_situation) %>%
  group_by(block) %>%
  mutate(qnumber = 1:n()) %>% # add question number (choice set number, nested in block)
  ungroup() 
# with(design, table(block, qnumber)) # check
## a2) get the weights that show assumed preferences also for working out dominant design
weights = read_excel('data/DCE design_2024.03.13.xlsx', sheet='weights', skip=0) %>%
  clean_names() %>%
  rename('aname' = 'ngene_name',
         'level' = 'code') %>%
  filter(!is.na(level))%>%
  mutate(weights = ifelse(weights=='Reference', '0', weights),
         weights = as.numeric(weights)) %>%
  select(aname, level, weights) %>%
  tidyr::fill(aname) # fill in missing names

## b) read in our text for the attributes and levels
text = read_excel('data/journal_dce_attributes_levels.xlsx', sheet='version 7') %>%
  mutate(level = as.numeric(str_sub(levels_text, 1, 1)),
         levels_text = str_remove_all(levels_text, '^[1-3]\\. '))%>%  # remove number from text
  filter(aname != 'open') # temporary for version 6

## c) merge design and text
design_long = select(design, block, choice_situation, starts_with('choice')) %>%
  pivot_longer(-c(choice_situation, block)) %>%
  mutate(choice = ifelse(str_detect(name, '1_'), 1, 2), # two choices
         aname = str_remove_all(name, 'choice[1-2]_')) %>%
  rename('level' = 'value') %>%
  filter(aname != 'open') # temporary for version 6
# 
design_long = left_join(text, design_long, by=c('aname','level')) 
# table(design_long$block) # check
# make 1 to `n` number within block (used for figure labelling)
design_long = group_by(design_long, block) %>%
  mutate(choice_num = as.numeric(as.factor(choice_situation))) %>% 
  ungroup()

# question: do any attributes have the same level? Answer:no for pilot, yes for second sample
combinations = select(design_long, block, aname, choice_situation, level, choice) %>%
  pivot_wider(values_from = 'level', names_from = 'choice')
filter(combinations, `1` == `2`) %>% arrange(block, choice_situation) %>% nrow()

## check if design can measure interactions
# ranking fixed and editor cutting moving
low = filter(design, (choice1_field == 1 & choice1_editor == 1)| 
                     (choice2_field == 1 & choice2_editor == 1)) %>%
  mutate(level = 'low')
high = filter(design, (choice1_field == 1 & choice1_editor == 2)| 
                      (choice2_field == 1 & choice2_editor == 2)) %>%
  mutate(level = 'high')
# ranking moving and editor cutting fixed
low = filter(design, (choice1_field == 1 & choice1_editor == 1)| 
               (choice2_field == 1 & choice2_editor == 1)) %>%
  mutate(level = 'low')
high = filter(design, (choice1_field == 3 & choice1_editor == 1)| 
                (choice2_field == 3 & choice2_editor == 1)) %>%
  mutate(level = 'high')
# find if there are any combinations
both = bind_rows(low, high) %>%
  select(level, block, qnumber) %>% # qnumber is file number for images (1 to 8)
  group_by(block) %>%
  pivot_wider(values_from = 'qnumber', names_from = 'level')

# calculate the relative difficulty of answering per choice set; 
for_dominant = left_join(design_long, weights, by=c('aname','level')) %>%
  select(block, aname, choice_num, weights, choice) %>%
  pivot_wider(values_from = 'weights', names_from = 'choice') %>%
  mutate(diff = `1` - `2`) %>% # difference in choices (depends on journal A or B, so do not use absolute); zero means harder choice, large positive or negative is an easier choice (based on our priors)
  group_by(block, choice_num) %>%
  summarise(sum = sum(diff)) %>%
  arrange(block, sum) %>%
  ungroup()
# make initial plot to get positions for text (see https://stackoverflow.com/questions/44991607/how-do-i-label-the-dots-of-a-geom-dotplot-in-ggplot2)
p <- ggplot(for_dominant, aes(x=sum, fill=factor(block))) +
  geom_dotplot(binwidth = 0.00025)+
  theme(legend.position = 'none')+
  facet_wrap(~block)+
  coord_flip()
# Get y-positions of points plotted by geom_dotplot
point.pos <- ggplot_build(p)$data[[1]]
# merge in choice num
choice_num = select(for_dominant, sum, block, choice_num) %>%
  rename('group' = 'block',
         'x' = 'sum')
to_plot = select(point.pos, x, stackpos, group) %>%
  left_join(choice_num, by=c('group','x'), relationship = "many-to-many") %>%
  group_by(group, x) %>%
  mutate(stackposr = as.numeric(as.factor(stackpos)),
         choice_numr = as.numeric(as.factor(choice_num))) %>%
  filter(stackposr == choice_numr) %>%
  ungroup()
# dotplot
dplot = ggplot(data= to_plot, aes(x=x, y=stackpos, col=factor(group), label=choice_num))+
  geom_point(size=7)+
  geom_text(col='white', size=5)+
  facet_wrap(~group)+
  theme_bw()+
  theme(legend.position = 'none', panel.grid.minor = element_blank())+
  coord_flip()+
  ylab('')+
  scale_y_continuous(labels=NULL, breaks = NULL, expand=c(0.1,0))+
  xlab('Choice balance')
ggsave(plot = dplot, filename = 'figures/dotplot_design_second.jpg', width = 4, height = 3, units= 'in', dpi=400)


## Part 2: make example sets for Natalia in Word - commented out as done previously
dummy = function(){
# create Word document
rmarkdown::render(input = '2_choice_sets_examples.Rmd',
                  output_file = '2_choice_sets_examples_v2.docx')
}


## Part 3a: show all levels
ft = select(text, -aname, -level) %>%
  flextable() %>%
  theme_box() %>%
  bg(i=c(4,5,8,9,12,13), bg='grey88') %>%
  width(j = 1, width = 5.1, unit = "cm") %>%
  width(j = 2, width = 4.9, unit = "cm") %>%
  merge_v(j=1)
save_as_image(ft, path = 'figures/all_levels.png', res=600)
# now for latex (for protocol)
ft = select(text, -aname, -level) 
print(xtable(ft), include.rownames = FALSE)

## Part 3b: make images for Qualtrics
make_qualtrics_image = function(indata, choicenum, merge_last = FALSE){
  # get text into nice format
  block = filter(indata, choice_situation == choicenum) %>% pull(block) %>% unique()
  choice_num = filter(indata, choice_situation == choicenum) %>% pull(choice_num) %>% unique()
  choice = filter(indata, choice_situation == choicenum) %>%
    group_by(aname, attribute) %>%
    select(choice, levels_text) %>%
    pivot_wider(values_from = levels_text, names_from = choice) %>%
    ungroup() %>%
    mutate(aname = case_when(
      aname == 'impf' ~ 'Journal ranking',
      aname == 'ease' ~ 'Format',
      TRUE ~ as.character(aname)
    )) %>%
    select(attribute, `1`, `2`) %>% # in consistent order
    rename('Journal A' = `1`,
           'Journal B' = `2`,
           'Attribute' = 'attribute') 
  # make into flextable
  ft <- flextable(choice) %>%
    width(j = 1, width = 5.1, unit = "cm") %>%
    width(j = 2:3, width = 4.9, unit = "cm") %>%
    bg(i=c(2,4,6), bg='grey88') %>%
    theme_box() %>%
    flextable::align(j=1, align='left', part='all') %>%
    flextable::align(j=2:3, align='center', part='all')
  if(merge_last == TRUE){
    ft = merge_h(ft, i = 6) # merge last row
  }
  
  # save as image; size chosen to look good in qualtrics (see https://davidgohel.github.io/flextable/reference/plot.flextable.html)
  # second design in `second` subfolder
  filename = paste("figures/qualtrics/second/block", block, "/choice_set_", choice_num, ".png", sep='')
  agg_png(
    filename = filename, width = 1877, height = 1001, units = 'px',
    background = "transparent", res = 400
  )
  plot(ft)
  invisible(dev.off())
  
  #png(filename, , pointsize = 10, bg='transparent', res=400)
  #print(ft)
  #invisible(dev.off())
  #save_as_image(ft, path = filename, res=400, expand=0) # is 2346 x 1252 pixels
  
  # needed for example table
  return(choice)
}

# run the function for all choices (with merging when there's no difference between journals)
for (i in 1:24){
  for_latex = make_qualtrics_image(indata = design_long, choicenum = i, merge_last = TRUE) # 
}
print(xtable(for_latex), include.rownames = FALSE)

## make a dominant version for the first task
# read in the dominant design
dominant = read_excel('data/DCE design_2024.01.18.xlsx', sheet='dominant', skip=0) %>%
  clean_names()
# merge design and text
dominant_long = select(dominant, block, choice_situation, starts_with('choice')) %>%
  pivot_longer(-c(choice_situation, block)) %>%
  mutate(choice_num = 1, # dummy needed to make figure labelling work
         choice = ifelse(str_detect(name, '1_'), 1, 2),
         aname = str_remove_all(name, 'choice[1-2]_')) %>%
  rename('level' = 'value')
dominant_long = right_join(text, dominant_long, by=c('aname','level')) 
# make picture:
for_latex = make_qualtrics_image(indata = dominant_long, choicenum = 1, merge_last = TRUE) # merge last as it is the same


### Part 4, make the scenario into an image ###
### not sure about this ###
scenario = read.table('scenario.txt', sep='!', header=FALSE, fill=TRUE)
