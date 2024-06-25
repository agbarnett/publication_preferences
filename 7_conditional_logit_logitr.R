# 7_conditional_logit_logitr.R
# run the conditional logit models using the logitr package
# June 2024
library(logitr)
library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)

## get tbroom.mixed## get the survey data ##
source('7_prepare_journal_data.R')
database = mutate(database, 
                  ID = paste(design, id, sep='.'),
                  stratan = as.numeric(as.factor(strata))) %>% # needs to be a number for logitr
  filter(!is.na(choicen)) %>% # exclude missing choice tasks
  mutate(IDnum = as.numeric(as.factor(ID))) %>% # convert to number for intercept
  arrange(IDnum, qnumber) # has to be in order

# run model, close to Sameera results
# "CHOICE" gives the constant =  "Alternative specific coefficient"
mnl_journal <- logitr(
  data    = database,
  outcome = 'choicen', # 1 = selected, 0 = not selected
  obsID   = 'stratan', # each unique choice
  panelID = 'IDnum', # for repeated data from same person
  pars    = c(
    'CHOICE','field1', 'field2', 'format', 'decision', 'review', 'editor', 'evidence'
  ),
  drawType = 'sobol',
  numDraws = 200,
  randPars = c(field1 = 'n', field2 = 'n', format = 'n', decision = 'n', review = 'n', editor = 'n', evidence = 'n')
)
summary(mnl_journal)

# separate by scenario
s1 = filter(database, scenario == 1)
s2 = filter(database, scenario == 2)
mnl_journal_s1 <- logitr(
  data    = s1,
  outcome = 'choicen', # 1 = selected, 0 = not selected
  obsID   = 'stratan', # each unique choice
  panelID = 'IDnum', # for repeated data from same person
  pars    = c(
    'CHOICE','field1', 'field2', 'format', 'decision', 'review', 'editor', 'evidence'
  ),
  drawType = 'sobol',
  numDraws = 500,
  randPars = c(field1 = 'n', field2 = 'n', format = 'n', decision = 'n', review = 'n', editor = 'n', evidence = 'n')
)
mnl_journal_s2 <- logitr(
  data    = s2,
  outcome = 'choicen', # 1 = selected, 0 = not selected
  obsID   = 'stratan', # each unique choice
  panelID = 'IDnum', # for repeated data from same person
  pars    = c(
    'CHOICE','field1', 'field2', 'format', 'decision', 'review', 'editor', 'evidence'
  ),
  drawType = 'sobol',
  numDraws = 500,
  randPars = c(field1 = 'n', field2 = 'n', format = 'n', decision = 'n', review = 'n', editor = 'n', evidence = 'n')
)
summary(mnl_journal_s1)
summary(mnl_journal_s2)

# plot
e1 = tidy(mnl_journal_s1, conf.int = TRUE)
e2 = tidy(mnl_journal_s2, conf.int = TRUE)
to_plot = bind_rows(e1, e2, .id = 'scenario') %>%
  filter(effect == 'fixed') %>%
  mutate(termn = case_when(
    term == 'field1' ~ 1,
    term == 'field2' ~ 2,
    term == 'format' ~ 3,
    term == 'decision' ~ 4,
    term == 'review' ~ 5,
    term == 'editor' ~ 6,
    term == 'evidence' ~ 7
  ))
labels = c('Highest JIF', 'Moderate JIF', 'Minor formatting', 'Fast decision',
           'Helpful review', 'Changes in wording/format', 'Useful for promotion')

# make into odds ratios
to_plot = mutate(to_plot,
                 estimate = exp(estimate),
                 conf.low = exp(conf.low),
                 conf.high = exp(conf.high))

#
gplot = ggplot(data =to_plot, aes(x=termn, y=estimate, ymin=conf.low, ymax=conf.high, col=factor(scenario)))+
  geom_hline(lty=2, col='grey55', yintercept=0)+
  scale_x_reverse(breaks = 1:7, labels=labels)+
  scale_y_log10()+
  geom_point(position = position_dodge(width=0.45), size=2)+
  geom_errorbar(position = position_dodge(width=0.45), width=0, linewidth=1.05)+
  scale_color_manual('Scenario', values=c("khaki3", "red2"))+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  xlab('')+
  ylab('Log odds')+
  coord_flip()
gplot
