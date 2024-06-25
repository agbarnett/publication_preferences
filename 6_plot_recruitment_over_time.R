# 6_plot_recruitment_over_time.R
# plot the recruitment by journal over time
# called from 6_summary.Rmd
# April 2024
library(ggplot2)
library(dplyr)
# get the data, from 5_read_qualtrics.R
load('data/5_AnalysisReady.RData')

# use today as last date
censor.date = max(data$start_date)+1 # one day after last survey ???  to update

# cumulative counts by journal
counts = group_by(data, start_date) %>%
  tally() %>%
  arrange(start_date) %>%
  mutate(csum = cumsum(n)) %>%
  ungroup()
# add zero at start
zeros = arrange(counts, start_date) %>%
  slice(1) %>% # earliest date
  ungroup() %>%
  mutate(csum=0)
# add final date at end
final = arrange(counts, desc(start_date)) %>%
  slice(1) %>%
  mutate(start_date = censor.date)

#
to_plot = bind_rows(zeros, counts, final) %>% 
  unique() # in case of duplicates due to adding final date

# plot
label1 = data.frame(start_date = as.Date('2024-03-26'), csum=20, label='First 204 sent')
label2 = data.frame(start_date = as.Date('2024-04-01'), csum=20, label='Second 1001 sent')
label3 = data.frame(start_date = as.Date('2024-04-09'), csum=20, label='First reminders sent')
label4 = data.frame(start_date = as.Date('2024-04-19'), csum=20, label='Final reminders sent')
gplot = ggplot(data=to_plot, aes(x=start_date, y=csum)) +
  geom_step(linewidth=1.05, col='darkseagreen4')+
  geom_text(data = label1, aes(x=start_date, y=csum, label=label), angle=90, col='grey55')+
  geom_text(data = label2, aes(x=start_date, y=csum, label=label), angle=90, col='grey55')+
  geom_text(data = label3, aes(x=start_date, y=csum, label=label), angle=90, col='grey55')+
  geom_text(data = label4, aes(x=start_date, y=csum, label=label), angle=90, col='grey55')+
  ylab('Cumulative responses')+
  xlab('Date')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position=c(0.2,0.85),
        plot.margin = margin(0, 5, 0, 0, "mm")) # trbl
#
jpeg('figures/recruitment.jpg', width=5, height=4, units='in', res=500, quality = 100)
print(gplot)
dev.off()

## for text
# number of weeks recruitment
recruitment_weeks = summarise(to_plot, min = min(start_date)) %>%
  mutate(today = as.Date(Sys.Date()),
         days = today - min,
         weeks = floor(as.numeric(days)/7)) %>%
  pull(weeks)

