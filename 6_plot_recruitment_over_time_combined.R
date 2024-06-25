# 6_plot_recruitment_over_time_combined.R
# plot the recruitment by journal over time (for first and second survey combined)
# called from 6_summary_combined.Rmd
# May 2024
library(ggplot2)
library(dplyr)

# data from 6_summary_combined.Rmd

# use today as last date
censor.date = as.Date('2024-05-31') # last day of May

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
label2 = data.frame(start_date = as.Date('2024-04-01'), csum=40, label='Second 1001 sent')
label3 = data.frame(start_date = as.Date('2024-04-09'), csum=70, label='First reminders')
label4 = data.frame(start_date = as.Date('2024-04-19'), csum=115, label='Final reminders')
label5 = data.frame(start_date = as.Date('2024-05-07'), csum=325, label='First 6374 sent')
label6 = data.frame(start_date = as.Date('2024-05-14'), csum=450, label='First reminder')
label7 = data.frame(start_date = as.Date('2024-05-21'), csum=270, label='Final reminder')
label8 = data.frame(start_date = as.Date('2024-03-26'), csum=0, label='First survey')
label9 = data.frame(start_date = as.Date('2024-05-07'), csum=0, label='Second survey')
text_cols = c("darkorchid3", "red3")
gplot = ggplot(data=to_plot, aes(x=start_date, y=csum)) +
  geom_step(linewidth=1.05, col='darkseagreen4')+
  geom_text(data = label1, aes(x=start_date, y=csum, label=label), adj=0, nudge_x=-0.5, angle=90, col=text_cols[1])+
  geom_text(data = label2, aes(x=start_date, y=csum, label=label), adj=0, angle=90, col=text_cols[1])+
  geom_text(data = label3, aes(x=start_date, y=csum, label=label), adj=0, angle=90, col=text_cols[1])+
  geom_text(data = label4, aes(x=start_date, y=csum, label=label), adj=0, angle=90, col=text_cols[1])+
  geom_text(data = label5, aes(x=start_date, y=csum, label=label), adj=0, nudge_x=-0.5, angle=90, col=text_cols[2])+
  geom_text(data = label6, aes(x=start_date, y=csum, label=label), adj=0, angle=90, col=text_cols[2])+
  geom_text(data = label7, aes(x=start_date, y=csum, label=label), adj=0, angle=90, col=text_cols[2])+
  geom_text(data = label8, aes(x=start_date, y=csum, label=label), adj=0, angle=0, nudge_y = -10, col=text_cols[1])+
  geom_text(data = label9, aes(x=start_date, y=csum, label=label), adj=0, angle=0, nudge_y = -10, col=text_cols[2])+
  ylab('Cumulative responses')+
  xlab('Date')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        legend.position=c(0.2,0.85),
        plot.margin = margin(0, 5, 0, 0, "mm")) # trbl
#
jpeg('figures/recruitment_combined.jpg', width=5, height=4, units='in', res=500, quality = 100)
print(gplot)
dev.off()

## for text
# number of weeks recruitment
recruitment_weeks = summarise(to_plot, min = min(start_date)) %>%
  mutate(today = as.Date(Sys.Date()),
         days = today - min,
         weeks = floor(as.numeric(days)/7)) %>%
  pull(weeks)

