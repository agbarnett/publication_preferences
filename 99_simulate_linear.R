# 99_simulate_linear.R
# simulate a linear association for a continuous variable (e.g., cost and preference) and the most efficient design for finding it
# March 2023
library(ggplot2)
library(janitor)
library(dplyr)

# key constants
N = 300 # Number of people to sample
range = c(0,100) # range for continuous variable
rangec = range - mean(range) # centre range just for convenience of scaling
or = 1.5 # odds ratio between continuous variable and preference (per 10 increase)
N.sim = 100 # number of simulations
noise = 10 # SD for error in expressing preferences

#
model_results = NULL
for (k in 1:N.sim){
  simulated1 = simulated2 = NULL
  for (person in 1:N){
    ## design 1: only from extremes
    r1 = log(or)*(rangec[1]/10) + rnorm(n=1, mean=0, sd=noise) > 0 # lower extreme
    r2 = log(or)*(rangec[2]/10) + rnorm(n=1, mean=0, sd=noise) > 0 # upper extreme
    # make data frame
    f1 = data.frame(design = 1, person = person, continuous = rangec[1], response = r1)
    f2 = data.frame(design = 1, person = person, continuous = rangec[2], response = r2)
    simulated1 = bind_rows(simulated1, f1, f2)
    ## design 2: random over the range
    lowerx = round(runif(n=1, min = range[1], max = mean(range))) # somewhere in the lower half
    minc = max(c(lowerx+20, mean(range))) # minimum distance between continuous variables of 20
    upperx = round(runif(n=1, min = mean(range), max = range[2])) # somewhere in the upper half
    r1 = log(or)*(lowerx/10) + rnorm(n=1, mean=0, sd=noise) > 0 # lower extreme
    r2 = log(or)*(upperx/10) + rnorm(n=1, mean=0, sd=noise) > 0 # upper extreme
    # make data frame
    nf1 = data.frame(design = 2, person = person, continuous = lowerx, response = r1)
    nf2 = data.frame(design = 2, person = person, continuous = upperx, response = r2)
    simulated2 = bind_rows(simulated2, nf1, nf2)
  }
  # run model
  model1 = glm(response==TRUE ~ I(continuous/10), data=simulated1, family = binomial())
  model2 = glm(response==TRUE ~ I(continuous/10), data=simulated2, family = binomial())
  s1 = summary(model1)
  s2 = summary(model2)
  res = data.frame(rbind(s1$coefficients[2,], s2$coefficients[2,])) %>%
    clean_names() %>%
    mutate(model = 1:2)
  model_results = bind_rows(model_results, res)
}

# Compare SE
gplot = ggplot(data = model_results, aes(x=factor(model), y=estimate))+
 # geom_hline(yintercept=log(or), lty=2, col='dark green')+
  geom_boxplot(fill='skyblue')+
  theme_bw()+
  xlab('')+
  scale_x_discrete(breaks=1:2, labels=c("Discrete",'Continuous'))+
  ylab('Log odds ratio')+
  coord_flip()
gplot
ggsave(filename='figures/simulation.jpg', plot=gplot, width=5, height=4, units='in', dpi=400)

#boxplot(std_error ~ factor(model), data = model_results)
#boxplot(estimate ~ factor(model), data = model_results)

