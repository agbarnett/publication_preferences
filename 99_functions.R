## 99_functions.R
# functions for DCE
# December 2023

## simulate choice data; assumes nominal levels
# could add interactions?
sim_choice = function(n_respondents = 200,  # number of respondents
           noise_sd = 0.1, # standard deviation for noise 
           design = design, # experimental design, must have variables 'choice[1|2]_[...]', block and qnumber
           effects = NULL){
           
# SD for noice:
# (very high noise gives example of a fully null model as any signal is drowned out by noise)
# (very low noise gives example of a deterministic scenario)

  # checks
  #lengths(design)
  
  # work out number of sets, assume equal number of sets per block
  # number of choice sets that each respondent will see
  n_sets = nrow(filter(design, block ==1))
  n_blocks = max(design$block)
  
  # random noise in choices
noise = matrix(rnorm(n_respondents*n_sets, mean = 0, sd = noise_sd), nrow = n_respondents, ncol = n_sets) 
# sample blocks and ensure balance
blocks = rep(1:n_blocks, ceiling(n_respondents/n_blocks))
blocks = sample(blocks, size=length(blocks), replace=FALSE)

# loop to randomly create choices for a single experiment
simulated = long = NULL
for (k in 1:n_respondents){
  this_block = blocks[k] # randomly sample block, same per participant
  for (q in 1:n_sets){ # loop through choice sets
    this_set1 = filter(design, block == this_block, qnumber == q) %>% # use one choice set
      select(starts_with('choice1')) %>%
      as.numeric()
    this_set2 = filter(design, block == this_block, qnumber == q) %>% # use one choice set
      select(starts_with('choice2'))%>%
      as.numeric()
    if(length(effects) != length(this_set1)){cat('Error, mismatch in effects and design')}
    eq1 = eq2 = 0
    for(j in 1:length(effects)){
      eq1 = eq1 + sum(effects[[j]]* (1:length(effects[[j]]) == this_set1[j])) # parameter effects times design matrix
      eq2 = eq2 + sum(effects[[j]]* (1:length(effects[[j]]) == this_set2[j])) # parameter effects times design matrix
    }
    eq1 = eq1 + noise[k,q] # just apply noise to one choice
    if(eq1 > eq2){this_choice = 1}
    if(eq1 <= eq2){this_choice = 2}
    
    ## make long version (how to include no decision, both set to FALSE?)
    this_set = filter(design, block == this_block, qnumber == q) # use one choice set
    choice_one = select(this_set, qnumber, contains('choice1')) %>%
      rename_with(.fn = ~gsub("choice1_", "", .), .cols=everything()) # rename columns
    choice_two = select(this_set, qnumber, contains('choice2')) %>%
      rename_with(.fn = ~gsub("choice2_", "", .), .cols=everything()) # rename columns
    this_long = bind_rows(choice_one, choice_two) %>%
      mutate(participant = k,
             alternative = 1:2,
             strata = paste(participant, '.', qnumber, sep=''), # create unique strata for each choice per participant (needed for clogit)
             choice = as.numeric(alternative == this_choice)) # create binary choice
    long = bind_rows(long, this_long)
    
    ## make wide version, change variable names
    this_set_wide = mutate(this_set, 
                           choice = this_choice,
                           participant = k) %>%
      rename_with(.fn = ~paste(., '_1', sep=''), .cols=starts_with('choice1')) %>% # add 1 to end
      rename_with(.fn = ~gsub("choice1_", "", .), .cols=starts_with('choice1')) %>% # remove all numbers from names
      rename_with(.fn = ~paste(., '_2', sep=''), .cols=starts_with('choice2')) %>% # add 2 to end
      rename_with(.fn = ~gsub("choice2_", "", .), .cols=starts_with('choice2')) # remove all numbers from names
    simulated = bind_rows(simulated, this_set_wide)
  }
}

## checks
# should all be 2:
tab = table(table(long$strata))
if(length(tab)>1 | names(tab)!= '2'){cat('Error in strata')}
# should be balanced and 1 or 2
tab = table(long$alternative)
if(tab[1] != tab[2]){cat('Error in balance')}
# check numbers
table(simulated$choice)
# check for balance across blocks
table(simulated$block)

# return
to.return = list()
to.return$long = long
to.return$simulated = simulated
return(to.return)

}


## 
plot_compare = function(model, effects){
  
  # get means and intervals
  coef = tidy(model, conf.int = TRUE)
  # remove reference category
  eff = NULL
  for (k in 1:length(effects)){
    eff = c(eff,  effects[[k]][-1] - effects[[k]][1]) # relative to reference category
  }
  # combine estimates and simulated sizes
  coef= mutate(coef, 
               e= eff,
               name = str_remove_all(term, 'factor\\(|\\)|[0-9]'))
  
  #
  plot = ggplot(data= coef, aes(x=e, y=estimate, ymin=conf.low, ymax=conf.high, col=name))+
    geom_point(position=position_dodge(width=0.05))+
    geom_errorbar(width=0, position=position_dodge(width=0.05))+
    xlab('Simulated effect')+
    ylab('Estimated effect')+
    theme_bw()+
    theme(legend.title = NULL)
  
  return(plot)
}


## check correlation of design matrix
check_cor = function(indata){
  cor = cor(indata)
  long = reshape2::melt(cor) %>%
    clean_names() %>%
    mutate(text = round(value, 2))
  tile = ggplot(data = long, aes(x=var1, y=var2, fill=value, label=text))+
    geom_tile()+
    geom_text(size=3)+
    scale_fill_gradient2('Correlation', low='navy', mid='white', high='darkred')+
    theme_bw()+
    xlab('')+
    ylab('')+
    theme(axis.text.x = element_text(angle=45, hjust=1))
  tile
}


# rename for tables
nice_rename = function(x){
  y = case_when(
    x == 'q3' ~ 'Choice set - dominant',
    x == 'q5' ~ 'Choice set 1',
    x == 'q7' ~ 'Choice set 2',
    x == 'q9' ~ 'Choice set 3',
    x == 'q11' ~ 'Choice set 4',
    x == 'q13' ~ 'Choice set 5',
    x == 'q15' ~ 'Choice set 6',
    x == 'q17' ~ 'Choice set 7',
    x == 'q20' ~ 'Choice set 8',
    x == 'q22' ~ 'Choice set - retest',
    x == 'q24' ~ 'DCE difficulty',
    x == 'q25' ~ 'Broad research area',
    x == 'q26' ~ 'Gender',
    x == 'q27' ~ 'Years working in research',
    x == 'q28' ~ 'Number of papers',
    x == 'q29' ~ 'Country',
    x == 'q30' ~ 'Publishing expectations',
    TRUE ~ as.character(x)
  )
  return(y)  
}

## extract country from email code
extract_country = function(email){
  codes = read_excel('data/country_codes.xlsx', skip=1) %>%
    mutate(name = str_remove(name, '^\\.')) %>%
    filter(name !='ac') # Acension island gets mixed with academic
  # loop through countries
  for (code in codes$name){ # loop through codes
    pattern = paste('\\.', code, '\\b', sep='')
    email = ifelse(str_detect(email, pattern), code, email)
  }
  # other common codes
  email = ifelse(str_detect(email, paste('\\.edu\\b', sep='')), 'edu', email)
  email = ifelse(str_detect(email, paste('\\.gov\\b', sep='')), 'gov', email)
  email = ifelse(str_detect(email, paste('\\.org\\b', sep='')), 'org', email)
  email = ifelse(str_detect(email, paste('\\bnhs\\.', sep='')), 'nhs', email)
  # combine .com but keep common ones
  email = ifelse(str_detect(email, paste('gmail\\.com\\b', sep='')), 'gmail', email)
  email = ifelse(str_detect(email, paste('163\\.com\\b', sep='')), '163', email)
  email = ifelse(str_detect(email, paste('126\\.com', sep='')), '126', email)
  email = ifelse(str_detect(email, paste('yahoo\\.com', sep='')), 'yahoo', email)
  email = ifelse(str_detect(email, paste('hotmail\\.com', sep='')), 'hotmail', email)
  email = ifelse(str_detect(email, paste('qq\\.com', sep='')), 'qq', email)
  email = ifelse(str_detect(email, paste('sina\\.com', sep='')), 'sina', email)
  email = ifelse(str_detect(email, paste('\\.com\\b', sep='')), 'com', email) # all other .com
  return(email)
}
