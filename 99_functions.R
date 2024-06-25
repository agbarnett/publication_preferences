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

# convert p-value from scientific
convert_pvalue = function(x){
  x = tolower(x)
  if(str_detect(x, pattern='<')){return(x)}
  if(str_detect(x, pattern='e')){
    power = as.numeric(str_extract(x, pattern='(?<=e-)[0-9]'))
    number = as.numeric(str_remove(x, pattern='e-[0-9]'))
    y = number * 10^(-power)
    y = format.pval(y, digits=3, eps=0.001)
    return(y)
  }
  return(x)
}

# rename for tables
nice_rename = function(x, version = 1 ){
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
    x == 'q30' & version ==1 ~ 'Publishing expectations',
    x == 'q30' & version ==2 ~ 'Publication target',
    x == 'q31' & version ==2 ~ 'Publication target number',
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
  email = ifelse(str_detect(email, paste('\\.net\\b', sep='')), 'net', email)
  email = ifelse(str_detect(email, paste('\\.cat\\b', sep='')), 'cat', email)
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


## plots ##

## plot function for interaction
plot_interaction = function(indata, varcov, main, interaction, xlab, xlabels, ltitle, llabels, colours){
  
  # slim down variance-covariance for interaction
  index = which(colnames(varcov) %in% main)
  vcov = varcov[index, index]
  
  #
  if (length(main) == 2){
    
    hjusts = c(0,1) # for later plot
    
    # both off
    f1 = data.frame(x = 0, line = 0, utility = 0, var = 0) 
    # one on
    est1 = filter(indata, term==main[1]) %>% pull(est)
    index = which(colnames(vcov) %in% main[1])
    var = as.numeric(varcov[index, index])
    f2 = data.frame(x = 1, line = 0, utility = est1, var = var)
    # one on
    est2 = filter(indata, term==main[2]) %>% pull(est)
    index = which(colnames(vcov) %in% main[2])
    var = as.numeric(varcov[index, index])
    f3 = data.frame(x = 0, line = 1, utility = est2, var = var) 
    # both on
    index = which(colnames(vcov) %in% main[2])
    var = sum(vcov) # both variance and twice covariance
    est3 = filter(indata, term==interaction) %>% pull(est)
    f4 = data.frame(x = 1, line = 1, utility = est1 + est2 + est3, var = var) 
    # concatenate 
    to_plot = bind_rows(f1, f2, f3, f4)
    breaks = c(0,1)
  }
  # 
  if (length(main) == 3){
    
    hjusts = c(0,0.5,1)
    
    # both off
    est0 = 0 
    var = 0
    f0 = data.frame(x = 0, line = 0, utility = est0, var = var) 
    # one on medium
    est1 = filter(indata, term==main[1]) %>% pull(est)
    index = which(colnames(vcov) %in% main[1])
    var = as.numeric(varcov[index, index])
    f1 = data.frame(x = 1, line = 0, utility = est1, var = var) 
    # one on high
    est2 = filter(indata, term==main[2]) %>% pull(est)
    index = which(colnames(vcov) %in% main[2])
    var = as.numeric(varcov[index, index])
    f2 = data.frame(x = 2, line = 0, utility = est2, var = var) 
    # other factor on
    est3 = filter(indata, term==main[3]) %>% pull(est)
    index = which(colnames(vcov) %in% main[3])
    var = as.numeric(varcov[index, index])
    f3 = data.frame(x = 0, line = 1, utility = est3, var = var) # 
    # medium interaction
    int1 = filter(indata, term==interaction[1]) %>% pull(est) 
    index = which(colnames(vcov) %in% c(main[1], main[3]))
    var = as.numeric(sum(varcov[index, index]))
    f4 = data.frame(x = 1, line = 1, utility = est1 + est3 + int1, var = var) 
    # high interaction
    int2 = filter(indata, term==interaction[2]) %>% pull(est)
    index = which(colnames(vcov) %in% c(main[2], main[3]))
    var = as.numeric(sum(varcov[index, index]))
    f5 = data.frame(x = 2, line = 1, utility = est2 + est3 + int2, var = var) 
    to_plot = bind_rows(f0, f1, f2, f3, f4, f5) 
    breaks = c(0,1,2)
  }
  
  # calculate CIs
  to_plot =  mutate(to_plot, se = sqrt(var),
           z = qnorm(0.975),
           lower = utility - (z*se),
           upper = utility + (z*se))
  
  
  # for margins see https://www.tidyverse.org/blog/2024/02/ggplot2-3-5-0-legends/#spacing-and-margins
  iplot = ggplot(data = to_plot, aes(x=x, y=utility, ymin=lower, ymax=upper, col=factor(line)))+
    geom_point(size=2.5, position = position_dodge(width=0.05))+
    geom_errorbar(width=0, linewidth=1.05, position = position_dodge(width=0.05))+
    geom_line(linewidth=1.05, position = position_dodge(width=0.05))+
    scale_x_continuous(breaks=breaks, labels = xlabels)+
    xlab(xlab)+
    ylab('Utility')+
    scale_color_manual(ltitle, labels=llabels, values = colours)+
    theme_bw()+
    theme(axis.text.x = element_text(hjust = hjusts), # squeeze in label text
          plot.margin = margin(t=2, r=1, l=4, b=0, "pt"), # reduce space outside plot region
          legend.box.spacing = unit(0, 'mm'), # reduce space between plot and legend
          legend.box.margin	= margin(t=0, r=0, b=0, l=0), # reduce space around legend
          legend.margin = margin(t=0, r=0, b=0, l=0, unit='mm'), # reduce space around legend
          legend.position = 'top',
          panel.grid.minor = element_blank())
  
  # export to figure
  main = str_remove_all(main, '_')
  main = paste(main, collapse = '_')
  fname = paste('figures/interaction', main, '.jpg', sep='')
  jpeg(filename = fname, width=5, height=4.5, units='in', res=500, quality=100)
  print(iplot)
  dev.off()
  
  # return
  iplot
}


### plot function for main results ###
plot_function = function(indata, legend_title, legend_labels, colours, no_y_labels = FALSE){
  
  # 
  if(legend_title == 'Overall'){
    gplot = ggplot(data =indata, aes(x=termn, y=est, ymin=lower, ymax=upper))+
      theme_bw()
    dodge_width = 0
  }
  if(legend_title != 'Overall'){
    gplot = ggplot(data =indata, aes(x=termn, y=est, ymin=lower, ymax=upper, col=factor(group)))+
      scale_color_manual(legend_title, labels = legend_labels, values=colours)+
      theme_bw()+
      theme(legend.position.inside = c(0.9, 0.1))
    dodge_width = 0.45
  }
  
  #
  gplot = gplot +
    geom_hline(lty=2, col='grey55', yintercept=0)+ # dotted line at zero utility
    scale_x_reverse(breaks = 1:7, labels=labels)+
    geom_point(position = position_dodge(width=dodge_width), size=2)+
    geom_errorbar(position = position_dodge(width=dodge_width), width=0, linewidth=1.05)+
    theme(plot.margin = margin(t=0, r=1, l=0, b=0, "pt"), # reduce space outside plot region
          legend.position = 'top',
          legend.box.spacing = unit(0, 'mm'), # reduce space between plot and legend
          legend.box.margin	= margin(t=0, r=0, b=0, l=0), # reduce space around legend
          legend.margin = margin(t=0, r=0, b=0, l=0, unit='mm'), # reduce space around legend
          panel.grid.minor = element_blank())+
    xlab('')+
    ylab('Utility')+
    coord_flip()
  if(no_y_labels==TRUE){ # remove labels for large plot
    gplot = gplot + scale_x_reverse(breaks = 1:7, labels=NULL)
  }
  
  # export to figure
  legend_title = str_remove_all(legend_title, '\n')
  legend_title = str_replace_all(legend_title, ' ', '_')
  fname = paste('figures/effects_', legend_title, '.jpg', sep='')
  jpeg(filename = fname, width=5, height=4.5, units='in', res=500, quality=100)
  print(gplot)
  dev.off()
  # return
  gplot
}

## function to process Sameera's results from Excel, used by 7_plot_nlogit_results.R 
process_results = function(sheet, interaction = FALSE, range, name_range){
  
  # excel file depends on interactio 
  infile = ifelse(interaction == FALSE,
                  'results/Results_06.06.2024b.xlsx',
                  'results/Results_20.06.2024.xlsx')
  
  # read results
  results = read_excel(infile, sheet=sheet, range=range, col_names = FALSE) %>%
    clean_names() %>%
    filter(!is.na(x2))

  # for interactions
  if(interaction == TRUE){
    column_names = read_excel(infile, sheet=sheet, range=name_range, col_names = FALSE) %>%
      clean_names() %>%
      as.character()
    names(results) = column_names
    row.names(results) = column_names
  }
    
  # for main effects:
  if(interaction == FALSE){
    names(results) = column_names
    results = filter(results, !str_detect(term, '^Ns'), term != 'ASC_A') %>% # remove standard errors
      mutate(est = str_remove_all(est, pattern = '\\*'),
             est = as.numeric(est),
             termn = case_when(
               term == 'RANK_H' ~ 1,
               term == 'RANK_M' ~ 2,
               term == 'STY_MIN' ~ 3,
               term == 'SPEED' ~ 4,
               term == 'REVIEW' ~ 5,
               term == 'EDITOR' ~ 6,
               term == 'EVIDENCE' ~ 7
             ))
    #
  }
  return(results)
}
