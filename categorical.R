# categorical script
# by Dexter Locke and Hillol Dutta, with input from Rinku Roy Chowdhury

# good for colors
library(RColorBrewer)
RColorBrewer::display.brewer.all() # great website http://colorbrewer2.org ! 

# lets aim for consistent colors 
pal_city <- brewer.pal(name = 'Set1', n = 6)  # this could be our city palette
pal_city                                      # prints the colors out in hex format

# fancy version of the same thing, but the number of classes comes from the data
pal_city <- brewer.pal(name = 'Set1',
                       n = length(unique(df$MSA)))  # this will be our city palette
                                                    # this could be useful if the number of categories changes
pal_city                                      # See, same colors!

# library(ggpubr) # should have been run in the last script, that gives the ggboxplot() function
# helpful instructions here: https://rpkgs.datanovia.com/ggpubr/reference/ggboxplot.html
# also helpful http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/
df %>% ggboxplot(y = 'Perc_Tree',
                 x = 'MSA') # basic box plot

# add colors
df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
                 x = 'MSA',       # categorical grouping variable
                 fill = 'MSA')    # color by the category

# add OUR colors from pal_city by copy/paste from 
df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
                 x = 'MSA',       # categorical grouping variable
                 fill = 'MSA',
                 palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"))  

# add OUR colors from pal_city
df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
                 x = 'MSA',       # categorical grouping variable
                 fill = 'MSA',
                 palette = 'Set1')# ggpubr is really smart! makes the same graph

df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
                 x = 'MSA',       # categorical grouping variable
                 fill = 'MSA',    # what to color by
                 palette = 'Set1',# what colors to use
                 ylim = c(0, 100),# the value of Y (% tree canopy) hypothetically can range from 0 to 100
                 ylab = 'Tree Canopy Cover (%)', # more attractive label
                 xlab = 'Metropolitan Statistical Area',
                 #add = 'jitter',  # Try turning this on and off with "#"
                 legend = '')

# add some stats
df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
                 x = 'MSA',       # categorical grouping variable
                 fill = 'MSA',    # what to color by
                 palette = 'Set1',# what colors to use
                 ylim = c(0, 100),# the value of Y (% tree canopy) hypothetically can range from 0 to 100
                 ylab = 'Tree Canopy Cover (%)', # more attractive label
                 xlab = 'Metropolitan Statistical Area',
                 #add = 'jitter',  # Try turning this on and off with "#"
                 legend = '') + 
  stat_compare_means() -> city_tree_plot# answers the question "are thes medians different from eachother"

# print the graphic, noticed we assigned with "->" at the end?
city_tree_plot

# but we want to know which pairs are different
city_comps <- list(c('PHX', 'MSP'),
                   c('PHX', 'MIA'),
                   c('PHX', 'LAX'),
                   c('PHX', 'BOS'),
                   c('PHX', 'BAL'))

df %>% mutate(city_urban = paste(MSA, Urbanicity, sep = '_')) %>% # new city - urbanicity variable
  ggboxplot(y = 'Perc_Tree', # continuous dependent variable
                 x = 'city_urban',       # categorical grouping variable
                 fill = 'MSA',    # what to color by
                 palette = 'Set1',# what colors to use
                 #ylim = c(0, 150),# the value of Y (% tree canopy) hypothetically can range from 0 to 100
                 ylab = 'Tree Canopy Cover (%)', # more attractive label
                 xlab = 'Metropolitan Statistical Area',
                 #add = 'jitter',  # Try turning this on and off with "#"
                 legend = '')

# unfinished
df %>% mutate(city_urban = paste(MSA, Urbanicity, sep = '_')) %>% # new city - urbanicity variable
  ggboxplot(y = 'Perc_Tree', # continuous dependent variable
            x = 'city_urban',       # categorical grouping variable
            fill = 'MSA',    # what to color by
            facet.by = 'MSA',
            palette = 'Set1',# what colors to use
            #ylim = c(0, 150),# the value of Y (% tree canopy) hypothetically can range from 0 to 100
            ylab = 'Tree Canopy Cover (%)', # more attractive label
            xlab = 'Metropolitan Statistical Area',
            #add = 'jitter',  # Try turning this on and off with "#"
            legend = '')


