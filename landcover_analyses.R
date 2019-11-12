# Script by Dexter H Locke and Hillol Dutta
# for analyzing land cover composition, as part of the MacroSystems Biology project

# started on
# Mon Oct 21 19:58:30 2019 ------------------------------


# clear the workspace
rm(list=ls())           # depending on what other project you have loaded,
                        # you may want to comment this out. This command
                        # removes everything from memory

# load some useful libraries
library(tidyverse)      # this is actually a collection of packages
library(sf)             # used for reading shapefiles
                        # lots of other great spatial functions, too, but we wont use them
library(sjPlot)         # nice graphs and tables supporting many models
library(lme4)           # fits multi-level models
library(ggpubr)         # mixes base stats functions with ggplot graphics, its great!
library(RColorBrewer)   # good for colors
library(cowplot)        # for multi-paned graphs NOTE THAT THIS MASKS ggplot2::ggsave()!!

# at the end lets make sure to do
citation('tidyverse') # but for each package. There is a package called grateful that is supposed to help with 
                      # citing each package but its kind of glitchy

# read in the data
# you will have to change the file path to match the location of the data
# on your computer
#sf <- read_sf('/Users/dlocke/temp_MSB4RoyChowdhuryDobler/CBGs/CBG_ALL_v20161014.shp') 
sf <- read_sf('D:/Macrobio/blockgroup_landcover/CBG_ALL_v20161014.shp') # this is working for us, 
                                                                        # but the better/ more sophistocated why is to something 
                                                                        # like this
# https://community.rstudio.com/t/project-oriented-workflow-setwd-rm-list-ls-and-computer-fires/3549

# These data were in that folder so I could share easily with Carlos
# but I'm keeping the R project locally here now:
getwd()               # "/Users/dlocke/msb/LandCoverPaper/landcover_analyses"

# take a look at the data of cbg
sf
names(sf)

# we don't actually need the polygons here, so lets pull the data frame out
# that's like working with the attribute table in ArcGIS, but not the spatial data
df <- dplyr::select(as.data.frame(sf), -geometry)

# here is another way to do what is shown above, just an FYI
# df <- st_drop_geometry(sf)

head(df)
names(df)

# what's most important here are the following columns
# Dependent variables (DV) we have too many! This will be the left side of the "~"
# Tree           # Tree canopy area
# Grass          # grass/shrub area
# Other          # non-tree, non-grass/shrub, non-water area (also known as "other")
# Water          # water area
# Perc_Tree      # % tree canopy cover
# Perc_Grass     # % grass cover
# Perc_Other     # % other area
# Perc_Water     # % water area
# NP_T           # Number of Patches Tree (distinct tree patches, group of pixels)
# MPA_T          # Mean Patch Area Tree (the average size of tree patches)
# CV_T           # TODO find out Coefficient of Variation for tree patches
# PAratio_T      # Parimieter Area ratio for tree canopy
# NP_G           # Number of Patches Grass
# MPA_G          # Mean Patch Area Grass
# CV_G           # TODO find out Coefficient of Variation for tree patches
# PAratio_G      # Parimieter Area ratio for tree canopy

# Independent variables (IV)
# INC_MED_HS is the MEDian HouSehold INCome of the block group
# P_White is the percentage of the population that is white
# P_Hisp is the percentage of the population that is hispanic
# P_Own is the percentage of the housing units that are owner-occupied
# HOUSE_AGE 
# SDE_STD

# random effects
table(df$MSA, useNA = 'ifany') # tabulates the number per category
prop.table(table(df$MSA, useNA = 'ifany')) # distributions of block groups, as a %

# or more refined
prop.table(table(df$MSA, useNA = 'ifany'))*100 # actually in percent form

# or even more refined! 
round(prop.table(table(df$MSA, useNA = 'ifany'))*100, 2)# rounding 2 places, much prettier

# try
help(table) # to see what the "useNA" argument is for


#### categorical analyses
####
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

# YES, see the Kruska-Wallis test results now?

# print the graphic, noticed we assigned with "->" at the end?
city_tree_plot

# but we want to know which pairs are different
city_comps <- list(c('PHX', 'MSP'),
                   c('PHX', 'MIA'),
                   c('PHX', 'LAX'),
                   c('PHX', 'BOS'),
                   c('PHX', 'BAL'))

# now doing multiple comparisions
df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
                 x = 'MSA',       # categorical grouping variable
                 fill = 'MSA',    # what to color by
                 palette = 'Set1',# what colors to use
                 ylim = c(0, 150),# ADDED space for new lables
                 ylab = 'Tree Canopy Cover (%)', # more attractive label
                 xlab = 'Metropolitan Statistical Area',
                 #add = 'jitter',  # Try turning this on and off with "#"
                 legend = '') + 
  stat_compare_means() + 
  stat_compare_means(comparisons = city_comps)-> city_tree_plot

# chceck it out!
city_tree_plot

# we'll return to this later
# df %>% mutate(city_urban = paste(MSA, Urbanicity, sep = '_')) %>% # new city - urbanicity variable
#   ggboxplot(y = 'Perc_Tree', # continuous dependent variable
#             x = 'city_urban',       # categorical grouping variable
#             fill = 'MSA',    # what to color by
#             palette = 'Set1',# what colors to use
#             #ylim = c(0, 150),# the value of Y (% tree canopy) hypothetically can range from 0 to 100
#             ylab = 'Tree Canopy Cover (%)', # more attractive label
#             xlab = 'Metropolitan Statistical Area',
#             #add = 'jitter',  # Try turning this on and off with "#"
#             legend = '')
# 
# 
# df %>% mutate(city_urban = paste(MSA, Urbanicity, sep = '_')) %>% # new city - urbanicity variable
#   ggboxplot(y = 'Perc_Tree', # continuous dependent variable
#             x = 'city_urban',       # categorical grouping variable
#             fill = 'MSA',    # what to color by
#             facet.by = 'MSA',
#             palette = 'Set1',# what colors to use
#             #ylim = c(0, 150),# the value of Y (% tree canopy) hypothetically can range from 0 to 100
#             ylab = 'Tree Canopy Cover (%)', # more attractive label
#             xlab = 'Metropolitan Statistical Area',
#             #add = 'jitter',  # Try turning this on and off with "#"
#             legend = '')


###
### continuous analyses but first lets scale the predictors, grand-mean scaling
### See this paper which addresses scalling
### guinis H, Gottfredson RK, Culpepper SA (2013) Best-Practice Recommendations for Estimating Cross-Level Interaction Effects Using Multilevel Modeling. J Manage 39(6):1490–1528.
df$Median_Household_Income <- scale(df$INC_MED_HS, center = T)
#df$`Percent_non-White`           <- scale(I(100 - df$P_White), center = T) # so this didn't really work out. :-/
df$Percent_nonWhite           <- scale(I(100 - df$P_White), center = T) # so this didn't really work out. :-/
df$Percent_Hispanic           <- scale(df$P_Hisp, center = T)
df$Percent_Own_House          <- scale(df$P_Own, center = T)
df$Housing_Age             <- scale(df$HOUS_AGE, center = T)
df$Terrain_Roughness       <- scale(df$SDE_STD, center = T) # WOW, the changed names print great in plot_model, I didn't know
                                                            # that plot_mod() changes "_" to " "

# first dependent variable: Perc_Tree
p_tree_mod <- lmer(Perc_Tree ~ Median_Household_Income + # fixed effects
                     Percent_nonWhite +
                     Percent_Hispanic + 
                     Percent_Own_House +
                     Housing_Age + 
                     Terrain_Roughness +
                    (1 | MSA),                               # random effects
                   data = df)

# FIXME LMER is 'wrong' because of the distribution of Perc_Tree - still need to fix this
# df$pct_tree <- (df$Perc_Tree / 100) + 0.0001
# par(mfrow=c(1, 2)); hist(df$Perc_Tree); hist(df$pct_tree)
# p_tree_mod <- glmmTMB::glmmTMB(pct_tree ~ Median_Household_Income + # fixed effects # just being very verbose with "::"
#                      Percent_nonWhite +
#                      Percent_Hispanic + 
#                      Percent_Own_House +
#                      Housing_Age + 
#                      Terrain_Roughness +
#                      (1 | MSA),                               # random effects
#                      data = df,
#                      family = beta_family())


# # these wre just examples to show how to display the different output types, so I'm turning them "off' with #
# plot_model(p_tree_mod)                      # coefficients, defaults to "est"
# plot_model(p_tree_mod) + theme_bw()         # better display?
# plot_model(p_tree_mod) + theme_bw(20)       # BIGGER lables, see 
# 
# plot_model(p_tree_mod, type = 're')         # random effects
# plot_model(p_tree_mod, type = 'std')        # standardized effects, in units of standard deviations
# plot_model(p_tree_mod, type = 'pred')       # standardized effects, in units of standard deviations
# 
# plot_model(p_tree_mod, type = 'diag')       # standardized effects, in units of standard deviations

# Plot model with forest-plot of estimates
# plot_model(p_tree_mod, title = "Continuous Tree model" ) # not really an helpful title

# see additional options here:
# https://strengejacke.github.io/sjPlot/articles/plot_model_estimates.html
# its fun to copy the examples and improve upon them
p_fe <- plot_model(p_tree_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Tree Canopy Cover (%):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(10)                                                      # number pertains to font size

# peak at the graph
p_fe

# random effects: p_tree_mod
p_re <- plot_model(p_tree_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Tree Canopy Cover (%):\nrandom effects',# "\n" means "new line"
                   #sort.est = 'sort.all,                                # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(10)                                                      # number pertains to font size

# peak at the graph
p_re

# combine the two graphs into one two-pane graph
p_tree_graph <- plot_grid(p_fe, # fixed effects, p_tree_mod,
                          p_re, # random effects, p_tree_mod,
                          labels = c('A', 'B'))
# looks great!
p_tree_graph

# save this out
ggplot2::ggsave(plot = p_tree_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/p_tree_canopy_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                         width  = 6.5, # this is  as wide as a normal Word Doc page
                         height = 2.5, # I had to play with this A LOT to get this to look right
                                       # the Plots tab in RStudio is not representative of what the
                                       # the saved version will look like
                units = 'in')

# Ok so that was A LOT of tricks at once. Lets slow this down!
# recall the :: notation means package::function. Both ggplot2 and cowplot have functions
# called "ggsave", we are using the ggplot2 version.

# paste0() combines text without spaces, its like:
# paste('string 1', 'more random text', sep='') is the same as
# paste0('string 1', 'more random text')

# another example
paste('Hillol', 'Dutta,', 'PhD student', 'at Clark University', sep = ' ')
paste('Hillol', 'Dutta,', 'PhD student', 'at Clark University', sep = ',') # strange, too many commas
paste0('Hillol', 'Dutta,', 'PhD student', 'at Clark University') # not enough spaces!
paste0('Hillol ', 'Dutta, ', 'PhD student ', 'at Clark University') # 'manually added spaces

# so 
paste0(getwd(), '/graphs/p_tree_canopy')

# gives me "/Users/dlocke/msb/LandCoverPaper/landcover_analyses/graphs/p_tree_canopy"
# and should give you "C:/Users/HDutta/Documents/Hillol_LCAnalyses/landcover_analyses/graphs/p_tree_canopy"

Sys.time() # gives the date and time in
           # "YYYY-MM-DD HH:MM:SS EST" format (EST is whatever time zone you have your machine set to), where
           # YYYY is year in 4-digit year, MM is two-digit month, DD days.. 
           # because it will never be the same time twice, our graphs will never overwrite the old file

#gsub()     # does Global SUBstitutions. Its like find and replace, see
help(gsub)

# '[[:punct:]]' # is part of regular expressions, which are very powerful. 
                # more info here: https://www.regular-expressions.info/quickstart.html
                # but for here just know that it means basically, remove punctuation (hence "punct")

# So this code gets the colon's out of Sys.time()
gsub('[[:punct:]]', '_', Sys.time())
       
# putting more of it together, this give me
paste0(getwd(),
       '/graphs/p_tree_canopy_',
       gsub('[[:punct:]]',
            '_',
            Sys.time()),
       '.png')
# give me 
# "/Users/dlocke/msb/LandCoverPaper/landcover_analyses/graphs/p_tree_canopy_2019_11_12 15_54_50.png"
# and should give you "C:/Users/HDutta/Documents/Hillol_LCAnalyses/landcover_analyses/graphs/p_tree_canopy_2019_11_12 15_54_50.png"
# but with a different day and time..

# hopefully that just made a new "graphs" folder on your computer.

# plot model diagnostics to check model assumptions
# FIXME (DHL): use Beta GLMMs in glmmTMB
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#beta-glmms
# TODO (DHL) save out the diagnostic plots
plot_model(p_tree_mod, type = 'diag') # this looks really bad. 


# TODO DHL: add other models to this tabular display
tab_model(p_tree_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= '% Tree Canopy Cover') # because of your great idea of renaming the variables when scaling, we dont need this
                                            # anymore. NICE WORK!
          # pred.labels = c('(Intercept)',
          #                 'Median Household Income',
          #                 '% non-White population',
          #               '% Hispanic population',
          #                 '% Owner Occupied Housing',
          #                 'Housing Age',
          #                 'Terrain Roughness'))

# lets write out the table
tab_model(p_tree_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = '% Tree Canopy Cover',
          file = paste0(getwd(), '/tables/p_tree_canopy_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))  # ".png" changed to ".html",
                                                                         # should open nicely in web browser

# Grass Model
# we had a lot of things we programmed last time, and I am REALLY lazy!
font_sz <- 10 # font size
fig_w   <- 6.5
fig_h   <- 2.5
fig_u   <- 'in'

# FIXME (DHL) to use better response distribution
p_grass_mod <- lmer(Perc_Grass ~Median_Household_Income + # fixed effects
                      Percent_nonWhite +
                      Percent_Hispanic + 
                      Percent_Own_House +
                      Housing_Age + 
                      Terrain_Roughness +
                     (1 | MSA),                               
                   data = df) 

# this is a little dangerous
# we are writting over the 'p_fe' object we made for trees..
p_fe <- plot_model(p_grass_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Grass Cover (%):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# peak at the graph
p_fe

# random effects: p_tree_mod
# we are writting over the 'p_fe' object we made for trees..
p_re <- plot_model(p_grass_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Grass Cover (%):\nrandom effects',# "\n" means "new line"
                   #sort.est = 'sort.all,                                # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# peak at the graph
p_re

# combine the two graphs into one two-pane graph
p_grass_graph <- plot_grid(p_fe, # fixed effects, p_grass_mod,
                          p_re, # random effects, p_grass_mod,
                          labels = c('A', 'B'))
# looks great!
p_grass_graph

# save this out
ggplot2::ggsave(plot = p_grass_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/p_grass_canopy_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)


# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(p_grass_mod, type = 'diag')


# Other Landcover Model (Hillol):
# Other

# FIXME (DHL) to use better response distribution
p_other_mod <- lmer(Perc_Other ~Median_Household_Income + # fixed effects
                      Percent_nonWhite +
                      Percent_Hispanic + 
                      Percent_Own_House +
                      Housing_Age + 
                      Terrain_Roughness +
                      (1 | MSA),                               
                    data = df) 

# this is a little dangerous
# we are writting over the 'p_fe' object we made for grass..
p_fe <- plot_model(p_other_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Other Cover (%):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# peak at the graph
p_fe

# random effects: p_other_mod
# we are writting over the 'p_fe' object we made for grass.
p_re <- plot_model(p_other_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Other Cover (%):\nrandom effects',# "\n" means "new line"
                   #sort.est = 'sort.all,                                # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# peak at the graph
p_re

# combine the two graphs into one two-pane graph
p_other_graph <- plot_grid(p_fe, # fixed effects
                           p_re, # random effects
                           labels = c('A', 'B'))
# looks great!
p_other_graph

# save this out
ggplot2::ggsave(plot = p_other_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/p_other_canopy_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)

# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(p_other_mod, type = 'diag')



# Water Model (Hillol):
# I think you see the patern here, so cutting out some of the intermediate steps

# FIXME (DHL) to use better response distribution
p_water_mod <- lmer(Perc_Water ~Median_Household_Income + # fixed effects
                      Percent_nonWhite +
                      Percent_Hispanic + 
                      Percent_Own_House +
                      Housing_Age + 
                      Terrain_Roughness +
                      (1 | MSA),                               
                    data = df) 
# save fe plot
p_fe <- plot_model(p_water_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Water Cover (%):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_re <- plot_model(p_water_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Water Cover (%):\nrandom effects',# "\n" means "new line"
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
p_water_graph <- plot_grid(p_fe, # fixed effects
                           p_re, # random effects
                           labels = c('A', 'B'))
# save this out
ggplot2::ggsave(plot = p_water_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/p_water_canopy_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)


# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(p_water_mod, type = 'diag')

# Percent cover tables
# lets write out the table
tab_model(p_tree_mod,     # four models
          p_grass_mod,
          p_other_mod,
          p_water_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = c('% Tree Canopy Cover', # for model lables
                        '% Grass Cover',
                        '% Other Cover',
                        '% Water Cover'),
          file = paste0(getwd(), '/tables/perc_cover_vars_',              
                        gsub('[[:punct:]]', '_', Sys.time()), '.html')) # comment out the "file" argument to view in RStudio
                                                                        # should open nicely in web browser when saved out.


# FIXME ALL MODELS ARE PROVISIONAL, the distrubtion is wrong. DHL TO FIX

# TODO: HD - see the "tab_model" above and try to do something like that for the 
# next set of 6 or 8 DVs. I can help. 

#Mean patch area Tree Model (Hillol):
# FIXME (DHL) to use better response distribution ??
MPT_mod <- lmer(MPA_T ~ Median_Household_Income+
                      Percent_nonWhite +
                      Percent_Hispanic + 
                      Percent_Own_House +
                      Housing_Age + 
                      Terrain_Roughness +
                      (1 | MSA),                               
                    data = df) 

p_fe <- plot_model(MPT_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Mean Patch Area (Trees):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_re <- plot_model(MPT_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Mean Patch Area (Trees):\nrandom effects',# "\n" means "new line"
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
MPT_graph <- plot_grid(p_fe, # fixed effects
                           p_re, # random effects
                           labels = c('A', 'B'))
# save this out
ggplot2::ggsave(plot = MPT_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/MPT_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)


# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(MPT_mod, type = 'diag')



# Mean patch area Grass Model (Hillol):
# FIXME (DHL) to use better response distribution ??
MPG_mod <- lmer(MPA_G ~ Median_Household_Income+
                  Percent_nonWhite +
                  Percent_Hispanic + 
                  Percent_Own_House +
                  Housing_Age + 
                  Terrain_Roughness +
                  (1 | MSA),                               
                data = df) 

p_fe <- plot_model(MPG_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Mean Patch Area (Grass):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_re <- plot_model(MPG_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Mean Patch Area (Grass):\nrandom effects',# "\n" means "new line"
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
MPG_graph <- plot_grid(p_fe, # fixed effects
                       p_re, # random effects
                       labels = c('A', 'B'))
# save this out
ggplot2::ggsave(plot = MPG_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/MPG_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)


# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(MPG_mod, type = 'diag')





# FIXME (DHL) to use better response distribution ??
# Number of patch area Tree Model (Hillol):
NPT_mod <- lmer(NP_T ~ Median_Household_Income+
                  Percent_nonWhite +
                  Percent_Hispanic + 
                  Percent_Own_House +
                  Housing_Age + 
                  Terrain_Roughness +
                  (1 | MSA),                               
                data = df) 

p_fe <- plot_model(NPT_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Number of Patches (Tree):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_re <- plot_model(NPT_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Number of Patches (Tree):\nrandom effects',# "\n" means "new line"
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
NPT_graph <- plot_grid(p_fe, # fixed effects
                       p_re, # random effects
                       labels = c('A', 'B'))
# save this out
ggplot2::ggsave(plot = NPT_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/NPT_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)

# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(NPT_mod, type = 'diag')





# FIXME (DHL) to use better response distribution ??
# Number of patches of Grass (Hillol):
NPG_mod <- lmer(NP_G ~ Median_Household_Income+
                  Percent_nonWhite +
                  Percent_Hispanic + 
                  Percent_Own_House +
                  Housing_Age + 
                  Terrain_Roughness +
                  (1 | MSA),                               
                data = df) 

p_fe <- plot_model(NPG_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Number of Patches (Grass):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_re <- plot_model(NPG_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Number of Patches (Grass):\nrandom effects',# "\n" means "new line"
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
NPG_graph <- plot_grid(p_fe, # fixed effects
                       p_re, # random effects
                       labels = c('A', 'B'))
# save this out
ggplot2::ggsave(plot = NPG_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/NPG_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)

# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(NPG_mod, type = 'diag')




## Dexter ended here.
# Tue Nov 12 17:41:10 2019 ------------------------------






#Perimeter-Area ratio of Tree canopy Model (Hillol):

PART_mod <- lmer(PAratio_T ~ Median_Household_Income+
                  Perc_nonWhite +
                  Perc_Hispanic + 
                  Perc_Own_House +
                  Housing_Age + 
                  Terrain_Roughness +
                  (1 | MSA),                               
                data = df) 

#Plot model with forest-plot of estimates
plot_model(PART_mod, title = "Perimeter-Area ratio of Tree canopy model" )

#Plot saved as image (png) file
ggplot2::ggsave(file="Perimeter-Area ratio of Tree canopy model.png",
                width=120, height=150, units = "mm")

#Plot model with random effects
plot_model(PART_mod, type = 're', title = "Random effects of Perimeter-Area ratio of Tree canopy model")
ggplot2::ggsave(file="Random Effects of Perimeter-Area ratio of Tree canopy model.png",
                width=120, height=150, units = "mm")

#Plot model to check model assumptions
plot_model(PART_mod, type = 'diag')

#table
tab_model(PART_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= 'Perimeter-Area ratio of Tree canopy',
          pred.labels = c('(Intercept)',
                          'Median Household Income',
                          '% non-White population',
                          '% Hispanic population',
                          '% Owner Occupied Housing',
                          'Housing Age',
                          'Terrain Roughness'))

#Perimeter-Area ratio of Grass Model (Hillol):

PARG_mod <- lmer(PAratio_G ~ Median_Household_Income+
                   Perc_nonWhite +
                   Perc_Hispanic + 
                   Perc_Own_House +
                   Housing_Age + 
                   Terrain_Roughness +
                   (1 | MSA),                               
                 data = df) 

#Plot model with forest-plot of estimates
plot_model(PARG_mod, title = "Perimeter-Area ratio of Grass model" )

#Plot saved as image (png) file
ggplot2::ggsave(file="Perimeter-Area ratio of Grass model.png",
                width=120, height=150, units = "mm")

#Plot model with random effects
plot_model(PARG_mod, type = 're', title = "Random effects of Perimeter-Area ratio of Grass model")
ggplot2::ggsave(file="Random Effects of Perimeter-Area ratio of Grass model.png",
                width=120, height=150, units = "mm")

#Plot model to check model assumptions
plot_model(PARG_mod, type = 'diag')

#table
tab_model(PARG_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= 'Perimeter-Area ratio of Grass',
          pred.labels = c('(Intercept)',
                          'Median Household Income',
                          '% non-White population',
                          '% Hispanic population',
                          '% Owner Occupied Housing',
                          'Housing Age',
                          'Terrain Roughness'))                   

#Coefficient of variation of Tree patch Model (Hillol):

CVT_mod <- lmer(CV_T ~ Median_Household_Income+
                   Perc_nonWhite +
                   Perc_Hispanic + 
                   Perc_Own_House +
                   Housing_Age + 
                   Terrain_Roughness +
                   (1 | MSA),                               
                 data = df) 

#Plot model with forest-plot of estimates
plot_model(CVT_mod, title = "Coefficient of variation of Tree patch model" )

#Plot saved as image (png) file
ggplot2::ggsave(file="Coefficient of variation of Tree patch model.png",
                width=120, height=150, units = "mm")

#Plot model with random effects
plot_model(CVT_mod, type = 're', title = "Random effects of Coefficient of variation of Tree patch model")
ggplot2::ggsave(file="Random Effects of Coefficient of variation of Tree patch model.png",
                width=120, height=150, units = "mm")

#Plot model to check model assumptions
plot_model(CVT_mod, type = 'diag')

#table
tab_model(CVT_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= 'Coefficient of variation of Tree patch',
          pred.labels = c('(Intercept)',
                          'Median Household Income',
                          '% non-White population',
                          '% Hispanic population',
                          '% Owner Occupied Housing',
                          'Housing Age',
                          'Terrain Roughness'))

#Coefficient of variation of Grass patch  Model (Hillol):

CVG_mod <- lmer(CV_G ~ Median_Household_Income+
                   Perc_nonWhite +
                   Perc_Hispanic + 
                   Perc_Own_House +
                   Housing_Age + 
                   Terrain_Roughness +
                   (1 | MSA),                               
                 data = df) 

#Plot model with forest-plot of estimates
plot_model(CVG_mod, title = "Coefficient of variation of Grass patch model" )

#Plot saved as image (png) file
ggplot2::ggsave(file="Coefficient of variation of Grass patch model.png",
                width=120, height=150, units = "mm")

#Plot model with random effects
plot_model(CVG_mod, type = 're', title = "Random effects of Coefficient of variation of Grass patch model")
ggplot2::ggsave(file="Random Effects of Coefficient of variation of Grass patch model.png",
                width=120, height=150, units = "mm")

#Plot model to check model assumptions
plot_model(CVG_mod, type = 'diag')

#table
tab_model(CVG_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= 'Coefficient of variation of Grass patch',
          pred.labels = c('(Intercept)',
                          'Median Household Income',
                          '% non-White population',
                          '% Hispanic population',
                          '% Owner Occupied Housing',
                          'Housing Age',
                          'Terrain Roughness')) 
#end




