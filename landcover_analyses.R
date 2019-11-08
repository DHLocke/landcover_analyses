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
   

# read in the data
# you will have to change the file path to match the location of the data
# on your computer
#sf <- read_sf('/Users/dlocke/temp_MSB4RoyChowdhuryDobler/CBGs/CBG_ALL_v20161014.shp') 
sf <- read_sf('D:/Macrobio/blockgroup_landcover/CBG_ALL_v20161014.shp')

# These data were in that folder so I could share easily with Carlos
# but I'm keeping the R project locally here now:
getwd()               # "/Users/dlocke/msb/LandCoverPaper/landcover_analyses"

# take a look at the data of cbg
sf
names(sf)

# we don't actually need the polygons here, so lets pull the data frame out
# that's like working with the attribute table in ArcGIS, but not the spatial data
df <- dplyr::select(as.data.frame(sf), -geometry)

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


# categorical analyses
<<<<<<< HEAD


# continuous analyses but first lets scale the predictors, grand-mean scaling
df$Median_Household_Income <- scale(df$INC_MED_HS, center = T)
df$Perc_nonWhite           <- scale(I(100 - df$P_White), center = T)
df$Perc_Hispanic           <- scale(df$P_Hisp, center = T)
df$Perc_Own_House          <- scale(df$P_Own, center = T)
df$Housing_Age             <- scale(df$HOUS_AGE, center = T)
df$Terrain_Roughness       <- scale(df$SDE_STD, center = T)

p_tree_mod <- lmer(Perc_Tree ~ Median_Household_Income + # fixed effects
                     Perc_nonWhite +
                     Perc_Hispanic + 
                     Perc_Own_House +
                     Housing_Age + 
                     Terrain_Roughness +
                    (1 | MSA),                               # random effects
                   data = df)

plot_model(p_tree_mod)                      # coefficients, defaults to "est"
plot_model(p_tree_mod) + theme_bw()         # better display?
plot_model(p_tree_mod) + theme_bw(20)       # BIGGER lables, see 

plot_model(p_tree_mod, type = 're')         # random effects
plot_model(p_tree_mod, type = 'std')        # standardized effects, in units of standard deviations
plot_model(p_tree_mod, type = 'pred')       # standardized effects, in units of standard deviations

plot_model(p_tree_mod, type = 'diag')       # standardized effects, in units of standard deviations

#Plot model with forest-plot of estimates
plot_model(p_grass_mod, title = "Continuous Tree model" )

#Plot saved as image (png) file
ggplot2::ggsave(file="Continuous Tree model.png",
                width=120, height=150, units = "mm")

#Plot model with random effects
plot_model(p_tree_mod, type = 're', title = "Random effects of Tree model")
ggplot2::ggsave(file="Random Effects of Tree model.png",
                width=120, height=150, units = "mm")

#Plot model to check model assumptions
plot_model(p_grass_mod, type = 'diag')

# TODO HD: copy "p_tree_mod" but for the other dependent variables like
# "Perc_Grass"

# TODO HD: add other models to this tabular display
tab_model(p_tree_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= '% Tree Canopy Cover',
          pred.labels = c('(Intercept)',
                          'Median Household Income',
                          '% non-White population',
                          '% Hispanic population',
                          '% Owner Occupied Housing',
                          'Housing Age',
                          'Terrain Roughness'))


#Grass Model (Hillol):

p_grass_mod <- lmer(Perc_Grass ~ Median_Household_Income+
                     Perc_nonWhite +
                     Perc_Hispanic + 
                     Perc_Own_House +
                     Housing_Age + 
                     Terrain_Roughness +
                     (1 | MSA),                               
                   data = df) 

#Plot model with forest-plot of estimates
plot_model(p_grass_mod, title = "Continuous Grass model" )

#Plot saved as image (png) file
ggplot2::ggsave(file="Continuous Grass model.png",
                width=120, height=150, units = "mm")

#Plot model with random effects
plot_model(p_grass_mod, type = 're', title = "Random effects of Grass model")
ggplot2::ggsave(file="Random Effects of Grass model.png",
                width=120, height=150, units = "mm")

#Plot model to check model assumptions
plot_model(p_grass_mod, type = 'diag')

#table
tab_model(p_grass_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= '% Grass Cover',
          pred.labels = c('(Intercept)',
                          'Median Household Income',
                          '% non-White population',
                          '% Hispanic population',
                          '% Owner Occupied Housing',
                          'Housing Age',
                          'Terrain Roughness'))

#Other Landcover Model (Hillol):

p_other_mod <- lmer(Perc_Other ~ Median_Household_Income+
                      Perc_nonWhite +
                      Perc_Hispanic + 
                      Perc_Own_House +
                      Housing_Age + 
                      Terrain_Roughness +
                      (1 | MSA),                               
                    data = df) 

#Plot model with forest-plot of estimates
plot_model(p_other_mod, title = "Continuous Other Lancover model" )

#Plot saved as image (png) file
ggplot2::ggsave(file="Continuous Other Landcover model.png",
                width=120, height=150, units = "mm")

#Plot model with random effects
plot_model(p_other_mod, type = 're', title = "Random effects of Othe Landcover model")
ggplot2::ggsave(file="Random Effects of Other Landcover model.png",
                width=120, height=150, units = "mm")

#Plot model to check model assumptions
plot_model(p_other_mod, type = 'diag')

#table
tab_model(p_other_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= '% Other Landcover',
          pred.labels = c('(Intercept)',
                          'Median Household Income',
                          '% non-White population',
                          '% Hispanic population',
                          '% Owner Occupied Housing',
                          'Housing Age',
                          'Terrain Roughness'))

#Water Model (Hillol):

p_other_mod <- lmer(Perc_Water ~ Median_Household_Income+
                      Perc_nonWhite +
                      Perc_Hispanic + 
                      Perc_Own_House +
                      Housing_Age + 
                      Terrain_Roughness +
                      (1 | MSA),                               
                    data = df) 

#Plot model with forest-plot of estimates
plot_model(p_other_mod, title = "Continuous Water model" )

#Plot saved as image (png) file
ggplot2::ggsave(file="Continuous Water model.png",
                width=120, height=150, units = "mm")

#Plot model with random effects
plot_model(p_other_mod, type = 're', title = "Random effects of Water model")
ggplot2::ggsave(file="Random Effects of Water model.png",
                width=120, height=150, units = "mm")

#Plot model to check model assumptions
plot_model(p_other_mod, type = 'diag')

#table
tab_model(p_other_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= '% Water areas',
          pred.labels = c('(Intercept)',
                          'Median Household Income',
                          '% non-White population',
                          '% Hispanic population',
                          '% Owner Occupied Housing',
                          'Housing Age',
                          'Terrain Roughness'))

#Mean patch area Tree Model (Hillol):

MPT_mod <- lmer(MPA_T ~ Median_Household_Income+
                      Perc_nonWhite +
                      Perc_Hispanic + 
                      Perc_Own_House +
                      Housing_Age + 
                      Terrain_Roughness +
                      (1 | MSA),                               
                    data = df) 

#Plot model with forest-plot of estimates
plot_model(MPT_mod, title = "Mean Patch Area Tree model" )

#Plot saved as image (png) file
ggplot2::ggsave(file="Mean Patch Area Tree model.png",
                width=120, height=150, units = "mm")

#Plot model with random effects
plot_model(MPT_mod, type = 're', title = "Random effects of Mean Patch Area Tree model")
ggplot2::ggsave(file="Random Effects of Mean Patch Area Tree model.png",
                width=120, height=150, units = "mm")

#Plot model to check model assumptions
plot_model(MPT_mod, type = 'diag')

#table
tab_model(MPT_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= 'Mean Patch areas-Tree',
          pred.labels = c('(Intercept)',
                          'Median Household Income',
                          '% non-White population',
                          '% Hispanic population',
                          '% Owner Occupied Housing',
                          'Housing Age',
                          'Terrain Roughness'))

#Mean patch area Grass Model (Hillol):

MPG_mod <- lmer(MPA_G ~ Median_Household_Income+
                  Perc_nonWhite +
                  Perc_Hispanic + 
                  Perc_Own_House +
                  Housing_Age + 
                  Terrain_Roughness +
                  (1 | MSA),                               
                data = df) 

#Plot model with forest-plot of estimates
plot_model(MPG_mod, title = "Mean Patch Area Grass model" )

#Plot saved as image (png) file
ggplot2::ggsave(file="Mean Patch Area Grass model.png",
                width=120, height=150, units = "mm")

#Plot model with random effects
plot_model(MPG_mod, type = 're', title = "Random effects of Mean Patch Area Grass model")
ggplot2::ggsave(file="Random Effects of Mean Patch Area Grass model.png",
                width=120, height=150, units = "mm")

#Plot model to check model assumptions
plot_model(MPG_mod, type = 'diag')

#table
tab_model(MPG_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= 'Mean Patch areas-Grass',
          pred.labels = c('(Intercept)',
                          'Median Household Income',
                          '% non-White population',
                          '% Hispanic population',
                          '% Owner Occupied Housing',
                          'Housing Age',
                          'Terrain Roughness'))
#end



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

#end




