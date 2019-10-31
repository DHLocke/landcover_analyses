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
sf <- read_sf('/Users/dlocke/temp_MSB4RoyChowdhuryDobler/CBGs/CBG_ALL_v20161014.shp') # this "verbose = TRUE" argument isn't really needed
      
# These data were in that folder so I could share easily with Carlos
# but I'm keeping the R project locally here now:
getwd()               # "/Users/dlocke/msb/LandCoverPaper/landcover_analyses"

# take a look at the data
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


# continuous analyses
# but first lets scale the predictors, grand-mean scaling
df$INC_MED_HS_s <- scale(df$INC_MED_HS, center = T)
df$P_nonWhite_s <- scale(I(100 - df$P_White), center = T)
df$P_Hisp_s     <- scale(df$P_Hisp, center = T)
df$P_Own_s      <- scale(df$P_Own, center = T)
df$HOUS_AGE_s   <- scale(df$HOUS_AGE, center = T)
df$SDE_STD_s    <- scale(df$SDE_STD, center = T)

p_tree_mod <- lmer(Perc_Tree ~ INC_MED_HS_s + # fixed effects
                     P_nonWhite_s +
                     P_Hisp_s + 
                     P_Own_s +
                     HOUS_AGE_s + 
                     SDE_STD_s +
                    (1 | MSA),                               # random effects
                   data = df)

plot_model(p_tree_mod)                      # coefficients, defaults to "est"
plot_model(p_tree_mod) + theme_bw()         # better display?
plot_model(p_tree_mod) + theme_bw(20)       # BIGGER lables, see 

plot_model(p_tree_mod, type = 're')         # random effects
plot_model(p_tree_mod, type = 'std')        # standardized effects, in units of standard deviations
plot_model(p_tree_mod, type = 'pred')        # standardized effects, in units of standard deviations

plot_model(p_tree_mod, type = 'diag')        # standardized effects, in units of standard deviations



# Fri Oct 25 11:10:50 2019 ------------------------------

