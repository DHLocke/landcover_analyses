# Script by Dexter H Locke and Hillol Dutta
# for analyzing land cover composition, as part of the MacroSystems Biology project

# some files of interest here:
# https://www.dropbox.com/home/MacroBio%20AAG%20gallery

# started on
# 0 setup: load libraries and read in data----
# Mon Oct 21 19:58:30 2019
# many updates documented via git

# clear the workspace
#rm(list=ls())           # depending on what other project you have loaded,
                        # you may want to comment this out. This command
                        # removes everything from memory

# NOTE HL: with object "packages" we can loop through the "citation()" function
# to cite all of the packages in one go. Search for "CITE THE PACKGES"

# packages we'll be using
packages <- c('tidyverse',   # this is actually a collection of packages
              'sf',          # used for reading shapefiles
                             # lots of other great spatial functions, too, but we wont use them
              'sjPlot',      # nice graphs and tables supporting many models
              'lme4',        # fits multi-level models
              'ggpubr',      # mixes base stats functions with ggplot graphics, its great!
              'RColorBrewer',# good for colors
              'cowplot',     # for multi-paned graphs NOTE THAT THIS MASKS ggplot2::ggsave()!!
              'janitor',     # cleans things up
              #'multcompView') # supports significance letters for multiple comparisons, helpful formattings
              'psych',         # useful data summaries
              'see',           # model diagnostics
              'performance',   # model diagnostics
              'tidylog')       # makes dplyr and tidyr very explicity


# check for all of the libraries
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}


# load some useful libraries
library(tidyverse)      # this is actually a collection of packages
library(sf)             # used for reading shapefiles
                        # lots of other great spatial functions, too, but we wont use them
library(sjPlot)         # nice graphs and tables supporting many models
library(lme4)           # fits multi-level models
library(ggpubr)         # mixes base stats functions with ggplot graphics, its great!
library(RColorBrewer)   # good for colors
library(cowplot)        # for multi-paned graphs NOTE THAT THIS MASKS ggplot2::ggsave()!!
library(janitor)        # cleans things up
#library(multcompView)    # supports significance letters for multiple comparisons, helpful formattings
library(psych)          # useful data summaries
library(see)            # model diagnostics
library(performance)    # model diagnostics
library(tidylog)
library(glmmTMB)

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
names(sf) # double check the names of the file

# we don't actually need the polygons here, so lets pull the data frame out
# that's like working with the attribute table in ArcGIS, but not the spatial data
df <- dplyr::select(as.data.frame(sf), -geometry)

# here is another way to do what is shown above, just an FYI
# df <- st_drop_geometry(sf)

head(df)
names(df)

## 1 double / sanity checks----
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
# prop.table(table(df$MSA, useNA = 'ifany')) # distributions of block groups, as a %
# 
# # or more refined
# prop.table(table(df$MSA, useNA = 'ifany'))*100 # actually in percent form

# or even more refined! 
round(prop.table(table(df$MSA, useNA = 'ifany'))*100, 2)# rounding 2 places, much prettier

# try
help(table) # to see what the "useNA" argument is for



# 2 descriptive statistics----
# counts per city
# inspired by https://github.com/sfirke/janitor
# df %>% tabyl(MSA) %>% 
#   adorn_totals('row') %>% 
#   adorn_pct_formatting() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/MSA_count_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)


## 2.1 urbanicity-----
table(df$Urbanicity, df$PNE_CODE, useNA = 'ifany')
table(df$Urbanicity) # 1 is urban, 2 is suburban, 3 is exurban

# below is commented out because the tables have been successfully written out
# df %>%
#   mutate(Urbanicity_fct = 
#            recode_factor(Urbanicity,
#                          `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban')) %>% # do we prefer 'rural'))
#   tabyl(Urbanicity_fct) %>% 
#   adorn_totals('row') %>% 
#   adorn_pct_formatting() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/Urbanicity_count_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)

# # city by urbanicity (sum by col)
# df %>%
#   mutate(Urbanicity_fct = 
#            recode_factor(Urbanicity,
#                          `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban')) %>% # do we prefer 'rural'))
#   tabyl(MSA, Urbanicity_fct) %>% 
#   adorn_totals('row') %>% 
#   adorn_percentages('col') %>%
#   adorn_pct_formatting() %>%
#   adorn_ns() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/MSA_Urbanicity_count_sum_by_col_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)

# city by urbanicity (sum by row)
# df %>%
#   mutate(Urbanicity_fct = 
#            recode_factor(Urbanicity,
#                          `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban')) %>% # do we prefer 'rural'))
#   tabyl(MSA, Urbanicity_fct) %>% 
#   adorn_totals('col') %>% 
#   adorn_percentages('row') %>%
#   adorn_pct_formatting() %>%
#   adorn_ns() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/MSA_Urbanicity_count_sum_by_row_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)


## 2.2 Affluence----
table(df$Affluence, df$PNE_CODE, useNA = 'ifany')
table(df$Affluence) # 1 high, 2 is medium, 3 is low

# below is commented out because the tables have been successfully written out
# df %>%
#   mutate(Affluence_fct = 
#            recode_factor(Affluence,
#                          `1` = 'High', `2` = 'Middle', `3` = 'Low')) %>%
#   tabyl(Affluence_fct) %>% 
#   adorn_totals('row') %>% 
#   adorn_pct_formatting() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/Affluence_count_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)

# # city by Affluence (sum by col)
# df %>%
#   mutate(Affluence_fct = 
#            recode_factor(Affluence,
#                          `1` = 'High', `2` = 'Middle', `3` = 'Low')) %>%
#   tabyl(MSA, Affluence_fct) %>% 
#   adorn_totals('row') %>% 
#   adorn_percentages('col') %>%
#   adorn_pct_formatting() %>%
#   adorn_ns() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/MSA_Affluence_count_sum_by_col_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)
# 
# # city by urbanicty (sum by row)
# df %>%
#   mutate(Affluence_fct = 
#            recode_factor(Affluence,
#                          `1` = 'High', `2` = 'Middle', `3` = 'Low')) %>%
#   tabyl(MSA, Affluence_fct) %>% 
#   adorn_totals('col') %>% 
#   adorn_percentages('row') %>%
#   adorn_pct_formatting() %>%
#   adorn_ns() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/MSA_Affluence_count_sum_by_row',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)
# 
# 
# # 2.3 MSA Urbanicity Affluence----
# below is commented out because the tables have been successfully written out
# df %>%
#   mutate(Urbanicity_fct = 
#            recode_factor(Urbanicity,
#                          `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban'),
#          Affluence_fct = 
#            recode_factor(Affluence,
#                          `1` = 'High', `2` = 'Middle', `3` = 'Low')) %>%
#   tabyl(MSA, Urbanicity_fct, Affluence_fct) %>% 
#   adorn_totals('col') %>% 
#   adorn_percentages('row') %>%
#   adorn_pct_formatting() %>%
#   adorn_ns() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/MSA_Urbanicity_Affluence_count_sum_by_row',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)
# 
# 
# df %>% select(Perc_Tree, Perc_Grass, Perc_Other, Perc_Water,
#               NP_T, MPA_T, CV_T, PAratio_T, NP_G, MPA_G, CV_G, PAratio_G) %>% 
#   describe(fast = TRUE) %>% 
#   mutate(variable = row.names(.),
#          `variable description` = c('tree canopy cover (%)',
#                                     'grass cover (%)',
#                                     'other area (%)',
#                                     'water area (%)',
#                                     'Number of Patches Tree (distinct tree patches, group of pixels)',
#                                     'Mean Patch Area Tree (the average size of tree patches)',
#                                     'TODO find out Coefficient of Variation for tree patches',
#                                     'Parimieter Area ratio for tree canopy',
#                                     'Number of Patches Grass',
#                                     'Mean Patch Area Grass',
#                                     'TODO find out Coefficient of Variation for tree patches',
#                                     'Parimieter Area ratio for tree canopy')) %>% 
#   #select(variable, min, max, mean, sd, se, range, 'variable description') %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/descriptive_stats_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)

# possibly an improved approach
# https://rpkgs.datanovia.com/ggpubr/reference/desc_statby.html
# group by MSA
# df %>% select(Perc_Tree, Perc_Grass, Perc_Other, Perc_Water,
#               NP_T, MPA_T, CV_T, PAratio_T, NP_G, MPA_G, CV_G, PAratio_G,
#               MSA) %>% 
#   group_by(MSA) %>% 
#   summarise_all(list(min = min, max = max, mean = mean, sd = sd)) %>%
#   ungroup() %>%  
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/descriptive_stats_MSA_wide_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)
# 
# df %>% select(Perc_Tree, Perc_Grass, Perc_Other, Perc_Water,
#               NP_T, MPA_T, CV_T, PAratio_T, NP_G, MPA_G, CV_G, PAratio_G,
#               MSA, Urbanicity) %>% 
#   group_by(MSA, Urbanicity) %>% 
#   summarise_all(list(min = min, max = max, mean = mean, sd = sd)) %>%
#   ungroup() %>%  
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/descriptive_stats_MSA_Urban_wide_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)
# 
# # group by MSA TALL
# df %>% select(Perc_Tree, Perc_Grass, Perc_Other, Perc_Water,
#               NP_T, MPA_T, CV_T, PAratio_T, NP_G, MPA_G, CV_G, PAratio_G,
#               MSA) %>% 
#   group_by(MSA) %>% 
#   summarise_all(list(min = min, max = max, mean = mean, sd = sd)) %>%
#   ungroup() %>%
#   t() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/descriptive_stats_MSA_tall_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = TRUE)
# 
# # group by MSA TALL
# df %>% select(Tree, Grass, Perc_Tree, Perc_Grass, 
#               NP_T, MPA_T, CV_T, PAratio_T, NP_G, MPA_G, CV_G, PAratio_G,
#               MSA) %>% 
#   group_by(MSA) %>% 
#   summarise_all(list(Min = min, Max = max, Mean = mean, SD = sd, Median=median)) %>%
#   ungroup() %>%
#   t() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/descriptive_stats_MSA_tall_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = TRUE)

### 2.4 THE MAIN DESCRIPTIVE TABLES-----
# 2.4.1 group by MSA, Urbanicity----
df %>% mutate(Urbanicity_fct = 
                recode_factor(Urbanicity,
                              `1` = 'Urban',
                              `2` = 'Suburban',
                              `3` = 'Exurban', .ordered = TRUE)) %>% 
  tabyl(MSA, Urbanicity_fct) %>% 
  write.csv(., file = paste0(getwd(), '/tables/descriptives/counts_per_MSA_Urb_Aff',
                             gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
            row.names = TRUE)

df %>% mutate(Urbanicity_fct = 
                recode_factor(Urbanicity,
                              `1` = 'Urban',
                              `2` = 'Suburban',
                              `3` = 'Exurban', .ordered = TRUE)) %>%
  rename(Median_Household_Income = INC_MED_HS,
         P_White = P_White,
         Percent_Hispanic = P_Hisp,
         Percent_Own_House = P_Own,
         Housing_Age = HOUS_AGE,
         Terrain_Roughness = SDE_STD) %>% 
  select(Tree, Grass, Perc_Tree, Perc_Grass,                        # currently dependent variables
         NP_T, MPA_T, CV_T, PAratio_T, NP_G, MPA_G, CV_G, PAratio_G,
         Median_Household_Income, P_White, Percent_Hispanic, # independent variables 
         Percent_Own_House, Housing_Age, Terrain_Roughness,           # independent variables 
         MSA, Urbanicity_fct) %>%                      # grouping variables
  group_by(MSA, Urbanicity_fct) %>%
  summarise_all(list(Min = min, Max = max, Mean = mean, Median = median, SD = sd, IQR = IQR)) %>%
  ungroup() %>%
  select(MSA, Urbanicity_fct,
         Tree_Min, Tree_Max, Tree_Mean, Tree_Median, Tree_SD, Tree_IQR,
         Grass_Min, Grass_Max, Grass_Mean, Grass_Median, Grass_SD, Grass_IQR,
         Perc_Tree_Min, Perc_Tree_Max, Perc_Tree_Mean, Perc_Tree_Median, Perc_Tree_SD, Perc_Tree_IQR,
         Perc_Grass_Min, Perc_Grass_Max, Perc_Grass_Mean, Perc_Grass_Median, Perc_Grass_SD, Perc_Grass_IQR,
         NP_T_Min, NP_T_Max, NP_T_Mean, NP_T_Median, NP_T_SD, NP_T_IQR,
         MPA_T_Min, MPA_T_Max, MPA_T_Mean, MPA_T_Median, MPA_T_SD, MPA_T_IQR,
         CV_T_Min, CV_T_Max, CV_T_Mean, CV_T_Median, CV_T_SD, CV_T_IQR,
         PAratio_T_Min, PAratio_T_Max, PAratio_T_Mean, PAratio_T_Median, PAratio_T_SD, PAratio_T_IQR,
         NP_G_Min, NP_G_Max, NP_G_Mean, NP_G_Median, NP_G_SD, NP_G_IQR,
         MPA_G_Min, MPA_G_Max, MPA_G_Mean, MPA_G_Median, MPA_G_SD, MPA_G_IQR,
         CV_G_Min, CV_G_Max, CV_G_Mean, CV_G_Median, CV_G_SD, CV_G_IQR,
         PAratio_G_Min, PAratio_G_Max, PAratio_G_Mean, PAratio_G_Median, PAratio_G_SD, PAratio_G_IQR,
         Median_Household_Income_Min, Median_Household_Income_Max, Median_Household_Income_Mean, Median_Household_Income_Median, Median_Household_Income_SD, Median_Household_Income_IQR,
         P_White_Min, P_White_Max, P_White_Mean, P_White_Median, P_White_SD, P_White_IQR,
         Percent_Hispanic_Min, Percent_Hispanic_Max, Percent_Hispanic_Mean, Percent_Hispanic_Median, Percent_Hispanic_SD, Percent_Hispanic_IQR,
         Percent_Own_House_Min, Percent_Own_House_Max, Percent_Own_House_Mean, Percent_Own_House_Median, Percent_Own_House_SD, Percent_Own_House_IQR,
         Housing_Age_Min, Housing_Age_Max, Housing_Age_Mean, Housing_Age_Median, Housing_Age_SD, Housing_Age_IQR,
         Terrain_Roughness_Min, Terrain_Roughness_Max, Terrain_Roughness_Mean, Terrain_Roughness_Median, Terrain_Roughness_SD, Terrain_Roughness_IQR) %>% 
  t() %>% # this transposes
  write.csv(., file = paste0(getwd(), '/tables/descriptives/descriptive_stats_MSA_Urb_tall_',
                             gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
            row.names = TRUE)

# 2.4.2 CITY AND AFFLUENCE----
df %>% mutate(Affluence_fct =
                recode_factor(Affluence,
                              `1` = 'High',
                              `2` = 'Middle',
                              `3` = 'Low', .ordered = TRUE)) %>% 
  tabyl(MSA, Affluence_fct) %>% 
  write.csv(., file = paste0(getwd(), '/tables/descriptives/counts_per_MSA_Aff_',
                             gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
            row.names = TRUE)

# THIS CONTAINS THE NEEDED STATS, TABLE ABOVE EXPLAINS SEEMING ERRONEOUS VALUES - BUT THEY MAKE
# SENSE IN LIGHT OF THE DISTRIBUTION OF THE BLOCK GROUPS PER STRATA.
df %>% mutate(Affluence_fct =
                recode_factor(Affluence,
                              `1` = 'High',
                              `2` = 'Middle',
                              `3` = 'Low', .ordered = TRUE)) %>%
  rename(Median_Household_Income = INC_MED_HS,
         P_White = P_White,
         Percent_Hispanic = P_Hisp,
         Percent_Own_House = P_Own,
         Housing_Age = HOUS_AGE,
         Terrain_Roughness = SDE_STD) %>% 
  select(Tree, Grass, Perc_Tree, Perc_Grass,                        # currently dependent variables
         NP_T, MPA_T, CV_T, PAratio_T, NP_G, MPA_G, CV_G, PAratio_G,
         Median_Household_Income, P_White, Percent_Hispanic, # independent variables 
         Percent_Own_House, Housing_Age, Terrain_Roughness,           # independent variables 
         MSA, Affluence_fct) %>%                      # grouping variables
  group_by(MSA, Affluence_fct) %>%
  summarise_all(list(Min = min, Max = max, Mean = mean, Median = median, SD = sd, IQR = IQR)) %>%
  ungroup() %>%
  select(MSA, Affluence_fct,
         Tree_Min, Tree_Max, Tree_Mean, Tree_Median, Tree_SD, Tree_IQR,
         Grass_Min, Grass_Max, Grass_Mean, Grass_Median, Grass_SD, Grass_IQR,
         Perc_Tree_Min, Perc_Tree_Max, Perc_Tree_Mean, Perc_Tree_Median, Perc_Tree_SD, Perc_Tree_IQR,
         Perc_Grass_Min, Perc_Grass_Max, Perc_Grass_Mean, Perc_Grass_Median, Perc_Grass_SD, Perc_Grass_IQR,
         NP_T_Min, NP_T_Max, NP_T_Mean, NP_T_Median, NP_T_SD, NP_T_IQR,
         MPA_T_Min, MPA_T_Max, MPA_T_Mean, MPA_T_Median, MPA_T_SD, MPA_T_IQR,
         CV_T_Min, CV_T_Max, CV_T_Mean, CV_T_Median, CV_T_SD, CV_T_IQR,
         PAratio_T_Min, PAratio_T_Max, PAratio_T_Mean, PAratio_T_Median, PAratio_T_SD, PAratio_T_IQR,
         NP_G_Min, NP_G_Max, NP_G_Mean, NP_G_Median, NP_G_SD, NP_G_IQR,
         MPA_G_Min, MPA_G_Max, MPA_G_Mean, MPA_G_Median, MPA_G_SD, MPA_G_IQR,
         CV_G_Min, CV_G_Max, CV_G_Mean, CV_G_Median, CV_G_SD, CV_G_IQR,
         PAratio_G_Min, PAratio_G_Max, PAratio_G_Mean, PAratio_G_Median, PAratio_G_SD, PAratio_G_IQR,
         Median_Household_Income_Min, Median_Household_Income_Max, Median_Household_Income_Mean, Median_Household_Income_Median, Median_Household_Income_SD, Median_Household_Income_IQR,
         P_White_Min, P_White_Max, P_White_Mean, P_White_Median, P_White_SD, P_White_IQR,
         Percent_Hispanic_Min, Percent_Hispanic_Max, Percent_Hispanic_Mean, Percent_Hispanic_Median, Percent_Hispanic_SD, Percent_Hispanic_IQR,
         Percent_Own_House_Min, Percent_Own_House_Max, Percent_Own_House_Mean, Percent_Own_House_Median, Percent_Own_House_SD, Percent_Own_House_IQR,
         Housing_Age_Min, Housing_Age_Max, Housing_Age_Mean, Housing_Age_Median, Housing_Age_SD, Housing_Age_IQR,
         Terrain_Roughness_Min, Terrain_Roughness_Max, Terrain_Roughness_Mean, Terrain_Roughness_Median, Terrain_Roughness_SD, Terrain_Roughness_IQR) %>% 
  t() %>% # this transposes
  write.csv(., file = paste0(getwd(), '/tables/descriptives/descriptive_stats_MSA_Aff_tall_',
                             gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
            row.names = TRUE)



# 2.4.3 all THREE TOGETHER----
# group by MSA, Urbanicity and Affluence
df %>% mutate(Urbanicity_fct = 
                recode_factor(Urbanicity,
                              `1` = 'Urban',
                              `2` = 'Suburban',
                              `3` = 'Exurban', .ordered = TRUE),
              Affluence_fct =
                recode_factor(Affluence,
                              `1` = 'High',
                              `2` = 'Middle',
                              `3` = 'Low', .ordered = TRUE)) %>% 
  tabyl(MSA, Urbanicity_fct, Affluence_fct) %>% 
  write.csv(., file = paste0(getwd(), '/tables/descriptives/counts_per_MSA_Urb_Aff_',
                             gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
            row.names = TRUE)

# THIS CONTAINS THE NEEDED STATS, TABLE ABOVE EXPLAINS SEEMING ERRONEOUS VALUES - BUT THEY MAKE
# SENSE IN LIGHT OF THE DISTRIBUTION OF THE BLOCK GROUPS PER STRATA.
df %>% mutate(Urbanicity_fct = 
                recode_factor(Urbanicity,
                              `1` = 'Urban',
                              `2` = 'Suburban',
                              `3` = 'Exurban', .ordered = TRUE),
              Affluence_fct =
                recode_factor(Affluence,
                              `1` = 'High',
                              `2` = 'Middle',
                              `3` = 'Low', .ordered = TRUE)) %>%
  rename(Median_Household_Income = INC_MED_HS,
         P_White = P_White,
         Percent_Hispanic = P_Hisp,
         Percent_Own_House = P_Own,
         Housing_Age = HOUS_AGE,
         Terrain_Roughness = SDE_STD) %>% 
  select(Tree, Grass, Perc_Tree, Perc_Grass,                        # currently dependent variables
         NP_T, MPA_T, CV_T, PAratio_T, NP_G, MPA_G, CV_G, PAratio_G,
         Median_Household_Income, P_White, Percent_Hispanic, # independent variables 
         Percent_Own_House, Housing_Age, Terrain_Roughness,           # independent variables 
         MSA, Urbanicity_fct, Affluence_fct) %>%                      # grouping variables
  group_by(MSA, Urbanicity_fct, Affluence_fct) %>%
  summarise_all(list(Min = min, Max = max, Mean = mean, Median = median, SD = sd, IQR = IQR)) %>%
  ungroup() %>%
  select(MSA, Urbanicity_fct, Affluence_fct,
         Tree_Min, Tree_Max, Tree_Mean, Tree_Median, Tree_SD, Tree_IQR,
         Grass_Min, Grass_Max, Grass_Mean, Grass_Median, Grass_SD, Grass_IQR,
         Perc_Tree_Min, Perc_Tree_Max, Perc_Tree_Mean, Perc_Tree_Median, Perc_Tree_SD, Perc_Tree_IQR,
         Perc_Grass_Min, Perc_Grass_Max, Perc_Grass_Mean, Perc_Grass_Median, Perc_Grass_SD, Perc_Grass_IQR,
         NP_T_Min, NP_T_Max, NP_T_Mean, NP_T_Median, NP_T_SD, NP_T_IQR,
         MPA_T_Min, MPA_T_Max, MPA_T_Mean, MPA_T_Median, MPA_T_SD, MPA_T_IQR,
         CV_T_Min, CV_T_Max, CV_T_Mean, CV_T_Median, CV_T_SD, CV_T_IQR,
         PAratio_T_Min, PAratio_T_Max, PAratio_T_Mean, PAratio_T_Median, PAratio_T_SD, PAratio_T_IQR,
         NP_G_Min, NP_G_Max, NP_G_Mean, NP_G_Median, NP_G_SD, NP_G_IQR,
         MPA_G_Min, MPA_G_Max, MPA_G_Mean, MPA_G_Median, MPA_G_SD, MPA_G_IQR,
         CV_G_Min, CV_G_Max, CV_G_Mean, CV_G_Median, CV_G_SD, CV_G_IQR,
         PAratio_G_Min, PAratio_G_Max, PAratio_G_Mean, PAratio_G_Median, PAratio_G_SD, PAratio_G_IQR,
         Median_Household_Income_Min, Median_Household_Income_Max, Median_Household_Income_Mean, Median_Household_Income_Median, Median_Household_Income_SD, Median_Household_Income_IQR,
         P_White_Min, P_White_Max, P_White_Mean, P_White_Median, P_White_SD, P_White_IQR,
         Percent_Hispanic_Min, Percent_Hispanic_Max, Percent_Hispanic_Mean, Percent_Hispanic_Median, Percent_Hispanic_SD, Percent_Hispanic_IQR,
         Percent_Own_House_Min, Percent_Own_House_Max, Percent_Own_House_Mean, Percent_Own_House_Median, Percent_Own_House_SD, Percent_Own_House_IQR,
         Housing_Age_Min, Housing_Age_Max, Housing_Age_Mean, Housing_Age_Median, Housing_Age_SD, Housing_Age_IQR,
         Terrain_Roughness_Min, Terrain_Roughness_Max, Terrain_Roughness_Mean, Terrain_Roughness_Median, Terrain_Roughness_SD, Terrain_Roughness_IQR) %>% 
  t() %>% # this transposes
  write.csv(., file = paste0(getwd(), '/tables/descriptives/descriptive_stats_MSA_Urb_Aff_tall_',
                             gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
            row.names = TRUE)


# 3. boxplot mania!----
# # fast an gives with-in city comparisons, but does not provide across-city comparisons
 df %>%
   mutate(Urbanicity_fct = 
            recode_factor(Urbanicity,
                          `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban',
                          .ordered = TRUE),
          Affluence_fct = 
            recode_factor(Affluence,
                          `1` = 'High', `2` = 'Middle', `3` = 'Low',
                          .ordered = TRUE),
          combo = paste0(Urbanicity_fct, "_", Affluence_fct)) %>%
   ggboxplot('combo', 'Perc_Tree',
             facet.by = 'MSA',
             ylim = c(0, 125),
             fill = 'Urbanicity_fct',
             palette = 'Set1',
             ylab = 'Tree Canopy Cover (%)', # more attractive label
             xlab = 'Metropolitan Statistical Area',
             legend = '') +
   stat_compare_means(comparisons = list(c('Urban', 'Suburban'),
                                         c('Suburban', 'Exurban'),
                                         c('Urban', 'Exurban')),
                      label = 'p.signif') + 
   theme(axis.text.x = element_text(angle = 90))
## TODO SW update box plots

df %>%
  mutate(Urbanicity_fct = 
           recode_factor(Urbanicity,
                         `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban',
                         .ordered = TRUE),
         Affluence_fct = 
           recode_factor(Affluence,
                         `1` = 'High', `2` = 'Medium', `3` = 'Low',
                         .ordered = TRUE),
         combo = paste0(Urbanicity_fct, Affluence_fct)) %>%
  ggboxplot('combo', 'Perc_Tree',
            facet.by = 'MSA',
            ylim = c(0, 125),
            fill = 'Affluence_fct',
            palette = 'Set1',
            ylab = 'Tree Canopy Cover (%)', # more attractive label
            xlab = 'Metropolitan Statistical Area',
            legend = '') +
  stat_compare_means(comparisons = list(c('High', 'Medium'),
                                        c('Medium', 'Low'),
                                        c('High', 'Low')),
                     label = 'p.signif')

# WORK IN PROGRESS, ignore for now
# df %>%
#   mutate(Urbanicity_fct = 
#            recode_factor(Urbanicity,
#                          `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban',
#                          .ordered = TRUE),
#          Affluence_fct = 
#            recode_factor(Affluence,
#                          `1` = 'High', `2` = 'Medium', `3` = 'Low',
#                          .ordered = TRUE),
#          urb_aff = interaction(Urbanicity_fct, Affluence_fct, sep = ' - ')) %>%
#   ggboxplot('urb_aff', 'Perc_Tree',
#             facet.by = 'MSA',
#             ylim = c(0, 125),
#             fill = 'Affluence_fct',
#             palette = 'Set3',
#             ylab = 'Tree Canopy Cover (%)', # more attractive label
#             xlab = 'Metropolitan Statistical Area',
#             legend = '') +
#   stat_compare_means(comparisons = list(c('High', 'Medium'),
#                                         c('Medium', 'Low'),
#                                         c('High', 'Low')),
#                      label = 'p.signif')

# under construction - ignore
df %<>% # create new blocking variable with combinations of MSA, Urbanicity and Affluence
  mutate(Urbanicity_fct = 
           recode_factor(Urbanicity,
                         `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban',
                         .ordered = TRUE),
         Affluence_fct = 
           recode_factor(Affluence,
                         `1` = 'High', `2` = 'Medium', `3` = 'Low',
                         .ordered = TRUE), 
         MSA_Urb_Aff = paste(MSA, Urbanicity_fct, Affluence_fct, sep = '_'))




# function to turn triangular p-val matrix into square matrix
# from https://fabiomarroni.wordpress.com/2017/03/25/perform-pairwise-wilcoxon-test-classify-groups-by-significance-and-plot-results/
tri.to.squ<-function(x)
{
  rn<-row.names(x)
  cn<-colnames(x)
  an<-unique(c(cn,rn))
  myval<-x[!is.na(x)]
  mymat<-matrix(1,nrow=length(an),ncol=length(an),dimnames=list(an,an))
  for(ext in 1:length(cn))
  {
    for(int in 1:length(rn))
    {
      if(is.na(x[row.names(x)==rn[int],colnames(x)==cn[ext]])) next
      mymat[row.names(mymat)==rn[int],colnames(mymat)==cn[ext]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
      mymat[row.names(mymat)==cn[ext],colnames(mymat)==rn[int]]<-x[row.names(x)==rn[int],colnames(x)==cn[ext]]
    }
    
  }
  return(mymat)
}



# the significance test
p_test <- pairwise.wilcox.test(df$Perc_Tree, df$MSA_Urb_Aff, p.adjust.method = 'holm', exact = FALSE) 

# letters denoting significantly different differences
my_letters <- multcompLetters(tri.to.squ(p_test$p.value),
                              compare = '<=',
                              threshold = 0.05,
                              Letters = letters)
# my_letters
# data.frame(my_letters$Letters)$my_letters

df %>% ggplot(aes(MSA_Urb_Aff, Perc_Tree)) +
  geom_boxplot() +
  ylim(0,200) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  annotate('text',
           x = 1:length(unique(df$MSA_Urb_Aff)),
           y = 90,
           label = data.frame(my_letters$Letters)$my_letters,
           angle = 90,
           hjust = 0)
# construction zone complete


# mod <- lmer(Perc_Tree ~ Urbanicity_fct*Affluence_fct + (1 | MSA), data = df)
mod <- lm(Perc_Tree ~ Urbanicity_fct*Affluence_fct*MSA, data = df)
plot_model(mod)
#plot_model(mod, type = 'int')
plot_model(mod,
           type = 'pred',
           terms = c('Urbanicity_fct', 'Affluence_fct', 'MSA'), 
           dodge = .45,
           dot.size = 3) + theme_bw(14)
# geom_hline(yintercept = 0 , col = 'light gray') +
# geom_hline(yintercept = 25, col = 'light gray') +
# geom_hline(yintercept = 50, col = 'light gray') +
# geom_hline(yintercept = 75, col = 'light gray')


tab_model(mod)

# 
# #### 4 categorical analyses----
# ####
# RColorBrewer::display.brewer.all() # great website http://colorbrewer2.org ! 
# 
# # lets aim for consistent colors 
# pal_city <- brewer.pal(name = 'Set1', n = 6)  # this could be our city palette
# pal_city                                      # prints the colors out in hex format
# 
# # fancy version of the same thing, but the number of classes comes from the data
# pal_city <- brewer.pal(name = 'Set1',
#                        n = length(unique(df$MSA)))  # this will be our city palette
# # this could be useful if the number of categories changes
# pal_city                                      # See, same colors!
# 
# # library(ggpubr) # should have been run in the last script, that gives the ggboxplot() function
# # helpful instructions here: https://rpkgs.datanovia.com/ggpubr/reference/ggboxplot.html
# # also helpful http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/
# df %>% ggboxplot(y = 'Perc_Tree',
#                  x = 'MSA') # basic box plot
# 
# # add colors
# df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
#                  x = 'MSA',       # categorical grouping variable
#                  fill = 'MSA')    # color by the category
# 
# # add OUR colors from pal_city by copy/paste from 
# df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
#                  x = 'MSA',       # categorical grouping variable
#                  fill = 'MSA',
#                  palette = c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33"))  
# 
# # add OUR colors from pal_city
# df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
#                  x = 'MSA',       # categorical grouping variable
#                  fill = 'MSA',
#                  palette = 'Set1')# ggpubr is really smart! makes the same graph
# 
# df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
#                  x = 'MSA',       # categorical grouping variable
#                  fill = 'MSA',    # what to color by
#                  palette = 'Set1',# what colors to use
#                  ylim = c(0, 100),# the value of Y (% tree canopy) hypothetically can range from 0 to 100
#                  ylab = 'Tree Canopy Cover (%)', # more attractive label
#                  xlab = 'Metropolitan Statistical Area',
#                  #add = 'jitter',  # Try turning this on and off with "#"
#                  legend = '')
# 
# # add some stats
 # df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
 #                  x = 'MSA',       # categorical grouping variable
 #                  fill = 'MSA',    # what to color by
 #                  palette = 'Set1',# what colors to use
 #                  ylim = c(0, 100),# the value of Y (% tree canopy) hypothetically can range from 0 to 100
 #                  ylab = 'Tree Canopy Cover (%)', # more attractive label
 #                  xlab = 'Metropolitan Statistical Area',
 #                  #add = 'jitter',  # Try turning this on and off with "#"
 #                  legend = '') + 
 #   stat_compare_means() -> city_tree_plot# answers the question "are thes medians different from eachother"
# 
# # YES, see the Kruska-Wallis test results now?
# 
# # print the graphic, noticed we assigned with "->" at the end?
# city_tree_plot
# 
# # but we want to know which pairs are different
#  city_comps <- list(c('PHX', 'MSP'),
#                     c('PHX', 'MIA'),
#                     c('PHX', 'LAX'),
#                     c('PHX', 'BOS'),
#                     c('PHX', 'BAL'))
# # 
# # # now doing multiple comparisions
#  df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
#                   x = 'MSA',       # categorical grouping variable
#                   fill = 'MSA',    # what to color by
#                   palette = 'Set1',# what colors to use
#                   ylim = c(0, 150),# ADDED space for new lables
#                   ylab = 'Tree Canopy Cover (%)', # more attractive label
#                   xlab = 'Metropolitan Statistical Area',
#                   #add = 'jitter',  # Try turning this on and off with "#"
#                   legend = '') + 
#    stat_compare_means() + 
#    stat_compare_means(comparisons = city_comps)-> city_tree_plot
# 
# # chceck it out!
# city_tree_plot

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

#SW updated boxplots DOESNOT include MSA in x - axis
df %>%
  mutate(Urbanicity_fct = 
           recode_factor(Urbanicity,
                         `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban',
                         .ordered = TRUE),
         urbanvar = (Urbanicity_fct)) %>%
  ggboxplot(y = 'Perc_Tree', # continuous dependent variable
            x = 'urbanvar',       # categorical grouping variable
            fill = 'MSA',    # what to color by
            facet.by = 'MSA',
            palette = 'Set1',# what colors to use
            #ylim = c(0, 150),# the value of Y (% tree canopy) hypothetically can range from 0 to 100
            ylab = 'Tree Canopy Cover (%)', # more attractive label
            xlab = '\n Metropolitan Statistical Area | Urbanicity', #Add an extra line between axis and axis title
            #add = 'jitter',  # Try turning this on and off with "#"
            legend = '') 

#SW updated boxplot with stats plots DV against Urbanicity
df %>%
  mutate(Urbanicity_fct = 
           recode_factor(Urbanicity,
                         `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban',
                         .ordered = TRUE)) %>%
  ggboxplot('Urbanicity_fct', 'Perc_Tree',
            facet.by = 'MSA',
            ylim = (c(0, 100)),
            fill = 'Urbanicity_fct',
            palette = 'Set1',
            orientation = 'horizontal',
            ylab = 'Tree Canopy Cover (%)', # more attractive label
            xlab = '\n Metropolitan Statistical Area | Urbanicity',
            legend = '') +
  stat_compare_means(comparisons = list(c('Urban', 'Suburban'),
                                        c('Suburban', 'Exurban'),
                                        c('Urban', 'Exurban')),
                     label = 'p.signif', label.y = c(94, 94, 100))
# hey, SW you can play with manualy setting bracket location ex # label.y = c(95, 90, 85)) 
# hey, SW you can rotate by adding "orientation = 'horizontal'" within the ggboxplot call

#SW updated boxplot with stats plots DV against Affluence
df %>%
  mutate(Affluence_fct = 
           recode_factor(Affluence,
                         `1` = 'High', `2` = 'Middle', `3` = 'Low',
                         .ordered = TRUE)) %>%
  ggboxplot('Affluence_fct', 'Perc_Tree',
            facet.by = 'MSA',
            ylim = c(0, 100),
            fill = 'Affluence_fct',
            palette = 'Set1',
            ylab = 'Tree Canopy Cover (%)', # more attractive label
            xlab = '\n Metropolitan Statistical Area | Soc. Economic Status',
            legend = '') +
  stat_compare_means(comparisons = list(c('High', 'Middle'),
                                        c('Middle', 'Low'),
                                        c('High', 'Low')),
                     label = 'p.signif', label.y = c(94, 94, 100))
            


# Which dependent variables are normally distributed?
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

# HOW ARE THE DV's DISTRIUBTED??----
# get the column index based on names, for the dependent varaibles (dv)
start_dv <- which(colnames(df) == 'Perc_Tree')
end_dv   <- which(colnames(df) == 'PAratio_G')

# 11 is too many!
end_dv - start_dv

for(i in seq(start_dv, end_dv)){
  # are they normally distributed?
  p <- df %>% ggdensity(x = names(df)[i],
                        add = 'mean',
                        rug = TRUE,
                        title = paste(names(df)[i])) # + scale_x_sqrt()
  # + scale_x_log10()
  # annotate(geom = 'text', paste0('One-sample Kolmogorov-Smirnov test p-val: ',
  #                                ks.test(x = df$Perc_Tree, y = pnorm)$p.value),
  #          x = median(df[,i]),
  #          y = .01)
  # significant means NOT NORMAL
  #shapiro.test(x = df$Perc_Tree)
  
  p_qq <- df %>% ggqqplot(names(df)[i],
                          title = paste0('qq-plot for: ', names(df)[i],
                                         '\npoints should be along line'))
  plot_grid(p, p_qq)
  
  ggplot2::ggsave(plot = plot_grid(p, p_qq),  # the graph we just made with plot_grid()
                  filename = paste0(getwd(), '/graphs/normality_', names(df)[i], '_',
                                    gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                  width  = 6.5, # this is  as wide as a normal Word Doc page
                  height = 4.5, # I had to play with this A LOT to get this to look right
                  # the Plots tab in RStudio is not representative of what the
                  # the saved version will look like
                  units = 'in')
  
  print(ks.test(x = df[,i], y = pnorm)) # significant means not normal
}

# LITTERALLY NONE OF THESE PASSED THE TEST
# 'Perc_' variable many need sqrt tranformation and/or use beta distribution?
# 'NP_T' and 'NP_G' are counts (poisson?) and *might* be log normal?        UPDATE, fails qqplot and ks.test
# 'MPA_T' and 'MPA_G' have zero-inflation issues -                          UPDATE, just model PHX seperately, after removing zeros, 
#                                                                                   AND logging
df %>% gghistogram(x = 'MPA_T', add = 'mean', rug = TRUE, 
                   color = 'MSA', fill = 'MSA', binwidth = 0.05,
                   alpha = .15) + scale_x_log10()

df %>% ggdensity(x = 'MPA_T', add = 'mean', rug = TRUE, 
                 color = 'MSA', fill = 'MSA',
                 alpha = .15) + scale_x_log10()

df %>% gghistogram(x = 'MPA_G', add = 'mean', rug = TRUE, 
                   color = 'MSA', fill = 'MSA', binwidth = 0.05,
                   alpha = .15) + scale_x_log10()

df %>% ggdensity(x = 'MPA_G', add = 'mean', rug = TRUE, 
                 color = 'MSA', fill = 'MSA',
                 alpha = .15) + scale_x_log10()

# PAratio_T is a nightmare, lols
df %>% gghistogram(x = 'PAratio_T', add = 'mean', rug = TRUE, 
                   color = 'MSA', fill = 'MSA', binwidth = 0.01,
                   alpha = .15) + scale_x_log10()

# the others are ratios..?
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#model-extensions



### 5. continuous models----
### 5.1.continuous analyses but first lets scale the predictors, grand-mean scaling----
### See this paper which addresses scalling
### guinis H, Gottfredson RK, Culpepper SA (2013) Best-Practice Recommendations for Estimating Cross-Level Interaction Effects Using Multilevel Modeling. J Manage 39(6):1490–1528.
df$Population_Density      <- scale(df$POPD_SQKM, center = T)
df$Median_Household_Income <- scale((df$INC_MED_HS / 1000), center = T)
df$Median_Household_Income_2<- scale((df$INC_MED_HS / 1000) * (df$INC_MED_HS / 1000), center = T) # convert to 1000s
#df$`Percent_non-White`           <- scale(I(100 - df$P_White), center = T) # so this didn't really work out. :-/
df$Percent_White           <- scale(df$P_White, center = T) # so this didn't really work out. :-/ DHL update: you just had wrong variable name its "P_White"
df$Percent_Hispanic        <- scale(df$P_Hisp, center = T)
df$Percent_Own             <- scale(df$P_Own, center = T)
df$Percent_Own_2           <- scale(df$P_Own*df$P_Own, center = T)
df$Housing_Age             <- scale(df$HOUS_AGE, center = T)
df$Housing_Age_2           <- scale(df$HOUS_AGE*df$HOUS_AGE, center = T)
df$Terrain_Roughness       <- scale(df$SDE_STD, center = T) # WOW, the changed names print great in plot_model, I didn't know
                                                            # that plot_mod() changes "_" to " "

# FIXME DHL (with SW) to determine functional form / transformation is needed
# TODO easy stats diagnositcs via check_distribution() function
# TODO make a summary table of funcational forms / transformations
# TODO update normality and combined effects graphs

# TODO experiment with square (and quantiles?) for income and Housing Age


# I am REALLY lazy!
font_sz <- 5 # font size
fig_w   <- 6.5
fig_h   <- 2.5
fig_u   <- 'in'
# 
# # can you fit glm to lm data?
# # make data
# n <- 5100                                      # length of data frame, approx our real data
# fake <- tibble(dv = abs(rnorm(n = n,               # 5100 / 6 = 850
#                               mean = 25,       # tree canopy has a mean ~25
#                               sd = 15)),        # tree canopy has a sd ~19
#                iv_1 = dv + rnorm(n, 40, 20),
#                iv_2 = dv + 1.5*rnorm(n, 75, 20),   # very similar to percent owner occupied
#                iv_3 = dv*-1 + rnorm(n, 35, 10),
#                iv_4_inc = dv + rnorm(n = n, mean = 60, sd = 24), # like income.. 
#                iv_5_inc_2 = dv - iv_4_inc*iv_4_inc,             # income squared
#                MSA = as.factor(rep(LETTERS[1:6], 850))) %>% 
#   arrange(MSA) # just to make it pretty
#                
# fake
# psych::pairs.panels(fake)
# 
# fake_lm <- lm(dv ~ iv_1 + iv_2 + iv_3 + iv_4_inc + iv_5_inc_2, data = fake)
# 
# plot_model(fake_lm)
# tab_model(fake_lm)
# 
# fake_glm_a <- glm(dv ~ iv_1 + iv_2 + iv_3 + iv_4_inc + iv_5_inc_2, data = fake,
#                 family = gaussian) # is normal
# 
# fake_glm_b <- glm(as.integer(dv) ~ iv_1 + iv_2 + iv_3 + iv_4_inc + iv_5_inc_2, data = fake,
#                 family = poisson()) # is normal
# 
# #plot_model(fake_glm)
# tab_model(fake_lm, fake_glm_a, fake_glm_b)
# tab_model(fake_glm_b, transform = 'exp')
# 


# 5.1.1. Perc_Tree first dependent variable:---- 
mod <- lme4::lmer(Perc_Tree ~
                    Population_Density + # fixed effects
                    Percent_Own + 
                    Percent_Own_2 + 
                    Housing_Age + 
                    Housing_Age_2 +
                    Median_Household_Income + 
                    Median_Household_Income_2 + 
                    Percent_White +
                    Percent_Hispanic + 
                    Terrain_Roughness +
                    (1 | MSA),                               # random effects
                  data = df)

plot_model(mod, type = 'diag') # diagnostics
result <- check_distribution(mod); result


# I think we can live with this model. 
# graph it
p_fe <- plot_model(mod,                                 # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   show.values = TRUE,
                   show.p = TRUE,
                   value.offset = .3,
                   title = 'Tree Canopy Cover (%):\nfixed effects', # "\n" means "new line"
                   #sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') + #,                      # adds the zero line back in that theme_bw() takes out
                   #axis.lim = c(.25, 1.25)) +             may want to standardize x axis            
  theme_bw()                                                     # number pertains to font size

p_fe # peak at the graph

# random effects graph
p_re <- plot_model(mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   show.values = TRUE,
                   value.offset = .3,
                   title = 'Tree Canopy Cover (%):\nrandom effects',# "\n" means "new line"
                   #sort.est = 'sort.all,                                # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw()

# peak at the graph
p_re

# combine the two graphs into one two-pane graph
p_tree_graph <- plot_grid(p_fe, # fixed effects
                          p_re, # random effects
                          labels = c('A', 'B'))
# looks great!
p_tree_graph

# save this out
ggplot2::ggsave(plot = p_tree_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/p_tree_mixed_effects_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                         width  = fig_w, # this is  as wide as a normal Word Doc page
                         height = fig_h, # I had to play with this A LOT to get this to look right
                                       # the Plots tab in RStudio is not representative of what the
                                       # the saved version will look like
                units = fig_u)

# TODO consider combining tables into one
tab_model(mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= '% Tree Canopy Cover') 

# lets write out the table
tab_model(mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = '% Tree Canopy Cover',
          file = paste0(getwd(), '/tables/model_fits/p_tree_canopy_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))  # ".png" changed to ".html",
                                                                         # should open nicely in web browser







# 5.1.2. Perc_Grass second dependent variable:---- 
mod <- lme4::lmer(Perc_Grass ~
                    Population_Density + # fixed effects
                    Percent_Own + 
                    #Percent_Own_2 + 
                    Housing_Age + 
                    Housing_Age_2 +
                    Median_Household_Income + 
                    Median_Household_Income_2 + 
                    Percent_White +
                    Percent_Hispanic + 
                    Terrain_Roughness +
                    (1 | MSA),                               # random effects
                  data = df)

plot_model(mod, type = 'diag') # diagnostics
result <- check_distribution(mod); result

# lets try gamma, but gamma can't take zeros.
df %>% filter(Perc_Grass > 0) -> df_grass # 18 rows cut, less 1%


mod_1 <- lme4::lmer(Perc_Grass ~ 
                      Population_Density + # fixed effects
                      Percent_Own + 
                      #Percent_Own_2 + 
                      Housing_Age + 
                      Housing_Age_2 +
                      Median_Household_Income + 
                      Median_Household_Income_2 + 
                      Percent_White +
                      Percent_Hispanic + 
                      Terrain_Roughness +
                      (1 | MSA),                               # random effects
                    data = df_grass)

# note that data has changed and "lmer" is now "glmer", and the family argument
mod_2 <- lme4::glmer(Perc_Grass ~ 
                       Population_Density + # fixed effects
                       Percent_Own + 
                       #Percent_Own_2 + 
                       Housing_Age + 
                       Housing_Age_2 +
                       Median_Household_Income + 
                       Median_Household_Income_2 + 
                       Percent_White +
                       Percent_Hispanic + 
                       Terrain_Roughness +
                    (1 | MSA),                               # random effects
                   data = df_grass, 
                   family=Gamma(link="log")) # family of glm

tab_model(mod_1, mod_2)
result <- check_distribution(mod_1); result
result <- check_distribution(mod_2); result
tab_model(mod_1, mod_2, transform = NULL)
tab_model(mod_1)
tab_model(mod_2)

plot_model(mod_1); plot_model(mod_2)

plot_model(mod, type = 'diag') # diagnostics
result <- check_distribution(mod); result
# I think we can live with this model. 

# graph it
p_fe <- plot_model(mod_2,                                 # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   show.values = TRUE,
                   show.p = TRUE,
                   value.offset = .3,
                   title = 'Grass Cover (%):\nfixed effects with Gamma distriubtion', # "\n" means "new line"
                   #sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') + #,                      # adds the zero line back in that theme_bw() takes out
  #axis.lim = c(.25, 1.25)) +             may want to standardize x axis            
  theme_bw()                                                     # number pertains to font size

p_fe # peak at the graph

# random effects graph
p_re <- plot_model(mod_2,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   show.values = TRUE,
                   value.offset = .3,
                   title = 'Grass Cover (%):\nrandom effects',# "\n" means "new line"
                   #sort.est = 'sort.all,                                # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw()

# peak at the graph
p_re

# combine the two graphs into one two-pane graph
p_grass_graph <- plot_grid(p_fe, # fixed effects, p_tree_mod,
                          p_re, # random effects, p_tree_mod,
                          labels = c('A', 'B'))
# looks great!
p_grass_graph

# save this out
ggplot2::ggsave(plot = p_grass_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/p_grass_mixed_effects_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # this is  as wide as a normal Word Doc page
                height = fig_h, # I had to play with this A LOT to get this to look right
                # the Plots tab in RStudio is not representative of what the
                # the saved version will look like
                units = fig_u)

# TODO: consider combining several tables into one
tab_model(mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= '% Grass Cover') 

# lets write out the table
tab_model(mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = '% Grass Cover',
          file = paste0(getwd(), '/tables/model_fits/p_grass_mixed_effects_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))  # ".png" changed to ".html",
# should open nicely in web browser



# Perc_Other     # % other area   - dont care
# Perc_Water     # % water area   - dont care


# 5.1.3 Number of Tree Patches----
# NP_T          
# Number of Patches Tree (distinct tree patches, group of pixels)
mod <- lme4::lmer(NP_T ~ Population_Density + # fixed effects
                    Median_Household_Income +
                    Percent_White +
                    Percent_Hispanic + 
                    Percent_Own_House +
                    Housing_Age + 
                    Terrain_Roughness +
                    (1 | MSA),                               # random effects
                  data = df)

plot_model(mod, type = 'diag') # diagnostics YEah - HARD NO
result <- check_distribution(mod); result # suggests log normal

# taking the log requires no zeros, so lets look for those first
df %>% filter(NP_T > 0) %>% 
  mutate(log_NP_T = log(NP_T)) -> df_NP_T # 22 rows, less than 1%

hist(df_NP_T$log_NP_T) # pretty

mod <- lme4::lmer(log_NP_T ~ Population_Density + # fixed effects
                    Median_Household_Income +
                    Percent_White +
                    Percent_Hispanic + 
                    Percent_Own_House +
                    Housing_Age + 
                    Terrain_Roughness +
                    (1 | MSA),                               # random effects
                  data = df_NP_T)

plot_model(mod, type = 'diag') # diagnostics YEah - HARD NO
result <- check_distribution(mod); result # suggests log normal

# I think we can live with this model. 
# graph it
p_fe <- plot_model(mod,                                 # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   show.values = TRUE,
                   show.p = TRUE,
                   value.offset = .3,
                   title = 'Log of Number of Tree Canopy Patches:\nfixed effects', # "\n" means "new line"
                   #sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') + #,                      # adds the zero line back in that theme_bw() takes out
  #axis.lim = c(.25, 1.25)) +             may want to standardize x axis            
  theme_bw()                                                     # number pertains to font size

p_fe # peak at the graph

# random effects graph
p_re <- plot_model(mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   show.values = TRUE,
                   value.offset = .3,
                   title = 'Log of Number of Tree Canopy Patches:\nrandom effects',# "\n" means "new line"
                   #sort.est = 'sort.all,                                # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw()

# peak at the graph
p_re

# combine the two graphs into one two-pane graph
log_NP_T_graph <- plot_grid(p_fe, # fixed effects
                            p_re, # random effects
                            labels = c('A', 'B'))
# looks great!
log_NP_T_graph

# save this out
ggplot2::ggsave(plot = log_NP_T_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/log_NP_T_mixed_effects_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # this is  as wide as a normal Word Doc page
                height = fig_h, # I had to play with this A LOT to get this to look right
                # the Plots tab in RStudio is not representative of what the
                # the saved version will look like
                units = fig_u)

# TODO consider combining tables into one
tab_model(mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= 'Log Number of Patches of Tree Canopy') 

# lets write out the table
tab_model(mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Log Number of Patches of Tree Canopy',
          file = paste0(getwd(), '/tables/model_fits/log_NP_T_mixed_effects_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))  # ".png" changed to ".html",
# should open nicely in web browser




# 5.1.4---- Mean Patch Area: TREES
# MPA_T          # Mean Patch Area Tree (the average size of tree patches)
mod <- lme4::lmer(MPA_T ~ Population_Density + # fixed effects
                    Median_Household_Income +
                    Percent_White +
                    Percent_Hispanic + 
                    Percent_Own_House +
                    Housing_Age + 
                    Terrain_Roughness +
                    (1 | MSA),                               # random effects
                  data = df)

plot_model(mod, type = 'diag') # diagnostics
result <- check_distribution(mod); result # VERY BAD

hist(df$MPA_T)
hist(log(df$MPA_T + 0.01)) # better!

df %>% filter(MPA_T > 0) %>% 
  mutate(log_MPA_T = log(MPA_T)) -> df_MPA_T # just 22 rows (<1%) dropped


mod <- lme4::lmer(log_MPA_T ~ Population_Density + # fixed effects
                    Median_Household_Income +
                    Percent_White +
                    Percent_Hispanic + 
                    Percent_Own_House +
                    Housing_Age + 
                    Terrain_Roughness +
                    (1 | MSA),                               # random effects
                  data = df_MPA_T) # NOTE the differnt data frame

plot_model(mod, type = 'diag') # diagnostics
result <- check_distribution(mod); result # VERY BAD
# I think we can live with this model. 

# graph it
p_fe <- plot_model(mod,                                 # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   show.values = TRUE,
                   show.p = TRUE,
                   value.offset = .3,
                   title = 'Log of Mean Patch Areas (Tree):\nfixed effects', # "\n" means "new line"
                   #sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') + #,                      # adds the zero line back in that theme_bw() takes out
  #axis.lim = c(.25, 1.25)) +             may want to standardize x axis            
  theme_bw()                                                     # number pertains to font size

p_fe # peak at the graph

# random effects graph
p_re <- plot_model(mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   show.values = TRUE,
                   value.offset = .3,
                   title = 'Log of Mean Patch Area (Tree):\nrandom effects',# "\n" means "new line"
                   #sort.est = 'sort.all,                                # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw()

# peak at the graph
p_re

# combine the two graphs into one two-pane graph
log_MPA_T_graph <- plot_grid(p_fe, # fixed effects
                          p_re, # random effects
                          labels = c('A', 'B'))
# looks great!
log_MPA_T_graph

# save this out
ggplot2::ggsave(plot = log_MPA_T_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/log_MPA_T_mixed_effects_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # this is  as wide as a normal Word Doc page
                height = fig_h, # I had to play with this A LOT to get this to look right
                # the Plots tab in RStudio is not representative of what the
                # the saved version will look like
                units = fig_u)

# TODO consider combining tables into one
tab_model(mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels= 'Log of Mean Patch Area of Tree Canopy') 

# lets write out the table
tab_model(mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Log of Mean Patch Area of Tree Canopy',
          file = paste0(getwd(), '/tables/model_fits/log_MPA_T_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))  # ".png" changed to ".html",
# should open nicely in web browser


# CV_T           #Coefficient of Variation for tree patches - too many DVs

## 5.1.4. Perimeter Area Ratio for tree canopy----
# PAratio_T      # Parimieter Area ratio for tree canopy
mod <- lme4::lmer(PAratio_T ~ Population_Density + # fixed effects
                    Median_Household_Income +
                    Percent_White +
                    Percent_Hispanic + 
                    Percent_Own_House +
                    Housing_Age + 
                    Terrain_Roughness +
                    (1 | MSA),                               # random effects
                  data = df)

plot_model(mod, type = 'diag') # diagnostics DEFINATELY NO
result <- check_distribution(mod); result # maybe lognormal?

# cant log zeros.. count first
df %>% filter(PAratio_T > 0) %>% 
  mutate(log_PAratio_T = log(PAratio_T)) -> df_PAratio_T # again only dropped a few (20) rows

hist(df_PAratio_T$log_PAratio_T) # eh not so sure yet

mod <- lme4::lmer(log_PAratio_T ~ Population_Density + # fixed effects
                    Median_Household_Income +
                    Percent_White +
                    Percent_Hispanic + 
                    Percent_Own_House +
                    Housing_Age + 
                    Terrain_Roughness +
                    (1 | MSA),                               # random effects
                  data = df_PAratio_T)

plot_model(mod, type = 'diag') # diagnostics HOMOSCEDASTICITY plot is really bad
result <- check_distribution(mod); result # maybe lognormal?

# NOT SURE HOW TO PROCEED with PAratio_T


# NP_G           # Number of Patches Grass
# MPA_G          # Mean Patch Area Grass
# CV_G           # TODO find out Coefficient of Variation for tree patches
# PAratio_G      # Parimieter Area ratio for tree canopy




## SAND BOX / THINGS NOT TO GET RID OF JUST YET-----



# CITE THE PACKGES
for(i in packages){
  print(citation(paste(i)))
}




# plot model diagnostics to check model assumptions
# FIXME (DHL): use Beta GLMMs in glmmTMB
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#beta-glmms
# TODO (DHL) save out the diagnostic plots
plot_model(p_tree_mod, type = 'diag') # this looks really bad. 




p_tree_mod <- glmmTMB::glmmTMB(I(Perc_Tree / 100) ~ Median_Household_Income + # fixed effects
                                 Percent_White +
                                 Percent_Hispanic + 
                                 Percent_Own_House +
                                 Housing_Age + 
                                 Terrain_Roughness +
                                 (1 | MSA),                               # random effects
                               data = df, list(family = 'beta', link = 'logit'))

family = beta_family())


# FIXME LMER is 'wrong' because of the distribution of Perc_Tree - still need to fix this
# df$pct_tree <- (df$Perc_Tree / 100) + 0.0001
# par(mfrow=c(1, 2)); hist(df$Perc_Tree); hist(df$pct_tree)
# p_tree_mod <- glmmTMB::glmmTMB(pct_tree ~ Median_Household_Income + # fixed effects # just being very verbose with "::"
#                      Percent_White +
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

