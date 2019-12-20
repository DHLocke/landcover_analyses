# Script by Dexter H Locke and Hillol Dutta
# for analyzing land cover composition, as part of the MacroSystems Biology project

# started on
# Mon Oct 21 19:58:30 2019 ------------------------------
# man updates documented via git

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
              'psych')        # useful data summaries

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
# prop.table(table(df$MSA, useNA = 'ifany')) # distributions of block groups, as a %
# 
# # or more refined
# prop.table(table(df$MSA, useNA = 'ifany'))*100 # actually in percent form

# or even more refined! 
round(prop.table(table(df$MSA, useNA = 'ifany'))*100, 2)# rounding 2 places, much prettier

# try
help(table) # to see what the "useNA" argument is for



# some descriptive statistics
# counts per city
# inspired by https://github.com/sfirke/janitor
# df %>% tabyl(MSA) %>% 
#   adorn_totals('row') %>% 
#   adorn_pct_formatting() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/MSA_count_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = FALSE)


  
# urbanicity
table(df$Urbanicity, df$PNE_CODE, useNA = 'ifany')
table(df$Urbanicity) # 1 is urban, 2 is suburban, 3 is exurban

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


# Affluence
table(df$Affluence, df$PNE_CODE, useNA = 'ifany')
table(df$Affluence) # 1 high, 2 is medium, 3 is low

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
# # MSA Urbanicity Affluence
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

### THE MAIN DESCRIPTIVE TABLES
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
         Percent_nonWhite = P_White,
         Percent_Hispanic = P_Hisp,
         Percent_Own_House = P_Own,
         Housing_Age = HOUS_AGE,
         Terrain_Roughness = SDE_STD) %>% 
  select(Tree, Grass, Perc_Tree, Perc_Grass,                        # currently dependent variables
         NP_T, MPA_T, CV_T, PAratio_T, NP_G, MPA_G, CV_G, PAratio_G,
         Median_Household_Income, Percent_nonWhite, Percent_Hispanic, # independent variables 
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
         Percent_nonWhite_Min, Percent_nonWhite_Max, Percent_nonWhite_Mean, Percent_nonWhite_Median, Percent_nonWhite_SD, Percent_nonWhite_IQR,
         Percent_Hispanic_Min, Percent_Hispanic_Max, Percent_Hispanic_Mean, Percent_Hispanic_Median, Percent_Hispanic_SD, Percent_Hispanic_IQR,
         Percent_Own_House_Min, Percent_Own_House_Max, Percent_Own_House_Mean, Percent_Own_House_Median, Percent_Own_House_SD, Percent_Own_House_IQR,
         Housing_Age_Min, Housing_Age_Max, Housing_Age_Mean, Housing_Age_Median, Housing_Age_SD, Housing_Age_IQR,
         Terrain_Roughness_Min, Terrain_Roughness_Max, Terrain_Roughness_Mean, Terrain_Roughness_Median, Terrain_Roughness_SD, Terrain_Roughness_IQR) %>% 
  t() %>% # this transposes
  write.csv(., file = paste0(getwd(), '/tables/descriptives/descriptive_stats_MSA_Urb_Aff_tall_',
                             gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
            row.names = TRUE)

# # group by MSA$Affluence
# df$Affluence_fct = 
#   recode_factor(df$Affluence,
#                 `1` = 'High', `2` = 'Medium', `3` = 'Low',
#                 .ordered = TRUE)
# df %>% select(Tree, Grass, Perc_Tree, Perc_Grass, 
#               NP_T, MPA_T, CV_T, PAratio_T, NP_G, MPA_G, CV_G, PAratio_G,
#               MSA, Affluence_fct) %>% 
#   group_by(MSA, Affluence_fct) %>% 
#   summarise_all(list(Min = min, Max = max, Mean = mean, SD = sd, Median=median)) %>%
#   ungroup() %>%
#   t() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/descriptive_stats_MSA_Affl_tall_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = TRUE)
# 
# # group by MSA$Urbanicity$Affluence
# 
# df %>% select(Tree, Grass, Perc_Tree, Perc_Grass, 
#               NP_T, MPA_T, CV_T, PAratio_T, NP_G, MPA_G, CV_G, PAratio_G,
#               MSA, Urbanicity_fct, Affluence_fct) %>% 
#   group_by(MSA, Urbanicity_fct, Affluence_fct) %>% 
#   summarise_all(list(Min = min, Max = max, Mean = mean, SD = sd, Median=median)) %>%
#   ungroup() %>%
#   t() %>% 
#   write.csv(., file = paste0(getwd(), '/tables/descriptives/descriptive_stats_MSA_Urb_Affl_tall_',
#                              gsub('[[:punct:]]', '_', Sys.time()), '.csv'),
#             row.names = TRUE)
# 
# 
# 
# # boxplot mania!
# # fast an gives with-in city comparisons, but does not provide across-city comparisons
# df %>%
#   mutate(Urbanicity_fct = 
#            recode_factor(Urbanicity,
#                          `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban',
#                          .ordered = TRUE),
#          Affluence_fct = 
#            recode_factor(Affluence,
#                          `1` = 'High', `2` = 'Middle', `3` = 'Low',
#                          .ordered = TRUE)) %>%
#   ggboxplot('Urbanicity_fct', 'Perc_Tree',
#             facet.by = 'MSA',
#             ylim = c(0, 125),
#             fill = 'Urbanicity_fct',
#             palette = 'Set2',
#             ylab = 'Tree Canopy Cover (%)', # more attractive label
#             xlab = 'Metropolitan Statistical Area',
#             legend = '') +
#   stat_compare_means(comparisons = list(c('Urban', 'Suburban'),
#                                         c('Suburban', 'Exurban'),
#                                         c('Urban', 'Exurban')),
#                      label = 'p.signif') 


df %>%
  mutate(Urbanicity_fct = 
           recode_factor(Urbanicity,
                         `1` = 'Urban', `2` = 'Suburban', `3` = 'Exurban',
                         .ordered = TRUE),
         Affluence_fct = 
           recode_factor(Affluence,
                         `1` = 'High', `2` = 'Medium', `3` = 'Low',
                         .ordered = TRUE)) %>%
  ggboxplot('Affluence_fct', 'Perc_Tree',
            facet.by = 'MSA',
            ylim = c(0, 125),
            fill = 'Affluence_fct',
            palette = 'Set3',
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
# #### categorical analyses
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
# city_comps <- list(c('PHX', 'MSP'),
#                    c('PHX', 'MIA'),
#                    c('PHX', 'LAX'),
#                    c('PHX', 'BOS'),
#                    c('PHX', 'BAL'))
# 
# # now doing multiple comparisions
# df %>% ggboxplot(y = 'Perc_Tree', # continuous dependent variable
#                  x = 'MSA',       # categorical grouping variable
#                  fill = 'MSA',    # what to color by
#                  palette = 'Set1',# what colors to use
#                  ylim = c(0, 150),# ADDED space for new lables
#                  ylab = 'Tree Canopy Cover (%)', # more attractive label
#                  xlab = 'Metropolitan Statistical Area',
#                  #add = 'jitter',  # Try turning this on and off with "#"
#                  legend = '') + 
#   stat_compare_means() + 
#   stat_compare_means(comparisons = city_comps)-> city_tree_plot
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

# the others are ratios..?
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#model-extensions



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
p_tree_mod <- glmmTMB::glmmTMB(I(Perc_Tree / 100) ~ Median_Household_Income + # fixed effects
                     Percent_nonWhite +
                     Percent_Hispanic + 
                     Percent_Own_House +
                     Housing_Age + 
                     Terrain_Roughness +
                    (1 | MSA),                               # random effects
                   data = test, family = beta_family(link = 'logit'))


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
p_tree_fe <- plot_model(p_tree_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Tree Canopy Cover (%):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black',
                   axis.lim = c(.25, 1.25)) +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(10)                                                      # number pertains to font size

# peak at the graph
p_tree_fe

# random effects: p_tree_mod
p_tree_re <- plot_model(p_tree_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Tree Canopy Cover (%):\nrandom effects',# "\n" means "new line"
                   #sort.est = 'sort.all,                                # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(10)                                                      # number pertains to font size

# peak at the graph
p_tree_re

# combine the two graphs into one two-pane graph
p_tree_graph <- plot_grid(p_tree_fe, # fixed effects, p_tree_mod,
                          p_tree_re, # random effects, p_tree_mod,
                          labels = c('A', 'B'))
# looks great!
p_tree_graph

# save this out
ggplot2::ggsave(plot = p_tree_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Combined effects of Tree Canopy Model_',
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
p_grass_fe <- plot_model(p_grass_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Grass Cover (%):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# peak at the graph
p_grass_fe

# random effects: p_tree_mod
# we are writting over the 'p_fe' object we made for trees..
p_grass_re <- plot_model(p_grass_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Grass Cover (%):\nrandom effects',# "\n" means "new line"
                   #sort.est = 'sort.all,                                # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# peak at the graph
p_grass_re

# combine the two graphs into one two-pane graph
p_grass_graph <- plot_grid(p_grass_fe, # fixed effects, p_grass_mod,
                          p_grass_re, # random effects, p_grass_mod,
                          labels = c('A', 'B'))
# looks great!
p_grass_graph

# save this out
ggplot2::ggsave(plot = p_grass_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Combined effects of Grass Cover Model__',
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
p_other_fe <- plot_model(p_other_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Other Cover (%):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# peak at the graph
p_other_fe

# random effects: p_other_mod
# we are writting over the 'p_fe' object we made for grass.
p_other_re <- plot_model(p_other_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Other Cover (%):\nrandom effects',# "\n" means "new line"
                   #sort.est = 'sort.all,                                # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# peak at the graph
p_other_re

# combine the two graphs into one two-pane graph
p_other_graph <- plot_grid(p_other_fe, # fixed effects
                           p_other_re, # random effects
                           labels = c('A', 'B'))
# looks great!
p_other_graph

# save this out
ggplot2::ggsave(plot = p_other_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Combined effects of Other cover Model_',
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
p_water_fe <- plot_model(p_water_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Water Cover (%):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_water_re <- plot_model(p_water_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Water Cover (%):\nrandom effects',# "\n" means "new line"
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
p_water_graph <- plot_grid(p_water_fe, # fixed effects
                           p_water_re, # random effects
                           labels = c('A', 'B'))
# save this out
ggplot2::ggsave(plot = p_water_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Combined effects of Water cover Model_',
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

p_MPT_fe <- plot_model(MPT_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Mean Patch Area (Tree):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_MPT_re <- plot_model(MPT_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Mean Patch Area (Tree):\nrandom effects',# "\n" means "new line"
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
MPT_graph <- plot_grid(p_MPT_fe, # fixed effects
                           p_MPT_re, # random effects
                           labels = c('A', 'B'))

MPT_graph


# save this out
ggplot2::ggsave(plot = MPT_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Mean Patch Area (Tree) Model_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)

MPT_graph

# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(MPT_mod, type = 'diag')

#tables
tab_model(MPT_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Mean Patch Area (Tree)',
          file = paste0(getwd(), '/tables/Mean Patch Area (Tree)_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))
tab_model(MPT_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Mean Patch Area (Tree)')

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

p_MPG_fe <- plot_model(MPG_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Mean Patch Area (Grass):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_MPG_re <- plot_model(MPG_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Mean Patch Area (Grass):\nrandom effects',# "\n" means "new line"
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
MPG_graph <- plot_grid(p_MPG_fe, # fixed effects
                       p_MPG_re, # random effects
                       labels = c('A', 'B'))
# save this out
ggplot2::ggsave(plot = MPG_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Mean Patch Area (Grass) Model_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)
MPG_graph


# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(MPG_mod, type = 'diag')

#tables
tab_model(MPG_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Mean Patch Areas (Grass)',
          file = paste0(getwd(), '/tables/Mean Patch Area (Grass)_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))

tab_model(MPG_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Mean Patch Area (Grass)')
          
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

p_NPT_fe <- plot_model(NPT_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Number of Patches (Tree):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_NPT_re <- plot_model(NPT_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Number of Patches (Tree):\nrandom effects',# "\n" means "new line"
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
NPT_graph <- plot_grid(p_NPT_fe, # fixed effects
                       p_NPT_re, # random effects
                       labels = c('A', 'B'))
NPT_graph

# save this out
ggplot2::ggsave(plot = NPT_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Number of Patches (Tree)_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)

# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(NPT_mod, type = 'diag')

#tables
tab_model(NPT_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'No of Patch Areas of Tree',
          file = paste0(getwd(), '/tables/Number of Patches (Tree) Model_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))

tab_model(NPT_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Number of Patches (Tree)')

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

p_NPG_fe <- plot_model(NPG_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                   type = 'est',                                    # more explcit that accepting the defaults
                   title = 'Number of Patches (Grass):\nfixed effects', # "\n" means "new line"
                   sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_NPG_re <- plot_model(NPG_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                   type = 're',                                    # more explcit that accepting the defaults
                   title = 'Number of Patches (Grass):\nrandom effects',# "\n" means "new line"
                   vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
NPG_graph <- plot_grid(p_NPG_fe, # fixed effects
                       p_NPG_re, # random effects
                       labels = c('A', 'B'))

NPG_graph
# save this out
ggplot2::ggsave(plot = NPG_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Number of Pacthes (Grass) Model_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)

#tables
tab_model(NPT_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'No of Patch Areas of Tree',
          file = paste0(getwd(), '/tables/Number of Patches (Tree)_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))

tab_model(NPT_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Number of Patches (Tree)')

# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(NPG_mod, type = 'diag')



#Perimeter-Area ratio of Tree canopy Model (Hillol):

PART_mod <- lmer(PAratio_T ~ Median_Household_Income+
                  Percent_nonWhite +
                  Percent_Hispanic + 
                  Percent_Own_House +
                  Housing_Age + 
                  Terrain_Roughness +
                  (1 | MSA),                               
                data = df) 

p_PART_fe <- plot_model(PART_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                       type = 'est',                                    # more explcit that accepting the defaults
                       title = 'Perimeter-Area Ratio (Tree):\nfixed effects', # "\n" means "new line"
                       sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                       vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_PART_re <- plot_model(PART_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                       type = 're',                                    # more explcit that accepting the defaults
                       title = 'Perimeter-Area Ratio (Tree):\nrandom effects',# "\n" means "new line"
                       vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
PART_graph <- plot_grid(p_PART_fe, # fixed effects
                       p_PART_re, # random effects
                       labels = c('A', 'B'))
PART_graph

# save this out
ggplot2::ggsave(plot = PART_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Perimeter-Area Ratio (Tree) Model_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)
#tables
tab_model(PART_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Perimeter-Area Ratio (Tree)',
          file = paste0(getwd(), '/tables/Perimeter-Area Ratio (Tree)_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))

tab_model(PART_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Perimeter-Area Ratio (Tree)')

# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(PART_mod, type = 'diag')

#Perimeter-Area Ratio of Grass Model (Hillol):
PARG_mod <- lmer(PAratio_T ~ Median_Household_Income+
                   Percent_nonWhite +
                   Percent_Hispanic + 
                   Percent_Own_House +
                   Housing_Age + 
                   Terrain_Roughness +
                   (1 | MSA),                               
                 data = df) 


p_PARG_fe <- plot_model(PARG_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                        type = 'est',                                    # more explcit that accepting the defaults
                        title = 'Perimeter-Area Ratio (Grass):\nfixed effects', # "\n" means "new line"
                        sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                        vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_PARG_re <- plot_model(PARG_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                        type = 're',                                    # more explcit that accepting the defaults
                        title = 'Perimeter-Area Ratio (Grass):\nrandom effects',# "\n" means "new line"
                        vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
PARG_graph <- plot_grid(p_PARG_fe, # fixed effects
                        p_PARG_re, # random effects
                        labels = c('A', 'B'))
PARG_graph

# save this out
ggplot2::ggsave(plot = PARG_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Perimeter-Area Ratio (Grass) Model_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)


#tables
tab_model(PARG_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Perimeter-Area Ratio (Grass)',
          file = paste0(getwd(), '/tables/Perimeter-Area Ratio (Grass)_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))


# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(PARG_mod, type = 'diag')


#Coefficient of variation of Tree patch Model (Hillol):

CVT_mod <- lmer(CV_T ~ Median_Household_Income+
                   Percent_nonWhite +
                   Percent_Hispanic + 
                   Percent_Own_House +
                   Housing_Age + 
                   Terrain_Roughness +
                   (1 | MSA),                               
                 data = df) 

# TODO make sure title fits on graph
p_CVT_fe <- plot_model(CVT_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                        type = 'est',                                    # more explcit that accepting the defaults
                        title = 'Coefficient of variation:\nof patch (Tree):\nfixed effects', # "\n" means "new line"
                        sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                        vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_CVT_re <- plot_model(CVT_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                        type = 're',                                    # more explcit that accepting the defaults
                        title = 'Coefficient of variation:\n of patch (Tree):\nrandom effects',# "\n" means "new line"
                        vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
CVT_graph <- plot_grid(p_CVT_fe, # fixed effects 
                        p_CVT_re, # random effects
                        labels = c('A', 'B'))
CVT_graph

# save this out
ggplot2::ggsave(plot = CVT_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Coefficient of variation of patch (Tree) Model_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)


#tables
tab_model(CVT_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Coefficient of variation of patch (Tree)',
          file = paste0(getwd(), '/tables/Coefficient of variation of patch (Tree)_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))

# TODO: HD Why use tab_model hereon the same model as is shown above? Why duplicate?
tab_model(CVT_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Coefficient of variation of patch (Tree)')

# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(CVT_mod, type = 'diag')

#Coefficient of variation of Grass patch  Model (Hillol):

CVG_mod <- lmer(CV_G ~ Median_Household_Income+
                  Percent_nonWhite +
                  Percent_Hispanic + 
                  Percent_Own_House +
                  Housing_Age + 
                  Terrain_Roughness +
                  (1 | MSA),                               
                data = df) 

p_CVG_fe <- plot_model(CVG_mod,                                      # save the model in "p_fe", short for Plot Fixed Effects
                       type = 'est',                                    # more explcit that accepting the defaults
                       title = 'Coefficient of variation:n\ of patch (Grass):\nfixed effects', # "\n" means "new line"
                       sort.est = TRUE,                                 # need to decide if consistent order is better than sorted
                       vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable

# save re plot
p_CVG_re <- plot_model(CVG_mod,                                     # save the model in "p_re", short for Plot RANDOM Effects
                       type = 're',                                    # more explcit that accepting the defaults
                       title = 'Coefficient of variation:n\ of patch (Grass):\nrandom effects',# "\n" means "new line"
                       vline.color = 'black') +                         # adds the zero line back in that theme_bw() takes out                              
  theme_bw(font_sz)      # see using the new local variable, NOW ALL GRAPHS WILL BE CONSITENT

# combine the two graphs into one two-pane graph
CVG_graph <- plot_grid(p_CVG_fe, # fixed effects
                       p_CVG_re, # random effects
                       labels = c('A', 'B'))
CVG_graph

# save this out
ggplot2::ggsave(plot = CVG_graph,  # the graph we just made with plot_grid()
                filename = paste0(getwd(), '/graphs/Coefficient of variation of patch (Grass) Model_',
                                  gsub('[[:punct:]]', '_', Sys.time()), '.png'),
                width  = fig_w, # see, *very* lazy
                height = fig_h, 
                units = fig_u)

#tables
tab_model(CVG_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Coefficient of variation of patch (Grass)',
          file = paste0(getwd(), '/tables/Coefficient of variation of patch (Grass)_',               # see how this is now "tables"
                        gsub('[[:punct:]]', '_', Sys.time()), '.html'))

# TODO HL: why dupliate this again?
#TO DHL: To show the table directly in the pane of R without opening the file from the local directory.
tab_model(CVG_mod,
          ci.hyphen = ' to ',
          show.ngroups = TRUE,
          dv.labels = 'Coefficient of variation of patch (Grass)')

# TODO (DHL) save out the diagnostic plots
#Plot model to check model assumptions
plot_model(CVG_mod, type = 'diag')

#End for Saturday Night-Hillol.

# OK, but what comes next? I don't understand this collection of code below.
# TODO HD: please delete or add helpful comments.
#Those were the codes that were prepared beforehand. Then comes the latest codes with the recent changes. I have deleted those.Hillol. 


df %>% ggplot(aes(x = MPA_T)) +
  geom_histogram(binwidth = .05) +
  scale_x_log10() +
  facet_wrap(vars(MSA))




# CITE THE PACKGES
for(i in packages){
  print(citation(paste(i)))
}
