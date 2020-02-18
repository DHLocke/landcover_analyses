#calling necessary libraries
library(ggplot2)
library(purrr)
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

#read in shapefile
sf <- read_sf('D:/Macrobio/blockgroup_landcover/CBG_ALL_v20161014.shp')

#create dataframe from shapfile
df <- dplyr::select(as.data.frame(sf), -geometry)

#square the IVs
housage_squared <- df$HOUS_AGE^2
medinc_squared <- df$INC_MED_HS^2
percwhite_squared <- df$P_White^2
perchis_squared <- df$P_Hisp^2
percownocc_squared <- df$P_Own^2
trough_squared <- df$SDE_STD^2
popden_squared <- df$POPD_SQKM^2

#create natural logs of mean patch area and patch number (tree and grass)
logMPT <- log(df$MPA_T)
logMPG <- log(df$MPA_G)
logNPT <- log(df$NP_T)
logNPG <- log(df$NP_G)

#tibble of new variables for diagnostic plots
tib1 <- tibble(housage_squared,medinc_squared, percwhite_squared, perchis_squared,
                     percownocc_squared, trough_squared, popden_squared, logMPG, logMPT, 
                     logNPG, logNPT)

#Bind new columns from tibl to original dataframe and assign to new dataframe
df2 <- cbind(tib1, df)


#Attempt to iterate through plot as a function for the sake of ease but abandoned 
#after realizing I needed to manually adjust x and y limits for each diagnostic plot
# scatter_fun = function(x, y) {
#   ggplot(df2, aes_string(x = x, y = y ) ) +
#      geom_point() +
#      geom_smooth(method = "loess", se = FALSE, color = "grey74") +
#      theme_bw() + 
#     coord_cartesian(ylim = c(0, 2000001)) +
#     stat_cor(method = "pearson", label.x = 10000, label.y = 1000000)
#       
# }

#Plots used to generate scatters for each IV. DV changed as needed
a <- ggscatter(df2, x = 'housage_squared', y = 'NP_G', 
               add = "reg.line", conf.int = FALSE, 
               cor.coef = TRUE, cor.method = "pearson",
               xlab = "House Age Squared", ylab = "Grass Patches", 
               cor.coeff.args = list(label.x = 2000, label.y = 30000, label.sep = '\n'))
ggpar(a, ylim = c(0, 30000), xlim = c(0, 4000))


b <- ggscatter(df2, x = 'medinc_squared', y = 'NP_G', 
          add = "reg.line", conf.int = FALSE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Median Income Squared", ylab = "Grass Patches", 
          cor.coeff.args = list(label.x = 4000040000, label.y = 30000, label.sep = '\n'))
ggpar(b, ylim = c(0, 30000), xlim = c(0, 20000400001))

c <- ggscatter(df2, x = 'percwhite_squared', y = 'NP_G', 
          add = "reg.line", conf.int = FALSE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Percent White Squared", ylab = "Grass Patches", 
          cor.coeff.args = list(label.x = 0, label.y = 30000, label.sep = '\n'))
ggpar(c, ylim = c(0, 30000), xlim = c(0, 10000))

d <- ggscatter(df2, x = 'perchis_squared', y = 'NP_G', 
          add = "reg.line", conf.int = FALSE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Percent Hispanic Squared", ylab = "Grass Patches", 
          cor.coeff.args = list(label.x = 0, label.y = 30000, label.sep = '\n'))
ggpar(d, ylim = c(0, 30000), xlim = c(0, 10000))

e <- ggscatter(df2, x = 'percownocc_squared', y = 'NP_G', 
          add = "reg.line", conf.int = FALSE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Percent Owner Occupied Squared", ylab = "Grass Patches", 
          cor.coeff.args = list(label.x = 0, label.y = 30000, label.sep = '\n'))
ggpar(e, ylim = c(0, 30000), xlim = c(0, 10000))

f <- ggscatter(df2, x = 'trough_squared', y = 'NP_G', 
          add = "reg.line", conf.int = FALSE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Terrain Roughness Squared", ylab = "Grass Patches", 
          cor.coeff.args = list(label.x = 0, label.y = 30000, label.sep = '\n'))
ggpar(f, ylim = c(0, 30000), xlim = c(0, .10))

g <- ggscatter(df2, x = 'popden_squared', y = 'logMPG', 
          add = "reg.line", conf.int = FALSE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Population Density Squared", ylab = "Log Mean Grass Area", 
          cor.coeff.args = list(label.x = 1000, label.y = 10
, label.sep = '\n')) 
ggpar(g, ylim = c(-10, 10), xlim = c(0, 22268.18))


#get a quick histogram for looking at frequency of tree and grass P/A ratio
hist(df2$PAratio_T, 
     main="Frequency of P/A Ratio for Tree", 
     xlab="P/A Ratio", 
     border="blue", 
     col="white",
     xlim=c(0,35000),
     ylim = c(0, 150),
     las=1, 
     breaks=5238)







