# load all libraries here in one place, 
# rather than loading repeatedly for each .Rmd. 
# can run this code in an .Rmd chunk by typing > source("scripts/libraries.R")


if (!require('renv')) install.packages('renv')


if (!require('pacman')) install.packages('pacman'); library('pacman')

# p_update(update = FALSE)  #Tells you which packages are out of date
# p_update()  #actually updates the out of date packages

pacman::p_load("tidyverse",
               "ggpubr",
               "ggthemes",
               "here",
               "snotelr",
               "dataRetrieval",
               "patchwork",
               "renv",
               "geosphere",
               "dataRetrieval")
### NOTE FROM BELLA ####
#  Use renv to clear create a reproducible environment
# run ??renv::renv for an intro the package.

# You can leave this stuff commented out.
# renv::init()
# Now when others open this project, renv will automatically bootstrap itself
# and download the appropriate version of renv. You may be prompted to down and 
# install all packages by running renv::restore()

