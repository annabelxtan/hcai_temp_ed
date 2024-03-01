#Packages to load

############ 1. Packages on CRAN ############

# list of packages to use (in alphabetical order and 5 per row)
# If new packages add here in alphabetical order

packages = c('dplyr', 'readxl', 'here', 'lubridate', 'tidyverse',
             'ggplot2', 'data.table', 'tmap', 'dlnm', 'splines', 'survival')

#tmap packages
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

library(tidyverse)
library(ggplot2)      # For plotting
library(tidycensus)   # For downloading Census data
library(tmap)         # For creating tmap
library(tmaptools)    # For reading and processing spatial data related to tmap
library(dplyr)        # For data wrangling
library(sf)           # For reading, writing and working with spatial objects
library(splitstackshape)

# load packages
invisible(lapply(packages, require, character.only = TRUE, quietly=TRUE))
