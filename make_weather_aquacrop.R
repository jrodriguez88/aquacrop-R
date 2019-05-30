#### Aquacrop-R make_weather
# https://github.com/jrodriguez88/aquacrop-R
# Author: Rodriguez-Espinoza J.
# 2019

### Load packages
library(tidyverse)
library(sirad)
library(lubridate)

### read_data from csv. format (oryza)

### Cal ETo 
vp <- es(30.2, 24.3)
date <- make_date(1998,1,3)

et0(29.6, 24.7, vp, 20, 0.95, 10, 2, 2, NA, date, 3.5)


## Split data and write .ETo / .PLU / Tnx / .CLI files.


