#### Aquacrop-R make_weather
# https://github.com/jrodriguez88/aquacrop-R
# Author: Rodriguez-Espinoza J.
# 2019

### Load packages
library(tidyverse)
library(data.table)
library(sirad)
library(lubridate)

### read_data from csv. format (oryza)
path <- paste0(getwd(), "/outputs/")
lat <- 3.5
lon <- -75.5
alt <- 22
wvel <- 2
loc <- "BOGU"
co2_file <- "MaunaLoa.CO2"

wth_data <- fread("data/Boerasire_Region3.txt", col.names = c("rain", "srad", "tmax", "tmin")) %>% as_tibble() %>%
    mutate(date = seq.Date(make_date(1998, 1, 1),
                           make_date(2018, 12, 31), "days")) %>%
    select(date, everything())

### Cal ETo

ETo_cal <- function(wth_data, lat, lon, alt, wvel){
    
    ## Estimate clear sky transmissivity
    extraT <- extrat(lubridate::yday(wth_data$date), lat)$ExtraTerrestrialSolarRadiationDaily
    
    ## cal trasmisivity 3% days
    tal <- cst(wth_data$srad, wth_data$date, radians(lat), extraT, 3)
    
    ETo <- wth_data %>%
        mutate(
            vp = sirad::es(tmax, tmin), 
            extraT = extraT, 
            ETo = sirad::et0(tmax, tmin, vp, srad, tal, alt, winds, 2, extraT)
        ) %>%
        pull(ETo)
    
}

ETo <- ETo_cal(wth_data, lat, lon, alt, wvel)

## Split data and write .ETo / .PLU / Tnx / .CLI files.

# Climate file .CLI
write_CLI <- function(path){
    sink(file = paste0(path, loc, ".CLI"), append = F)   
cat(paste(loc), sep = "\n")
cat(" 6.0   : AquaCrop Version (March 2017)", sep = "\n")
cat(paste0(loc, ".Tnx"), sep = "\n")
cat(paste0(loc, ".ETo"), sep = "\n")
cat(paste0(loc, ".PLU"), sep = "\n")
cat(paste(co2_file), sep = "\n")

sink()    

}
write_CLI(path)

# Temperature file .Tnx
write_Tnx <- function(path){
    sink(file = paste0(path, loc, ".Tnx"), append = F)   
cat(paste0(loc, " : daily temperature data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
cat("\n")
cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
cat(paste0("     ", day(min(wth_data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
cat(paste0("     ", month(min(wth_data$date)), "  : First month of record"), sep = "\n")
cat(paste0("  ", year(min(wth_data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
cat("\n")
cat(paste0("  Tmin (C)   TMax (C)", sep = "\n"))
cat("=======================", sep = "\n")
write.table(data.frame(tmin = sprintf("%10.1f", wth_data$tmin),
                       tmax = sprintf("%10.1f", wth_data$tmax)), 
            row.names = F, quote = F, col.names = F)

sink()

}
write_Tnx(path)

write_PLU <- function(path){
    sink(file = paste0(path, loc, ".PLU"), append = F)
cat(paste0(loc, " : daily rainfall data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
cat("\n")
cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
cat(paste0("     ", day(min(wth_data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
cat(paste0("     ", month(min(wth_data$date)), "  : First month of record"), sep = "\n")
cat(paste0("  ", year(min(wth_data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
cat("\n")
cat(paste0("  Total Rain (mm)", sep = "\n"))
cat("=======================", sep = "\n")
writeLines(sprintf("%10.1f", wth_data$rain))
sink()

}
write_PLU(path)

write_ETo <- function(path){
    sink(file = paste0(path, loc, ".ETo"), append = F)
    cat(paste0(loc, " : daily ETo data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
    cat("\n")
    cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
    cat(paste0("     ", day(min(wth_data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
    cat(paste0("     ", month(min(wth_data$date)), "  : First month of record"), sep = "\n")
    cat(paste0("  ", year(min(wth_data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
    cat("\n")
    cat(paste0("  Average ETo (mm/day)", sep = "\n"))
    cat("=======================", sep = "\n")
    writeLines(sprintf("%10.1f", ETo))
    sink()
    
}
write_ETo(path) 
