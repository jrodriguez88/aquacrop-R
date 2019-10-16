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
#dir.create(paste0(getwd(), "/outputs/weather"))
#path <- paste0(getwd(), "/outputs/weather/")
#id_name <- "test_name"
#lat <- 13.9
#alt <- 657
#co2_file <- "MaunaLoa.CO2"
#wth_data <- read.csv("data/weather_to_aquacrop.csv") %>% 
#    mutate(date = ymd(date))




make_weather_aquacrop <- function(path, id_name, wth_data, lat, alt, co2_file = "MaunaLoa.CO2") {
    
    stopifnot(require(sirad))
    
    ### Cal ETo
    ETo_cal <- function(wth_data, lat, alt, alt_ws = 2){
        
        ## Estimate clear sky transmissivity
        extraT <- extrat(lubridate::yday(wth_data$date), radians(lat))$ExtraTerrestrialSolarRadiationDaily
        
        ## cal trasmisivity 3% days
        #    tal <- cst(RefRad = wth_data$srad, days = wth_data$date, extraT = extraT, lat = radians(lat), perce = 5)
        
        ETo <- wth_data %>%
            mutate(
                es = sirad::es(tmax, tmin), 
                ea = es*rhum/100,
                #            vp = es-ea,
                extraT = extraT, 
                ETo = sirad::et0(tmax, tmin, ea, srad, 0.85, alt, wvel, alt_ws, extraT),
                ETo = case_when(is.na(ETo) ~ 0.611*exp(17.27*tmin/(tmin+237.3)),
                                TRUE ~ ETo)
            ) %>%
            pull(ETo)
        
    }
    
    ETo <- ETo_cal(wth_data, lat, alt)
    
    data <- wth_data %>%
        mutate(
            tmin  = case_when(is.na(tmin) ~ mean(wth_data$tmin, na.rm = T),
                           TRUE ~ tmin),
            tmax = case_when(is.na(tmax) ~ mean(wth_data$tmax, na.rm = T),
                            TRUE ~ tmax),
            rain = case_when(is.na(rain) ~ mean(wth_data$tmin, na.rm = T),
                             TRUE ~ rain)
        ) 
    
    ## Split data and write .ETo / .PLU / Tnx / .CLI files.
    
    # Climate file .CLI
    write_CLI <- function(id_name){
        sink(file = paste0(path, id_name, ".CLI"), append = F)   
        cat(paste(id_name, "- by https://github.com/jrodriguez88"), sep = "\n")
        cat(" 6.0   : AquaCrop Version (March 2017)", sep = "\n")
        cat(paste0(id_name, ".Tnx"), sep = "\n")
        cat(paste0(id_name, ".ETo"), sep = "\n")
        cat(paste0(id_name, ".PLU"), sep = "\n")
        cat(paste(co2_file), sep = "\n")
        
        sink()    
        
    }
    write_CLI(id_name)
    
    # Temperature file .Tnx
    write_Tnx <- function(id_name){
        sink(file = paste0(path , id_name, ".Tnx"), append = F)   
        cat(paste0(id_name, " : daily temperature data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
        cat("\n")
        cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
        cat(paste0("     ", day(min(wth_data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
        cat(paste0("     ", month(min(wth_data$date)), "  : First month of record"), sep = "\n")
        cat(paste0("  ", year(min(wth_data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
        cat("\n")
        cat(paste0("  Tmin (C)   TMax (C)", sep = "\n"))
        cat("=======================", sep = "\n")
        write.table(data.frame(tmin = sprintf("%10.1f", data$tmin),
                               tmax = sprintf("%10.1f", data$tmax)), 
                    row.names = F, quote = F, col.names = F)
        
        sink()
        
    }
    write_Tnx(id_name)
    
    write_PLU <- function(id_name){
        sink(file = paste0(path, id_name, ".PLU"), append = F)
        cat(paste0(id_name, " : daily rainfall data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
        cat("\n")
        cat(paste0("     1  : Daily records (1=daily, 2=10-daily and 3=monthly data)"), sep = "\n")
        cat(paste0("     ", day(min(wth_data$date)), "  : First day of record (1, 11 or 21 for 10-day or 1 for months)") , sep = "\n")
        cat(paste0("     ", month(min(wth_data$date)), "  : First month of record"), sep = "\n")
        cat(paste0("  ", year(min(wth_data$date)), "  : First year of record (1901 if not linked to a specific year)") , sep = "\n")
        cat("\n")
        cat(paste0("  Total Rain (mm)", sep = "\n"))
        cat("=======================", sep = "\n")
        writeLines(sprintf("%10.1f", data$rain))
        sink()
        
    }
    write_PLU(id_name)
    
    write_ETo <- function(id_name){
        sink(file = paste0(path, id_name, ".ETo"), append = F)
        cat(paste0(id_name, " : daily ETo data (", format(min(wth_data$date), "%d %B %Y"), " - ", format(max(wth_data$date), "%d %B %Y"), ")"))
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
    write_ETo(id_name)
    
    
}


#make_weather_aquacrop(path, id_name, wth_data, lat, alt, co2_file = "MaunaLoa.CO2")

 

