### Make project Calibration

#### Aquacrop make_projects
# https://github.com/jrodriguez88/aquacrop-R
# Author: Rodriguez-Espinoza J.
# 2019


### Load packages, path and functions

#library(tidyverse)
#library(data.table)
#library(lubridate)
#
#
## Path of aquacrop files
#aquacrop_files <- paste0(getwd(), "/data/aquacrop_files/")
#plugin_path <- paste0(getwd(), "/plugin1/")
#
#
## function to calculate HUH (growing thermal units) _ tbase,    topt,and thigh depends of crop
#HUH_cal <- function(tmax, tmin, tbase = 8, topt = 30, thigh = 42.5) {
#    
#    tav <- (tmin + tmax)/2
#    
#    h <- 1:24
#    
#    Td <- tav + (tmax - tmin)*cos(0.2618*(h - 14))/2 
#    
#    huh <- Td %>% enframe(name = NULL, "td") %>%
#        mutate(HUH = case_when(td <= tbase | td >= thigh ~ 0,
#                               td > tbase | td <= topt ~ (td - tbase)/24,
#                               td > topt | td < thigh ~ (topt-(td - topt)*(topt - tbase)/(thigh - topt))/24))
#    
#    sum(huh$HUH)   
#    
#} 
#
#
#### set locality and planting_window dates (<= 1 month). require wth_data 
#clim_data <- read.csv("data/weather_to_aquacrop.csv") %>% 
#    mutate(date = ymd(date)) %>% 
#    mutate(HUH = map2_dbl(tmax, tmin, HUH_cal))
##

## Set sowing dates. planting_window dates (<= 40  days), 
#sowing_date = ymd("2016-06-24")
make_project_by_date <- function(id_name, sowing_dates, cultivar, soil, clim_data, max_crop_duration = 150, aquacrop_files, plugin_path){
    
    ## Create sowing dates vector, use when requiere 1 date 
    #    sowing_dates  <- c(sowing_date - (5:1), sowing_date + (0:4))
##### add function // eval inputs    
    ### load aquacrop files
    clim_file <- list.files(aquacrop_files, pattern = paste0(id_name, ".CLI")) %>% str_remove(".CLI")
    co2_file <-  list.files(aquacrop_files, ".CO2")
    crop_file <- list.files(aquacrop_files, pattern = paste0(cultivar, ".CRO"))
    irri_file <- list.files(aquacrop_files, ".IRR") %>% c(., "rainfed")
    man_file <-  list.files(aquacrop_files, ".MAN")
    soil_file <- list.files(aquacrop_files, paste0(soil, ".SOL"))
    ini_file <-  list.files(aquacrop_files, ".SW0")
    proj_file <- list.files(aquacrop_files, ".PRM")
    
    ### Default parameters,  
    def_params <- read_lines(paste0(aquacrop_files, proj_file), skip = 6, n_max = 21) 
    
    
    ### Create multiple combinations of params
    params <- expand.grid(aquacrop_files,
                          clim_file,
                          co2_file,
                          crop_file,
                          irri_file, 
                          man_file,
                          soil_file,
                          ini_file,
                          max_crop_duration,
                          sowing_dates) %>% 
        as_tibble() %>%
        setNames(c("aquacrop_files",
                   "clim_file",
                   "co2_file",
                   "crop_file",
                   "irri_file", 
                   "man_file",
                   "soil_file",
                   "ini_file",
                   "max_crop_duration",
                   "sowing_date"))
    
    
    ## Function to calculate and create crop growing cycles
    cal_cycles_project <- function(clim_data,
                                   aquacrop_files,
                                   clim_file,
                                   co2_file,
                                   crop_file,
                                   irri_file, 
                                   man_file,
                                   soil_file,
                                   ini_file,
                                   max_crop_duration,
                                   sowing_date) {
        
        # path files
        path_files <- aquacrop_files %>% str_replace_all(pattern = "/", replacement = "\\\\")
        
        ### extract "GDDays: from sowing to maturity" from CRO_file
        gdd_mt <- read_lines(file = paste0(aquacrop_files, crop_file)) %>%
            str_subset("GDDays: from sowing to maturity|GDDays: from transplanting to maturity") %>% 
            str_extract("[0-9]+") %>% as.numeric
        
        
        # calculate crop duration 
        crop_duration <- clim_data %>% 
            dplyr::filter(date >= sowing_date,
                          date <= sowing_date + max_crop_duration) %>%
            mutate(sum_gdd = cumsum(HUH)) %>%
            dplyr::filter(sum_gdd<= gdd_mt) %>% 
            count() %>% pull(n)
        
        # Calculate numeric dates
        first_day <- as.numeric(sowing_date - make_date(1900, 12, 31))
        last_day <- first_day + crop_duration
        mat_date <- as.Date(last_day, origin = make_date(1900, 12, 31))
        
        #Write grow cycles
        path_data <- function(){
            
            cat(paste0(first_day, "    : First day of simulation period - ", format(sowing_date, "%d %b %Y")))
            cat('\n')
            cat(paste0(last_day,  "    : Last day of simulation period - ",  format(mat_date, "%d %b %Y")))
            cat('\n')
            cat(paste0(first_day, "    : First day of cropping period - " , format(sowing_date, "%d %b %Y")))
            cat('\n')
            cat(paste0(last_day,  "    : Last day of cropping period - "  , format(mat_date, "%d %b %Y")))
            cat('\n')    
            cat("-- 1. Climate (CLI) file", sep = '\n')
            cat(paste0(clim_file, ".CLI"), sep = '\n')
            cat(paste0(path_files), sep = '\n')
            cat("1.1 Temperature (TMP) file", sep = '\n')
            cat(paste0(clim_file, ".Tnx"), sep = '\n') 
            cat(paste0(path_files), sep = '\n')
            cat("1.2 Reference ET (ETo) file", sep = '\n')
            cat(paste0(clim_file, ".ETo"), sep = '\n')
            cat(paste0(path_files), sep = '\n')
            cat("1.3 Rain (PLU) file", sep = '\n')
            cat(paste0(clim_file, ".PLU"), sep = '\n')
            cat(paste0(path_files), sep = '\n')
            cat("1.4 Atmospheric CO2 (CO2) file", sep = '\n')
            cat(paste(co2_file), sep = '\n')
            cat(paste0(path_files), sep = '\n')
            cat("-- 2. Crop (CRO) file", sep = '\n')
            cat(paste(crop_file), sep = '\n')
            cat(paste0(path_files), sep = '\n')
            cat("-- 3. Irrigation (IRR) file", sep = '\n')
            if(irri_file=="rainfed"){
                cat("(None)", sep = '\n')
                cat("(None)", sep = '\n')
            } else {
                cat(paste(irri_file), sep = '\n')
                cat(paste0(path_files), sep = '\n')
            }
            cat("-- 4. Management (MAN) file", sep = '\n')
            cat(paste(man_file), sep = '\n')
            cat(paste0(path_files), sep = '\n')
            cat("-- 5. Soil profile (SOL) file", sep = '\n')
            cat(paste(soil_file), sep = '\n')
            cat(paste0(path_files), sep = '\n')
            cat("-- 6. Groundwater (GWT) file", sep = '\n')
            cat("(None)", sep = '\n')
            cat("(None)", sep = '\n')
            cat("-- 7. Initial conditions (SW0) file", sep = '\n')
            cat(paste(ini_file), sep = '\n')
            cat(paste0(path_files), sep = '\n')
            cat("-- 8. Off-season conditions (OFF) file", sep = '\n')
            cat("(None)", sep = '\n')
            cat("(None)", sep = '\n')
        }
        
        list(capture.output(path_data()))
        
    }
    
    
    ## Function to compute all runs for params table
    runs_cal <- function(params, clim_data) {
        
        params %>% mutate(runs = cal_cycles_project(clim_data, 
                                                    aquacrop_files,
                                                    clim_file,
                                                    co2_file,
                                                    crop_file,
                                                    irri_file, 
                                                    man_file,
                                                    soil_file,
                                                    ini_file,
                                                    max_crop_duration,
                                                    sowing_date)) 
        
    }
    
    sim_cycles <- split(params, 1:nrow(params)) %>% 
        map(., ~runs_cal(., clim_data)) %>%
        bind_rows() 
    
    
    ## Write PRM files
    write_projects <- function(sim_cycles, path, def_params, soil){
        
        #    description <-  paste(unique(sim_cycles$crop_file), 
        #                       unique(sim_cycles$clim_file),
        #                       unique(sim_cycles$soil_file),
        #                       unique(sim_cycles$irri_file), sep = " - ")
        
        prm_name <- paste0(unique(sim_cycles$clim_file), "_",
                           unique(sim_cycles$crop_file), "_",
                           soil, "_",
                           unique(sim_cycles$irri_file)) %>% 
            str_replace_all(pattern = "[.]+", replacement = "") %>%
            paste0(., ".PRM")
        
        suppressWarnings(dir.create(paste0(path, "/", "LIST")))
        
        sink(file = paste(path, "LIST", prm_name, sep = "/"), append = F)
        cat(paste("by https://github.com/jrodriguez88"))
        cat('\n')
        cat("6.0       : AquaCrop Version (March 2017)")
        cat('\n')
        writeLines(sim_cycles$runs[[1]][1:4])
        writeLines(def_params)
        writeLines(sim_cycles$runs[[1]][-c(1:4)])
        walk(.x=sim_cycles$runs[-1], ~writeLines(.x))
        sink()    
        
    }
    
    map(.x = split(sim_cycles, 
                   list(sim_cycles$crop_file, 
                        sim_cycles$irri_file, 
                        sim_cycles$soil_file)),
        ~write_projects(.x, plugin_path, def_params, soil))
    
    #    toc()
    #25.57 sec elapsed by 1 crop, 
}

#make_project_by_date("AVT_14", sowing_date, 130, clim_data, aquacrop_files, plugin_path)



#tic()
#system("plugin/ACsaV60.exe")
#toc()
# 1400.54 sec elapsed 
# set: 1 climate, 
#      2 crops, 6 soils, 2 irri, 21 years , 6 planting dates/year (by week)
# 3024 simulations


#load("data/eval_set.RData")
#set.seed(1234)
#test_set <- sample_n(data_to_project, 10) 
#data_to_project %>%
#data_to_project %>% slice(1:3) %>% 
#    mutate(to_project = pmap(list(x = id_name, y = Fecha_Siembra, z = clim_data, k = crop),
#                             function(x,y,z,k) list(id_name = x, 
#                                                    sowing_date = y, 
#                                                    clim_data = z,
#                                                    cultivar = k
#                                                    ))) %>% 
#    pull(to_project) %>%
#    map(~make_project_by_date(.x$id_name, .x$sowing_date, .x$cultivar, 130, .x$clim_data, aquacrop_files, plugin_path, "id2"))
#
#tic()
#system("plugin/ACsaV60.exe")
#toc()
## 880 sec elapsed  - cal_set
#
#
#### Make project_historic    
#
#
#
#load("data/AVT_set.RData")
#
#set.seed(1234)
#test_data <- data_to_project_ %>% filter(Region %in% c("RACCS", "II", "IV", "V")) %>% group_split(Departamento) %>% 
#        map(~sample_n(.x, size = 3)) %>% bind_rows()

## Funtion to calculate  dates from window sowing in weather series
#make_dates
make_hist_dates <- function (imonth = 6, fmonth = 8, clim_data, date_breaks = 5) {
    
    range_data <- year(range(clim_data$date))
    
    dates_base <- seq.Date(make_date(month = imonth, day=1), make_date(month = fmonth+1, day=1), by = date_breaks)
    
    map(dates_base,  ~make_date(year=range_data[1]:(range_data[2]-1), month = month(.x), day = day(.x)))
    
    
}


#to_aquacrop_historic <- test_data %>% select(id_name, Region, Departamento, Municipio, lat, lon, crop, clim_data) %>%
#    mutate(sowing_dates = map(clim_data, ~make_dates(clim_data = .x))) %>%
#    unnest(sowing_dates, .preserve = c(clim_data, crop)) %>% mutate(id2 = paste0("id", 1:399))
#
#to_aquacrop_historic %>% split(., rep(1:4, length.out = nrow(.), each = ceiling(nrow(.)/4))) %>% 
#    set_names(paste0(getwd(), "/plugin", 1:4,"/")) %>% bind_rows(.id = "plugin_path") %>%
#    #data_to_project %>%# slice(1:3) %>% +
#    mutate(to_project = pmap(list(x = id_name, y = sowing_dates, z = clim_data, k = crop, m = id2, n = plugin_path),
#                             function(x,y,z,k,m, n) list(id_name = x, 
#                                                    sowing_dates = y, 
#                                                    clim_data = z,
#                                                    cultivar = k,
#                                                    id2 = m,
#                                                    plugin_path = n
#                             ))) %>% 
#    pull(to_project) %>%
#    map(~make_project_by_date(.x$id_name, .x$sowing_dates, .x$cultivar, 130, .x$clim_data, aquacrop_files, .x$plugin_path, .x$id2))


# Function to calculate dates for simulation// clim_data must to contain date colunm
#star_sow <- c(4,1)   #c(month, day)
#end_sow <- c(5,3)  
sowing_dates_cal <- function(start_sow, end_sow, clim_data, by = "weeks") {
    
    start_sowing_date <- make_date(month = start_sow[1], day = start_sow[2]) %>% yday
    end_sowing_date <- make_date(month = end_sow[1], day = end_sow[2]) %>% yday
    
    seq.Date(range(clim_data$date)[1], 
             range(clim_data$date)[2], by=by) %>%
        enframe(name = NULL, value = "sow_dates") %>%
        filter(yday(sow_dates) >= start_sowing_date, 
               yday(sow_dates) <= end_sowing_date) %>% pull(sow_dates)
}


## Fucntion to convert resampling outputs (by Esquivel) to aquacrop-R format

from_resampling_to_aquacrop <- function(data_resampling, localidad, crop, soil, start_sow = 5, id_esc = NULL, get_sample = 25, tbase = 8, date_breaks = 5){
    message("Hay cambios en esta funcion, posiblemnete necesite modificar los argumentos")
    
    ## Read data from resampling + arguments    
    to_aquacropR <- data_resampling$data[[1]] %>%
        mutate(data = map(data, ~.x %>% 
                              rename(rain = prec) %>% 
                              mutate(date=make_date(year, month, day))%>%
                              select(-c(year, month, day))))
    
    ###extract initial dates from first resampling scenarie
    # star_date <- 
    
    start_sow <- c(start_sow,1)   #c(month, day)
    end_sow <- c(start_sow+1,2)  
    
    
    ## Conditional to sample data    
    if(is.na(get_sample)){
        data <- to_aquacropR
    } else {
        data <- to_aquacropR  %>% sample_n(.,  size = get_sample)
    }
    
    
    op <- data %>%
        mutate(clim_data = map(data, ~.x %>% 
                                   mutate(HUH = ((tmax + tmin)/2) - tbase))) %>%
        mutate(sowing_dates = map(clim_data, ~sowing_dates_cal(start_sow, end_sow, .x, by = date_breaks)),
               crop = crop,
               soil = soil, 
               id_name = paste0(localidad, id, "_", id_esc)) %>% 
        #data_to_project %>%# slice(1:3) %>% +
        mutate(to_project = pmap(list(x = id_name, 
                                      y = sowing_dates,
                                      z = clim_data, 
                                      k = crop, 
                                      m = soil,
                                      n = plugin_path),
                                 function(x,y,z,k,m, n) list(id_name = x, 
                                                             sowing_dates = y, 
                                                             clim_data = z,
                                                             cultivar = k,
                                                             plugin_path = n,
                                                             soil = m)))
}



                      