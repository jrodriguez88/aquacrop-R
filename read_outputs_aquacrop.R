#### Aquacrop read_outputs_aquacrop
# https://github.com/jrodriguez88/aquacrop-R
# Author: Rodriguez-Espinoza J.
# 2019

### Load packages
library(tidyverse)
library(data.table)
library(lubridate)

### Working directory
path <- paste0(getwd(), "/plugin/OUTP/")

# season files
season_files <- list.files(path, pattern = "season")

# daily files
daily_files <- list.files(path, pattern = "day")

###Name variables season/daily files. Name string structure
#filename_var <- c("locality", "region", "crop", "soil", "crop_sys")


### Function to read season project output
read_aquacrop_season <- function(file, path){
    
    names <- read_lines(paste0(path, file))[[3]] %>%
        str_trim() %>%
        str_split(pattern = "[ ]+") %>%
        flatten_chr() %>% 
        c(., "File")
    
    data <- fread(paste0(path, file), skip = 4) %>%
        setNames(names)
    
    return(data)
    
}
#data_season <- read_aquacrop_season(season_files, path)

### Function to read daily project output
read_aquacrop_day <- function(file, path){
    
    day_file <- read_lines(paste0(path, file))
    
    var_names <- read_lines(paste0(path, file))[[4]] %>% 
        str_replace_all("(?<=[[:alpha:]]) (?=\\d+)", "") %>% 
        str_trim() %>%
        str_split(pattern = "[ ]+") %>%
        flatten_chr() %>% c("Run", .)
    
    find_run <- day_file %>%
        str_detect("Run:") %>%
        which()+2
    
    nlines <- c(find_run[-1], length(day_file))-find_run-2
    
    arg_list <- list(file= paste0(path, file),
                     skip = find_run, 
                     nrows = nlines)
    
    # Read_pmap
    dat <- suppressWarnings(pmap(arg_list, fread)) %>%
        bind_rows(.id = "run") %>%
        setNames(var_names)
    
    return(dat)
    
}

### Function to summary and plot


####read_aquacrop_season(file, path)
###season_data <- map(.x = season_files, ~read_aquacrop_season(.x, path)) %>%
###    bind_rows() %>% 
###    mutate(File = str_replace(File, ".PRM", "")) %>%
###    separate(File, filename_var, sep = "_")
###
###daily_data <- map(.x = daily_files, ~read_aquacrop_day(.x, path)) %>%
###    set_names(daily_files) %>%
###    bind_rows(.id = "File") %>% 
###    mutate(File = str_replace(File, "PRMday.OUT", "")) %>%
###    separate(File, filename_var, sep = "_")
###
##### Season plots
###
###season_data %>% 
###    select(locality, Yield, BioMass, Cycle, Rain, crop_sys) %>% 
###    gather("var", "value", -c(locality, crop_sys)) %>% ggplot(aes(value)) +
###    geom_histogram(bins = 10, color="grey") + facet_wrap(var ~., scales = "free") + 
###    theme_classic()
###
###season_data %>% mutate(date = make_date(Year1, Month1, Day1)) %>%
###    select(locality, Yield, BioMass, Cycle, Rain, crop_sys) %>% 
###    gather("var", "value", -c(locality, crop_sys)) %>% 
###    ggplot(aes(locality, value)) +
###    geom_boxplot(aes(color=crop_sys)) + 
###    facet_wrap(var ~., scales = "free") + 
###    theme_classic()
###
###season_data %>% mutate(date = make_date(Year1, Month1, Day1)) %>%
###    select(date, locality, soil, Yield, BioMass, Cycle, Rain, crop_sys) %>% 
###    gather("var", "value", -c(date, locality, soil, crop_sys)) %>% 
###    ggplot(aes(year(date), value)) +
###    stat_summary(aes(color = crop_sys), fun.data = mean_sdl, position=position_jitter()) + 
###    facet_wrap(var ~ ., scales = "free") + 
###    theme_bw()
###
###season_data %>% mutate(date = make_date(Year1, Month1, Day1)) %>%
###    select(date, locality, soil, Yield, BioMass, Tr,ExpStr, StoStr,TempStr, crop_sys) %>% 
###    gather("var", "value", -c(date, locality, soil, crop_sys)) %>% 
###    ggplot(aes(crop_sys, value)) +
###    geom_boxplot(aes(fill = soil)) + 
###    facet_wrap(var ~ ., scales = "free") + 
###    theme_classic()
