#### Aquacrop-R make_soils
# https://github.com/jrodriguez88/aquacrop-R
# Author: Rodriguez-Espinoza J.
# 2019

### Load packages
library(tidyverse)

### Read data from csv 

data <- read.csv("data/soil_to_aquacrop.csv")
id_name <- "soilname"
CN <- 72
REW <- 11
### Function to write . SOL files

make_soil_aquacrop <- function(id_name, data, CN, REW, fromSoilGrids = T, model_version = 6.1) {
    
    data <- as.data.frame(data)

    sink(paste0(id_name, ".SOL"), F)    
    cat(paste0(id_name, " AquaCrop soil file - by https://github.com/jrodriguez88"))
    cat('\n')
    cat(paste0("        ", model_version,"                 : AquaCrop Version (May 2018)"), sep = "\n")
    cat(paste0("       ", CN, "                   : CN (Curve Number)") , sep = "\n")
    cat(paste0("       ", REW, "                   : Readily evaporable water from top layer (mm)"), sep = "\n")
    cat(paste0("        ", nrow(data), "                   : number of soil horizons") , sep = "\n")
    cat(paste0("       -9                   : variable no longer applicable"), sep = "\n")
    cat(paste0("  Thickness  Sat   FC    WP     Ksat   Penetrability  Gravel  CRa       CRb           description"), sep = "\n")
    cat(paste0("  ---(m)-   ----(vol %)-----  (mm/day)      (%)        (%)    -----------------------------------------"), sep = "\n")
    write.table(data.frame(Thickness    = sprintf("%8.2f", data[["Thickness"]]    ),
                           Sat          = sprintf("%7.1f", data[["Sat"]]          ),
                           FC           = sprintf("%5.1f", data[["FC"]]           ),
                           WP           = sprintf("%5.1f", data[["WP"]]           ),
                           Ksat         = sprintf("%7.1f", data[["Ksat"]]         ),
                           Penetrability= sprintf("%10.0f",data[["Penetrability"]]),
                           Gravel       = sprintf("%9.0f", data[["Gravel"]]       ),
                           CRa          = sprintf("%13.6f",data[["CRa"]]          ),
                           CRb          = sprintf("%9.6f", data[["CRb"]]          ),
                           description  = sprintf("%16s",  data[["description"]]  )),
                row.names = F, quote = F, col.names = F)
    sink()
    
}

#make_soil_aquacrop(id_name, data, CN, REW, fromSoilGrids = T, model_version = 6.1)

### organize soil data
# Inf from API query
from_soilgrids_to_aquacrop <- function(id_name, soilgrids_data, Penetrability = 100) {
    
    # Depths of sl in soilgrids    
    dept_value <- tibble(sd1 = -0.025, 
                         sd2 = -0.1, 
                         sd3 = -0.225, 
                         sd4 = -0.45, 
                         sd5 = -0.8, 
                         sd6 = -1.5, 
                         sl1 = 0.0, 
                         sl2 = -0.05, 
                         sl3 = -0.15, 
                         sl4 = -0.3, 
                         sl5 = -0.6, 
                         sl6 = -1.0, 
                         sl7 = -2.0, 
                         xd1 = -0.2, 
                         xd2 = -0.5) %>%
        dplyr::select(contains("sl")) %>% 
        gather(sl, depth)
    
    
    ## transform data to aquacrop format
    data_inp <- soilgrids_data %>% 
        gather(sl, value, -soil_vars) %>% 
        spread(soil_vars, value) %>%
        left_join(dept_value, by="sl") %>%# rename(WCWP = WWP, WCST = AWCtS) %>%
        mutate(Penetrability = Penetrability,
               TKL = c(0, diff(abs(depth))),
               WCFC = WWP + AWCh1,
               SSKS = 75*24*10*((((1-(BLDFIE/2650))*100)-WCFC)/(WCFC)^2),   #Method developed by Suleiman and Ritchie (2001)
               STC = get_STC(SNDPPT, CLYPPT),
               CRa = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-0.3112 - SSKS*10^(-5)),
                               str_detect(STC, "Lo|SiLo|Si") ~ (-0.4986 + SSKS*9*10^(-5)),
                               str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-0.5677 - SSKS*4*10^(-5)),
                               str_detect(STC, "SiClLo|SiCl|Cl") ~ (-0.6366 + SSKS*8*10^(-4))),
               CRb = case_when(str_detect(STC, "Sa|LoSa|SaLo") ~ (-1.4936 + 0.2416*log(SSKS)),
                               str_detect(STC, "Lo|SiLo|Si") ~ (-2.1320 + 0.4778*log(SSKS)),
                               str_detect(STC, "SaCl|SaClLo|ClLo") ~ (-3.7189 + 0.5922*log(SSKS)),
                               str_detect(STC, "SiClLo|SiCl|Cl") ~ (-1.9165 + 0.7063*log(SSKS)))) %>%
        rename(WCWP = WWP, WCST = AWCtS, Gravel = CRFVOL) %>% 
        dplyr::select(TKL, WCST, WCFC, WCWP, SSKS, Penetrability, Gravel, CRa, CRb, STC) %>%
        setNames(c("Thickness", "Sat", "FC", "WP", "Ksat", "Penetrability", "Gravel", "CRa", "CRb", "description"))
    
    #CN: Curve number (dimensionless)
    CN <- data_inp[1,] %>% 
        mutate(CN = case_when(Ksat <= 10 ~ 85,
                              Ksat > 10 & Ksat <=50 ~ 80,
                              Ksat > 50 & Ksat <=250 ~ 75,
                              Ksat > 250 ~ 65)) %>% pull(CN)
    
    
    # REW: Readily Evaporable Water (mm)
    REW <- data_inp[1,] %>%
        mutate(REW_cal = (10*(FC - WP/2)*0.04),
               REW = case_when(REW_cal >=15 ~ 15, 
                               REW_cal < 0 ~ 0,
                               TRUE ~ REW_cal)) %>% pull(REW) %>% sprintf("%1.f", .)
    
    
    return(list(id_name = id_name, data = data_inp[-1,], CN = CN, REW = REW))
    
}
