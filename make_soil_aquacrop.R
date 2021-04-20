#### Aquacrop-R make_soils
# https://github.com/jrodriguez88/aquacrop-R
# Author: Rodriguez-Espinoza J.
# 2019

### Load packages
#library(tidyverse)

### Read data from csv 

#data <- read.csv("data/soil_to_aquacrop.csv")
#id_name <- "soilname"
#CN <- 72
#REW <- 11
### Function to write . SOL files

make_soil_aquacrop <- function(path, id_name, data, CN, REW, model_version = 6.1) {
    
    data <- as.data.frame(data)

    sink(paste0(path, "/", id_name, ".SOL"), F)    
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

#make_soil_aquacrop(id_name, data, CN, REW, model_version = 6.1)

### organize soil data
# Inf from API query


#https://cran.r-project.org/web/packages/soiltexture/vignettes/soiltexture_vignette.pdf
### 'get_STC' function to get Soil Texture Class from soil sand, clay content.. based USDA system class
get_STC <- function(S, C, sysclass="USDA") {
    stopifnot(require(soiltexture))
    
    Si <- 100-(S+C)
    dat <- data.frame(SAND=S, CLAY=C, SILT=Si)
    
    STC <- TT.points.in.classes(
        tri.data = dat,
        class.sys = paste0(sysclass, ".TT"),
        PiC.type = "t"
    )
    
    return(STC)
    
}
