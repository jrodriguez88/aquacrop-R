#### Aquacrop plot_applications
# https://github.com/jrodriguez88/aquacrop-R
# Author: Rodriguez-Espinoza J.
# 2020


# Group of functions to graph different uses and aplication of Aquacrop-R


# Plot for agroclimate forecast- yield forecast
#file_str <- c("clima", "cultivar", "soil", "crop_sys")
plot_agroclim_forecast <- function(season_data, id_name, file_str = NA, yield_units = "t/ha", x_breaks = "5 days"){
    
    if(is.na(file_str)){
        data <- season_data
    } else {
        data <- season_data %>% 
            mutate(date = make_date(Year1, Month1, Day1),
                   File = str_replace(File, ".PRM", "")) %>%
            separate(File, file_str, sep = "_") %>%
            mutate(crop_sys = ifelse(str_detect(crop_sys, pattern = "IRR"), "irrigated", crop_sys))
    }
    
    
    if(yield_units == "qq/mz"){
        data <- data %>% mutate(Yield = Yield*700/46)
    } else if(yield_units == "kg/ha") {
        data <- data %>% mutate(Yield = Yield*1000)
    }
    
    data %>%
        ggplot(aes(x = date, 
                   y = Yield, fill = crop_sys, group = interaction(crop_sys, date))) +
        #  geom_col() + 
        #  stat_summary(fun.y = mean, geom = "bar") +
        #  stat_summary(fun.data = mean_sdl, geom = "errorbar") +
        geom_boxplot() + 
        #  facet_wrap(cultivar ~.) +
        facet_grid(soil ~ cultivar, scales = "free") +
        #  scale_x_date(date_labels = "%b %d")+
        theme_bw() +
        theme(
            legend.position="bottom",
            #    legend.title = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
            strip.text = element_text(face = "bold")) +
        #  guides(fill=legend.title())
        labs(x= "Fecha",
             y= paste0("Rendimiento (", yield_units, ") - *(peso seco)"),
             title = paste0("Simulacion Agroclimatica - ", id_name),
             subtitle = "Modelo de cultivo: AquaCrop (V6) - http://www.fao.org/aquacrop/",
             fill = "Sistema de Cultivo: ", 
             caption = "Fuente: AquaCrop-R(https://github.com/jrodriguez88/aquacrop-R)") +
        scale_fill_manual(values = c(irrigated = "#33CC00", rainfed = "#E69F00"), 
                          labels = c("Irrigado", "Secano")) +
        scale_x_date(date_breaks = x_breaks , date_labels =  "%b %d",
                     limits = c(min(data$date), max(data$date)))
    
    
    
}

# Plor hidric demand to rise potential production
plot_agroclim_hidric <- function(season_data, id_name, file_str = NA, x_breaks = "5 days"){
    
    if(is.na(file_str)){
        data <- season_data
    } else {
        data <- season_data %>% 
            mutate(date = make_date(Year1, Month1, Day1),
                   File = str_replace(File, ".PRM", "")) %>%
            separate(File, file_str, sep = "_") %>%
            mutate(crop_sys = ifelse(str_detect(crop_sys, pattern = "IRR"), "irrigated", crop_sys))
    }
    
    
    data %>%
        ggplot(aes(x = date, 
                   y = Irri, color = soil, group = interaction(soil, date))) +
        stat_summary(fun.data = mean_cl_boot) +
        #  facet_wrap(cultivar ~.) +
        facet_grid(. ~ cultivar, scales = "free") +
        #  scale_x_date(date_labels = "%b %d")+
        theme_bw() +
        theme(
            legend.position="bottom",
            #    legend.title = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
            strip.text = element_text(face = "bold")) +
        #  guides(fill=legend.title())
        labs(x= "Fecha",
             y= paste0("Requerimientos hidricos (mm)"),
             title = paste0("Simulacion Agroclimatica - ", id_name),
             subtitle = "Modelo de cultivo: AquaCrop (V6) - http://www.fao.org/aquacrop/",
             color = "Suelo: ", 
             caption = "Fuente: AquaCrop-R(https://github.com/jrodriguez88/aquacrop-R)") +
        scale_color_viridis_d(option = "E") +
        scale_x_date(date_breaks = x_breaks , date_labels =  "%b %d", limits = c(min(data$date), max(data$date)))
    
    
    
}



theme_jre <- theme(
    legend.position="bottom",
    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold"))



