#### Analize data 1er simulacion

### Load packages
library(tidyverse)
library(data.table)
library(lubridate)
library(skimr)

source("read_outputs_aquacrop.R")

#load("data/cal_set.RData")

#Name structure of project files 
filename_var <- c("proj1", "id_w", "cultivar", "cropfile", "proj2", "id_s", "crop_sys", "irri")
season_data <- map(.x = season_files, ~read_aquacrop_season(.x, path)) %>%
    bind_rows() %>% 
    mutate(File = str_replace(File, ".PRM", "")) %>%
    separate(File, filename_var, sep = "_")


obs_vs_sim <- season_data %>% 
    mutate(crop_sys = if_else(crop_sys=="AVT", "Potencial", "Secano"), 
           id_name = paste0(proj1, "_", id_w)) %>%
    dplyr::select(-c(proj1, id_w, cultivar, cropfile, proj2, id_s, irri)) %>%
    nest(-c(crop_sys, id_name)) %>%
    mutate(yield_sim = map_dbl(data, ~.x %>% 
                                   summarise(y = median(Yield)) %>%
                                   pull(y)),
           cycle_sim = map_dbl(data, ~.x %>% 
                                   summarise(y = median(Cycle)) %>%
                                   pull(y))) %>% 
    left_join(data_to_project, by = "id_name")



metrics_data <- obs_vs_sim %>% filter(crop_sys == "Secano") %>% 
    select(id_name, Region, Departamento, Tipo_Siembra, Rendimiento_tn_dm, yield_sim, Duracion_cultivo, cycle_sim) %>%
    left_join(obs_vs_sim %>% filter(crop_sys != "Secano") %>% 
    select(id_name, yield_sim, cycle_sim) %>% 
        rename(yield_sim_pot = yield_sim, cycle_sim_pot =  cycle_sim), by = "id_name") %>%
    setNames(c("id_name", "Region", "Departamento", "Tipo_Siembra", "yield_obs", "yield_sim", "cycle_obs", "cycle_sim", "yield_pot", "cycle_pot")) %>%
    filter(yield_sim>0.5) %>%
    gather(var, value, -c(id_name, Region, Departamento, Tipo_Siembra)) %>%
    mutate(Data = case_when(str_detect(var, "_obs") ~ "Observado",
                            str_detect(var, "_sim") ~ "Simulado",
                            str_detect(var, "_pot") ~ "Potencial"),
           Variable = if_else(str_detect(var, "yield"),  "Rendimiento", "Duracion_cultivo")) %>% select(-var) %>% 
    mutate(Data = factor(Data, levels = c("Observado", "Simulado", "Potencial")),
           value = if_else(Variable=="Rendimiento", value*1.25*700/46, value)) ### EN quintales/ manzana   1quintal = 46kg  1manzana = 0.7ha


metrics_data %>% spread(Data, value) %>% 
        group_split(Variable) %>% set_names(sort(unique(metrics_data$Variable))) %>%
    #filter(Variable == "Rendimiento") %>% 
    map(~.x %>% #group_by(Region) %>%
    summarise(n = n(), 
              rmse = sqrt(mean((Observado - Simulado)^2, na.rm = T)),
              nrmse = rmse/mean(Simulado, na.rm = T),
              d = 1 - ((sum((Observado - Simulado)^2, na.rm = T))/
                           sum((abs(Observado - mean(Simulado, na.rm = T)) +
                                    abs(Simulado - mean(Simulado, na.rm = T)))^2, na.rm = T)),
              E = 1 - ((sum((Observado - Simulado)^2, na.rm = T))/
                           sum((Simulado - mean(Simulado, na.rm = T))^2, na.rm = T)),
              rsq = summary(lm(Observado ~ Simulado))$r.squared))


#
#obs_vs_sim %>% filter(crop_sys == "Secano") %>% 
#    select(Region, Departamento, Tipo_Siembra, Rendimiento_tn_dm, yield_sim) %>%
#    gather(data, Rendimiento, -c(Region, Departamento, Tipo_Siembra)) %>%
#    mutate(data = if_else(data == "Rendimiento_tn_dm", "Observado", "Simulado")) %>%
#    ggplot(aes(Departamento, Rendimiento)) +
#    geom_boxplot(aes(fill = data)) +
#    facet_wrap(~Departamento, scales = "free_x")
#



metrics_data %>% 
    ggplot(aes(value)) +
    geom_boxplot(aes(x = Data, y = value, fill = Data)) +
    facet_grid(Variable~Region, scales = "free") +
    theme_bw()+
    theme(
        axis.text.x = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) + labs(y="Quint/Mz      -      Dias")

metrics_data  %>%
    ggplot(aes(Data, value)) + 
    stat_summary(aes(color=Data), fun.data = mean_cl_boot) +
    facet_grid(Variable~Region, scales = "free") +
    theme_bw()+
    theme(
        axis.text.x = element_blank(),
        legend.position="bottom",
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) 






