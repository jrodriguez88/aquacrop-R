#### Aquacrop agroclim_forecaster
# https://github.com/jrodriguez88/aquacrop-R
# Authors: Rodriguez-Espinoza J. // Esquivel A.
# 2020


# Group of functions to connect probabilistic seasonal forecast with Aquacrop model,  using  Aquacrop-R



#blind fucntion to load requeriments
load_agroclim_requeriments <- function(){
    
    
### In developing functions 
    source("https://raw.githubusercontent.com/jrodriguez88/csmt/master/get_data/get_data_nasapower.R", encoding = "UTF-8")
    source("https://raw.githubusercontent.com/jrodriguez88/csmt/master/get_data/get_data_soilgrids.R", encoding = "UTF-8")
    source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_soil_aquacrop.R", encoding = "UTF-8")
    #    source("https://raw.githubusercontent.com/jrodriguez88/ciat_tools/master/remuestreo_mod.R", encoding = "UTF-8")
    source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_weather_aquacrop.R", encoding = "UTF-8")
    source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/make_project_by_date.R", encoding = "UTF-8")
    source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/read_outputs_aquacrop.R", encoding = "UTF-8")
    source("https://raw.githubusercontent.com/jrodriguez88/aquacrop-R/master/plot_applications.R", encoding = "UTF-8")
    
    inpack <<- function(pack){
        new_pack <- pack[!(pack %in% installed.packages()[, "Package"])]
        if (length(new_pack)) 
            install.packages(new_pack, dependencies = TRUE)
        sapply(pack, require, character.only = TRUE)
    }
    
}

### For CAF - abril- 2020
crear_directorios <- function(path = ""){ 

directorio <<- paste0(getwd(), path) 
directorio_datos <<- paste0(directorio, "/data/")
directorio_resultados <<- paste0(directorio, "/resultados/")
aquacrop_files <<- paste0(directorio, "/aquacrop_files/")
plugin_path <<- paste0(directorio, "/plugin/")

}

#########################################################
### jrodriguez88 functions
## Graficar Remuestreo 
plot_prob_forecast <- function(pronostico, id_label = NULL){
    
    pronostico %>% mutate(Type = factor(Type, c("above", "normal", "below")),
                          Season = factor(Season, c('DJF', 'JFM', 'FMA', 'MAM', 'AMJ', 'MJJ', 'JJA', 'JAS', 'ASO', 'SON', 'OND', 'NDJ'))) %>%
        ggplot(aes(x = Season, y = Prob, fill = Type)) + 
        geom_col(position = "dodge", color="darkgray") +
        theme_minimal() +
#        scale_fill_manual(values = c(above = "blue", normal = "lightgreen", below = "red"), 
        scale_fill_manual(values = c(above = "chartreuse4", normal = "goldenrod1", below = "saddlebrown"), 
                          labels = c("Arriba de lo Normal", "Cercano a lo Normal", "Debajo de lo Normal")) +
        labs(title = "Prediccion Climatica Estacional", 
             subtitle = id_label,
             x = "Trimestre",
             y = "Probabilidad (%)", 
             fill = "Categoria Terciles: ") #+
    #   theme(
    #     legend.position="bottom") 
    # 
    
}

plot_resampling <- function(data_resampling, weather_data, id_label = NULL, stat = "mean") {
    
    #Set Names and labels  
    var_name = c("rain", "prec", "srad", "tmin", "tmax", "rhum", "wvel")
    var_label = paste(var_name, c('(mm)', '(mm)', '(MJ/m²d)', '(°C)', '(°C)', '(%)', '(m/s)'))
    names(var_label) <- var_name
    
    #Function to summarize daily to monthly data 
    to_monthly <- function(data, ...){
        data %>% 
            group_by(year, month) %>%
            summarise(prec = sum(prec, ...), 
                      tmin = mean(tmin, ...), 
                      tmax = mean(tmax, ...), .groups = 'drop' 
                      #            srad = mean(srad), 
                      #            rhum = mean(rhum),
                      #            wvel = mean(wvel)
            ) %>% #write.csv("climate_data_monthly.csv")
            ungroup() 
    }
    
    #Convert historic data to monthly data  
    monthly_data <- to_monthly(weather_data, na.rm = T)  
    
    #Historic climatological means  
    data_summary <- monthly_data %>% 
        select(-year) %>% group_by(month) %>% summarise_all(stat) %>% 
        pivot_longer(cols = -c(month), names_to = "var", values_to = "value_clim")
    
    #Daily scenaries to monthly     
    data_escenarios <- data_resampling$data[[1]]$data %>% bind_rows(.id = "id") %>%
        nest(data = -id) %>% mutate(data = map(data, ~to_monthly(.x))) %>%
        unnest(data) %>%
        #    dplyr::select(-c(year)) %>%
        pivot_longer(cols = -c(year, month, id), names_to = "var", values_to = "value")
    
    data_plot <- data_escenarios %>% left_join(data_summary) %>% mutate(date = make_date(year, month, 1))
    
    
    #  data_esc_min <- data_resampling$Esc_Type[[1]]$data[[4]] %>% 
    #    to_monthly() %>%  #dplyr::select(-c(year)) %>%
    #    pivot_longer(cols = -c(year, month), names_to = "var", values_to = "value") %>% 
    #    mutate(date = make_date(year, month, 15))
    #  
    #  data_esc_max <- data_resampling$Esc_Type[[1]]$data[[1]] %>% 
    #    to_monthly() %>%  #dplyr::select(-c(year)) %>%
    #    pivot_longer(cols = -c(year, month), names_to = "var", values_to = "value") %>% 
    #    mutate(date = make_date(year, month, 15)) 
    
    #Plot seasonal forecast
    ggplot() +
        #    geom_jitter(data = data_escenarios, aes(x = month, y = value, group = month)) +
        #    geom_jitter(data = data_plot,
        #                aes(date, value), color = "grey", alpha = 0.7, width = 5) +
        geom_boxplot(data = data_plot, aes(x = date, y = value, fill = var, group = month), alpha = 0.7) +
        geom_line(data = data_plot,
                  aes(date, value_clim, color = "Normal_Climatologica"),
                  linetype = "twodash", size = 0.80) +
        geom_point(data = data_plot,
                   aes(date, value_clim), color = "red") +
        #    geom_line(data = data_esc_min, 
        #              aes(month, value, color = "Rango_probable"),  
        #              linetype = "twodash", size = 0.50) +
        #    geom_line(data = data_esc_max,
        #              aes(month, value),
        #              color = "red", linetype = "twodash", size = 0.50) +
        facet_wrap(var ~ ., scales = "free", labeller = labeller(var = var_label)) +
        #    scale_x_continuous(labels = function(x) month.abb[x], breaks = 1:12) +
#        scale_fill_manual(values = c(prec = "#619CFF", tmax = "orange1", tmin = "gold2"),
        scale_fill_manual(values = c(prec = "#619CFF", tmax = "orangered3", tmin = "orange3"),                  
                          labels= c("Precipitacion", "Temperatura Maxima", "Temperatura Minima")) +
        scale_color_manual(values = c(Normal_Climatologica = "blue")) + #, Media_cimatologica = "red")) +
        #  xlim(1,6) +
        theme_bw() + #guides(fill=FALSE) +
        theme(
            legend.position="bottom",
            legend.title = element_blank(),
            panel.grid.minor = element_blank(),
            strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
            strip.text = element_text(face = "bold")) +
        labs(title = paste0("Prediccion Climatica - ", id_label),
             subtitle = paste0("Escenarios de Remuestreo Historico: ", min(monthly_data$year), " > ", max(monthly_data$year)), 
             caption = paste0("Estadistico para calcular climatologia: ", stat),
             x = "Mes",
             y =  NULL) 
    
}

plot_weather_series <- function(weather_data, id_label = NULL){
    
    #Set Names and labels  
    var_name = c("rain", "prec", "srad", "tmin", "tmax", "rhum", "wvel")
    var_label = paste(var_name, c('(mm)', '(mm)', '(MJ/m²d)', '(°C)', '(°C)', '(%)', '(m/s)'))
    names(var_label) <- var_name
    
    to_monthly <- function(data, ...){
        data %>% 
            group_by(year, month) %>%
            summarise(prec = sum(prec, ...), 
                      tmin = mean(tmin, ...), 
                      tmax = mean(tmax, ...), .groups = 'drop') %>%  
            #            srad = mean(srad), 
            #            rhum = mean(rhum),
            #            wvel = mean(wvel)
            #write.csv("climate_data_monthly.csv")
            ungroup() 
    }
    
    #Convert historic data to monthly data  
    monthly_data <- to_monthly(weather_data, na.rm = T) 
    
    
    monthly_data %>%
        dplyr::select(-c(year)) %>%
        pivot_longer(cols = -c(month), names_to = "var", values_to = "value") %>%
        ggplot(aes(month, value, fill= var, group = month)) +
        geom_boxplot(alpha = 0.7) +
        scale_x_continuous(labels = function(x) month.abb[x], breaks = 1:12) +
        #  stat_summary(fun.data = mean_cl_normal, geom="bar") +
        #  geom_line(data = monthly_summary %>% 
        #                pivot_longer(cols = -c(month), names_to = "var", values_to = "value"), 
        #              aes(month, value, color = var)) +
        #           scale_alpha_discrete(range = c(0.9, 0.5)) + 
        facet_grid(var ~ ., scales = "free", labeller = labeller(var = var_label)) +
        theme_bw() + guides(fill=FALSE) +
        theme(
            panel.grid.minor = element_blank(),
            strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
            strip.text = element_text(face = "bold")) +
        labs(title = paste0("Climatologia de ",  id_label),
             subtitle = "Boxplot Mensual",
             x = "Mes",
             y =  NULL) +
        scale_fill_manual(values = c(prec = "#619CFF", tmax = "orangered3", tmin = "orange3"))+
        scale_color_manual(values = c(prec = "#619CFF", tmax = "orangered3", tmin = "orange3"))
    
}

### Functions for resampling /// by https://github.com/aesquivel94

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
# 4. Resampling
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
resampling <-  function(data, CPT_prob, year_forecast){
    
    ##Auxiliar functions
    
    central_month <- function(month_cent){
        
        ini_m <- str_sub(month.abb, 1, 1)
        season <- paste0(ini_m, lead(ini_m),lead(ini_m, n = 2) )
        # season <- glue::glue('{ini_m}{lead(ini_m)}{lead(ini_m, n = 2)}')
        season <- case_when(season == 'NDNA' ~ 'NDJ', season == 'DNANA' ~ 'DJF', TRUE ~ as.character(season)) 
        season <- tibble(cent = c(2:12, 1), season)
        
        # season_cent <- season[month_cent]
        season_cent <- season %>% filter(cent == month_cent) %>% .$season
        
        return(season_cent)}
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # 1. Fix february: depends if leap year it's true or false.
    # ***** These functions are used in the resampling function (F.to.resampling). ***** 
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    
    # (F.to.resampling). It's use when leap == FALSE (this function add a row in each february with 28 days). 
    add_29_day <- function(to_change){
        
        Dato_C <- to_change %>%  
            nest(-year) %>% 
            mutate(data = purrr::map(.x = data, .f = function(.x){
                data_add <- bind_rows(.x, .x %>% sample_n(size = 1) %>% mutate(day = 29)) 
                return(data_add)})) %>% 
            unnest %>% 
            dplyr::select(day, month,  year, prec,  tmax,  tmin)
        return(Dato_C)}
    
    # (F.to.resampling). It's use when leap == TRUE (this function delete a row in each february with 29 days). 
    less_29_day <- function(to_change){
        
        Dato_C <- to_change %>% 
            nest(-year) %>% 
            mutate(data = purrr::map(.x = data, .f = function(.x){
                data_less <- .x %>% slice(-n())
                return(data_less)})) %>% 
            unnest %>% 
            dplyr::select(day, month,  year, prec,  tmax,  tmin) 
        return(Dato_C)}
    
    # (F.to.resampling). This function organize the february data.
    change_Leap <- function(leap_forecast, feb_data){
        
        data_to_change <- feb_data %>% 
            mutate(leap = leap_year(year)) %>% 
            nest(-leap)
        
        if (leap_forecast == TRUE) { # if year_forecast == TRUE (all days need to have 29 days).
            data_to_change <- data_to_change %>% 
                mutate(data = purrr::map_if(.x = data, .p = leap == FALSE , .f = add_29_day))
        } else {
            data_to_change <- data_to_change %>% 
                mutate(data = purrr::map_if(.x = data, .p = leap ==  TRUE, .f = less_29_day))
        }
        
        data_to_change <- data_to_change %>% 
            unnest %>% 
            dplyr::select(-leap) %>%  
            arrange(year) 
        
        return(data_to_change) }
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # 2. Organize Probability, monthly data and daily data. 
    # ***** These functions are used in the resampling function (F.to.resampling). ***** 
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    
    # (F.to.resampling). .1. Put in the probabily object start month of season and end month. 
    season_to_months <-  function(season){
        
        all_seasons <-  paste0(str_sub(month.abb, 1, 1), lead(str_sub(month.abb, 1, 1)),
                               lead(lead(str_sub(month.abb, 1, 1), n = 1))) %>% 
            tibble(x = . ) %>% 
            mutate(x = ifelse(x == 'NDNA', 'NDJ', ifelse(x == 'DNANA', 'DJF', x))) %>% 
            mutate(start_month = 1:12, end_month = c(3:12, 1, 2))
        
        all_seasons <- all_seasons %>% 
            filter(str_detect(x, as.character(season)) ) %>% 
            dplyr::select(-x)
        
        return(all_seasons)}
    
    # (F.to.resampling). .2.This function organize and classify monthly data by category for one season.
    do_organize_data <- function(Season, xi, data, Intial_year, last_year){
        
        month_ini <- xi %>% 
            dplyr::select(start_month) %>% 
            unique() %>% 
            as.numeric()
        
        month_end <- xi %>% 
            dplyr::select(end_month) %>% 
            unique() %>% 
            as.numeric()
        
        if(Season == 'NDJ'){
            new_data <- data %>%
                filter(month %in% c(11,12,1)) %>% 
                mutate(year_M = ifelse(month == 1, year, year+1)) %>% 
                filter(year_M >= (Intial_year + 1), year_M < (last_year +1 ))%>%
                group_by(year_M) %>% 
                summarise(prec = sum(prec)) %>% 
                mutate(year = year_M - 1) %>% 
                dplyr::select(year, prec)
            
        } else if(Season == 'DJF'){
            new_data <- data %>%
                filter(month %in% c(12,1,2)) %>% 
                mutate(year_M = ifelse(month %in% 1:2, year, year+1)) %>% 
                filter(year_M >= (Intial_year + 1), year_M < (last_year +1 ))  %>%
                group_by(year_M) %>% 
                summarise(prec = sum(prec)) %>% 
                mutate(year = year_M - 1) %>% 
                dplyr::select(year, prec)
            
        } else{
            new_data <-  data %>%
                filter(between(month, month_ini, month_end)) %>%
                group_by(year) %>%
                summarise(prec = sum(prec))%>% 
                dplyr::select(year, prec)
        }
        
        # Quantiles of monthly averages are generated ... (be careful how they were generated). 
        quantile <- quantile(new_data$prec, probs = c(0.33, 0.66))
        
        # Classification of the monthly series ...
        new_data <-  new_data %>% 
            # mutate(condtion = case_when( prec < quantile[1] ~  'below', prec > quantile[2] ~ 'above' ,TRUE ~ 'normal')  ) %>%
            mutate(condtion = ifelse(prec < quantile[1], 'below', ifelse(prec > quantile[2], 'above', 'normal')) ) %>%
            nest(-condtion)
        
        return(new_data)}
    
    # (F.to.resampling). .3. This function create 100 samples category (only name).
    sample_category <- function(Prob){
        
        # Does the re-sampling of the categories...
        Type_cat <- tibble( id = 1:100) %>% 
            mutate(sample_cat = purrr::map(.x = id, .f = function(.x){
                sample_n(Prob,  size = 1, weight = Prob) }))
        
        return(Type_cat)}
    
    # =-=-=-=-=
    # (F.to.resampling). .4. This function dependent of the category, we do the year sample.
    year_function <- function(base_cat, mothly_data){
        
        by_cat <- function(cat, mothly_data){
            # cat <- base_cat %>% filter(row_number() < 2 ) %>% unnest %>% select( Type)
            
            mothly_data <- mothly_data %>% 
                filter(condtion == cat$Type) %>% ####################################
            unnest %>% 
                sample_n(size = 1) %>% 
                dplyr::select(-prec)
            
            return(mothly_data)}
        
        year_sample <- base_cat %>% 
            mutate(sample =  purrr::map(.x = sample_cat, .f = by_cat, mothly_data = mothly_data)) %>% 
            # dplyr::select(-sample_cat) %>%
            unnest %>% 
            dplyr::select( -Type, -Prob)
        
        return(year_sample)}
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
    # 3. Daily data
    # ***** These functions are used in the resampling function (F.to.resampling). ***** 
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
    # (F.to.resampling). This function extract daily data using sample year. 
    day_sample <- function(Season, cat, data, Intial_year, last_year){
        
        month_ini <- cat %>% 
            dplyr::select(start_month) %>% 
            unique() %>% 
            as.numeric()
        
        month_end <- cat %>% 
            dplyr::select(end_month) %>% 
            unique() %>% 
            as.numeric()
        
        # Filter by season data serie.
        if(Season == 'NDJ'){
            Daily_filter <- data %>%
                filter(month %in% c(11,12,1)) %>% 
                mutate(year_M = ifelse(month == 1, year, year+1)) %>% 
                filter(year_M >= (Intial_year + 1), year_M < (last_year + 1))%>%
                mutate(year = year_M - 1) %>% 
                dplyr::select(-year_M)
            
        } else if(Season == 'DJF'){
            Daily_filter <- data %>%
                filter(month %in% c(12,1,2)) %>% 
                mutate(year_M = ifelse(month %in% 1:2, year, year+1)) %>% 
                filter(year_M >= (Intial_year + 1), year_M < (last_year +1 ))%>%
                mutate(year = year_M - 1) %>% 
                dplyr::select(-year_M)
            
        } else{
            Daily_filter <-  data %>%
                filter(between(month, month_ini, month_end)) 
        }
        
        Daily_data <- cat %>% 
            dplyr::select(-start_month, -end_month) %>% 
            mutate(daily_data = purrr::map(.x = year, .f = function(.x){
                Daily_filter %>% filter(year == .x)})) %>% 
            dplyr::select(-year)
    }
    
    # (F.to.resampling). This function return a tibble with daily sceneries min and max. 
    Find_Summary <- function(daily_by_season){
        # Only the monthly grouping is done.
        monthly <- daily_by_season %>%
            group_by(year) %>%
            summarise(monthly = sum(prec))
        
        median <- round(nrow(monthly)/2, 0)
        
        # the minimum and maximum precitation is extracted.
        Min_Max <-  monthly %>%
            arrange(monthly) %>%
            slice(c(1, median, n())) %>%
            mutate(Type = c('min', 'median', 'max')) %>%
            dplyr::select(-monthly)
        
        Lenght <-  daily_by_season %>%
            filter(year %in% Min_Max$year) %>%
            count(id) %>%
            filter(row_number() == 1) %>%
            dplyr::select(n) %>%
            as.numeric
        
        Indicators <-  daily_by_season %>%
            filter(year %in% Min_Max$year) %>%
            dplyr::select(-id) %>%
            unique %>% nest(-year) %>%
            mutate(Type = case_when(year == Min_Max$year[Min_Max$Type == 'min'] ~ 'min',
                                    year == Min_Max$year[Min_Max$Type == 'median'] ~ 'median',
                                    year == Min_Max$year[Min_Max$Type == 'max'] ~ 'max')) %>%
            unnest() %>%
            # mutate(Type = rep(Min_Max$Type, each = Lenght )) %>%
            nest(-Type)
        
        a <- Indicators %>% filter(Type == 'min') %>% unnest()
        b <- Indicators %>% filter(Type == 'max') %>% unnest()
        
        ab <- bind_cols(a, dplyr::select(b, -Type, -day, -month, -year) %>% setNames(paste0(names(.), 1))) %>%
            dplyr::mutate(Type = 'mean_mm' , prec = (prec+prec1)/2, tmax = (tmax+tmax1)/2, tmin = (tmin + tmin1)/2 ) %>%
            dplyr::select(-prec1, -tmin1, -tmax1) %>% nest(-Type)
        
        Indicators <- Indicators %>% bind_rows(., ab)
        
        return(Indicators)}
    
    ######################################################################################################## 
    ######################################################################################################## 
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # 1. Fix february: depends if leap year it's true or false.
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    season1 <- CPT_prob %>% dplyr::select(Season) %>% unique() %>% filter(row_number() == 1) %>% .$Season
    
    year_f_leap <- ifelse(season1 %in% c('ASO', 'SON', 'OND', 'NDJ', 'DJF'), year_forecast + 1, year_forecast)
    
    # Create a new data (with standard february).
    data <- data %>% 
        mutate(month_P = month) %>% 
        nest(-month_P) %>% 
        mutate(data = purrr::map_if(.x = data ,.p = month_P == 2 ,
                                    .f = change_Leap, leap_forecast = leap_year(year_f_leap))) %>% 
        dplyr::select(data) %>% 
        unnest %>% 
        arrange(year)
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-= 
    # =-=-=-=-=  Do years (start year and end year)...
    
    Intial_year <- data %>% dplyr::select(year) %>%  unique %>% slice(1) %>% as.numeric()
    
    last_year <- data %>% dplyr::select(year) %>% unique %>% slice(n()) %>% as.numeric()
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # 2. Organize Probability, monthly data and daily data. 
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    
    # Add start_month and end_month. 
    Times <- CPT_prob %>% 
        nest(-Season) %>% 
        mutate(Times = purrr::map(.x = Season, .f = season_to_months)) %>% 
        unnest(Times) %>% 
        unnest() %>% 
        nest(-Season)
    
    # In this part we create a new variable with monthly data classify.
    Times <- Times %>% 
        rename(xi = data) %>% 
        mutate(month_data = purrr::map2(.x = Season, .y = xi, 
                                        .f = do_organize_data, data = data, 
                                        Intial_year = Intial_year, last_year = last_year))
    
    # This function do the 100 category samples.   
    Times <- Times %>% mutate(cat = purrr::map(.x = xi,.f = sample_category))
    
    # =-=-=-=-=-=-=-=-
    # This function do the year sample depends of the sample category.
    Times <- Times %>% mutate(cat = purrr::map2(.x = cat, .y = month_data, .f = year_function))
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    Base_years <- Times %>% 
        mutate(order = paste0(letters[1:2], '.',Season)) %>% 
        dplyr::select(order, cat) %>% 
        unnest %>%
        dplyr::select(order, year) %>% 
        nest(-order) %>%
        pivot_wider(names_from = order, values_from = data) %>%
        unnest %>% 
        set_names(paste0(letters[1:2], '.',  Times$Season)) %>% 
        cbind(id = 1:100, .)
    
    # This function extract daily data using sample year.  
    daily_data <- Times %>% 
        mutate(daily_data = purrr::map2(.x = Season, .y = cat, .f = day_sample, 
                                        data = data, Intial_year = Intial_year, 
                                        last_year = last_year)) %>% 
        dplyr::select(Season, daily_data)
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--=-=-=-=
    data_to_esc <-  daily_data %>% 
        unnest %>% 
        dplyr::select(-condtion) %>% 
        nest(-id) %>%  
        mutate(data = purrr::map(.x = data, .f = function(.x){ .x %>% unnest()})) %>%
        unnest() 
    
    # add extra
    months <- data_to_esc %>% dplyr::select(month) %>% unique()
    cond_change <- isTRUE(months[[1, 1]] > 7 & sum(months < 6) >= 1) == TRUE
    
    Escenaries <-  data_to_esc %>%
        mutate(year = year_forecast) %>% 
        mutate(year = ifelse(cond_change == TRUE & month < 6, year + 1, year))  %>%
        # mutate(year = ifelse(Season %in% c('NDJ', 'DJF') & month == 1, year + 1, ifelse(Season == 'DJF' & month == 2, year + 1, year))) %>%
        dplyr::select(-Season) %>% 
        nest(-id) 
    
    # Here was Find_Summary function
    # In this part we create sceneries with min and max years 
    # (building from aggregate precipitation).
    Esc_Type <- data_to_esc %>%
        nest(-Season) %>%
        mutate(Summary = purrr::map(.x = data, .f = Find_Summary)) %>% 
        dplyr::select(-data) %>% 
        unnest() %>% 
        unnest %>% 
        arrange(Type) %>%
        mutate(year = year_forecast) %>% 
        mutate(year = ifelse(cond_change == TRUE & month < 6, year + 1, year)) %>% 
        dplyr::select(-Season) %>% 
        nest(-Type)
    
    
    
    Esc_Type <- data_to_esc %>%
        nest(-Season) %>% 
        mutate(data1 = purrr::map(.x = data, .f = function(x){x <- x %>% dplyr::select(-id) %>% group_by(day, month) %>% summarise_all(mean) %>% arrange(month, day)})) %>% 
        dplyr::select(-data) %>%
        dplyr::mutate(Type = 'mean') %>% 
        unnest()  %>%
        mutate(year = year_forecast) %>% 
        mutate(year = ifelse(cond_change == TRUE & month < 6, year + 1, year)) %>% 
        dplyr::select(-Season) %>% 
        nest(-Type) %>%  
        bind_rows(Esc_Type , .)
    
    
    # This object is the mix with 3 data set (sceneries, sample years and sceneries types).
    All_data <- bind_cols( Escenaries %>% mutate(Row = 'a') %>% nest(-Row),
                           Base_years %>% mutate(Row1 = 'a') %>% nest(-Row1) %>% rename(Base_years = data)) %>% 
        bind_cols(Esc_Type %>% mutate(Row2 = 'a') %>% nest(-Row2) %>% rename(Esc_Type = data) ) %>% 
        dplyr::select(-Row1, -Row2)
    # dplyr::select(-Row)
    
    return(All_data)}

# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
#  5. Function to save all files from resampling.  
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# ***** INPUT 
# * station: name of the station.
# * Esc_all: resampling function output.
# * path_out: path for save files.

# *****  OUTPUT
# This function save resampling files, summary files and escenario_a (years resampled). 

# ***** Note: This function save files.
function_to_save <- function(station, Esc_all, path_out){
    # station = Resam$id[1]; Esc_all = Resam$Escenaries[[1]]
    
    # Daily sceneries (generated with resampling).
    Escenaries <- Esc_all %>%
        dplyr::select(data) %>% 
        unnest
    
    Esc_C <- Escenaries %>% 
        mutate(data = purrr::map(.x = data, .f = function(x){mutate(x, day = as.integer(day), month = as.integer(month), year = as.integer(year))}))%>% 
        mutate(file_name = paste0(path_out, '/',station, '/', station, '_escenario_', id, '.csv')) 
    
    # Creation of the data folder (where the results will be saved). 
    # ifelse(dir.exists(glue::glue('{path_out}{station}')) == FALSE, dir.create(glue::glue('{path_out}{station}')), 'ok')
    ifelse(dir.exists(paste0(path_out, '/',station)) == FALSE, 
           dir.create(paste0(path_out, '/',station)), 'ok')
    
    
    # Creation of the data folder (where the results will be saved). 
    # ifelse(dir.exists(glue::glue('{path_out}summary')) == FALSE, dir.create(glue::glue('{path_out}summary')), 'ok')
    ifelse(dir.exists(paste0(path_out, '/','summary')) == FALSE, 
           dir.create(paste0(path_out, '/','summary')), 'ok')
    
    # Creation of the data folder (where the results will be saved). 
    # ifelse(dir.exists(glue::glue('{path_out}validation')) == FALSE, dir.create(glue::glue('{path_out}validation')), 'ok')
    ifelse(dir.exists(paste0(path_out, '/', 'validation')) == FALSE, 
           dir.create(paste0(path_out, '/', 'validation')), 'ok')
    
    
    # Save daily sceneries.
    walk2(.x = Esc_C$data, .y = Esc_C$file_name, 
          .f = function(.x, .y){ readr::write_csv(x = .x, path = .y)})
    
    # Save scenarios type. 
    Type_Esc <- Esc_all %>% 
        dplyr::select(Esc_Type) %>% 
        unnest %>% 
        mutate(data = purrr::map(.x = data, .f = function(x){mutate(x, day = as.integer(day), month = as.integer(month), year = as.integer(year))}))%>%
        mutate(file_name = paste0(path_out, '/summary/', station, '_escenario_', Type, '.csv'))
    
    walk2(.x = Type_Esc$data, .y = Type_Esc$file_name, 
          .f = function(.x, .y){ write_csv(x = .x, path = .y)})
    
    # Save resampling years.
    Esc_all %>% 
        dplyr::select(Base_years) %>% 
        unnest %>% 
        mutate_all(.funs = as.integer) %>%
        write_csv(., path = paste0(path_out, '/validation/', station, '_Escenario_A.csv'))
    
    # =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
    # summary variables files creation.
    # Levels <- Esc_C %>% 
    #   dplyr::select(data) %>% 
    #   unnest %>% 
    #   dplyr::select(month) %>% 
    #   unique 
    # 
    # summaries <- Esc_C %>% 
    #   dplyr::select(id, data) %>% 
    #   unnest %>% 
    #   mutate(month = factor(month,Levels$month))  %>% 
    #   group_by(id, month, year) %>% 
    #   summarise(prec = sum(prec), tmax = mean(tmax), tmin = mean(tmin)) %>% 
    #   ungroup() %>% 
    #   dplyr::select(-id) %>%
    #   group_by(month) %>%
    #   group_by(year, month) %>%
    #   summarise(prec_avg = mean(prec), prec_max = max(prec), prec_min = min(prec),
    #             tmax_avg = mean(tmax), tmax_max = max(tmax), tmax_min = min(tmax),
    #             tmin_avg = mean(tmin), tmin_max = max(tmin), tmin_min = min(tmin)) %>%
    #   ungroup()
    # 
    # summaries <- summaries %>% 
    #   gather(variable, values, -month, -year) %>% 
    #   nest(-variable) %>% 
    #   mutate(data = purrr::map2(.x = variable, .y = data, .f = function(.x, .y){
    #     
    #     if(str_detect(.x , 'sol_rad_') == TRUE){
    #       .y <- .y  %>% 
    #         set_names(c('year', 'month', str_replace(.x , 'sol_rad_', '')))
    #     } else{
    #       .y <- .y  %>% 
    #         set_names(c('year', 'month', str_extract( .x ,'_[a-z]+') %>% str_replace('_', '')))
    #     }
    #     return(.y)})) %>% 
    #   # mutate(file_name = glue::glue('{path_out}summary/{station}_{variable}.csv'))
    #   mutate(file_name = paste0(path_out, '/summary/', station, '_', variable, '.csv'))
    # 
    # 
    # # Aqui se guardan los archivos...
    # walk2(.x = summaries$data, .y = summaries$file_name,
    #       .f = function(.x, .y){write_csv(x = .x, path = .y)})
}






