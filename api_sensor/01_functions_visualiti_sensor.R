### import and read data sensor (soil/rain Visualiti Sensor) - CAF IDEAM project 
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022# Read data - Soil moisture sensor


### libraries
library(tidyverse)
library(tidyr)
library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(lubridate)
library(ggplot2)
library(data.table)

## data 

## read txt files
path_data <- "data/soil_sensor/granada/GRANADA/ESTACIÓN NUBIA/M146/"
files <- list.files(path_data, full.names = T, recursive = T, pattern = ".txt")

### funcion para importar datos de sensores Visualiti con conexion "Bluetooth Terminal HC-05
# file = txt file # - Bluetooth Terminal HC-05.txt

import_sensor_data <- function(file){
  
  tag_ini <- "---INI"
  tag_fin <- "---FIN"
  
  
  skip_lines <- read_lines(file) %>% str_detect(tag_ini) %>% which() + 1
  
  n_lines <- read_lines(file) %>% str_detect(tag_fin) %>% which() - skip_lines - 2
  
  
  raw_data <- fread(file, skip = skip_lines, nrows = n_lines) 
  
  var_names <- colnames(raw_data)
  
  
  if ("m3/m3"  %in% var_names) {
    
    cat("Datos de Sensor de humedad de suelo", sep = '\n')
    
    new_names <- c("id", "date", paste0("sensor_humedad_", 1:(length(var_names) - 2)))
    
    raw_data <- raw_data %>%
      set_names(new_names) %>%
      mutate(date_time = as_datetime(date), hour = format(date_time, "%H"), 
             date = as_date(date_time)) %>% 
      dplyr::select(id, date_time, date, hour, everything())
    
  } else if ("mm"  %in% var_names) {
    
    cat("Datos de Pluviometro", sep = '\n')
    new_names <- c("id", "date", paste0("pluviometro_", 1:(length(var_names) - 2)))
    
    raw_data <- raw_data %>%
      set_names(new_names) %>%
      mutate(date_time = as_datetime(date), 
             date = as_date(date_time)) %>% 
      dplyr::select(id, date_time, date, everything())
    
    
  } else {cat("No se identifican datos")}

  
  return(raw_data)
    
}

# Funciones que convuertes datos crudos de sensores en datos diarios

soilraw_to_daily <- function(soilraw_data, nmin = 12){
  
  daily_data_soil <- soilraw_data %>% distinct() %>% group_by(id, date) %>% 
    summarise(n = n(), 
              Profundidad_0_20cm = mean(sensor_humedad_1, na.rm = T), 
              Profundidad_20_40cm = mean(sensor_humedad_2, na.rm = T)) %>%
    ungroup() %>% 
    filter(n>=nmin) %>% 
    pivot_longer(cols = -c(id, date, n), names_to = "Humedad_del_Suelo")# minimo % de los datos colectados por dia
  
  
  return(daily_data_soil)
  
  
}

rainraw_to_daily <- function(rainraw_data){
  
  rain_daily_data <- rainraw_data %>% distinct() %>%
    group_by(id, date) %>% 
    summarise(n = n(), Precipitacion = sum(pluviometro_1, na.rm = T)) %>%
    ungroup() ## add distinct data
  
  return(rain_daily_data)
  
}


# Funciones para graficar datos de sensores

plot_soil_data <- function(soil_daily_data) {
  
  plot_sensor <- soil_daily_data %>% ggplot() +
    geom_line(aes(date, value, color = Humedad_del_Suelo), cex = 1.1) +
    ylim(3, 60) + scale_x_date(date_breaks = "2 day", date_labels = "%b %d") +
    geom_hline(aes(yintercept=4, linetype = "Punto de Marchitez Permanente"), colour = "red") + 
    geom_hline(aes(yintercept=10, linetype = "Punto de Marchitez Permanente"), colour = "red") + 
    geom_hline(aes(yintercept=17, linetype = "Punto de Marchitez Permanente"), colour = "red") + 
    geom_text(aes(soil_daily_data$date[[1]], 4, label = "Arenoso", vjust = 0), size = 4) +
    geom_text(aes(soil_daily_data$date[[1]], 10, label = "Franco", vjust = 0), size = 4) +
    geom_text(aes(soil_daily_data$date[[1]], 17, label = "Arcilloso", vjust = 0), size = 4) +
    geom_hline(aes(yintercept=9, linetype = "Capacidad de Campo"), colour = "green") +
    geom_hline(aes(yintercept=22, linetype = "Capacidad de Campo"), colour = "green") +
    geom_hline(aes(yintercept=35, linetype = "Capacidad de Campo"), colour = "green") +
    geom_text(aes(soil_daily_data$date[[1]], 9, label = "Arenoso", vjust = 0), size = 4) +
    geom_text(aes(soil_daily_data$date[[1]], 22, label = "Franco", vjust = 0), size = 4) +
    geom_text(aes(soil_daily_data$date[[1]], 35, label = "Arcilloso", vjust = 0), size = 4) +
    labs(title = paste0("Registro de humedad del suelo del sensor ", soil_daily_data$id[[1]]),
         #        subtitle = "30-year Data 1990-2019 \nCrop Season:  A = May-Apr  --  B = Sep-Oct",
         x= "Dia",
         y= "Porcentaje (%)",
         color = "Humedad Volumetrica (% -  m³/m³): ") + 
    #       linetype = "Referencias tipo:") +
    #  scale_x_continuous(labels = function(x) month.abb[x], breaks = 1:12) +
    theme_bw() + scale_color_viridis_d(option = "E") +
    theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      legend.position="bottom",
      #    legend.title = element_blank(),
      #    panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) + 
    guides(color = guide_legend(order = 2))+
    scale_linetype_manual(name = "     Referencias: ", values = c(2, 2),
                          guide = guide_legend(
                            override.aes = list(color = c("green", "red"))))
  
  
  return(plot_sensor)
  
  
}

plot_rain_data <- function(rain_daily_data){

  
  plot_pluvio <- rain_daily_data %>% ggplot() +
    geom_col(aes(date, Precipitacion), color = "blue", width = 0.5, fill = "lightblue") +
    scale_x_date(date_breaks = "2 day", date_labels = "%b %d") +
    labs(title = paste0("Registro de lluvias del sensor ", rain_daily_data$id[[1]]),
         #        subtitle = "30-year Data 1990-2019 \nCrop Season:  A = May-Apr  --  B = Sep-Oct",
         x= "Dia",
         y= "Precipitacion (mm)") +
    #       color = "Humedad del suelo (% -  m³/m³): ", 
    #       linetype = "Referencias tipo:") +
    #  scale_x_continuous(labels = function(x) month.abb[x], breaks = 1:12) +
    theme_bw() + 
    theme(#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      #    legend.position="bottom",
      #    legend.title = element_blank(),
      #    panel.grid.minor = element_blank(),
      strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
      strip.text = element_text(face = "bold")) 
  
return(plot_pluvio)  
  
}


# prueba de uso - precipitacion

test_data <- files %>% map(import_sensor_data) 

test_data %>% bind_rows() %>% 
 # distinct() %>% 
  rainraw_to_daily %>% slice(-c(1,2)) %>% 
  plot_rain_data() %>% 
  plotly::ggplotly()


# prueba de uso - Suelo

test_data <- files %>% map(import_sensor_data) 

test_data %>% bind_rows() %>%# View()
  # distinct() %>% 
  soilraw_to_daily %>% #slice(-c(1:44)) %>% 
  plot_soil_data() %>% 
  plotly::ggplotly()





