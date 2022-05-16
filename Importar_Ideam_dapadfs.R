## Importar datos IDEAM en dapadfs -
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022


## dependences
library(tidyverse)
library(sf)
library(stringi)
library(lubridate)
library(naniar)
library(data.table)
#library(raster)
#library(plotly)
#library(htmlwidgets)
#library(ggspatial)
#library(rgdal)
#library(gdalUtils)

## Read data
#Locations

load(file = "data/catalog.RDS")
#load(file = "data/ws_rice_zones.RData")

#############################################################
## Filter data by Departamento, Categoria, status
dpto <- c("SUCRE", "TOLIMA", "CORDOBA", "META")#, "CORDOBA", "VALLE DEL CAUCA")

cat <- c("CLIMATOLOGICA PRINCIPAL", "CLIMATOLOGICA ORDINARIA", 
         "AGROMETEOROLOGICA", "SINOPTICA PRINCIPAL",
         "SINOPTICA SECUNDARIA", "METEOROLOGICA ESPECIAL", "PLUVIOMETRICA" )

status <- c("ACTIVA", "SUSPENDIDA", "FUERA DE SERVICIO")


## Filter IDEAM catalog
filter_ideam <- function (catalog, dpto , cat, status, min_alt = 0, max_alt = 1500) {
  
  catalog %>% 
    mutate(Departamento = toupper(stri_trans_general(Departamento,"Latin-ASCII")),
           Municipio = toupper(stri_trans_general(Municipio,"Latin-ASCII"))) %>%
    filter(Departamento %in% dpto,
           Categoria %in% cat,
           Estado %in% status,
           Altitud < max_alt,
           Altitud > min_alt)
  #               yfunc > min_year)
  
  
}

data_filter <- filter_ideam(catalog, dpto, cat, status) %>%
  filter(alt_raster <2500)





## Weather data
###############################################
## Scan in data_cluster4
path <- "//alliancedfs.alliance.cgiar.org/data_cluster_4/observed/weather_station/col-ideam/daily-raw"

## Set variables to join
var_selec <- c("prec", "tmax", "tmin", "sbright", "rhum")

## Set IDEAM id station 
ideam_id <- data_filter$id

scan_ciat_db <- function (path, ideam_id, var_selec) {
  
  list_folder <- list.dirs(path, recursive = F)
  pat_finder <- paste0(ideam_id, collapse = "|")
  var_names <- str_extract(list_folder, pattern = "(?<=raw/)(.*\n?)(?=-per)")
  
  
  ## Create a list with file names by var
  list_files_var <- pmap(list(path = list_folder, pattern = pat_finder), list.files)
  names(list_files_var) <- var_names
  
  ## Select station id tha contain all variable c("prec", "tmax", "tmin", "sbright", "rhum") 
  list_id <- map(list_files_var, ~str_extract(.,  "[^_]+")) %>% 
    map(., ~ enframe(., name = NULL)) %>%
    .[var_selec[1]] %>%
    reduce(inner_join, by = "value")
  
  ## List files of select vars
  list_files_select <- pmap(list(
    path = str_subset(list_folder, 
                      paste(var_selec, collapse = "|")),
    pattern = paste0(list_id$value, collapse = "|")),
    list.files)
  
  ## Set list name as path folder
  names(list_files_select) <- str_subset(list_folder, paste(var_selec, collapse = "|"))
  
  return(list_files_select)
  
}

#list_files_select <- scan_ciat_db(path, ideam_id, var_selec)

list_files_select <- map(var_selec, ~scan_ciat_db(path, ideam_id, .x))



## Read data for each id. Convert to Tibble. This function take a list of path files by var, return a tibble
get_station_data <- function(list_files_select) {
  
  read_raw <- function(file) {
    
    data.table::fread(file) %>%
      mutate(Date = lubridate::ymd(Date),
             Value = as.numeric(Value))
    
    
  }
  
  list_files_select %>% 
    #    map(., ~enframe(., name = NULL)) %>% bind_rows() %>%
    mutate(file = paste0(path, "/", value),
           data = map(file, read_raw),
           id = as.numeric(str_extract(.$value,  "[^_]+")),
           var = str_extract(file, "(?<=raw/)(.*\n?)(?=-per)")) %>%
    dplyr::select(id, var, path, data)
  
}


#wdata_tb <- get_station_data(list_files_select)

wdata_tb <- list_files_select %>% 
  map(., ~enframe(., name = "path")) %>% 
  bind_rows() %>% unnest(value) %>% 
  split(., .$path) %>%
  map(~get_station_data(.x)) 

##  ----- LOADED DATA --->

## Filter raw data, extract dates and "NA" percent...
ideam_raw <- wdata_tb %>% map( ~.x %>%
                                 dplyr::select(-path) %>% group_by(var) %>%
                                 unnest(data) %>% spread(var, Value) %>%
                                 nest(-id) %>%
                                 mutate(idate = map(data, ~ min(.x$Date)) %>% do.call("c", .),
                                        fdate = map(data, ~ max(.x$Date)) %>% do.call("c", .),
                                        years = time_length(fdate-idate, "years"),
                                        na_percent = map(data, ~ pct_miss(.x %>% dplyr::select(-Date))) %>% flatten_dbl())) %>%
  #                                 filter(years > 20)) %>% 
  set_names(str_extract(names(.), "(?<=raw/)(.*\n?)(?=-per)"))
#  bind_rows(.id = "var")


ws_selected <- ideam_raw %>% bind_rows(.id = "var") %>%
  #  dplyr::select(data) %>% 
  left_join(catalog %>% 
              dplyr::select(id, Nombre, Categoria, Departamento, Municipio,lat, lon))

save(ws_selected, file = "caf_ideam_data_clima.RData")


load("caf_ideam_data_clima.RData")



ws_join <- ws_selected %>%
#  dplyr::filter(years >= 10, na_percent < 15) %>% 
  dplyr::filter(var %in% c("prec", "tmax", "tmin")) %>% 
  mutate(var = case_when(var == "prec" ~ "rain",
            TRUE ~ var)) %>%
  #  filter (Categoria != "PLUVIOMETRICA") %>% 
  #  dplyr::select(-c(idate:na_percent)) %>%
  mutate(data = map(data, ~.x %>% set_names(c("date", "value")))) %>%
  unnest(data) %>% dplyr::select(id, lat, lon, date, var, value)  %>% distinct() %>%
  pivot_wider(names_from = var) %>%
  nest(data = -c(id:lon))



wth_monthly_caf_ideam_sites <- ws_join %>% 
  mutate(data =  map(data, ~.x %>%
                       #pivot_wider(names_from = var) %>% 
                       # rename(rain = prec) %>% 
                       basic_qc()  %>%
                       impute_mean_wth() %>%
                       
                       daily_to_monthly()) %>%
                       pivot_longer(cols = -c(year, month), names_to = "var")) %>% 
  unnest(data) %>% 
  distinct() %>%
  pivot_wider(names_from = var) %>% 
  nest(data = c(year:tmin))



save(wth_monthly_caf_ideam_sites, file = "wth_monthly_caf_ideam_site.RData")



