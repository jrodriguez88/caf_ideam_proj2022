## Descargar y extraer datos de SoilGrids -
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022


##load packages
library(tidyverse)
library(sf)
library(data.table)
library(raster)
library(plotly)
library(ggspatial)
library(rgdal)
library(gdalUtils)


### Soil sites - departamentos proyecto 
COL_shp <-  getData('GADM', country='COL', level=1) %>% st_as_sf()
COL2_shp <- getData('GADM', country='COL', level=2) %>% st_as_sf()

dptos_select <- COL_shp %>% 
  dplyr::filter(HASC_1 %in% c("CO.SU", "CO.ME", "CO.CO", "CO.TO"))

municipios_select <- COL2_shp %>% 
  dplyr::filter(HASC_2 %in% c("CO.SU.SB", "CO.SU.SM", "CO.ME.PZ","CO.ME.GR", 
                              "CO.CO.CE","CO.CO.CT", "CO.TO.AR", "CO.TO.ES")) 



## Soil data spatial info - Bbox - extend departamentos
igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
crop_layer_col <- st_transform(dptos_select, igh)
bbox_col <- st_bbox(crop_layer_col)
limites_layer_col <- c(bbox_col$xmin, bbox_col$ymax, bbox_col$xmax, bbox_col$ymin)



#params for soilgrids - configurar/ automatizar descarga de datos

main_url = "https://files.isric.org/soilgrids/latest/data/"
soil_vars = c("bdod", "clay", "nitrogen", "sand", "silt", "soc", "phh2o")
metric_soil = c("mean") 
depths = c("0-5cm", "5-15cm", "15-30cm", "30-60cm")

soilgrids_urls <- paste0("/vsicurl/", main_url, soil_vars, "/") %>% 
  map2(., soil_vars, ~paste0(.x, .y, "_",  depths, "_",  metric_soil, ".vrt")) %>% 
  unlist() 


names_files_soil <- soilgrids_urls %>% 
  map(~str_split(.x, "data/") %>% 
        map_chr(2)) %>% unlist() %>% 
  str_remove('.*/') %>% str_replace("vrt", "tif")


# download raster data for bbox, Tif format- requiere Gdal instalado en pc
map2(soilgrids_urls, names_files_soil, 
     ~gdal_translate(.x, paste0("soilgrids/", .y),
                     tr = c(250,250),
                     projwin = limites_layer_col,
                     projwin_srs = igh,
                     verbose = TRUE))





shape_crop_co <- crop_layer_col %>% group_split(HASC_1)
test_municipios_select <- st_transform(municipios_select, igh) %>% 
  filter(str_detect(HASC_2, "CO.CO"))


dptos_caf_soilgrids <- map(shape_crop_co, 
                           raster %>%
                             crop(test_crop_co) %>% 
                             mask(test_crop_co))

### Calcula el promedio por variable (0 - 60 cm profundidad)

raster_mean60 <- map(soil_vars, ~str_subset(paste0("soilgrids/", names_files_soil), .x) %>%
                     map(raster) %>%
                     brick() %>% 
                     calc(mean))


dpto_shp <- shape_crop_co %>% enframe(name = "dpto") %>%
  mutate(name = map_chr(value, ~.x$NAME_1 )) %>%
  dplyr::select(name, value)  %>% rename(dpto_shp = value)


var_raster <- raster_mean60 %>%  set_names(soil_vars) %>% 
  enframe(name = "var", value = "raster")
## Tidy data


test_data_soil <- shape_crop_co %>% enframe(name = "dpto") %>%
  mutate(name = map_chr(value, ~.x$NAME_1 )) %>%
  dplyr::select(name, value)  %>% rename(dpto_shp = value) %>%
  mutate(var_raster  = list(var_raster)) %>% unnest(var_raster) %>% 
  mutate(plot_raster  = map2(raster, dpto_shp, ~.x %>% crop(.y) %>% mask(.y)),
         tag_name = paste(name, var, sep = "_"))
  
  
#  plot(test_data_soil$plot_raster[[3]], main = "test11")

##Descarga descripcion de variables soilgrids

library(rvest)

soil_units <- "https://www.isric.org/explore/soilgrids/faq-soilgrids#What_is_SoilGrids" %>% 
  read_html() %>% 
  html_nodes("table")  %>% 
  html_table(fill = T)
## summary

soilgris_description <- soil_units[[1]] %>% 
  set_names(c("var", "description", "default_units", "factor", "units" ))


## Convierte raster -selection en puntos
soilgrids_data_points <- test_data_soil %>% 
  left_join(soilgris_description) %>% 
  mutate(soil_raster_points = map2(plot_raster, factor, 
                                   ~((.x)/(.y)) %>% 
                                     raster::as.data.frame(., xy = T)))


## Funcion para graficar datos soilgrids por departamento 
plot_soilgrids <- function(var, soil_raster_points, description, dpto_shp, units) {
  
  dpto <- dpto_shp$NAME_1[[1]]
  title_plot <- paste0("Departamento de ", dpto)
  sub_plot <- description
  capt_plot <- "Source: SoilGrids - https://files.isric.org/soilgrids/"
  fill_var <- paste0(var, " - ", units)
  
  
  plot <- soil_raster_points %>% 
    ggplot() + 
    geom_raster(aes(x=x, y=y, fill = layer)) +
    geom_sf(data = dpto_shp, alpha = 0, fill = "transparent") +
    labs(title = title_plot,
         subtitle = sub_plot,
         caption = capt_plot,
         fill = fill_var,
         x = "Longitud", y = "Latitud") +
    annotation_north_arrow(location = "tl", height = unit(0.7, "cm"),
                           width = unit(0.7, "cm")) +
    scale_fill_viridis_c(na.value = "transparent" , direction = 1) + 
    theme_bw()
  
  return(plot)
  
  
}


plots_soilgrids <- soilgrids_data_points %>%
  dplyr::select(var, soil_raster_points, description, dpto_shp, units)  %>% 
  mutate(plot = pmap(., plot_soilgrids))

#p22 <- plots_soilgrids$plot[[1]]
#ggplotly(p22)

## save plots 
walk2(paste0(soil_data_final$tag_name, ".png"), plots_soilgrids$plot, 
      ~ggsave(filename = .x, plot = .y, units = "mm", height = 125, width = 180))














