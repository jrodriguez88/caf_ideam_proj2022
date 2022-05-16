## Extract idh from raster data-
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022



##load packages
library(readxl)
library(tidyverse)
library(raster)


## Path data
path_data <- "data/IDH/Archivos IDH/RTA_MENSUAL/"
COL_shp <-  getData('GADM', country='COL', level=1) %>% st_as_sf()


## Create tibble of files 
files <- list.files(path_data, full.names = T, recursive = T, pattern = fixed("IDH_")) %>% 
  enframe(name = NULL, value = "path") %>% 
  dplyr::filter(str_detect(path, ".aux.xml", negate = T)) %>%
  mutate(file = word(path, 8, sep = "/"),
         year = word(path, 6, sep = "/"),
         month = word(path, 7, sep = "/"))


## load stations location (coordinates - latitude + longitude)
sites <- read_excel("data/IDH/estaciones_extract.xlsx") #%>% 
#  dplyr::select(-c(Departamento:Station_Name)) %>%
#  set_names(c("id", "lat", "lon"))


# read idh raster data
idh_raster_tib <- files %>% 
  mutate(idh_raster = map(path, raster))
  

#extract data by points
idh_points_stations <- idh_raster_tib %>%
  mutate(coords = list(data.frame(x= sites$Longitud, y= sites$Latitud)),
        data = map2(idh_raster, coords, raster::extract))



###Plot points to extract

test_raster <- idh_raster_tib$idh_raster[[1]] %>% 
  crop(COL_shp) %>% mask(COL_shp)
ggplot() + 
  geom_raster(data = raster::as.data.frame(test_raster, xy = T), aes(x=x, y=y, fill = IDH_299x353))+
  geom_sf(data = COL_shp, alpha = 0, fill = "transparent") +
  geom_point(data = sites, aes(Longitud, Latitud), color = "red") +
  labs(title = "Test Points IDH - IDEAM",
       x = "Longitud", y = "Latitud") + guides(fill="none") +
  annotation_north_arrow(location = "tl", height = unit(0.7, "cm"),
                         width = unit(0.7, "cm")) +
  scale_fill_viridis_c(na.value = "transparent" , direction = 1) + 
  theme_bw()



## tidy data  
idh_caf_points_stations <- idh_points_stations %>%
  mutate(data = map(data, ~.x %>%
  enframe(name = "id", value = "idh") %>%
  mutate(id = sites$id))) %>% dplyr::select(-idh_raster, -coords) %>%
  unnest(data) %>% left_join(sites) %>%
  dplyr::select(id, Latitud:Station_Name, year, month, idh) %>%
  nest(data_idh = c(year, month, idh))


save(files, sites, idh_raster_raw, idh_caf_points_stations, file = "IDH_Ideam_data.RData")


idh_caf_points_stations %>% 
  dplyr::select(Station_Name, data_idh) %>%
  unnest(data_idh) %>% pivot_wider(names_from = Station_Name, values_from = idh) %>%
  write_csv(file = "IDH_Ideam_stations.csv")


  
