### Plots C3 final- CAF IDEAM project 
# Author: Rodriguez-Espinoza J.
# https://github.com/jrodriguez88/
# 2022# Read data - requiere datos en INPUT_data AgroclimR

##library

library(tidyverse)
library(readxl)
library(data.table)
library(lubridate)
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/utils/utils_crop_model.R", encoding = "UTF-8")
source("https://raw.githubusercontent.com/jrodriguez88/agroclimR/r_package/R_package/utils/clim_tools.R", encoding = "UTF-8")
library(ggpubr)


path <- "data_final/libros_campo/"
file <- list.files(path, full.names = T, pattern = "xlsx") 


data <- read_INPUT_data(file[2])

crop <- "maiz"

### Row number in AGRO_man sheet
i <- 19

id <- data$AGRO_man$ID[[i]]
loc_id <- data$AGRO_man$LOC_ID[[i]]
finca <- data$AGRO_man$FINCA[[i]]
sensor <- data$AGRO_man %>% dplyr::filter(ID ==  id) %>%
  pull(SENSOR_ID) %>% str_split(", ") %>% pluck(1)



soil <- data$SOIL_obs %>% dplyr::filter(ID ==  id)

wth <- data$WTH_obs %>% filter(LOC_ID == loc_id)

phen <- data$PHEN_obs %>%  dplyr::filter(ID ==  id)
sow_date <- as.Date(phen$PDAT)
harv_date <- as.Date(phen$MDAT2)
grow <- data$PLANT_gro %>% dplyr::filter(ID ==  id)
yield <- data$YIELD_obs %>% dplyr::filter(ID ==  id)
hsoil <- read_csv(list.files("data_final/sensores_suelo/", full.names = T, pattern = sensor[1])) %>%
  #  mutate(date = mdy(date)) %>%
  #  filter(n>=12) %>%
  #  pivot_longer(cols = -c(id, date, n), names_to = "Humedad_del_Suelo") %>%
  right_join(
    enframe(
      seq.Date(sow_date, harv_date, "1 day"), name = NULL, value = "date"))
rain <- dplyr::select(wth, DATE, RAIN) %>% set_names(tolower(names(.))) %>%
  dplyr::mutate(date = as.Date(date), rain = as.numeric(rain)) %>%
  dplyr::filter(date >= sow_date, date <= harv_date)


if(crop =="maiz"){
  
  gstage <- data.frame(
  Growth_Phase = c("Emergencia", "Vegetativa", "Floracion", "Madurez"),
  start=c(sow_date, as.Date(phen$EDAT), as.Date(phen$`V6-V7`), as.Date(phen$FDAT)),
  end=c(as.Date(phen$EDAT), as.Date(phen$`V6-V7`), as.Date(phen$FDAT), harv_date)) %>%
  mutate(Growth_Phase=factor(Growth_Phase, c("Emergencia", "Vegetativa", "Floracion", "Madurez")))
  
} else if(crop == "arroz"){
  
  gstage <- data.frame(
    Growth_Phase = c("Emergencia", "Vegetativa", "Reproductiva", "Madurez"),
    start=c(sow_date, as.Date(phen$EDAT), as.Date(phen$IDAT), as.Date(phen$FDAT)),
    end=c(as.Date(phen$EDAT), as.Date(phen$IDAT), as.Date(phen$FDAT), as.Date(phen$MDAT))) %>% 
    mutate(Growth_Phase=factor(Growth_Phase, c("Emergencia", "Vegetativa", "Reproductiva", "Madurez")))
  
}



test_plot1 <- ggplot() +
  geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin=0, ymax= max(rain$rain)) +
  geom_col(data = rain %>% mutate(name = "Precipitacion"),
           aes(x = date, y = rain, color = "Precipitacion"), color = "blue", width = 0.5, fill = "lightblue") +
  # geom_line(data = hsoil, aes(date, value, color = Humedad_del_Suelo)) +
  #  ylim(3, 60) +
  scale_x_date(limits = c(as.Date(sow_date), as.Date(harv_date)), date_breaks = "7 day", date_labels = "%b %d") +
  #  scale_y_continuous(breaks = )
  #  xlim(as.Date(sow_date), as.Date(harv_date)) +
  #geom_hline(aes(yintercept = soil$WCST[1], linetype = "Saturacion"), colour = "blue") +
  #geom_hline(aes(yintercept = soil$WCST[2], linetype = "Saturacion"), colour = "blue") +
  #geom_hline(aes(yintercept = soil$WCFC[1], linetype = "Capacidad de Campo"), colour = "green") +
  #geom_hline(aes(yintercept = soil$WCFC[2], linetype = "Capacidad de Campo"), colour = "green") +
  #geom_hline(aes(yintercept = soil$WCWP[1], linetype = "Punto de Marchitez Permanente"), colour = "red") +
  #geom_hline(aes(yintercept = soil$WCWP[2], linetype = "Punto de Marchitez Permanente"), colour = "red") +
  #  geom_hline(aes(yintercept=17, linetype = "Punto de Marchitez Permanente"), colour = "red") +
  scale_fill_manual(values = alpha(c("coral4", "chartreuse", "darkgreen", "orange1"), 0.2)) +
  labs(title = paste0("Registro de Precipitacion y  humedad del suelo del sensor ", hsoil$id[[1]], " - Finca: ", finca),
       subtitle = paste0("Textura de suelo: ", "Capa 1: ",
                         soil$STC[1], "(", soil$DEPTH[1], " cm) ",  " -- Capa 2: ",
                         soil$STC[2], "(", soil$DEPTH[2], " cm) "),
       x = NULL,
       #x= "Fecha",
       y= "Precipitacion (mm)",
       color = "Humedad Volumetrica (% -  m³/m³): ",
       fill = "Etapa:") +
  #       linetype = "Referencias tipo:") +
  #  scale_x_continuous(labels = function(x) month.abb[x], breaks = 1:12) +
  theme_bw() + scale_color_viridis_d(option = "E") +
  theme(legend.position="none",#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        #   legend.position="bottom",
        #    legend.title = element_blank(),
        #    panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold")) +
  guides(color = guide_legend(order = 1)) +
  scale_linetype_manual(name = "Referencias: ", values = c(2, 2, 2),
                        breaks = c("Saturacion", "Capacidad de Campo", "Punto de Marchitez Permanente"),
                        guide = guide_legend(
                          override.aes = list(color = c("blue", "green", "red"))))
test_plot1
test_plot2 <- ggplot() +
  #  geom_col(data = rain %>% mutate(name = "Precipitacion"), aes(x = date, y = rain, color = name), color = "blue", width = 0.5, fill = "lightblue") +
  geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin=0, ymax= 70) +
  #  geom_point(data = hsoil, aes(date, value, color = Humedad_del_Suelo) , cex = 1.1) +
  geom_line(data = hsoil %>% filter(value>15), aes(date, value, color = Humedad_del_Suelo, group = Humedad_del_Suelo), na.rm = T, cex = 1.1) +
  #   ylim(3, 60) +
  scale_y_continuous(breaks = seq(0, 70, by = 10), limits=c(20, 70)) +
  scale_x_date(limits = c( sow_date, harv_date), date_breaks = "7 day", date_labels = "%b %d") +#,  expand = c(0, 0)) +
  #  xlim(as.Date(sow_date), as.Date(harv_date)) +
  geom_hline(aes(yintercept = soil$WCST[1], linetype = "Saturacion"), colour = "blue") +
  geom_hline(aes(yintercept = soil$WCST[2], linetype = "Saturacion"), colour = "blue") +
  geom_hline(aes(yintercept = soil$WCFC[1], linetype = "Capacidad de Campo"), colour = "green") +
  geom_hline(aes(yintercept = soil$WCFC[2], linetype = "Capacidad de Campo"), colour = "green") +
  geom_hline(aes(yintercept = soil$WCWP[1], linetype = "Punto de Marchitez Permanente"), colour = "red") +
  geom_hline(aes(yintercept = soil$WCWP[2], linetype = "Punto de Marchitez Permanente"), colour = "red") +
  scale_fill_manual(guide = guide_legend(nrow = 2),values = alpha(c("coral4", "chartreuse", "darkgreen", "orange1"), 0.2)) +
  labs(
    #title = paste0("Registro de humedad del suelo del sensor ", hsoil$id[[1]], " - Finca: ", finca),
    #x= "Fecha",
    x = NULL,
    y= "Porcentaje (%)",
    color = "Humedad Volumetrica (% -  m³/m³): ",
    fill = "Etapa del cultivo:")  +
  #subtitle = paste0("Textura de suelo: ", "Capa 1: ",
  #                 soil$STC[1], "(", soil$DEPTH[1], " cm) ",  " -- Capa2: ",
  #                 soil$STC[2], "(", soil$DEPTH[2], " cm) ")) +
  #       linetype = "Referencias tipo:") +
  guides(color = guide_legend(order = 1, title.position = "top", ncol = 2), fill = guide_legend(ncol = 2, title.position = "top")) +
  #  scale_x_continuous(labels = function(x) month.abb[x], breaks = 1:12) +
  theme_bw() + scale_color_viridis_d(option = "E") +
  theme(#axis.text.x = element_blank(),
    legend.position="bottom", #legend.box = "horizontal",
    #    legend.title = element_blank(),
    #    panel.grid.minor = element_blank(),
    strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
    strip.text = element_text(face = "bold")) +
  scale_linetype_manual(name = "Referencias Analisis Suelo: ", values = c(2, 2, 2),
                        breaks = c("Saturacion", "Capacidad de Campo", "Punto de Marchitez Permanente"),
                        guide = guide_legend(ncol = 2, title.position = "top",
                                             override.aes = list(color = c("blue", "green", "red"))))
test_plot2
test_bio <- ggplot() +
  geom_rect(data = gstage, aes(NULL, NULL, xmin=start, xmax=end, fill=Growth_Phase), ymin=0, ymax = max(grow$WAGT_OBS)) +
  geom_point(data =  grow, aes(x = as.Date(SAMPLING_DATE), y = WAGT_OBS)) +
  geom_errorbar(data =  grow, aes(x = as.Date(SAMPLING_DATE),
                                  ymin = WAGT_OBS - WAGT_SE,
                                  ymax = WAGT_OBS  + WAGT_SE, width = 4)) +
  theme_bw() + labs(title =  "Biomasa Total Acumulada", y = "Kg/ha") +
  scale_fill_manual(guide = guide_legend(nrow = 2),values = alpha(c("coral4", "chartreuse", "darkgreen", "orange1"), 0.2)) +
  theme(legend.position="none",#axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        #   legend.position="bottom",
        #    legend.title = element_blank(),
        #    panel.grid.minor = element_blank(),
        strip.background=element_rect(fill="white", size=1.5, linetype="solid"),
        strip.text = element_text(face = "bold"))
tt <- ggarrange(test_plot1, test_bio, test_plot2, nrow = 2, ncol = 2, widths = c(2,1))
ggsave(plot = tt, filename = paste0(id, ".png"), units = "mm", width = 380, height = 250)

