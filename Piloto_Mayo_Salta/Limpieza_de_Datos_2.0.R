# Cargamos las Librerias
library(readr)
library(dplyr)
library(here)
library(ggplot2)
library(plotrix)

######### ANALISIS POR SUJETO ##########

# Cargamos los datos por csv y armarmos un csv por sujeto

# Recordar cargar el directorio de trabajo antes de continuar.

Sujetos <- list()

Directorio <- "~/Exp_Dist_Piloto_Mayo_2023-main/Data"

for (i in 1:27) {
  Sujeto <- paste0("Sujeto_", i)
  file_list <- list.files(path = Directorio,
                          pattern =  paste0("^S", i, "_.*\\.csv$"),
                          full.names = TRUE)
  nueva_lista <- bind_rows(lapply(file_list, read_csv)) %>%
    mutate(ID = Sujeto)
  Sujetos[[Sujeto]] <- nueva_lista
}
  rm(nueva_lista, file_list, i, Sujeto, Directorio)

#Eliminamos los Sujetos que presentan valores superiores a 150 e inferiores a 1 m en su mayoria
  
Sujetos$Sujeto_5 <- filter(Sujetos$Sujeto_5, respuesta < 20) 
Sujetos$Sujeto_19 <- filter(Sujetos$Sujeto_19, respuesta > 0.2) 
Sujetos$Sujeto_24 <- filter(Sujetos$Sujeto_24, respuesta < 8) 
  
  
# Eliminamos Los Tibbles de sujetos que tiraron data muy lejana
  
Sujetos$Sujeto_3 <- NULL
Sujetos$Sujeto_2 <- NULL
Sujetos$Sujeto_4 <- NULL 
Sujetos$Sujeto_13 <- NULL # Espacio Peripersonal
Sujetos$Sujeto_23 <- NULL # Ceguera Total
Sujetos$Sujeto_24 <- NULL # Espacio Peripersonal
  
#Reescribimos las columnas de conidicion para que digan los valores

for (Num_Sujeto in 1:21) {

Sujetos[[Num_Sujeto]] <- Sujetos[[Num_Sujeto]] %>% 
  mutate(condicion = case_when(
    condicion == 0 ~ "Wide-Wide",
    condicion == 1 ~ "Low-Low",
    condicion == 2 ~ "High-High",
    condicion == 3 ~ "High-Low",
    condicion == 4 ~ "Low-High",
    TRUE ~ as.character(condicion)
    ))


# Graficamos todas las condiciones por sujeto modificando la ubicacion en la liste de tibbles


Grafica <- ggplot(Sujetos[[Num_Sujeto]], aes(x=distancia, 
                                             y=respuesta,
                                             color = condicion)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # facet_grid(rows = vars(condicion)) +
  labs(title = Sujetos[[Num_Sujeto]]$ID)
theme_minimal()

ggsave(filename = paste0("Intra_All_", Num_Sujeto, ".png"), plot = Grafica, width = 6, height = 4, dpi = 300)
}


##### Tibbles promediados ####
# Todos

Sub_All <- tibble()

for (i in 1:21) {
  nueva_data <-  Sujetos[[i]]  
  Sub_All <- rbind(Sub_All, nueva_data) %>%
    mutate(condicion = case_when(
      condicion == 0 ~ "Wide-Wide",
      condicion == 1 ~ "Low-Low",
      condicion == 2 ~ "High-High",
      condicion == 3 ~ "High-Low",
      condicion == 4 ~ "Low-High",
      TRUE ~ as.character(condicion)))
}


Prom_All <- Sub_All %>%
  group_by(condicion, distancia) %>%
  summarise(promedio = mean(respuesta), 
            sd = sd(respuesta), 
            std = std.error(respuesta))

### GRAFICAS ###
# Graficas Generales

Inf_Condicion = "Wide-Wide"

G_Cond <- ggplot(Sub_All %>%
                   filter(grepl(paste0(Inf_Condicion), condicion)), aes(x=distancia,
                                                              y=respuesta,
                                                              color = ID)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # scale_x_log10() +
  # scale_y_log10() +
  # facet_grid(rows = vars(ID)) +
  labs(title = paste0("Valores Individuales ", Inf_Condicion)) +
  theme_minimal()

G_Cond

#GUARDAR
ggsave(filename = pasteO("Valores Individuales ", Inf_Condicion), plot = G_Cond, width = 12, height = 8, dpi = 600)


#### Graficas de Promedios ####

G_Prom <- ggplot(Prom_All %>% 
                   filter(grepl(paste0(Inf_Condicion), condicion)), aes(x=distancia,
                                                              y=promedio,
                                                              color = condicion)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # scale_x_log10() +
  # scale_y_log10() +
  geom_errorbar(aes(ymin=promedio-std, ymax=promedio+std), width = 0.2) +
  # facet_grid(rows = vars(ID)) +
  labs(title = paste0("Promedio y Barras de Error")) +
  theme_minimal()

G_Prom

#GUARDAR
ggsave(filename = pasteO("Promedio y Barras de Error ", Inf_Condicion), plot = G_Prom, width = 12, height = 8, dpi = 600)

#Graficos Promedio Total

G_Prom_All <- ggplot(Prom_All, aes(x=distancia,
                               y=promedio,
                               color = condicion)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # scale_x_log10() +
  # scale_y_log10() +
  geom_errorbar(aes(ymin=promedio-std, ymax=promedio+std), width = 0.2) +
  # facet_grid(rows = vars(ID)) +
  labs(title = "Promedio y Barras de Error") +
  theme_minimal()

G_Prom_All

#GUARDAR
ggsave(filename = "Promedio y Barras de Error", plot = G_Prom_All, width = 12, height = 8, dpi = 600)
