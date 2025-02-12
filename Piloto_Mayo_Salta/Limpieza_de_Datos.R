# Cargamos las Librerias
library(readr)
library(dplyr)
library(here)
library(ggplot2)

######### ANALISIS POR SUJETO ##########

# Cargamos los datos por csv y armarmos un csv por sujeto

# Recordar cargar el directorio de trabajo antes de continuar.

Sujetos <- list()

Directorio <- "~/Doctorado/Piloto_Mayo_2023/Data"

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

######### ANALISIS POR CONDICIÓN ##########

Wide_Wide <- tibble()
Low_Low <- tibble()
High_High <- tibble()
High_Low <- tibble()
Low_High <- tibble()

for (i in 1:21) {
  nueva_data <-  filter(Sujetos[[i]], condicion == 0)  
  Wide_Wide <- rbind(Wide_Wide, nueva_data)
}

for (i in 1:21) {
  nueva_data <-  filter(Sujetos[[i]], condicion == 1)  
  Low_Low <- rbind(Low_Low, nueva_data)
}

for (i in 1:21) {
  nueva_data <-  filter(Sujetos[[i]], condicion == 2)  
  High_High <- rbind(High_High, nueva_data)
}

for (i in 1:21) {
  nueva_data <-  filter(Sujetos[[i]], condicion == 3)  
  High_Low <- rbind(High_Low, nueva_data)
}

for (i in 1:21) {
  nueva_data <-  filter(Sujetos[[i]], condicion == 4)  
  Low_High <- rbind(Low_High, nueva_data)
}

## Grafica por Condición ##

## Wide - Wide ##

G_Wide_Wide_Sujeto <- ggplot(Wide_Wide, aes(x=distancia, 
                                          y=respuesta,
                                          color = ID)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # facet_grid(rows = vars(ID)) +
  labs(title = "Wide-Wide [Sujetos]") +
  theme_minimal()

ggsave(filename = "G_Wide_Wide_Sujeto.jpg", plot = G_Wide_Wide_Sujeto, width = 12, height = 8, dpi = 600)

G_Wide_Wide_All <- ggplot(Wide_Wide, aes(x=distancia, 
                                       y=respuesta)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # facet_grid(rows = vars(ID)) +
  labs(title = "Wide-Wide [All]") +
  theme_minimal()

ggsave(filename = "G_Wide_Wide_All.jpg", plot = G_Wide_Wide_All, width = 12, height = 8, dpi = 600)

## Low - Low ##

G_Low_Low_Sujeto <- ggplot(Low_Low, aes(x=distancia, 
                                          y=respuesta,
                                          color = ID)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # facet_grid(rows = vars(ID)) +
  labs(title = "Low-Low [Sujetos]") +
  theme_minimal()

ggsave(filename = "G_Low_Low_Sujeto.jpg", plot = G_Low_Low_Sujeto, width = 12, height = 8, dpi = 600)


G_Low_Low_All <- ggplot(Low_Low, aes(x=distancia, 
                                       y=respuesta)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # facet_grid(rows = vars(ID)) +
  labs(title = "Low-Low [All]") +
  theme_minimal()

ggsave(filename = "G_Low_Low_All.jpg", plot = G_Low_Low_All, width = 12, height = 8, dpi = 600)

## High - High ##

G_High_High_Sujeto <- ggplot(High_High, aes(x=distancia, 
                                 y=respuesta,
                                 color = ID)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # facet_grid(rows = vars(ID)) +
  labs(title = "High-High [Sujetos]") +
  theme_minimal()

ggsave(filename = "G_High_High_Sujeto.jpg", plot = G_High_High_Sujeto, width = 12, height = 8, dpi = 600)


G_High_High_All <- ggplot(High_High, aes(x=distancia, 
                                 y=respuesta)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # facet_grid(rows = vars(ID)) +
  labs(title = "High-High [All]") +
  theme_minimal()

ggsave(filename = "G_High_High_All.jpg", plot = G_High_High_All, width = 12, height = 8, dpi = 600)

## High - Low ##

G_High_Low_Sujeto <- ggplot(High_Low, aes(x=distancia, 
                                            y=respuesta,
                                            color = ID)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # facet_grid(rows = vars(ID)) +
  labs(title = "High-Low [Sujetos]") +
  theme_minimal()

ggsave(filename = "G_High_Low_Sujeto.jpg", plot = G_High_Low_Sujeto, width = 12, height = 8, dpi = 600)


G_High_Low_All <- ggplot(High_Low, aes(x=distancia, 
                                         y=respuesta)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # facet_grid(rows = vars(ID)) +
  labs(title = "High-High [All]") +
  theme_minimal()

ggsave(filename = "G_High_Low_All.jpg", plot = G_High_Low_All, width = 12, height = 8, dpi = 600)

## High - Low ##

G_Low_High_Sujeto <- ggplot(Low_High, aes(x=distancia, 
                                          y=respuesta,
                                          color = ID)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # facet_grid(rows = vars(ID)) +
  labs(title = "Low-High [Sujetos]") +
  theme_minimal()

ggsave(filename = "G_Low_High_Sujeto.jpg", plot = G_Low_High_Sujeto, width = 12, height = 8, dpi = 600)

G_Low_High_All <- ggplot(Low_High, aes(x=distancia, 
                                       y=respuesta)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  # facet_grid(rows = vars(ID)) +
  labs(title = "Low-High [All]") +
  theme_minimal()

ggsave(filename = "G_Low_High_All.jpg", plot = G_Low_High_All, width = 12, height = 8, dpi = 600)