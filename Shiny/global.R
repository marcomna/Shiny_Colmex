library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(haven)
library(readxl)
library(doBy)
library(rdrop2)
library(gganimate)
library(dashboardthemes)
library(gganimate)
library(ggplot2)

linebreaks <- function(n){HTML(strrep(br(),n))}

### SECCIÓN MUNDIAL ###

owid <- read_excel("www/owid-covid-data.xlsx", guess_max = 132431)
owid$fecha <- as.Date(owid$date)

owid_m <- subset(owid, iso_code == "OWID_WRL")
gc()

owid_m_casos <- owid_m %>% 
  select(date, total_cases, total_cases_per_million, new_cases, new_cases_smoothed, new_cases_smoothed_per_million)
colnames(owid_m_casos) <- c("Fecha", "Casos totales", "Casos totales por millon de habitantes", 
                            "Casos nuevos", "Promedio movil de 7 días de casos nuevos", 
                            "Promedio movil de 7 días por millon de habitantes de casos nuevos")
owid_m_casos <- gather(owid_m_casos, Variable1, Casos, c("Casos totales", 
                                                         "Casos totales por millon de habitantes", 
                                                         "Casos nuevos", "Promedio movil de 7 días de casos nuevos", 
                                                         "Promedio movil de 7 días por millon de habitantes de casos nuevos"), 
                       factor_key = T)

owid_m_muertes <- owid_m %>% 
  select(date, total_deaths, total_deaths_per_million, new_deaths, new_deaths_smoothed, new_deaths_smoothed_per_million)
colnames(owid_m_muertes) <- c("Fecha", "Muertes totales", 
                              "Muertes totales por millon de habitantes", "Muertes nuevas", 
                              "Promedio movil de 7 días de muertes nuevas", 
                              "Promedio movil de 7 días por millon de habitantes de muertes nuevas")
owid_m_muertes <- gather(owid_m_muertes, Variable2, Muertes, c("Muertes totales", 
                                                               "Muertes totales por millon de habitantes", 
                                                               "Muertes nuevas", "Promedio movil de 7 días de muertes nuevas", 
                                                               "Promedio movil de 7 días por millon de habitantes de muertes nuevas"), 
                         factor_key = T)

### CONTINENTES ###

owid <- read_excel("www/owid-covid-data.xlsx", guess_max = 132431)
owid$fecha <- as.Date(owid$date)

owid_cont <- owid %>% 
  filter(iso_code %in% c("OWID_AFR", "OWID_ASI", "OWID_EUR", 
                         "OWID_NAM", "OWID_OCE", "OWID_SAM"))


owid_cont <- owid_cont %>% 
  select(date, location, total_cases_per_million, new_cases_smoothed_per_million, total_deaths_per_million, 
         new_deaths_smoothed_per_million, people_vaccinated_per_hundred, 
         people_fully_vaccinated_per_hundred, new_vaccinations_smoothed_per_million)

colnames(owid_cont) <- c("Fecha", "Continente", "Casos totales por millon de habitantes", 
                         "Promedio movil de 7 días por millon de habitantes de casos nuevos", 
                         "Muertes totales por millon de habitantes", 
                         "Promedio movil de 7 días por millon de habitantes de muertes nuevas", 
                         "Personas vacunadas de cada cien", "Personas completamente vacunadas de cada cien",
                         "Promedio movil de 7 días de nuevas vacunas aplicadas por millon de habitantes")
owid_cont <- gather(owid_cont, Variable3, Valor, c("Casos totales por millon de habitantes", 
                                                   "Promedio movil de 7 días por millon de habitantes de casos nuevos", 
                                                   "Muertes totales por millon de habitantes", 
                                                   "Promedio movil de 7 días por millon de habitantes de muertes nuevas", 
                                                   "Personas vacunadas de cada cien", "Personas completamente vacunadas de cada cien",
                                                   "Promedio movil de 7 días de nuevas vacunas aplicadas por millon de habitantes"))

owid_cont <- spread(owid_cont, key=Continente, value=Valor)

colnames(owid_cont) <- c("Fecha", "Variable3", "Africa", "Asia", "Europa", "Norteamerica", "Oceania", "Sudamerica")

### GRÁFICAS DE BARRAS ###

owid_barras_casos <- owid %>% 
  select(continent, new_cases, date)

owid_barras_casos <- owid_barras_casos %>%
  filter(new_cases >= 0 & continent != "NA")

names(owid_barras_casos) <- c("Continente", "NuevosCasos", "Fecha")

owid_barras_casos$Continente[owid_barras_casos$Continente == "Europe"] <- "Europa"
owid_barras_casos$Continente[owid_barras_casos$Continente == "North America"] <- "Norteamerica"
owid_barras_casos$Continente[owid_barras_casos$Continente == "South America"] <- "Sudamerica"

###

owid_barras_muertes <- owid %>% 
  select(continent, new_deaths, date)

owid_barras_muertes <- owid_barras_muertes %>%
  filter(new_deaths >= 0 & continent != "NA")

names(owid_barras_muertes) <- c("Continente", "NuevasMuertes", "Fecha")

owid_barras_muertes$Continente[owid_barras_muertes$Continente == "Europe"] <- "Europa"
owid_barras_muertes$Continente[owid_barras_muertes$Continente == "North America"] <- "Norteamerica"
owid_barras_muertes$Continente[owid_barras_muertes$Continente == "South America"] <- "Sudamerica"


### MAPAS MUNDIAL ###

owid$datemonth <- format(as.Date(owid$date), "%Y-%m")

owidmapas <- owid %>%
  select(continent, datemonth, location, iso_code, total_cases_per_million, total_deaths_per_million)
  
owidmapas <- owidmapas %>% group_by(continent, datemonth, iso_code) %>%
  summarise(total_cases_per_million = mean(total_cases_per_million, na.rm = T), total_deaths_per_million = mean(total_deaths_per_million, na.rm = T))

owid2 <- subset(owid, iso_code != "OWID_AFR" & iso_code != "OWID_ASI"
                & iso_code != "OWID_EUR" & iso_code != "OWID_EUN" & iso_code != "OWID_KOS"
                & iso_code != "OWID_NAM" & iso_code != "OWID_OCE" & iso_code != "OWID_SAM"
                & iso_code != "OWID_INT" & iso_code != "OWID_WRL")

owidmapas2 <- owid2 %>%
  select(continent, datemonth, location, iso_code, total_cases_per_million, total_deaths_per_million)

owidmapas2 <- owidmapas2 %>% group_by(continent, datemonth, iso_code) %>%
  summarise(total_cases_per_million = mean(total_cases_per_million, na.rm = T), total_deaths_per_million = mean(total_deaths_per_million, na.rm = T))


### SECCIÓN LATINOAMÉRICA ###


owidLA <- subset(owid, iso_code != "OWID_AFR" & iso_code != "OWID_ASI"
                & iso_code != "OWID_EUR" & iso_code != "OWID_EUN" & iso_code != "OWID_KOS"
                & iso_code != "OWID_NAM" & iso_code != "OWID_OCE" & iso_code != "OWID_SAM"
                & iso_code != "OWID_INT" & iso_code != "OWID_WRL")

owidLA <- owidLA %>% 
  filter(location %in% c("Colombia", "Mexico", "Argentina", "Brazil", "Peru"))

owidLA <- owidLA %>% 
  select(date, location, total_cases_per_million, new_cases_smoothed_per_million, total_deaths_per_million, 
         new_deaths_smoothed_per_million, people_vaccinated_per_hundred, 
         people_fully_vaccinated_per_hundred, new_vaccinations_smoothed_per_million)

colnames(owidLA) <- c("Fecha", "Pais", "Casos totales por millon de habitantes", 
                         "Promedio movil de 7 días por millon de habitantes de casos nuevos", 
                         "Muertes totales por millon de habitantes", 
                         "Promedio movil de 7 días por millon de habitantes de muertes nuevas", 
                         "Personas vacunadas de cada cien", "Personas completamente vacunadas de cada cien",
                         "Promedio movil de 7 días de nuevas vacunas aplicadas por millon de habitantes")

owidLA <- gather(owidLA, Variable4, Valor, c("Casos totales por millon de habitantes", 
                                                   "Promedio movil de 7 días por millon de habitantes de casos nuevos", 
                                                   "Muertes totales por millon de habitantes", 
                                                   "Promedio movil de 7 días por millon de habitantes de muertes nuevas", 
                                                   "Personas vacunadas de cada cien", "Personas completamente vacunadas de cada cien",
                                                   "Promedio movil de 7 días de nuevas vacunas aplicadas por millon de habitantes"))

owidLA <- spread(owidLA, key=Pais, value=Valor)

### GRÁFICAS DE BARRAS LA ###

owid_barras_casos_la <- owid %>% 
  select(location, new_cases, date)

owid_barras_casos_la <- owid_barras_casos_la %>%
  filter(new_cases >= 0 & location %in% c("Mexico", "Colombia", "Brazil", "Argentina", "Peru", "Chile" ))

names(owid_barras_casos_la) <- c("Pais", "NuevosCasos", "Fecha")

owid_barras_casos_la$Pais[owid_barras_casos_la$Pais == "Mexico"] <- "México"
owid_barras_casos_la$Pais[owid_barras_casos_la$Pais == "Brazil"] <- "Brasil"
owid_barras_casos_la$Pais[owid_barras_casos_la$Pais == "Peru"] <- "Perú"

###

owid_barras_muertes_la <- owid %>% 
  select(location, new_deaths, date)

owid_barras_muertes_la <- owid_barras_muertes_la %>%
  filter(new_deaths >= 0 & location %in% c("Mexico", "Colombia", "Brazil", "Argentina", "Peru", "Chile" ))

names(owid_barras_muertes_la) <- c("Pais", "NuevasMuertes", "Fecha")

owid_barras_muertes_la$Pais[owid_barras_muertes_la$Pais == "Mexico"] <- "México"
owid_barras_muertes_la$Pais[owid_barras_muertes_la$Pais == "Brazil"] <- "Brasil"
owid_barras_muertes_la$Pais[owid_barras_muertes_la$Pais == "Peru"] <- "Perú"


###

owid_barras_vacunas_la <- owid %>% 
  select(location, new_vaccinations, date)

owid_barras_vacunas_la <- owid_barras_vacunas_la %>%
  filter(new_vaccinations >= 0 & location %in% c("Mexico", "Colombia", "Brazil", "Argentina", "Peru", "Chile" ))

names(owid_barras_vacunas_la) <- c("Pais", "NuevasVacunas", "Fecha")

owid_barras_vacunas_la$Pais[owid_barras_vacunas_la$Pais == "Mexico"] <- "México"
owid_barras_vacunas_la$Pais[owid_barras_vacunas_la$Pais == "Brazil"] <- "Brasil"
owid_barras_vacunas_la$Pais[owid_barras_vacunas_la$Pais == "Peru"] <- "Perú"

###############

glosario_total <- read_csv("www/glosario.csv")




