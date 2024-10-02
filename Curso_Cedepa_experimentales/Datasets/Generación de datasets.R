# ==========================================
# Clase 3: Diseño de Bloques Completos al Azar (DBCA) y Comparaciones Múltiples
# ==========================================

# -----------------------------
# 0. Preparación del Entorno
# -----------------------------

# Limpiar el entorno de trabajo para evitar conflictos con variables anteriores
rm(list = ls())

# Establecer semillas para reproducibilidad
set.seed(123)

# Instalar y cargar paquetes necesarios
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("agricolae")
# install.packages("car")
# install.packages("readr")
# install.packages("rstatix")
# install.packages("FSA")
# install.packages("userfriendlyscience")
# install.packages("nlme")
# install.packages("emmeans")
# install.packages("multcompView")

library(dplyr)                # Para manipulación de datos
library(ggplot2)              # Para visualizaciones
library(agricolae)            # Para diseños experimentales y análisis
library(car)                  # Para pruebas estadísticas
library(readr)                # Para leer y escribir archivos CSV
library(rstatix)              # Para pruebas estadísticas y comparaciones múltiples
library(FSA)                  # Para comparaciones de Dunn
library(userfriendlyscience)  # Para la prueba de Games-Howell
library(nlme)                 # Para modelos lineales generalizados
library(emmeans)              # Para obtener y comparar medias ajustadas
library(multcompView)        # Para asignar letras a grupos

# -----------------------------
# 1. Definición del Diseño Experimental
# -----------------------------

# Definir tratamientos y bloques
trt <- c("Tratamiento_A", "Tratamiento_B", "Tratamiento_C", "Tratamiento_D")
block <- c("Bloque_1", "Bloque_2", "Bloque_3", "Bloque_4")

# Crear el diseño DBCA utilizando agricolae
diseño_dbca <- design.rcbd(trt = trt, r = length(block), seed = 42)

# Reemplazar los números de bloque por las etiquetas de bloque
diseño_dbca$book$block <- factor(diseño_dbca$book$block, levels = 1:length(block), labels = block)

diseño_dbca$book

# -----------------------------
# 2. Generación de los Cuatro Datasets
# -----------------------------

# -----------------------------
# 2.1. Dataset A: Normalidad y Homogeneidad de Varianzas
# -----------------------------

# Simular datos con distribución normal y varianzas homogéneas
set.seed(123)

DBCA_datos_normal <- diseño_dbca$book %>%
  mutate(
    # Efecto del tratamiento
    efecto_tratamiento = case_when(
      trt == "Tratamiento_A" ~ 10,
      trt == "Tratamiento_B" ~ 15,
      trt == "Tratamiento_C" ~ 20,
      trt == "Tratamiento_D" ~ 25
    ),
    # Efecto del bloque (asumimos cero para simplificar)
    efecto_bloque = 0,
    # Generar el rendimiento con distribución normal y misma varianza
    Rendimiento = rnorm(n = n(), mean = efecto_tratamiento, sd = 2)
  ) %>%
  select(trt, block, Rendimiento)

DBCA_datos_normal

# Guardar el dataset en formato CSV
write_csv(DBCA_datos_normal, "DBCA_datos_normal.csv")

# -----------------------------
# 2.2. Dataset B: No Normalidad y No Homogeneidad de Varianzas
# -----------------------------

# Simular datos con distribución gamma (no normal) y varianzas heterogéneas
set.seed(456)

DBCA_datos_no_normal <- diseño_dbca$book %>%
  mutate(
    # Efecto del tratamiento
    efecto_tratamiento = case_when(
      trt == "Tratamiento_A" ~ 10,
      trt == "Tratamiento_B" ~ 15,
      trt == "Tratamiento_C" ~ 20,
      trt == "Tratamiento_D" ~ 25
    ),
    # Varianza heterogénea por tratamiento
    sd_tratamiento = case_when(
      trt == "Tratamiento_A" ~ 1,
      trt == "Tratamiento_B" ~ 3,
      trt == "Tratamiento_C" ~ 5,
      trt == "Tratamiento_D" ~ 7
    ),
    # Generar el rendimiento con distribución gamma
    Rendimiento = rgamma(n = n(), shape = efecto_tratamiento, scale = sd_tratamiento)
  ) %>%
  select(trt, block, Rendimiento)

DBCA_datos_no_normal

# Guardar el dataset en formato CSV
write_csv(DBCA_datos_no_normal, "DBCA_datos_no_normal.csv")

# -----------------------------
# 2.3. Dataset C: Normalidad pero Sin Homogeneidad de Varianzas
# -----------------------------

# Simular datos con distribución normal pero varianzas heterogéneas
set.seed(789)

DBCA_datos_normal_no_homocedasticidad <- diseño_dbca$book %>%
  mutate(
    # Efecto del tratamiento
    efecto_tratamiento = case_when(
      trt == "Tratamiento_A" ~ 10,
      trt == "Tratamiento_B" ~ 15,
      trt == "Tratamiento_C" ~ 20,
      trt == "Tratamiento_D" ~ 25
    ),
    # Varianza heterogénea por tratamiento
    sd_tratamiento = case_when(
      trt == "Tratamiento_A" ~ 2,
      trt == "Tratamiento_B" ~ 4,
      trt == "Tratamiento_C" ~ 6,
      trt == "Tratamiento_D" ~ 8
    ),
    # Generar el rendimiento con distribución normal y varianzas diferentes
    Rendimiento = rnorm(n = n(), mean = efecto_tratamiento, sd = sd_tratamiento)
  ) %>%
  select(trt, block, Rendimiento)

DBCA_datos_normal_no_homocedasticidad

# Guardar el dataset en formato CSV
write_csv(DBCA_datos_normal_no_homocedasticidad, "DBCA_datos_normal_no_homocedasticidad.csv")

# -----------------------------
# 2.4. Dataset D: No Normalidad pero Sí Homogeneidad de Varianzas
# -----------------------------

# Simular datos con distribución exponencial (no normal) pero varianzas homogéneas
set.seed(101112)

DBCA_datos_no_normal_homocedasticidad <- diseño_dbca$book %>%
  mutate(
    # Efecto del tratamiento
    efecto_tratamiento = case_when(
      trt == "Tratamiento_A" ~ 10,
      trt == "Tratamiento_B" ~ 15,
      trt == "Tratamiento_C" ~ 20,
      trt == "Tratamiento_D" ~ 25
    ),
    # Varianza homogénea (misma desviación estándar)
    sd_tratamiento = 5,
    # Generar el rendimiento con distribución exponencial
    Rendimiento = rexp(n = n(), rate = 1/sd_tratamiento) + efecto_tratamiento
  ) %>%
  select(trt, block, Rendimiento)

DBCA_datos_no_normal_homocedasticidad

# Guardar el dataset en formato CSV
write_csv(DBCA_datos_no_normal_homocedasticidad, "DBCA_datos_no_normal_homocedasticidad.csv")
