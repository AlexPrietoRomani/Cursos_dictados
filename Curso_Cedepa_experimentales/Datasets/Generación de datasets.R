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
    # Efecto base común para todos los tratamientos
    efecto_base = 50,  
    
    # Aumentar significativamente la diferencia entre tratamientos
    efecto_tratamiento = case_when(
      trt == "Tratamiento_A" ~ 20,   # Cambiar de 15 a 20
      trt == "Tratamiento_B" ~ 60,   # Cambiar de 35 a 60
      trt == "Tratamiento_C" ~ 100,  # Cambiar de 55 a 100
      trt == "Tratamiento_D" ~ 140   # Cambiar de 75 a 140
    ),
    
    # Reducir a cero el efecto de los bloques para evitar su influencia
    efecto_bloque = 0,
    
    # Generar el rendimiento con distribución normal y menor variabilidad dentro de tratamientos
    Rendimiento = rnorm(n = n(), mean = efecto_base + efecto_tratamiento + efecto_bloque, sd = 2)
  ) %>%
  select(trt, block, Rendimiento)

# Mostrar una vista previa del dataset
head(DBCA_datos_tratamiento)

# Guardar el dataset en formato CSV
write.csv(DBCA_datos_normal, "C:\\Users\\alexa\\OneDrive\\Documentos\\Cursos_dictados\\Curso_Cedepa_experimentales\\Datasets\\DBCA_datos_normal.csv")

# -----------------------------
# 2.2. Dataset B: No Normalidad y No Homogeneidad de Varianzas
# -----------------------------

# Establecer semilla para reproducibilidad
set.seed(123)

# Crear el diseño DBCA básico con 4 bloques y 4 tratamientos, 5 parcelas por tratamiento por bloque (total 80)
n_blocks <- 4
n_trts <- 4
n_reps <- 5  # Parcelas por tratamiento por bloque

# Crear el dataframe diseño_dbca$book
DBCA_datos_no_normal <- data.frame(
  plots = 1:(n_blocks * n_trts * n_reps),
  block = rep(paste("Bloque", 1:n_blocks), each = n_trts * n_reps),
  trt = rep(rep(c("Tratamiento_A", "Tratamiento_B", "Tratamiento_C", "Tratamiento_D"), each = n_reps), times = n_blocks)
)

# Asignar los rendimientos manualmente para garantizar las características deseadas
DBCA_datos_no_normal$Rendimiento <- NA

# **Asignación de Rendimientos para Cada Tratamiento**

# Tratamiento_A: Distribución normal con varianza baja (sd = 2)
DBCA_datos_no_normal$Rendimiento[DBCA_datos_no_normal$trt == 'Tratamiento_A'] <- rnorm(n_reps * n_blocks, mean = 50, sd = 2)

# Tratamiento_B: Distribución exponencial (sesgada a la derecha) con varianza alta (rate = 1/5, varianza = 25)
DBCA_datos_no_normal$Rendimiento[DBCA_datos_no_normal$trt == 'Tratamiento_B'] <- rexp(n_reps * n_blocks, rate = 1/5)

# Tratamiento_C: Distribución log-normal (altamente sesgada) con varianza muy alta
DBCA_datos_no_normal$Rendimiento[DBCA_datos_no_normal$trt == 'Tratamiento_C'] <- rlnorm(n_reps * n_blocks, meanlog = 3, sdlog = 1)

# Tratamiento_D: Distribución normal con outliers extremos para aumentar la varianza
# Generar 4 valores normales y 1 outlier por bloque
for (b in 1:n_blocks) {
  indices <- which(DBCA_datos_no_normal$block == paste("Bloque", b) & DBCA_datos_no_normal$trt == 'Tratamiento_D')
  DBCA_datos_no_normal$Rendimiento[indices[1:4]] <- rnorm(4, mean = 50, sd = 2)
  DBCA_datos_no_normal$Rendimiento[indices[5]] <- 150  # Outlier extremo
}

# Visualizar las primeras filas del dataset
print("Primeras filas del dataset:")
print(head(DBCA_datos_no_normal))

# **Verificación de los Supuestos**

# Ajustar el modelo ANOVA
anova_model <- aov(Rendimiento ~ trt + block, data = DBCA_datos_no_normal)

# Obtener los residuos del modelo
residuos <- residuals(anova_model)

# **Prueba de Shapiro-Wilk para Normalidad**
shapiro_b <- shapiro.test(residuos)
print("Prueba de Shapiro-Wilk para normalidad (Dataset B):")
print(shapiro_b)

# **Prueba de Levene para Homogeneidad de Varianzas**
levene_b <- leveneTest(Rendimiento ~ trt, data = DBCA_datos_no_normal)
print("Prueba de Levene para homogeneidad de varianzas (Dataset B):")
print(levene_b)

# **Varianzas por Tratamiento**
variances <- tapply(DBCA_datos_no_normal$Rendimiento, DBCA_datos_no_normal$trt, var)
print("Varianzas por tratamiento:")
print(variances)

# **Visualizaciones Gráficas**

# Histograma de los rendimientos
hist(DBCA_datos_no_normal$Rendimiento, main = "Histograma de Rendimientos (Dataset B)", 
     xlab = "Rendimiento", col = "lightgreen")

# Gráfico Q-Q de los residuos
qqnorm(residuos, main = "Q-Q Plot de Residuales (Dataset B)")
qqline(residuos, col = "red")

# Boxplot por tratamiento
boxplot(Rendimiento ~ trt, data = DBCA_datos_no_normal, 
        main = "Boxplot de Rendimiento por Tratamiento (Dataset B)",
        xlab = "Tratamiento", ylab = "Rendimiento")

# Guardar el dataset en formato CSV
write_csv(DBCA_datos_no_normal, "C:\\Users\\alexa\\OneDrive\\Documentos\\Cursos_dictados\\Curso_Cedepa_experimentales\\Datasets\\DBCA_datos_no_normal.csv")

# -----------------------------
# 2.3. Dataset C: Normalidad pero Sin Homogeneidad de Varianzas
# -----------------------------

# Simular datos con distribución normal pero varianzas heterogéneas
set.seed(789)

DBCA_datos_normal_no_homocedasticidad <- diseño_dbca$book %>%
  mutate(
    # Efecto del tratamiento (aumentando las diferencias entre ellos)
    efecto_tratamiento = case_when(
      trt == "Tratamiento_A" ~ 10,
      trt == "Tratamiento_B" ~ 30,  # Aumentar significativamente el efecto
      trt == "Tratamiento_C" ~ 50,  # Aumentar significativamente el efecto
      trt == "Tratamiento_D" ~ 70   # Aumentar significativamente el efecto
    ),
    # Aumentar considerablemente la diferencia en la varianza
    sd_tratamiento = case_when(
      trt == "Tratamiento_A" ~ 5,    # Tratamiento con menor varianza
      trt == "Tratamiento_B" ~ 20,   # Aumentar considerablemente la varianza
      trt == "Tratamiento_C" ~ 30,   # Incremento mayor en la varianza
      trt == "Tratamiento_D" ~ 40    # Tratamiento con mayor varianza
    ),
    # Generar el rendimiento con distribución normal y varianzas muy diferentes
    Rendimiento = rnorm(n = n(), mean = efecto_tratamiento, sd = sd_tratamiento)
  ) %>%
  select(trt, block, Rendimiento)

DBCA_datos_normal_no_homocedasticidad

# Guardar el dataset en formato CSV
write_csv(DBCA_datos_normal_no_homocedasticidad, "C:\\Users\\alexa\\OneDrive\\Documentos\\Cursos_dictados\\Curso_Cedepa_experimentales\\Datasets\\DBCA_datos_normal_no_homocedasticidad.csv")

# -----------------------------
# 2.4. Dataset D: No Normalidad pero Sí Homogeneidad de Varianzas
# -----------------------------

# Cargar el paquete necesario
library(car)  # Para la prueba de Levene

# Establecer semilla para reproducibilidad
set.seed(123)

# Crear el diseño DBCA básico con 4 bloques y 4 tratamientos
diseño_dbca <- list()
diseño_dbca$book <- data.frame(
  plots = 1:16,
  block = rep(paste("Bloque", 1:4), each = 4),
  trt = rep(c("Tratamiento_A", "Tratamiento_B", "Tratamiento_C", "Tratamiento_D"), times = 4)
)

# Crear el dataframe DBCA_datos_no_normal_homocedasticidad a partir de diseño_dbca$book
DBCA_datos_no_normal_homocedasticidad <- diseño_dbca$book

# Asignar los rendimientos manualmente para garantizar las características deseadas
DBCA_datos_no_normal_homocedasticidad$Rendimiento <- NA

# **Asignación de Rendimientos para Cada Tratamiento**

# Tratamiento_A: Distribución uniforme
DBCA_datos_no_normal_homocedasticidad$Rendimiento[DBCA_datos_no_normal_homocedasticidad$trt == 'Tratamiento_A'] <- runif(4, min=40, max=60)

# Tratamiento_B: Distribución uniforme
DBCA_datos_no_normal_homocedasticidad$Rendimiento[DBCA_datos_no_normal_homocedasticidad$trt == 'Tratamiento_B'] <- runif(4, min=40, max=60)

# Tratamiento_C: Distribución uniforme
DBCA_datos_no_normal_homocedasticidad$Rendimiento[DBCA_datos_no_normal_homocedasticidad$trt == 'Tratamiento_C'] <- runif(4, min=40, max=60)

# Tratamiento_D: Distribución uniforme
DBCA_datos_no_normal_homocedasticidad$Rendimiento[DBCA_datos_no_normal_homocedasticidad$trt == 'Tratamiento_D'] <- runif(4, min=40, max=60)

# **Verificación de los Supuestos**

# Ajustar el modelo ANOVA
anova_model <- aov(Rendimiento ~ trt + block, data = DBCA_datos_no_normal_homocedasticidad)

# Obtener los residuos del modelo
residuos <- residuals(anova_model)

# **Prueba de Shapiro-Wilk para Normalidad**
shapiro_b <- shapiro.test(residuos)
print("Prueba de Shapiro-Wilk para normalidad (Dataset B):")
print(shapiro_b)

# **Prueba de Levene para Homogeneidad de Varianzas**
levene_b <- leveneTest(Rendimiento ~ trt, data = DBCA_datos_no_normal_homocedasticidad)
print("Prueba de Levene para homogeneidad de varianzas (Dataset B):")
print(levene_b)

# **Varianzas por Tratamiento**
variances <- tapply(DBCA_datos_no_normal_homocedasticidad$Rendimiento, DBCA_datos_no_normal_homocedasticidad$trt, var)
print("Varianzas por tratamiento:")
print(variances)

# Guardar el dataset en formato CSV
write_csv(DBCA_datos_no_normal_homocedasticidad, "C:\\Users\\alexa\\OneDrive\\Documentos\\Cursos_dictados\\Curso_Cedepa_experimentales\\Datasets\\DBCA_datos_no_normal_homocedasticidad.csv")
