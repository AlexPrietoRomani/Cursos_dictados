# ==========================================
# Clase 5: Diseños Avanzados y Resumen General
# ==========================================

# -----------------------------
# 0. Preparación del Entorno
# -----------------------------

# Limpiar el entorno de trabajo
rm(list = ls())

# Instalar y cargar paquetes necesarios
# Descomenta las líneas de instalación si no tienes los paquetes instalados
# install.packages("dplyr")
# install.packages("agricolae")
# install.packages("DiGGer")
# install.packages("lattice")
# install.packages("lme4")
# install.packages("emmeans")
# install.packages("ggplot2")
# install.packages("reshape2")

library(dplyr)        # Manipulación de datos
library(agricolae)    # Diseños experimentales y análisis
library(DiGGer)       # Diseños óptimos de experimentos
library(lattice)      # Visualización de datos
library(lme4)         # Modelos lineales mixtos
library(emmeans)      # Comparaciones de medias ajustadas
library(ggplot2)      # Visualizaciones avanzadas
library(reshape2)     # Transformación de datos

# -----------------------------
# 1. Diseños Avanzados
# -----------------------------

# 1.1. Diseño de bloques aumentados: concepto y uso en mejoramiento

# Un diseño de bloques aumentados (Augmented Block Design, ABD) se utiliza cuando tenemos
# tratamientos control (checks) y nuevos tratamientos (genotipos) que queremos evaluar.
# Los controles se replican en todos los bloques, mientras que los nuevos tratamientos
# aparecen una sola vez.

# Supongamos que tenemos 3 controles y 15 nuevos genotipos a evaluar en 5 bloques.

# Definir los controles y nuevos tratamientos
controles <- c("Control_A", "Control_B", "Control_C")
nuevos_tratamientos <- paste("Genotipo", 1:15, sep = "_")

# Número de bloques
num_bloques <- 5

# Crear el diseño ABD utilizando el paquete agricolae
diseño_abd <- design.dau(
  trt1 = controles,
  trt2 = nuevos_tratamientos,
  r = num_bloques,
  seed = 123,
  serie = 2
)

# Mostrar el diseño
print("Diseño de Bloques Aumentados (ABD):")
print(diseño_abd$book)

# Simular datos de rendimiento
set.seed(456)
datos_abd <- diseño_abd$book %>%
  mutate(
    Rendimiento = ifelse(
      TRT %in% controles,
      rnorm(n(), mean = 50, sd = 5),  # Controles
      rnorm(n(), mean = 55, sd = 7)   # Nuevos genotipos
    )
  )

# Mostrar los datos simulados
print("Datos Simulados para ABD:")
print(datos_abd)

# 1.1.1. Análisis del ABD

# Ajustar un modelo lineal mixto utilizando lme4
# Consideramos los controles como efectos fijos y los genotipos como efectos aleatorios

# Convertir variables a factores
datos_abd$Block <- as.factor(datos_abd$Block)
datos_abd$TRT <- as.factor(datos_abd$TRT)

# Crear una variable para identificar controles y nuevos genotipos
datos_abd <- datos_abd %>%
  mutate(
    Tipo = ifelse(TRT %in% controles, "Control", "Genotipo")
  )

# Ajustar el modelo
modelo_abd <- lmer(
  Rendimiento ~ TRT + (1 | Block),
  data = datos_abd
)

# Resumen del modelo
summary(modelo_abd)

# Obtener las medias ajustadas de los tratamientos
medias_abd <- emmeans(modelo_abd, "TRT")

# Comparaciones múltiples
comparaciones_abd <- pairs(medias_abd, adjust = "tukey")

# Mostrar resultados
print("Medias Ajustadas de los Tratamientos:")
print(medias_abd)

print("Comparaciones Múltiples entre Tratamientos:")
print(comparaciones_abd)

# Visualización de las medias ajustadas
plot(medias_abd) + labs(title = "Medias Ajustadas de Rendimiento por Tratamiento (ABD)")

# 1.2. Diseño de filas y columnas: control de variabilidad bidireccional 

# Los diseños de filas y columnas, como el diseño de cuadrados latinos o diseños resolubles,
# se utilizan para controlar la variabilidad en dos direcciones, por ejemplo, filas y columnas
# en un campo experimental.

# Supongamos que queremos implementar un diseño alfa-látice utilizando el paquete DiGGer

# 1.2.1. Implementación de un diseño alfa-látice con DiGGer

# Definir parámetros del diseño
num_tratamientos <- 16
bloque_size <- 4
num_replicaciones <- 2

# Generar el diseño utilizando DiGGer
design_alfa <- DiGGer(
  t = num_tratamientos,
  r = num_replicaciones,
  b = num_tratamientos / bloque_size,
  k = bloque_size,
  rowcol = TRUE
)

# Obtener el plan del diseño
plan_alfa <- getDesign(design_alfa)

# Convertir el plan a un data frame
datos_alfa <- data.frame(
  Row = as.vector(row(plan_alfa)),
  Column = as.vector(col(plan_alfa)),
  Tratamiento = as.vector(plan_alfa)
)

# Mostrar el diseño
print("Diseño Alfa-Látice:")
print(datos_alfa)

# 1.2.2. Simulación y análisis de datos en diseño alfa-látice

# Simular datos de rendimiento
set.seed(789)
datos_alfa <- datos_alfa %>%
  mutate(
    Rendimiento = rnorm(n(), mean = 50 + as.numeric(Tratamiento), sd = 5)
  )

# Convertir variables a factores
datos_alfa$Row <- as.factor(datos_alfa$Row)
datos_alfa$Column <- as.factor(datos_alfa$Column)
datos_alfa$Tratamiento <- as.factor(datos_alfa$Tratamiento)

# Ajustar el modelo lineal mixto
modelo_alfa <- lmer(
  Rendimiento ~ Tratamiento + (1 | Row) + (1 | Column),
  data = datos_alfa
)

# Resumen del modelo
summary(modelo_alfa)

# Obtener las medias ajustadas de los tratamientos
medias_alfa <- emmeans(modelo_alfa, "Tratamiento")

# Comparaciones múltiples
comparaciones_alfa <- pairs(medias_alfa, adjust = "tukey")

# Mostrar resultados
print("Medias Ajustadas de los Tratamientos (Diseño Alfa-Látice):")
print(medias_alfa)

print("Comparaciones Múltiples entre Tratamientos:")
print(comparaciones_alfa)

# Visualización de las medias ajustadas
plot(medias_alfa) + labs(title = "Medias Ajustadas de Rendimiento por Tratamiento (Alfa-Látice)")

# 1.3. DBCA desbalanceados: manejo de datos faltantes 

# En la práctica, a veces tenemos datos faltantes en nuestros experimentos.
# Es importante saber cómo manejar diseños desbalanceados.

# Supongamos que en nuestro experimento DBCA anterior (Clase 3), tenemos datos faltantes.

# Reutilizaremos el diseño de bloques completos al azar (DBCA) de la Clase 3
# Simularemos datos faltantes eliminando algunas observaciones

# Generar el diseño DBCA original
tratamientos <- c("Fertilizante_A", "Fertilizante_B", "Fertilizante_C", "Fertilizante_D")
bloques <- c("Bloque_1", "Bloque_2", "Bloque_3", "Bloque_4")

set.seed(123)
diseño_dbca <- design.rcbd(trt = tratamientos, r = length(bloques), seed = 123)
datos_dbca <- diseño_dbca$book

# Simular datos de rendimiento
set.seed(456)
datos_dbca <- datos_dbca %>%
  mutate(
    Rendimiento = rnorm(n = n(), mean = ifelse(Trt == "Fertilizante_A", 50,
                                               ifelse(Trt == "Fertilizante_B", 60,
                                                      ifelse(Trt == "Fertilizante_C", 55, 65))) +
                          ifelse(Block == "Bloque_1", -5,
                                 ifelse(Block == "Bloque_2", 0,
                                        ifelse(Block == "Bloque_3", 5, 10))),
             sd = 2)
  )

# Introducir datos faltantes
set.seed(789)
datos_dbca_missing <- datos_dbca %>%
  sample_frac(size = 0.9)  # Mantener el 90% de los datos

# Mostrar los datos con valores faltantes
print("Datos del DBCA con Datos Faltantes:")
print(datos_dbca_missing)

# 1.3.1. Análisis del DBCA desbalanceado

# Convertir variables a factores
datos_dbca_missing$Block <- as.factor(datos_dbca_missing$Block)
datos_dbca_missing$Trt <- as.factor(datos_dbca_missing$Trt)

# Ajustar el modelo ANOVA
anova_dbca_unbalanced <- aov(Rendimiento ~ Trt + Block, data = datos_dbca_missing)
summary(anova_dbca_unbalanced)

# Interpretación:
# - A pesar de los datos faltantes, el modelo ANOVA puede ajustarse.
# - Sin embargo, la suma de cuadrados no es equilibrada y se utilizan métodos de estimación ajustados.

# Comparaciones múltiples con medias ajustadas
library(emmeans)
medias_dbca_unbalanced <- emmeans(anova_dbca_unbalanced, "Trt")
comparaciones_dbca_unbalanced <- pairs(medias_dbca_unbalanced, adjust = "tukey")

# Mostrar resultados
print("Medias Ajustadas de los Tratamientos (DBCA Desbalanceado):")
print(medias_dbca_unbalanced)

print("Comparaciones Múltiples entre Tratamientos:")
print(comparaciones_dbca_unbalanced)

# Visualización de las medias ajustadas
plot(medias_dbca_unbalanced) + labs(title = "Medias Ajustadas de Rendimiento por Tratamiento (DBCA Desbalanceado)")

# 1.4. Implementación básica de estos diseños en R 

# Ya hemos visto ejemplos de implementación básica de diseños avanzados en R utilizando
# los paquetes agricolae y DiGGer. Es importante familiarizarse con la documentación
# de estos paquetes para explorar más opciones y funcionalidades.

# -----------------------------
# 2. Resumen General y Consideraciones Prácticas
# -----------------------------

# 2.1. Guía para elección del diseño apropiado

# - **DCA**: Cuando no hay fuentes de variabilidad conocidas.
# - **DBCA**: Cuando hay una fuente de variabilidad que puede ser controlada mediante bloques.
# - **Diseños Factoriales**: Cuando se estudian dos o más factores simultáneamente.
# - **Diseños Avanzados (ABD, Alfa-Látice, etc.)**: Cuando se necesita controlar múltiples fuentes de variabilidad o se tienen restricciones experimentales.

# 2.2. Importancia del tamaño de muestra y poder estadístico 

# - Un tamaño de muestra adecuado aumenta la precisión y el poder estadístico.
# - El poder estadístico es la probabilidad de detectar un efecto real si existe.
# - Se debe equilibrar entre recursos disponibles y la necesidad de obtener resultados confiables.

# 2.3. Mejores prácticas para interpretación y presentación 

# - Verificar los supuestos de los modelos estadísticos.
# - Utilizar visualizaciones para comunicar los resultados de manera efectiva.
# - Presentar las medias ajustadas y comparaciones múltiples de manera clara.

# -----------------------------
# 3. Cierre y Recursos Adicionales
# -----------------------------

# 3.1. Paquetes de R especializados para agronomía

# - **agricolae**: Amplia gama de funciones para diseños y análisis en agronomía.
# - **DiGGer**: Generación de diseños óptimos para experimentos agrícolas.
# - **asreml**: Análisis estadístico avanzado para modelos mixtos (comercial).
# - **lme4** y **nlme**: Modelos lineales mixtos para datos balanceados y desbalanceados.

# 3.2. Recursos en línea para aprendizaje continuo

# - **R Documentation**: https://www.rdocumentation.org/
# - **R-bloggers**: https://www.r-bloggers.com/
# - **The Comprehensive R Archive Network (CRAN)**: https://cran.r-project.org/
# - **Cursos en línea**: Coursera, edX, y DataCamp ofrecen cursos sobre R y estadística.
