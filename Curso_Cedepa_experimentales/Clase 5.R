# ==========================================
# Clase 5: Diseños Avanzados y Resumen General
# ==========================================

# -----------------------------
# 0. Preparación del Entorno
# -----------------------------

# Limpiar el entorno de trabajo
rm(list = ls())

# Establecer la semilla para reproducibilidad
set.seed(123)

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
# install.packages("tidyr")

library(dplyr)        # Manipulación de datos
library(agricolae)    # Diseños experimentales y análisis
library(DiGGer)       # Diseños óptimos de experimentos
library(lattice)      # Visualización de datos
library(lme4)         # Modelos lineales mixtos
library(emmeans)      # Comparaciones de medias ajustadas
library(ggplot2)      # Visualizaciones avanzadas
library(reshape2)     # Transformación de datos
library(tidyr)        # Manipulación de datos

# -----------------------------
# 1. Diseños Avanzados
# -----------------------------

# 1.1. Diseño de Cuadrado Latino

# El diseño de Cuadrado Latino se utiliza para controlar dos fuentes de variabilidad
# además del tratamiento. Se estructura en filas y columnas, donde el número de tratamientos
# es igual al número de filas y columnas.

# Supongamos que queremos evaluar 4 variedades de trigo (A, B, C, D) en un campo con
# variación en dos direcciones: fertilidad (filas) y humedad (columnas).

# Definir los tratamientos
tratamientos <- c("A", "B", "C", "D")

# Generar el diseño de Cuadrado Latino utilizando el paquete agricolae
diseño_cl <- design.lsd(trt = tratamientos, seed = 123, serie = 2)

# Mostrar el diseño
print("Diseño de Cuadrado Latino:")
print(diseño_cl$book)

# Visualizar el diseño en forma de matriz
matriz_cl <- xtabs(~ Row + Col, data = diseño_cl$book, subset = TRT)
print("Matriz del Diseño de Cuadrado Latino:")
print(matriz_cl)

# Simular datos de rendimiento
set.seed(456)
datos_cl <- diseño_cl$book %>%
  mutate(
    Rendimiento = rnorm(n(), mean = 50 + ifelse(TRT == "A", 5,
                                                ifelse(TRT == "B", 0,
                                                       ifelse(TRT == "C", -5, 10))),
                        sd = 2)
  )

# Mostrar los datos simulados
print("Datos Simulados para el Diseño de Cuadrado Latino:")
print(datos_cl)

# Convertir variables a factores
datos_cl$Row <- as.factor(datos_cl$Row)
datos_cl$Col <- as.factor(datos_cl$Col)
datos_cl$TRT <- as.factor(datos_cl$TRT)

# Ajustar el modelo ANOVA
modelo_cl <- aov(Rendimiento ~ TRT + Row + Col, data = datos_cl)
summary(modelo_cl)

# Comparaciones múltiples con prueba de Tukey
comparaciones_cl <- HSD.test(modelo_cl, "TRT", group = TRUE)
print("Comparaciones Múltiples entre Tratamientos (Cuadrado Latino):")
print(comparaciones_cl$groups)

# Visualización de las medias por tratamiento
ggplot(datos_cl, aes(x = TRT, y = Rendimiento)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Rendimiento por Variedad (Cuadrado Latino)",
       x = "Variedad",
       y = "Rendimiento")

# 1.2. Diseño de Filas y Columnas

# Este diseño es útil cuando tenemos variabilidad en dos direcciones pero el número de tratamientos
# no es igual al número de filas y columnas.

# Supongamos que queremos evaluar 6 tratamientos de fertilizantes en un campo con 3 filas y 4 columnas.

# Definir los tratamientos
tratamientos_fc <- paste("Fert", 1:6, sep = "_")

# Generar el diseño de Filas y Columnas utilizando el paquete agricolae
diseño_fc <- design.graeco(trt1 = tratamientos_fc[1:3], trt2 = tratamientos_fc[4:6], seed = 123, serie = 2)

# Nota: En este ejemplo, utilizamos un diseño Grecolatino para acomodar más tratamientos.

# Mostrar el diseño
print("Diseño de Filas y Columnas:")
print(diseño_fc$book)

# Simular datos de rendimiento
set.seed(789)
datos_fc <- diseño_fc$book %>%
  mutate(
    Rendimiento = rnorm(n(), mean = 60 + as.numeric(as.factor(trt1))*2 - as.numeric(as.factor(trt2))*1.5, sd = 3)
  )

# Mostrar los datos simulados
print("Datos Simulados para el Diseño de Filas y Columnas:")
print(datos_fc)

# Convertir variables a factores
datos_fc$Row <- as.factor(datos_fc$Row)
datos_fc$Col <- as.factor(datos_fc$Col)
datos_fc$trt1 <- as.factor(datos_fc$trt1)
datos_fc$trt2 <- as.factor(datos_fc$trt2)

# Ajustar el modelo ANOVA
modelo_fc <- aov(Rendimiento ~ trt1 + trt2 + Row + Col, data = datos_fc)
summary(modelo_fc)

# Comparaciones múltiples
comparaciones_fc_trt1 <- HSD.test(modelo_fc, "trt1", group = TRUE)
comparaciones_fc_trt2 <- HSD.test(modelo_fc, "trt2", group = TRUE)

print("Comparaciones Múltiples entre Tratamientos trt1 (Filas y Columnas):")
print(comparaciones_fc_trt1$groups)

print("Comparaciones Múltiples entre Tratamientos trt2 (Filas y Columnas):")
print(comparaciones_fc_trt2$groups)

# Visualización de las medias por tratamiento trt1
ggplot(datos_fc, aes(x = trt1, y = Rendimiento)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Rendimiento por Fertilizante trt1 (Filas y Columnas)",
       x = "Fertilizante trt1",
       y = "Rendimiento")

# Visualización de las medias por tratamiento trt2
ggplot(datos_fc, aes(x = trt2, y = Rendimiento)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Rendimiento por Fertilizante trt2 (Filas y Columnas)",
       x = "Fertilizante trt2",
       y = "Rendimiento")

# 1.3. Diseño de Parcelas Divididas (Split-Plot)

# Este diseño se utiliza cuando hay factores que requieren diferentes tamaños de parcela.

# Supongamos que queremos evaluar dos sistemas de riego (riego por goteo y aspersión) y tres variedades de maíz (V1, V2, V3).

# Definir los factores
sistemas_riego <- c("Goteo", "Aspersion")
variedades_maiz <- c("V1", "V2", "V3")
bloques <- c("Bloque1", "Bloque2", "Bloque3")

# Generar el diseño de parcelas divididas
diseño_pd <- design.split(
  trt1 = sistemas_riego,
  trt2 = variedades_maiz,
  r = length(bloques),
  design = "rcbd",
  seed = 123,
  serie = 2
)

# Mostrar el diseño
print("Diseño de Parcelas Divididas:")
print(diseño_pd$book)

# Simular datos de rendimiento
set.seed(321)
datos_pd <- diseño_pd$book %>%
  mutate(
    Rendimiento = rnorm(n(), mean = ifelse(sistema == "Goteo", 70, 65) +
                          ifelse(variedad == "V1", 5, ifelse(variedad == "V2", 0, -5)),
                        sd = 4)
  )

# Mostrar los datos simulados
print("Datos Simulados para el Diseño de Parcelas Divididas:")
print(datos_pd)

# Convertir variables a factores
datos_pd$Block <- as.factor(datos_pd$Block)
datos_pd$sistema <- as.factor(datos_pd$sistema)
datos_pd$variedad <- as.factor(datos_pd$variedad)

# Ajustar el modelo ANOVA para parcelas divididas
modelo_pd <- aov(Rendimiento ~ sistema * variedad + Error(Block/sistema), data = datos_pd)
summary(modelo_pd)

# Obtener las medias ajustadas
library(emmeans)
medias_pd <- emmeans(modelo_pd, ~ sistema * variedad)

# Comparaciones múltiples
comparaciones_pd <- pairs(medias_pd, adjust = "tukey")
print("Medias Ajustadas y Comparaciones (Parcelas Divididas):")
print(medias_pd)
print(comparaciones_pd)

# Visualización de las interacciones
interaction.plot(datos_pd$sistema, datos_pd$variedad, datos_pd$Rendimiento,
                 fun = mean, type = "b", col = c("red", "blue"),
                 pch = c(16, 18), xlab = "Sistema de Riego", ylab = "Rendimiento",
                 main = "Interacción entre Sistema de Riego y Variedad")

# 1.4. Diseño de Bloques Aumentados

# Este diseño es útil cuando se tienen muchos tratamientos que no pueden ser completamente replicados.

# Supongamos que queremos evaluar 3 controles y 15 nuevos genotipos de frijol en 5 bloques.

# Definir los controles y nuevos tratamientos
controles <- c("Control_A", "Control_B", "Control_C")
nuevos_tratamientos <- paste("Genotipo", 1:15, sep = "_")
num_bloques <- 5

# Crear el diseño de Bloques Aumentados utilizando el paquete agricolae
diseño_ba <- design.dau(
  trt1 = controles,
  trt2 = nuevos_tratamientos,
  r = num_bloques,
  seed = 123,
  serie = 2
)

# Mostrar el diseño
print("Diseño de Bloques Aumentados:")
print(diseño_ba$book)

# Simular datos de rendimiento
set.seed(456)
datos_ba <- diseño_ba$book %>%
  mutate(
    Rendimiento = ifelse(
      TRT %in% controles,
      rnorm(n(), mean = 50, sd = 2),  # Controles
      rnorm(n(), mean = 55, sd = 3)   # Nuevos genotipos
    )
  )

# Mostrar los datos simulados
print("Datos Simulados para el Diseño de Bloques Aumentados:")
print(datos_ba)

# Convertir variables a factores
datos_ba$Block <- as.factor(datos_ba$Block)
datos_ba$TRT <- as.factor(datos_ba$TRT)

# Crear una variable para identificar controles y nuevos genotipos
datos_ba <- datos_ba %>%
  mutate(
    Tipo = ifelse(TRT %in% controles, "Control", "Genotipo")
  )

# Ajustar un modelo lineal mixto utilizando lme4
# Consideramos los bloques como efectos aleatorios y los tratamientos como efectos fijos
modelo_ba <- lmer(
  Rendimiento ~ TRT + (1 | Block),
  data = datos_ba
)

# Resumen del modelo
summary(modelo_ba)

# Obtener las medias ajustadas de los tratamientos
medias_ba <- emmeans(modelo_ba, "TRT")

# Comparaciones múltiples entre tratamientos
comparaciones_ba <- pairs(medias_ba, adjust = "tukey")

# Mostrar resultados
print("Medias Ajustadas de los Tratamientos (Bloques Aumentados):")
print(medias_ba)

print("Comparaciones Múltiples entre Tratamientos:")
print(comparaciones_ba)

# Visualización de las medias ajustadas
plot(medias_ba) + labs(title = "Medias Ajustadas de Rendimiento por Tratamiento (Bloques Aumentados)")

# -----------------------------
# 2. Resumen General y Consideraciones Prácticas
# -----------------------------

# 2.1. Guía para elección del diseño apropiado

# - **DCA**: Cuando no hay fuentes de variabilidad conocidas.
# - **DBCA**: Cuando hay una fuente de variabilidad que puede ser controlada mediante bloques.
# - **Cuadrado Latino**: Cuando se necesitan controlar dos fuentes de variabilidad y el número de tratamientos es limitado.
# - **Filas y Columnas**: Cuando se controlan dos fuentes de variabilidad y se requiere flexibilidad en el número de tratamientos.
# - **Parcelas Divididas**: Cuando se tienen factores que requieren diferentes tamaños de parcela o diferentes niveles de manejo.
# - **Bloques Aumentados**: Cuando se tienen muchos tratamientos y recursos limitados, utilizando controles replicados.

# 2.2. Importancia del tamaño de muestra y poder estadístico 

# - Un tamaño de muestra adecuado aumenta la precisión y el poder estadístico.
# - El poder estadístico es la probabilidad de detectar un efecto real si existe.
# - Se debe equilibrar entre recursos disponibles y la necesidad de obtener resultados confiables.

# 2.3. Mejores prácticas para interpretación y presentación 

# - Verificar los supuestos de los modelos estadísticos (normalidad, homocedasticidad).
# - Utilizar visualizaciones para comunicar los resultados de manera efectiva.
# - Presentar las medias ajustadas y comparaciones múltiples de manera clara.
# - Documentar detalladamente el diseño y análisis para reproducibilidad.

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

