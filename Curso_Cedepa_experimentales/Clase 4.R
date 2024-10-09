# ==========================================
# Clase 4: Diseños Factoriales y Análisis de Interacciones
# ==========================================

# -----------------------------
# 0. Preparación del Entorno
# -----------------------------

# Limpiar el entorno de trabajo para evitar conflictos con variables anteriores
rm(list = ls())

# Instalar y cargar paquetes necesarios
# Si no tienes los paquetes instalados, descomenta las líneas de instalación
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("agricolae")
# install.packages("car")
# install.packages("reshape2")
# install.packages("emmeans")
# install.packages("readr")

library(dplyr)        # Para manipulación de datos
library(ggplot2)      # Para visualizaciones
library(agricolae)    # Para diseños experimentales y análisis
library(car)          # Para pruebas estadísticas
library(reshape2)     # Para manipulación de datos
library(emmeans)      # Para comparaciones múltiples y medias ajustadas
library(readr)        # Para leer archivos CSV

# -----------------------------
# 1. Transformaciones de Datos
# -----------------------------

# En esta sección, aplicaremos diversas transformaciones a conjuntos de datos simulados
# para mejorar la normalidad y homogeneidad de varianzas cuando sea necesario.

# 1.1. Valores Muy Altos o Desviación a la Derecha - Transformación Logarítmica

# Generar datos con valores muy altos y sesgo a la derecha
set.seed(123)
datos_altos <- rexp(100, rate = 0.1)  # Distribución exponencial (sesgada a la derecha)

# Visualizar histograma antes de la transformación
hist(datos_altos, main = "Datos Originales - Valores Altos", xlab = "Valor", col = "lightblue")

# Aplicar transformación logarítmica
datos_log <- log(datos_altos)

# Visualizar histograma después de la transformación
hist(datos_log, main = "Datos Transformados - Logarítmica", xlab = "Log(Valor)", col = "salmon")

# Comparar normalidad con gráficos Q-Q
par(mfrow = c(1, 2))
qqnorm(datos_altos, main = "Q-Q Plot Datos Originales")
qqline(datos_altos, col = "red")
qqnorm(datos_log, main = "Q-Q Plot Datos Transformados")
qqline(datos_log, col = "blue")
par(mfrow = c(1, 1))

# 1.2. Valores Muy Bajos o Desviación a la Izquierda - Transformación de Raíz Cuadrada

# Generar datos con valores bajos y sesgo a la izquierda
set.seed(123)
datos_bajos <- rbeta(100, shape1 = 5, shape2 = 2) * 10  # Escalar para tener valores bajos

# Visualizar histograma antes de la transformación
hist(datos_bajos, main = "Datos Originales - Valores Bajos", xlab = "Valor", col = "lightgreen")

df <- data.frame(valores = datos_bajos)

ggplot(df, aes(y = valores)) +
  geom_boxplot()

# Aplicar transformación de raíz cuadrada
datos_raiz <- sqrt(datos_bajos)

# Visualizar histograma después de la transformación
hist(datos_raiz, main = "Datos Transformados - Raíz Cuadrada", xlab = "sqrt(Valor)", col = "orange")

df <- data.frame(valores = datos_raiz)

ggplot(df, aes(y = valores)) +
  geom_boxplot()

# Comparar normalidad con gráficos Q-Q
par(mfrow = c(1, 2))
qqnorm(datos_bajos, main = "Q-Q Plot Datos Originales")
qqline(datos_bajos, col = "red")
qqnorm(datos_raiz, main = "Q-Q Plot Datos Transformados")
qqline(datos_raiz, col = "blue")
par(mfrow = c(1, 1))

# 1.3. Datos Negativos o Cero - Transformación de Potencia

# Generar datos con valores negativos y ceros
set.seed(123)
datos_negativos <- rnorm(100, mean = -5, sd = 3)

# Añadir ceros
datos_negativos[sample(1:100, 10)] <- 0

# Visualizar histograma antes de la transformación
hist(datos_negativos, main = "Datos Originales - Negativos y Ceros", xlab = "Valor", col = "purple")

df <- data.frame(valores = datos_negativos)

ggplot(df, aes(y = valores)) +
  geom_boxplot()

# Ajustar datos para evitar valores negativos o cero
constante <- abs(min(datos_negativos)) + 1
datos_ajustados <- datos_negativos + constante

# Aplicar transformación de potencia (por ejemplo, λ = 0.5)
lambda <- 0.5
datos_potencia <- datos_ajustados^lambda

# Visualizar histograma después de la transformación
hist(datos_potencia, main = paste("Datos Transformados - Potencia (λ =", lambda, ")"), xlab = "Valor Transformado", col = "pink")

df <- data.frame(valores = datos_potencia)

ggplot(df, aes(y = valores)) +
  geom_boxplot()

# Comparar normalidad con gráficos Q-Q
par(mfrow = c(1, 2))
qqnorm(datos_negativos, main = "Q-Q Plot Datos Originales")
qqline(datos_negativos, col = "red")
qqnorm(datos_potencia, main = "Q-Q Plot Datos Transformados")
qqline(datos_potencia, col = "blue")
par(mfrow = c(1, 1))

# 1.4. Datos con Variabilidad Proporcional a la Media - Logaritmo Natural

# Generar datos donde la varianza es proporcional a la media
set.seed(123)
medias <- seq(10, 100, length.out = 100)
datos_varianza_media <- rnorm(100, mean = medias, sd = medias * 0.2)

# Visualizar relación entre media y varianza antes de la transformación
plot(medias, tapply(datos_varianza_media, as.factor(round(medias)), var), main = "Varianza vs Media", xlab = "Media", ylab = "Varianza")

# Aplicar transformación logarítmica
datos_log_varianza <- log(datos_varianza_media)

# Visualizar relación entre media y varianza después de la transformación
plot(medias, tapply(datos_log_varianza, as.factor(round(medias)), var), main = "Varianza vs Media (Log Transformada)", xlab = "Media", ylab = "Varianza")

# 1.5. Valores Extremos y Sesgos Fuertes - Transformación Inversa

# Generar datos con valores extremos y sesgo
set.seed(123)
datos_extremos <- c(rnorm(95, mean = 50, sd = 5), rnorm(5, mean = 200, sd = 5))

# Visualizar histograma antes de la transformación
hist(datos_extremos, main = "Datos Originales - Valores Extremos", xlab = "Valor", col = "lightgray")

# Aplicar transformación inversa
datos_inversos <- 1 / datos_extremos

# Visualizar histograma después de la transformación
hist(datos_inversos, main = "Datos Transformados - Inversa", xlab = "1/Valor", col = "lightyellow")

# Comparar normalidad con gráficos Q-Q
par(mfrow = c(1, 2))
qqnorm(datos_extremos, main = "Q-Q Plot Datos Originales")
qqline(datos_extremos, col = "red")
qqnorm(datos_inversos, main = "Q-Q Plot Datos Transformados")
qqline(datos_inversos, col = "blue")
par(mfrow = c(1, 1))

# 1.6. Datos de Conteo (Enteros y Positivos) - Raíz Cuarta

# Generar datos de conteo
set.seed(123)
datos_conteo <- rpois(100, lambda = 4)

# Visualizar histograma antes de la transformación
hist(datos_conteo, main = "Datos Originales - Conteo", xlab = "Valor", col = "cyan")

# Aplicar transformación de raíz cuarta
datos_raiz4 <- sqrt(datos_conteo + 0.5)

# Visualizar histograma después de la transformación
hist(datos_raiz4, main = "Datos Transformados - Raíz Cuarta", xlab = "sqrt(Valor + 0.5)", col = "magenta")

# Comparar normalidad con gráficos Q-Q
par(mfrow = c(1, 2))
qqnorm(datos_conteo, main = "Q-Q Plot Datos Originales")
qqline(datos_conteo, col = "red")
qqnorm(datos_raiz4, main = "Q-Q Plot Datos Transformados")
qqline(datos_raiz4, col = "blue")
par(mfrow = c(1, 1))

# 1.7. Datos con Distribución Binomial - Transformación Arcoseno

# Generar datos de proporciones
set.seed(123)
n_trials <- 100  # Número de ensayos
datos_binomiales <- rbinom(100, size = n_trials, prob = 0.3) / n_trials  # Proporciones entre 0 y 1

# Visualizar histograma antes de la transformación
hist(datos_binomiales, main = "Datos Originales - Proporciones", xlab = "Proporción", col = "lightgreen")

# Aplicar transformación arcoseno
datos_arcsin <- asin(sqrt(datos_binomiales))

# Visualizar histograma después de la transformación
hist(datos_arcsin, main = "Datos Transformados - Arcoseno", xlab = "arcsin(sqrt(Proporción))", col = "orange")

# Comparar normalidad con gráficos Q-Q
par(mfrow = c(1, 2))
qqnorm(datos_binomiales, main = "Q-Q Plot Datos Originales")
qqline(datos_binomiales, col = "red")
qqnorm(datos_arcsin, main = "Q-Q Plot Datos Transformados")
qqline(datos_arcsin, col = "blue")
par(mfrow = c(1, 1))

# -----------------------------
# 2. Diseños Factoriales
# -----------------------------

# 2.1. Concepto y tipos comunes en agronomía

# En agronomía, los diseños factoriales se utilizan para estudiar el efecto de dos o más factores
# y sus posibles interacciones sobre una variable respuesta.
# Un diseño factorial 2^k implica k factores, cada uno con 2 niveles.
# Estos diseños permiten evaluar no solo los efectos individuales (efectos principales) de cada factor,
# sino también cómo interactúan entre sí para afectar la variable respuesta.

# 2.2. Implementación de un diseño 2^k en R

# Definir los factores y sus niveles
# Factor A: Fertilizante (2 niveles)
# Factor B: Tipo de Suelo (2 niveles)

# Cargar los datos simulados
# Suponiendo que el archivo "Factorial.csv" está en la carpeta "Datasets"

datos_simulados <- read_csv("Datasets/Factorial.csv")

datos_simulados <- read.delim("Clipboard")

# Verificar los datos cargados
head(datos_simulados)

str(datos_simulados)

summary(datos_simulados)

# Ajustar el modelo ANOVA con interacción
anova_factorial <- aov(Rendimiento ~ Fertilizante, data = datos_simulados)

anova_factorial <- aov(Rendimiento ~ Fertilizante + blocks, data = datos_simulados)

anova_factorial <- aov(Rendimiento ~ Fertilizante * Tipo_Suelo, data = datos_simulados)

# Mostrar el resumen del ANOVA
summary(anova_factorial)

# Calcular el coeficiente de variación (CV)
cv.model <- function(modelo){
  CV <- (sqrt(deviance(modelo)/df.residual(modelo)) / mean(fitted(modelo))) * 100
  return(paste("Coeficiente de Variación (CV):", round(CV, 2), "%"))
}

cv.model(anova_factorial)
print(cv.model(anova_factorial))

# Interpretación:
# - Efectos principales: Fertilizante y Tipo de Suelo
# - Interacción: Fertilizante:Tipo_Suelo
# - Valores p asociados indican si los efectos son significativos
# - Un efecto significativo sugiere que el factor tiene un impacto en el rendimiento

# -----------------------------
# 3. Pruebas de Normalidad y Homogeneidad de Varianzas
# -----------------------------

# 3.1. Prueba de normalidad de los residuales (Shapiro-Wilk)

# Obtener los residuales del modelo ANOVA
residuos <- residuals(anova_factorial)

# Realizar la prueba de Shapiro-Wilk
shapiro_test <- shapiro.test(residuos)
print("Prueba de Shapiro-Wilk para normalidad de los residuales:")
print(shapiro_test)

# Interpretación:
# - Si el valor p es mayor que 0.05, no se rechaza la hipótesis nula de normalidad.
# - Si el valor p es menor que 0.05, se rechaza la hipótesis de normalidad.

hist(datos_simulados$Rendimiento)

# Gráfico Q-Q de los residuales
qqnorm(residuos)
qqline(residuos, col = "red")

# 3.2. Prueba de homogeneidad de varianzas (Levene)

# Realizar la prueba de Levene
levene_test <- leveneTest(Rendimiento ~ interaction(Fertilizante, Tipo_Suelo), data = datos_simulados)
print("Prueba de Levene para homogeneidad de varianzas:")
print(levene_test)

ggplot(datos_simulados, aes(x = datos_simulados$Tratamiento, y= Rendimiento)) +
  geom_boxplot()

# Interpretación:
# - Si el valor p es mayor que 0.05, no se rechaza la hipótesis nula de homogeneidad de varianzas.
# - Si el valor p es menor que 0.05, se rechaza la hipótesis de homogeneidad.

# -----------------------------
# 4. Análisis de Interacciones
# -----------------------------

# 4.1. Interpretación de efectos principales e interacciones

# Explicación:
# - Un efecto principal significativo indica que un factor tiene un efecto sobre la variable respuesta, independientemente del otro factor.
# - Una interacción significativa sugiere que el efecto de un factor depende del nivel del otro.

# 4.2. Creación e interpretación de gráficos de interacción

# Crear un gráfico de interacción utilizando ggplot2
interaction_plot <- ggplot(datos_simulados, aes(x = Fertilizante, y = Rendimiento, color = Tipo_Suelo, group = Tipo_Suelo)) +
  stat_summary(fun = mean, geom = "point", size = 3) +  # Puntos de medias
  stat_summary(fun = mean, geom = "line") +             # Líneas de medias
  theme_minimal() +
  labs(title = "Gráfico de Interacción entre Fertilizante y Tipo de Suelo",
       x = "Tipo de Fertilizante",
       y = "Rendimiento Promedio",
       color = "Tipo de Suelo")

# Mostrar el gráfico de interacción
print(interaction_plot)

# Interpretación del gráfico:
# - Si las líneas son paralelas, no hay interacción entre los factores.
# - Si las líneas se cruzan o no son paralelas, existe interacción entre los factores.
# - Analizar cómo cambia el rendimiento según las combinaciones de niveles de los factores.

# 4.3. Análisis post-hoc en diseños factoriales

# Si encontramos efectos significativos, podemos realizar comparaciones múltiples

# 4.3.1. Comparaciones múltiples para efectos principales

# Prueba de Tukey para el efecto principal de Fertilizante
tukey_fert <- HSD.test(anova_factorial, "Fertilizante", group = TRUE)
print("Resultados de la Prueba de Tukey para Fertilizante:")
print(tukey_fert$groups)

bar.group(tukey_fert$groups, ylim = c(0,70))

# Prueba de Tukey para el efecto principal de Tipo_Suelo
tukey_suelo <- HSD.test(anova_factorial, "Tipo_Suelo", group = TRUE)
print("Resultados de la Prueba de Tukey para Tipo de Suelo:")
print(tukey_suelo$groups)

bar.group(tukey_suelo$groups, ylim = c(0,70))

# 4.3.2. Comparaciones múltiples para interacciones

datos_simulados

# Crear una nueva variable que combine los niveles de Fertilizante y Tipo_Suelo
datos_simulados <- datos_simulados %>%
  mutate(Tratamiento = interaction(Fertilizante, Tipo_Suelo))

# Realizar ANOVA considerando la nueva variable de tratamiento
anova_interaccion <- aov(Rendimiento ~ Tratamiento, data = datos_simulados)

# Prueba de Tukey para las combinaciones de tratamientos
tukey_trat <- HSD.test(anova_interaccion, "Tratamiento", group = TRUE)
print("Resultados de la Prueba de Tukey para las Interacciones:")
print(tukey_trat$groups)

bar.group(tukey_trat$groups, ylim = c(0,75))

# Visualización de los resultados de comparaciones múltiples


tuckey <- data.frame(tukey_trat$groups)

tuckey <- tuckey %>%
  mutate(Tratamiento = row.names(tuckey))

str(tuckey)

# Preparar datos para el gráfico
media_tratamientos <- datos_simulados %>%
  group_by(Tratamiento) %>%
  summarise(Media = mean(Rendimiento), SD = sd(Rendimiento)) %>%
  left_join(tuckey %>% rename(Tratamiento = Tratamiento), by = "Tratamiento")

# Crear un gráfico de barras con las medias y letras de grupos homogéneos
ggplot(media_tratamientos, aes(x = Tratamiento, y = Media, fill = Tratamiento)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Media - SD, ymax = Media + SD), width = 0.2) +
  geom_text(aes(label = groups, y = Media + SD + 1), vjust = 0) +
  theme_minimal() +
  labs(title = "Rendimiento por Tratamiento con Grupos Homogéneos (Tukey)",
       x = "Tratamiento",
       y = "Rendimiento Promedio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# -----------------------------
# Información Teórica Adicional
# -----------------------------

# **Diseños Factoriales:**
# - Permiten estudiar el efecto individual (principal) de cada factor y las interacciones entre ellos.
# - Son eficientes ya que requieren menos experimentos que probar cada factor por separado.
# - En un diseño 2^k, se tienen k factores con 2 niveles cada uno, lo que permite evaluar interacciones de primer orden.

# **Análisis de Interacciones:**
# - La interacción entre factores ocurre cuando el efecto de un factor depende del nivel del otro.
# - Es crucial interpretar las interacciones antes que los efectos principales, ya que una interacción significativa puede alterar la interpretación de los efectos individuales.

# **Pruebas de Normalidad y Homogeneidad:**
# - Los supuestos del ANOVA incluyen la normalidad de los residuales y la homogeneidad de varianzas.
# - La prueba de Shapiro-Wilk evalúa la normalidad de los residuales.
# - La prueba de Levene evalúa la homogeneidad de las varianzas entre los grupos.
# - Si los supuestos no se cumplen, puede ser necesario transformar los datos o utilizar modelos alternativos.

# **Interpretación de Resultados:**
# - Valores p significativos indican diferencias o efectos importantes, pero deben interpretarse en contexto.
# - Los gráficos de interacción y las comparaciones múltiples ayudan a entender cómo los factores afectan la variable respuesta.

# **Mejores Prácticas:**
# - Siempre verificar los supuestos antes de confiar en los resultados del ANOVA.
# - Utilizar visualizaciones para complementar los análisis estadísticos.
# - Reportar claramente los resultados, incluyendo medidas de dispersión y significancia.


