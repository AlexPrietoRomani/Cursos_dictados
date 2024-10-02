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

library(dplyr)        # Para manipulación de datos
library(ggplot2)      # Para visualizaciones
library(agricolae)    # Para diseños experimentales y análisis
library(car)          # Para pruebas estadísticas
library(reshape2)     # Para manipulación de datos
library(emmeans)      # Para comparaciones múltiples y medias ajustadas

# -----------------------------
# 1. Diseños Factoriales
# -----------------------------

# 1.1. Concepto y tipos comunes en agronomía

# En agronomía, los diseños factoriales se utilizan para estudiar el efecto de dos o más factores
# y sus posibles interacciones sobre una variable respuesta.
# Un diseño factorial 2^k implica k factores, cada uno con 2 niveles.
# Estos diseños permiten evaluar no solo los efectos individuales (efectos principales) de cada factor,
# sino también cómo interactúan entre sí para afectar la variable respuesta.

# 1.2. Implementación de un diseño 2^k en R

# Definir los factores y sus niveles
# Factor A: Tipo de Fertilizante (2 niveles)
# Factor B: Nivel de Riego (2 niveles)

# Crear una lista con los factores y sus niveles
factores <- list(
  Fertilizante = c("Orgánico", "Químico"),
  Riego = c("Bajo", "Alto")
)

# Generar todas las combinaciones posibles de los niveles de los factores
# Utilizando la función expand.grid() para crear el diseño factorial completo
diseño_factorial <- expand.grid(factores)

# Mostrar el diseño factorial
print("Diseño Factorial 2^2:")
print(diseño_factorial)

# Añadir replicaciones al diseño
# Supongamos que tenemos 3 repeticiones por combinación de tratamiento
repeticiones <- 3

# Crear el diseño completo con replicaciones
diseño_factorial_completo <- diseño_factorial[rep(seq_len(nrow(diseño_factorial)), each = repeticiones), ]

# Añadir una columna de identificación para las unidades experimentales
diseño_factorial_completo$Unidad <- 1:nrow(diseño_factorial_completo)

# Mostrar el diseño completo con replicaciones
print("Diseño Factorial Completo con Replicaciones:")
print(diseño_factorial_completo)

# Simular datos de rendimiento (variable respuesta)
# Supongamos efectos aditivos de los factores y una interacción
# Establecer efectos de los factores
set.seed(123)  # Para reproducibilidad
efecto_fertilizante <- c(Orgánico = 50, Químico = 60)
efecto_riego <- c(Bajo = -5, Alto = 5)
interacción <- matrix(c(0, 5, -5, 0), nrow = 2, byrow = TRUE,
                      dimnames = list(c("Orgánico", "Químico"), c("Bajo", "Alto")))

# Generar los datos simulados
datos_simulados <- diseño_factorial_completo %>%
  mutate(
    # Obtener el efecto del fertilizante
    efecto_fert = efecto_fertilizante[Fertilizante],
    # Obtener el efecto del riego
    efecto_rieg = efecto_riego[Riego],
    # Obtener el efecto de la interacción
    efecto_inter = mapply(function(fert, rieg) interacción[fert, rieg], Fertilizante, Riego),
    # Calcular el rendimiento como la suma de los efectos más un error aleatorio
    Rendimiento = efecto_fert + efecto_rieg + efecto_inter + rnorm(n(), mean = 0, sd = 2)
  )

# Mostrar los datos simulados
print("Datos Simulados con Rendimiento:")
print(datos_simulados)

# 1.3. ANOVA para diseños factoriales: interpretación

# Realizar ANOVA factorial
# Convertir los factores a variables categóricas (factor)
datos_simulados$Fertilizante <- as.factor(datos_simulados$Fertilizante)
datos_simulados$Riego <- as.factor(datos_simulados$Riego)

# Ajustar el modelo ANOVA con interacción
anova_factorial <- aov(Rendimiento ~ Fertilizante * Riego, data = datos_simulados)

# Mostrar el resumen del ANOVA
summary(anova_factorial)

# Calcular el coeficiente de variación (CV)
cv.model(anova_factorial)

# Interpretación:
# - Efectos principales: Fertilizante y Riego
# - Interacción: Fertilizante:Riego
# - Valores p asociados indican si los efectos son significativos
# - Un efecto significativo sugiere que el factor tiene un impacto en el rendimiento

# -----------------------------
# 2. Pruebas de Normalidad y Homogeneidad de Varianzas
# -----------------------------

# 2.1. Prueba de normalidad de los residuales (Shapiro-Wilk)

# Obtener los residuales del modelo ANOVA
residuos <- residuals(anova_factorial)

# Realizar la prueba de Shapiro-Wilk
shapiro_test <- shapiro.test(residuos)
print("Prueba de Shapiro-Wilk para normalidad de los residuales:")
print(shapiro_test)

# Interpretación:
# - Si el valor p es mayor que 0.05, no se rechaza la hipótesis nula de normalidad.
# - Si el valor p es menor que 0.05, se rechaza la hipótesis de normalidad.

# Gráfico Q-Q de los residuales
qqnorm(residuos)
qqline(residuos, col = "red")

# 2.2. Prueba de homogeneidad de varianzas (bartlett)

# Realizar la prueba de bartlett
bartlett_test <- bartlett.test(Rendimiento ~ interaction(Fertilizante,Riego), data = datos_simulados)
print("Prueba de Levene para homogeneidad de varianzas:")
print(bartlett_test)

# Interpretación:
# - Si el valor p es mayor que 0.05, no se rechaza la hipótesis nula de homogeneidad de varianzas.
# - Si el valor p es menor que 0.05, se rechaza la hipótesis de homogeneidad.

# -----------------------------
# 3. Análisis de Interacciones
# -----------------------------

# 3.1. Interpretación de efectos principales e interacciones

# Explicación:
# - Un efecto principal significativo indica que un factor tiene un efecto sobre la variable respuesta, independientemente del otro factor.
# - Una interacción significativa sugiere que el efecto de un factor depende del nivel del otro factor.

# 3.2. Creación e interpretación de gráficos de interacción

# Crear un gráfico de interacción utilizando ggplot2
interaction_plot <- ggplot(datos_simulados, aes(x = Fertilizante, y = Rendimiento, color = Riego, group = Riego)) +
  stat_summary(fun = mean, geom = "point", size = 3) +  # Puntos de medias
  stat_summary(fun = mean, geom = "line") +             # Líneas de medias
  theme_minimal() +
  labs(title = "Gráfico de Interacción entre Fertilizante y Riego",
       x = "Tipo de Fertilizante",
       y = "Rendimiento Promedio",
       color = "Nivel de Riego")

# Mostrar el gráfico de interacción
print(interaction_plot)

# Interpretación del gráfico:
# - Si las líneas son paralelas, no hay interacción entre los factores.
# - Si las líneas se cruzan o no son paralelas, existe interacción entre los factores.
# - Analizar cómo cambia el rendimiento según las combinaciones de niveles de los factores.

# 3.3. Análisis post-hoc en diseños factoriales

# Si encontramos efectos significativos, podemos realizar comparaciones múltiples

# 3.3.1. Comparaciones múltiples para efectos principales

# Prueba de Tukey para el efecto principal de Fertilizante
tukey_fert <- HSD.test(anova_factorial, "Fertilizante", group = TRUE)
print("Resultados de la Prueba de Tukey para Fertilizante:")
print(tukey_fert$groups)

# Prueba de Tukey para el efecto principal de Riego
tukey_riego <- HSD.test(anova_factorial, "Riego", group = TRUE)
print("Resultados de la Prueba de Tukey para Riego:")
print(tukey_riego$groups)

# 3.3.2. Comparaciones múltiples para interacciones

# Crear una nueva variable que combine los niveles de Fertilizante y Riego
datos_simulados <- datos_simulados %>%
  mutate(Tratamiento = interaction(Fertilizante, Riego))

# Realizar ANOVA considerando la nueva variable de tratamiento
anova_interaccion <- aov(Rendimiento ~ Tratamiento, data = datos_simulados)

# Prueba de Tukey para las combinaciones de tratamientos
tukey_trat <- HSD.test(anova_interaccion, "Tratamiento", group = TRUE)
print("Resultados de la Prueba de Tukey para las Interacciones:")
print(tukey_trat$groups)

# Visualización de los resultados de comparaciones múltiples

# Preparar datos para el gráfico
media_tratamientos <- datos_simulados %>%
  group_by(Tratamiento) %>%
  summarise(Media = mean(Rendimiento), SD = sd(Rendimiento)) %>%
  left_join(tukey_trat$groups %>% rename(Tratamiento = Treatment), by = "Tratamiento")

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

