# ==========================================
# Clase 2: Estadística Descriptiva y Diseño Completamente al Azar (DCA)
# ==========================================

# -----------------------------
# 0. Preparación del Entorno
# -----------------------------

# Limpiar el entorno de trabajo
rm(list = ls())

# Instalar y cargar paquetes necesarios
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("agricolae")
# install.packages("car")

library(dplyr)
library(ggplot2)
library(agricolae)
library(car)

# -----------------------------
# 1. Cargar y Preparar los Datos
# -----------------------------

# Leer los datos desde un archivo CSV separado por punto y coma
data_papa <- read.csv("potato DCA/PTPVS112016_CANAYPATA_exp1.csv", header = TRUE, sep = ";")

# Ver las primeras filas del conjunto de datos
head(data_papa)

# Ver la estructura de los datos
str(data_papa)

# Seleccionar las columnas específicas: NTP, NPH, NMTPL, MTWPL, ATW
datos_seleccionados <- data_papa %>%
  select(NTP, NPH, NMTPL, MTWPL, ATW)

# Ver las primeras filas de los datos seleccionados
head(datos_seleccionados)

# -----------------------------
# 1. Estadística Descriptiva en R
# -----------------------------

# 1.1. Cálculo de medidas de tendencia central y dispersión 

# Convertir datos a formato largo para facilitar el resumen por variable
datos_long <- datos_seleccionados %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

# Calcular medidas de tendencia central y dispersión para cada variable
medidas_descriptivas <- datos_long %>%
  group_by(Variable) %>%
  summarise(
    Media = mean(Valor, na.rm = TRUE),
    Mediana = median(Valor, na.rm = TRUE),
    SD = sd(Valor, na.rm = TRUE),
    Varianza = var(Valor, na.rm = TRUE),
    Rango = max(Valor, na.rm = TRUE) - min(Valor, na.rm = TRUE)
  )

# Imprimir las medidas descriptivas
print("Medidas de Tendencia Central y Dispersión por Variable:")
print(medidas_descriptivas)

# 1.2. Visualización básica con ggplot2: histogramas, boxplots

# Crear un data frame para ggplot2
datos_visual <- data_papa %>%
  select(INSTN, NTP, NPH, NMTPL, MTWPL, ATW)

# Histograma de ATW
hist_ATW <- ggplot(datos_visual, aes(x = ATW)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Histograma del Peso promedio de tubérculos (g) (ATW)",
       x = "Peso promedio de tubérculos (g)",
       y = "Frecuencia")
print(hist_ATW)

# Boxplot de ATW
box_ATW <- ggplot(datos_visual, aes(y = ATW, x = INSTN)) +
  geom_boxplot(fill = "lightgreen", color = "darkgreen") +
  theme_minimal() +
  labs(title = "Boxplot del Peso promedio de tubérculos (g) (ATW)",
       y = "Peso promedio de tubérculos (g)")
print(box_ATW)

# Histograma de NPH
hist_NPH <- ggplot(datos_visual, aes(x = NPH)) +
  geom_histogram(binwidth = 1, fill = "salmon", color = "black") +
  theme_minimal() +
  labs(title = "Histograma del Número de Plantas Cosechadas (NPH)",
       x = "Número de Plantas Cosechadas",
       y = "Frecuencia")
print(hist_NPH)

# Boxplot de NPH
box_NPH <- ggplot(datos_visual, aes(y = NPH, x = INSTN)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  theme_minimal() +
  labs(title = "Boxplot del Número de Plantas Cosechadas (NPH)",
       y = "Número de Plantas Cosechadas")
print(box_NPH)

# -----------------------------
# 2. Diseño Completamente al Azar (DCA)
# -----------------------------

# 2.1. Concepto y aplicaciones en ensayos agrícolas
# Nota: Esta parte se discute teóricamente en la clase.

# 2.2. Implementación de un DCA en R
# Asumimos que 'INSTN' es el factor de genotipo y 'REP' es la replicación.
# Asegúrate de que 'INSTN' sea un factor
data_papa$INSTN <- as.factor(data_papa$INSTN)

# Crear el diseño DCA utilizando la librería agricolae
# Agrupamos los datos por 'REP' y 'INSTN'

# Verificar número de tratamientos y repeticiones
num_tratamientos <- length(levels(data_papa$INSTN))
num_replicaciones <- length(unique(data_papa$REP))

cat("Número de Tratamientos:", num_tratamientos, "\n")
cat("Número de Replicaciones:", num_replicaciones, "\n")

# Crear el diseño DCA
# Aunque la data ya está estructurada, usamos agricolae para realizar ANOVA y otros análisis
# No es necesario recrear el diseño, pero podemos organizar los datos adecuadamente

# 2.3. ANOVA de una vía: interpretación de resultados

# Seleccionar una variable para ANOVA, por ejemplo, MTWPL (Marketable tuber weight per plant)
# Asegúrate de que 'INSTN' es el factor y la variable es numérica

anova_data <- data_papa %>%
  select(INSTN, MTWPL) %>%
  na.omit()

# Realizar ANOVA
anova_result <- aov(MTWPL ~ INSTN, data = anova_data)
summary(anova_result)

# Interpretación:
# - Si el valor p < 0.05, hay diferencias significativas entre tratamientos.
# - Si el valor p >= 0.05, no hay diferencias significativas.

# Prueba post-hoc de Tukey si ANOVA es significativa
if(summary(anova_result)[[1]][["Pr(>F)"]][1] < 0.05){
  tukey_result <- HSD.test(anova_result, "INSTN", group = TRUE)
  print(tukey_result)
} else {
  cat("No se encontraron diferencias significativas entre los tratamientos.\n")
}

# -----------------------------
# 3. Verificación de supuestos
# -----------------------------

# 3.1. Pruebas de normalidad y gráficos Q-Q

# Prueba de Shapiro-Wilk para normalidad de los residuales
shapiro_result <- shapiro.test(anova_result$residuals)
print(shapiro_result)

# Interpretación:
# - Si p < 0.05, los residuales no siguen una distribución normal.
# - Si p >= 0.05, no hay evidencia de no normalidad.

# Gráfico Q-Q de los residuales con ggplot
qq_plot <- ggplot(data = data.frame(residuals = anova_result$residuals), aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  theme_minimal() +
  labs(title = "Gráfico Q-Q de los Residuales del ANOVA",
       x = "Cuantiles Teóricos",
       y = "Cuantiles Muestrales")
print(qq_plot)

# Gráfico Q-Q de los residuales con ggplot
qqnorm(residuals(anova_result))
qqline(residuals(anova_result), col = "red")

# 3.2. Pruebas de homogeneidad de varianzas

# Prueba de Levene para homogeneidad de varianzas
levene_result <- leveneTest(MTWPL ~ INSTN, data = anova_data)
print(levene_result)

# Interpretación:
# - Si p < 0.05, las varianzas no son homogéneas.
# - Si p >= 0.05, no hay evidencia de heterogeneidad de varianzas.

# -----------------------------
# 4. Análisis de datos cualitativos
# -----------------------------

# 4.1. Uso de chi-cuadrado para variables categóricas en agronomía 

# Crear una variable categórica basada en MTWPL, por ejemplo, alta y baja producción
# Definir un umbral, por ejemplo, la mediana
umbral_MTWPL <- median(anova_data$MTWPL, na.rm = TRUE)
anova_data <- anova_data %>%
  mutate(MTWPL_cat = ifelse(MTWPL > umbral_MTWPL, "Alta", "Baja"))

# Crear una tabla de contingencia entre INSTN y MTWPL_cat
tabla_contingencia <- table(anova_data$INSTN, anova_data$MTWPL_cat)

# Mostrar la tabla de contingencia
cat("Tabla de Contingencia entre Tratamientos (INSTN) y Categoría de MTWPL:\n")
print(tabla_contingencia)

# Realizar la prueba de chi-cuadrado
chi_cuadrado_result <- chisq.test(tabla_contingencia)

# Mostrar resultados
print(chi_cuadrado_result)

# Interpretación:
# - Si p < 0.05, hay una asociación significativa entre los tratamientos y la categoría de MTWPL.
# - Si p >= 0.05, no hay asociación significativa.
