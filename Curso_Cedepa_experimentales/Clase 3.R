# ==========================================
# Clase 3: Diseño de Bloques Completos al Azar (DBCA) y Comparaciones Múltiples
# ==========================================

# -----------------------------
# 0. Preparación del Entorno
# -----------------------------

# Limpiar el entorno de trabajo para evitar conflictos con variables anteriores
rm(list = ls())

# Instalar y cargar paquetes necesarios
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("agricolae")
# install.packages("car")
# install.packages("readr")
# install.packages("rstatix")
# install.packages("FSA")
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
library(nlme)                 # Para modelos lineales generalizados
library(emmeans)              # Para obtener y comparar medias ajustadas
library(multcompView)        # Para asignar letras a grupos

# -----------------------------
# 1. Diseño de Bloques Completos al Azar (DBCA)
# -----------------------------

# 1.1. Concepto y aplicaciones en campos heterogéneos

# El Diseño de Bloques Completos al Azar (DBCA) es un diseño experimental utilizado cuando
# existe heterogeneidad en el campo que puede afectar los resultados del experimento.
# La variabilidad se controla agrupando unidades experimentales similares en bloques.
# Dentro de cada bloque, los tratamientos se asignan aleatoriamente.

# Aplicaciones:
# - Experimentos agrícolas donde hay gradientes de fertilidad, humedad u otras condiciones.
# - Ensayos clínicos y estudios donde se necesita controlar variables externas.

# 1.2. Implementación de DBCA en R con los tres datasets

# Definimos los tratamientos y los bloques.
trt_levels <- c("Tratamiento_A", "Tratamiento_B", "Tratamiento_C", "Tratamiento_D")
block_levels <- c("Bloque_1", "Bloque_2", "Bloque_3", "Bloque_4")

# -----------------------------
# 2. Análisis del Dataset A: Normalidad y Homogeneidad de Varianzas
# -----------------------------

# 2.1. Cargar el Dataset A
datos_a <- read_csv("Cursos_dictados/Curso_Cedepa_experimentales/Datasets/DBCA_datos_normal.csv")

# 2.2. ANOVA
anova_a <- aov(Rendimiento ~ trt + block, data = datos_a)

summary(anova_a)

# Coeficiente de varianza (CV)
cv.model(anova_a)

# 2.3. Verificación de Supuestos

# Prueba de Normalidad (Shapiro-Wilk)

shapiro_a <- shapiro.test(anova_a$residuals)

summary(shapiro_a)

print(shapiro_a)

# GRaficos de Normalidad (Q-Q plots)
qqnorm(residuals(anova_a))
qqline(residuals(anova_a), col = "red")

# Prueba de Homogeneidad de Varianzas (Levene)
levene_a <- leveneTest(Rendimiento ~ trt, data = datos_a)
barlet <- bartlett.test(Rendimiento ~ trt, data = datos_a)

print(levene_a)

# Crear un boxplot de Rendimiento por Tratamiento
ggplot(datos_a, aes(x = trt, y = Rendimiento, fill = trt)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot de Rendimiento por Tratamiento",
       x = "Tratamiento",
       y = "Rendimiento") +
  theme(
    plot.title = element_text(family = "mono", size = 20, color = "red", face = "bold"),
    plot.title.x = element_text(family = "mono")
    )

ggplot(datos_a, aes(x = trt, y = Rendimiento, fill = trt)) + 
  

# Interpretación:
# - Si p > 0.05 en ambas pruebas, los supuestos se cumplen y podemos proceder con confianza.

# 2.4. Comparaciones Múltiples (Prueba de Tukey)
tukey_a <- HSD.test(anova_a, "trt", group = TRUE)
print("Resultados de la Prueba de Tukey HSD (Dataset A):")
print(tukey_a$groups)

# 2.5. Gráfico de Barras con Letras (Utilizando bar.group de agricolae)
bar.group(tukey_a$groups, horiz = FALSE,
          col = terrain.colors(length(unique(datos_a$trt))),
          main = "Rendimiento Promedio por Tratamiento (Dataset A)",
          ylab = "Rendimiento Promedio",
          xlab = "Tratamiento", ylim = c(0, 30), las = 1)

# -----------------------------
# 3. Análisis del Dataset B: Sin Normalidad y Sin Homogeneidad de Varianzas
# -----------------------------

# 3.1. Cargar el Dataset B
datos_b <- read_csv("DBCA_datos_no_normal.csv",show_col_types = FALSE)

# Convertir 'trt' y 'block' a factores
datos_b <- datos_b %>%
  mutate(
    trt = factor(trt, levels = trt_levels),
    block = factor(block, levels = block_levels)
  )

# 3.2. ANOVA
anova_b <- aov(Rendimiento ~ trt + block, data = datos_b)
summary(anova_b)

# Coeficiente de varianza (CV)
cv.model(anova_b)

# 3.3. Verificación de Supuestos

# Prueba de Normalidad (Shapiro-Wilk)
residuos_b <- residuals(anova_b)
shapiro_b <- shapiro.test(residuos_b)
print("Prueba de Shapiro-Wilk para normalidad (Dataset B):")
print(shapiro_b)

# GRaficos de Normalidad (Q-Q plots)
qqnorm(residuos_b, main = "Q-Q Plot de Residuales (Dataset B)")
qqline(residuos_b, col = "red")

# Prueba de Homogeneidad de Varianzas (Levene)
levene_b <- leveneTest(Rendimiento ~ trt, data = datos_b)
print("Prueba de Levene para homogeneidad de varianzas (Dataset B):")
print(levene_b)

# Crear un boxplot de Rendimiento por Tratamiento
ggplot(datos_b, aes(x = trt, y = Rendimiento, fill = trt)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot de Rendimiento por Tratamiento",
       x = "Tratamiento",
       y = "Rendimiento") +
  theme(legend.position = "none")

# Interpretación:
# - Ambos supuestos no se cumplen (p < 0.05), por lo que el ANOVA estándar no es apropiado.

# 3.4. Alternativas

# 3.4.1. Transformación de Datos
# Intentamos una transformación logarítmica
datos_b <- datos_b %>%
  mutate(Rendimiento_log = log(Rendimiento))

# ANOVA con datos transformados
anova_b_trans <- aov(Rendimiento_log ~ trt + block, data = datos_b)
summary(anova_b_trans)

# Coeficiente de varianza (CV)
cv.model(anova_b_trans)

# Verificación de supuestos en datos transformados
residuos_b_trans <- residuals(anova_b_trans)
shapiro_b_trans <- shapiro.test(residuos_b_trans)
levene_b_trans <- leveneTest(Rendimiento_log ~ trt, data = datos_b)

print("Prueba de Shapiro-Wilk para normalidad (Datos Transformados, Dataset B):")
print(shapiro_b_trans)
print("Prueba de Levene para homogeneidad de varianzas (Datos Transformados, Dataset B):")
print(levene_b_trans)

# Si los supuestos aún no se cumplen, considerar pruebas no paramétricas.

# 3.4.2. Prueba No Paramétrica (Kruskal-Wallis)
kruskal_b <- kruskal.test(Rendimiento ~ trt, data = datos_b)
print("Prueba de Kruskal-Wallis (Dataset B):")
print(kruskal_b)

# 3.4.3. Realizar Comparaciones Múltiples No Paramétricas (Prueba de Dunn)
comparaciones_dunn_b <- datos_b %>%
  dunn_test(Rendimiento ~ trt, p.adjust.method = "bonferroni") %>%
  add_significance()

comparaciones_dunn_b

# Nota: En casos donde los supuestos no se cumplen y las transformaciones no ayudan,
# se recomienda utilizar métodos no paramétricos.

# -----------------------------
# 4. Análisis del Dataset C: Normalidad pero Sin Homogeneidad de Varianzas
# -----------------------------

# 4.1. Cargar el Dataset C
datos_c <- read_csv("DBCA_datos_normal_no_homocedasticidad.csv", show_col_types = FALSE)

# Convertir 'trt' y 'block' a factores
datos_c <- datos_c %>%
  mutate(
    trt = factor(trt, levels = trt_levels),
    block = factor(block, levels = block_levels)
  )

# 4.2. ANOVA
anova_c <- aov(Rendimiento ~ trt + block, data = datos_c)
summary(anova_c)

# Coeficiente de Varianza (CV)
cv.model(anova_c)

# 4.3. Verificación de Supuestos

# Prueba de Normalidad (Shapiro-Wilk)
residuos_c <- residuals(anova_c)
shapiro_c <- shapiro.test(residuos_c)
print("Prueba de Shapiro-Wilk para normalidad (Dataset C):")
print(shapiro_c)

# Gráfico Q-Q de los residuales
qqnorm(residuos_c, main = "Q-Q Plot de Residuales (Dataset C)")
qqline(residuos_c, col = "red")

# Prueba de Homogeneidad de Varianzas (Levene)
levene_c <- leveneTest(Rendimiento ~ trt, data = datos_c)
print("Prueba de Levene para homogeneidad de varianzas (Dataset C):")
print(levene_c)

# Crear un boxplot de Rendimiento por Tratamiento
ggplot(datos_c, aes(x = trt, y = Rendimiento, fill = trt)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot de Rendimiento por Tratamiento (Dataset C)",
       x = "Tratamiento",
       y = "Rendimiento") +
  theme(legend.position = "none")

# Interpretación:
# - Normalidad se cumple (p > 0.05), pero homogeneidad de varianzas no (p < 0.05).

# 4.4. Alternativas

# 4.4.1. Transformación de Datos
# Intentamos una transformación logarítmica
datos_c <- datos_c %>%
  mutate(Rendimiento_log = log(Rendimiento))

# ANOVA con datos transformados
anova_c_trans <- aov(Rendimiento_log ~ trt + block, data = datos_c)
summary(anova_c_trans)

# Coeficiente de varianza (CV)
cv.model(anova_c_trans)

# Verificación de supuestos en datos transformados
residuos_c_trans <- residuals(anova_c_trans)
shapiro_c_trans <- shapiro.test(residuos_c_trans)
levene_c_trans <- leveneTest(Rendimiento_log ~ trt, data = datos_c)

print("Prueba de Shapiro-Wilk para normalidad (Datos Transformados, Dataset C):")
print(shapiro_c_trans)
print("Prueba de Levene para homogeneidad de varianzas (Datos Transformados, Dataset C):")
print(levene_c_trans)

# Si después de la transformación la homogeneidad se cumple (p > 0.05), podemos proceder.

# 4.4.2. Usar Modelos que Permitan Varianzas Heterogéneas
# Ajustar un modelo lineal generalizado con varianzas diferentes por tratamiento

modelo_c <- gls(Rendimiento ~ trt + block, data = datos_c,
                weights = varIdent(form = ~ 1 | trt))
summary(modelo_c)

# 4.4.3. Comparaciones Múltiples con emmeans
library(emmeans)

# Obtener las medias ajustadas
emmeans_c <- emmeans(modelo_c, "trt")

# Comparaciones múltiples tipo Tukey ajustadas
pairs_c <- pairs(emmeans_c, adjust = "tukey")
print("Comparaciones Múltiples con emmeans (Dataset C):")
print(pairs_c)

# -----------------------------
# 5. Análisis del Dataset D: No Normalidad pero Sí Homogeneidad de Varianzas
# -----------------------------

# 5.1. Cargar el Dataset D
datos_d <- read_csv("DBCA_datos_no_normal_homocedasticidad.csv", show_col_types = FALSE)

# 5.2. Convertir 'trt' y 'block' a factores con niveles específicos
datos_d <- datos_d %>%
  mutate(
    trt = factor(trt, levels = trt_levels),
    block = factor(block, levels = block_levels)
  )

# 5.3. ANOVA Estándar (Opcional)
# Si los tamaños de muestra son equilibrados, ANOVA puede ser robusto frente a la no normalidad
anova_d <- aov(Rendimiento ~ trt + block, data = datos_d)
summary(anova_d)

# 5.4. Coeficiente de Varianza (CV)
cv.model(anova_d)


# 5.5. Verificación de Supuestos

# Prueba de Normalidad (Shapiro-Wilk)
residuos_d <- residuals(anova_d)
shapiro_d <- shapiro.test(residuos_d)
print("Prueba de Shapiro-Wilk para normalidad (Dataset D):")
print(shapiro_d)

# Gráficos de Normalidad (Q-Q plots)
qqnorm(residuos_d, main = "Q-Q Plot de Residuales (Dataset D)")
qqline(residuos_d, col = "red")

# Prueba de Homogeneidad de Varianzas (Levene)
levene_d <- leveneTest(Rendimiento ~ trt, data = datos_d)
print("Prueba de Levene para homogeneidad de varianzas (Dataset D):")
print(levene_d)

# Crear un boxplot de Rendimiento por Tratamiento
ggplot(datos_d, aes(x = trt, y = Rendimiento, fill = trt)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot de Rendimiento por Tratamiento (Dataset D)",
       x = "Tratamiento",
       y = "Rendimiento") +
  theme(legend.position = "none")

# 5.6. Alternativas

# 5.6.1. Prueba No Paramétrica (Kruskal-Wallis)
kruskal_d <- kruskal.test(Rendimiento ~ trt, data = datos_d)
print("Prueba de Kruskal-Wallis (Dataset D):")
print(kruskal_d)

# 5.6.2. Comparaciones Múltiples No Paramétricas (Prueba de Dunn) con Letras
comparaciones_dunn_d <- datos_d %>%
  dunn_test(Rendimiento ~ trt, p.adjust.method = "bonferroni") %>%
  add_significance()

comparaciones_dunn_d

# 5.7. Visualización de Resultados

# Crear un boxplot de Rendimiento por Tratamiento
ggplot(datos_d, aes(x = trt, y = Rendimiento, fill = trt)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Boxplot de Rendimiento por Tratamiento (Dataset D)",
       x = "Tratamiento",
       y = "Rendimiento") +
  theme(legend.position = "none")

# Crear un gráfico de violín de Rendimiento por Tratamiento
ggplot(datos_d, aes(x = trt, y = Rendimiento, fill = trt)) +
  geom_violin(trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  theme_minimal() +
  labs(title = "Gráfico de Violín de Rendimiento por Tratamiento (Dataset D)",
       x = "Tratamiento",
       y = "Rendimiento") +
  theme(legend.position = "none")

# -----------------------------
# 6. Conclusiones y Recomendaciones
# -----------------------------
#
# | Dataset | Normalidad | Homogeneidad de Varianzas | ANOVA Posible | Pruebas Post-hoc       | 
# |---------|------------|--------------------------|---------------|-------------------------|
# | A       | Sí         | Sí                       | Sí            | Sí (Tukey, Duncan)      |
# | B       | No         | No                       | No*           | No                      |
# | C       | Sí         | No                       | Sí**          | No***                   |
# | D       | No         | Sí                       | No*           | No                      |
#
# Leyenda de Notas Adicionales:
# - *En estos casos, se recomienda usar pruebas no paramétricas como Kruskal-Wallis.
# - **Se puede usar ANOVA con correcciones como Welch o Brown-Forsythe.
# - ***Aunque el ANOVA es posible, las pruebas post-hoc tradicionales no son recomendables. Se pueden usar alternativas como Games-Howell.
#
# -----------------------------
# Información Teórica Adicional
# -----------------------------

# **Sobre el DBCA:**
# - El bloqueo es una estrategia para controlar la variabilidad no controlable experimentalmente.
# - Los bloques deben ser lo más homogéneos posible internamente y diferentes entre sí.
# - La aleatorización dentro de los bloques asegura que los tratamientos se asignen sin sesgo.

# **Sobre las Pruebas de Comparaciones Múltiples:**
# - **Prueba de Tukey HSD:** Asume homogeneidad de varianzas y normalidad. No es apropiada si estos supuestos no se cumplen.
# - **Prueba de Games-Howell:** No asume homogeneidad de varianzas. Es apropiada cuando las varianzas son heterogéneas.
# - **Pruebas No Paramétricas:** Utilizadas cuando los datos no cumplen con normalidad ni homogeneidad.

# **Consideraciones al Elegir una Prueba:**
# - Verificar los supuestos antes de seleccionar la prueba.
# - El nivel de significancia deseado.
# - El riesgo aceptable de cometer errores Tipo I (falsos positivos).
# - La naturaleza de los datos y el diseño experimental.

# **Mejores Prácticas:**
# - Siempre verificar los supuestos del ANOVA.
# - Utilizar transformaciones o métodos alternativos cuando los supuestos no se cumplen.
# - Reportar claramente los resultados y los métodos utilizados para abordar violaciones de supuestos.
