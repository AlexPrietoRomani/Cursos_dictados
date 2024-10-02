# ==============================================================================================================================
#                                      Clase 1: Introducción a R y Conceptos Básicos de Diseño Experimental
# ==============================================================================================================================

# ==========================================
# 1. Introducción a R y RStudio
# ==========================================

# 1.1. Instalación y configuración básica
# Nota: La instalación de R y RStudio se realiza fuera de este script.
# Asegúrate de que R y RStudio estén instalados antes de comenzar.

# 1.2. Interfaz de RStudio y funciones básicas
# Explicación visual de RStudio se realiza directamente en el entorno de RStudio.

# 1.3. Comandos esenciales: asignación, funciones, vectores

# Asignación de valores a variables
# Utilizamos el operador <- para asignar valores
rendimiento <- 150  # Rendimiento de cultivo en quintales por hectárea
altura_planta <- 75  # Altura promedio de las plantas en cm

# Imprimir valores en la consola
print(rendimiento)
print(altura_planta)

# Uso de funciones básicas
# Calcular la raíz cuadrada del rendimiento
sqrt_rendimiento <- sqrt(rendimiento)
print(sqrt_rendimiento)

# Creación de vectores
# Vector de rendimientos de diferentes parcelas
rendimientos <- c(145, 152, 158, 149, 160)

# Vector de alturas de plantas correspondientes
alturas <- c(73, 76, 78, 75, 80)

# Imprimir vectores
print(rendimientos)
print(alturas)

# Operaciones con vectores
# Calcular el rendimiento total
rendimiento_total <- sum(rendimientos)
print(rendimiento_total)

# Calcular el rendimiento promedio
rendimiento_promedio <- mean(rendimientos)
print(rendimiento_promedio)

# 1.4. Importación de datos agrícolas simples

# Tenemos un archivo CSV llamado "PTPVS112016_CANAYPATA_exp1.xslx" con columnas "Parcela", "Rendimiento", "Altura"
# Ejemplo de cómo importar estos datos
install.packages("readxl")
library(readxl)

# Especificar el nombre del archivo Excel
nombre_archivo <- "potato DCA/PTPVS112016_CANAYPATA_exp1.xlsx"

# Construir la ruta completa al archivo (si es necesario)
ruta_completa <- file.path(getwd(), nombre_archivo)

# Leer el archivo Excel
data <- read_excel(ruta_completa, sheet = "F4_harvest_mother")

# Importar datos desde un archivo CSV
data_papa <- read.csv("potato/PTPVS112016_CANAYPATA_exp1.csv", header = TRUE, sep = ";")

# Ver las primeras filas del conjunto de datos importado
head(data_papa)

# Ver los tipos de variables del conjunto de datos
str(data_papa)

# Mostrar un resumen estadístico de los datos importados
summary(data_papa)

# Instalar y cargar paquetes
install.packages("tidyverse") # paquete 'tidyverse' para manipulación de datos
library(tidyverse)

# Usar dplyr para seleccionar columnas específicas
datos_seleccionados <- data_papa %>%
  select(NTP, NPH, NMTPL, MTWPL, ATW)

# Mostrar los datos seleccionados
print(datos_seleccionados)

# ==========================================
# 2. Conceptos básicos de diseño experimental
# ==========================================

# 2.1. Definición y objetivos en contexto agrícola
# Un diseño experimental es un plan estructurado para realizar experimentos que permite
# la recolección de datos de manera eficiente y la obtención de conclusiones válidas.

# Objetivos en contexto agrícola:
# - Determinar el efecto de diferentes tratamientos (fertilizantes, variedades, etc.) sobre variables de interés (rendimiento, altura, etc.).
# - Minimizar la variabilidad no controlada.
# - Maximizar la precisión de las estimaciones de los efectos.

# 2.2. Principios de Fisher aplicados a experimentos de campo
# R.A. Fisher propuso varios principios clave para el diseño experimental:
# - **Aleatorización**: Asignar tratamientos a unidades experimentales de forma aleatoria para evitar sesgos.
# - **Bloqueo**: Agrupar unidades experimentales similares en bloques para controlar la variabilidad.
# - **Replicación**: Repetir tratamientos múltiples veces para estimar la variabilidad experimental.

# Ejemplo de aleatorización en R
set.seed(123)  # Para reproducibilidad

# Supongamos que tenemos 4 tratamientos y 3 repeticiones
tratamientos <- rep(c("Fertilizante_A", "Fertilizante_B", "Fertilizante_C", "Fertilizante_D"), each = 3)
# Aleatorizar el orden de los tratamientos
tratamientos_aleatorios <- sample(tratamientos)
print(tratamientos_aleatorios)

# 2.3. Terminología clave en diseños agronómicos
# - **Tratamiento**: Condición o conjunto de condiciones aplicadas a las unidades experimentales.
# - **Unidad experimental**: Elemento básico del experimento al que se aplican los tratamientos.
# - **Bloque**: Grupo de unidades experimentales similares.
# - **Factor**: Variable categórica que se manipula en el experimento (e.g., tipo de fertilizante).
# - **Nivel**: Cada categoría dentro de un factor (e.g., Fertilizante_A, Fertilizante_B).


# ==========================================
# 3. Tipos de variables en experimentos agrícolas
# ==========================================

# 3.1. Variables cuantitativas
# Son variables que se pueden medir numéricamente.

# Ejemplos:
# - Rendimiento (quintales por hectárea)
# - Altura de planta (cm)

# Creación de un vector de rendimientos
rendimientos <- c(150, 155, 160, 158, 162, 165, 170, 168, 172, 175, 180, 178)
print(rendimientos)

# Cálculo de estadísticas descriptivas
media_rendimiento <- mean(rendimientos)
sd_rendimiento <- sd(rendimientos)
print(paste("Media del rendimiento:", media_rendimiento))
print(paste("Desviación estándar del rendimiento:", sd_rendimiento))

# 3.2. Variables cualitativas
# Son variables que describen cualidades o categorías.

# Ejemplos:
# - Resistencia a enfermedades (resistente, susceptible)
# - Tipo de suelo (arenoso, arcilloso, limoso)

# Creación de un factor para resistencia a enfermedades
resistencia <- factor(c("Resistente", "Susceptible", "Resistente", "Resistente", "Susceptible", "Susceptible"))
print(resistencia)

# Resumen de la variable cualitativa
summary(resistencia)

# ==========================================
# 4. Introducción a tipos de diseños experimentales utilizando la librería agricolae
# ==========================================

# Instalación y carga de la librería agricolae
install.packages("agricolae")
library(agricolae)

# 4.1. Diseño Completamente al Azar (DCA)
# *Descripción:*
# - Es el diseño experimental más simple. En este diseño, los tratamientos se asignan completamente al azar a las unidades experimentales.
# - Es útil cuando no hay fuentes conocidas de variación dentro de las unidades experimentales, lo que garantiza que cualquier variación observada se deba únicamente al tratamiento aplicado.

# *Características:*
# - Se emplea cuando las unidades experimentales son homogéneas.
# - Cada tratamiento tiene el mismo número de repeticiones, y no hay restricciones en la asignación de tratamientos.

# *Puntos clave:*
# - Aleatorización completa: Garantiza que cualquier diferencia entre las unidades experimentales se distribuya de manera aleatoria entre los tratamientos.
# - Simplicidad: Es muy sencillo de implementar en estudios donde no hay restricciones o fuentes adicionales de variación.

## Ejemplo de DCA
set.seed(456)
dca <- design.crd(trt = c("Fertilizante_A", "Fertilizante_B", "Fertilizante_C", "Fertilizante_D"),
                 r = 3,  # Número de repeticiones por tratamiento
                 serie = 2, 
                 seed = 456)  # Semilla para garantizar reproducibilidad

# Imprimir el libro de campo (fieldbook)
print("Diseño Completamente al Azar (DCA):")
print(dca$book)

# Ver la estructura original del libro de campo
str(dca$book)

# Renombrar la tercera columna a "Trt"
colnames(dca$book)[3] <- "Trt"

# Verificar la estructura después del renombramiento
str(dca$book)

### Con ggplot2
# Agregar columnas de fila y columna para una cuadrícula 3x4
dca_plot <- dca$book %>%
  mutate(Fila = rep(1:3, each = 4),
         Columna = rep(1:4, times = 3))

# Crear el gráfico de la distribución de tratamientos
ggplot(dca_plot, aes(x = Columna, y = Fila)) +  # Iniciar el objeto ggplot
  geom_tile(aes(fill = Trt), color = "black") +  # Añadir celdas coloreadas según el tratamiento
  geom_text(aes(label = Trt), color = "black", size = 4) +  # Añadir etiquetas de tratamiento dentro de las celdas
  scale_fill_brewer(palette = "Set3") +  # Elegir una paleta de colores predefinida para los tratamientos
  labs(title = "Diseño Completamente al Azar (DCA)",  # Añadir título al gráfico
       x = "Columna",  # Etiqueta para el eje X
       y = "Fila",     # Etiqueta para el eje Y
       fill = "Tratamiento") +  # Etiqueta para la leyenda de colores
  theme_minimal() +  # Aplicar un tema minimalista al gráfico para un aspecto limpio
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),  # Personalizar el título: tamaño, negrita, y centrado
    axis.title = element_text(size = 16),  # Personalizar las etiquetas de los ejes: tamaño
    axis.text = element_text(size = 14),   # Personalizar el texto de los ejes: tamaño
    legend.title = element_text(size = 16),  # Personalizar el título de la leyenda: tamaño
    legend.text = element_text(size = 14)    # Personalizar el texto de la leyenda: tamaño
  )


# 4.2. Diseño en Bloques Completamente al Azar (DBCA)
# *Descripción:*
# - Se utiliza cuando las unidades experimentales no son homogéneas. Las unidades experimentales se agrupan en bloques homogéneos y dentro de cada bloque, los tratamientos se asignan al azar.
# - Es útil para controlar las fuentes de variabilidad externas (como la posición, fertilidad del suelo, etc.) que pueden afectar la respuesta.

# *Características:*
# - Cada bloque debe ser lo más homogéneo posible.
# - Los bloques representan grupos de unidades experimentales que comparten características similares.

# *Puntos clave:*
# - Bloques: Controlan la variación entre las unidades experimentales. Es decir, se agrupan por bloques aquellas unidades experimentales que son similares en alguna característica.
# - Varianza reducida: Al bloquear, se reduce la variación entre las unidades experimentales dentro de cada bloque, lo que aumenta la precisión del experimento.

# Ejemplo de DBCA
set.seed(789)
dbca <- design.rcbd(trt = c("Fertilizante_A", "Fertilizante_B", "Fertilizante_C", "Fertilizante_D"),
                    r = 3,
                    serie = 2,  # Número de bloques
                    seed = 789)  # Semilla para reproducibilidad

# Imprimir el libro de campo (fieldbook)
print("Diseño en Bloques Completamente al Azar (DBCA):")
print(dbca$book)

# Renombrar la tercera columna a "Trt"
colnames(dbca$book)[3] <- "Trt"

# Preparar los datos para ggplot2
dbca_plot <- dbca$book %>%
  select(plots, block, Trt) %>%        # Seleccionar las columnas relevantes
  mutate(
    Bloque = as.numeric(as.character(block)),    # Convertir replicación a numérico
    Columna = (plots - 100) %% 4 + 1        # Asignar columnas basadas en 'plots'
  )

# Crear el gráfico de la distribución de tratamientos en bloques
ggplot(dbca_plot, aes(x = Columna, y = Bloque)) +
  geom_tile(aes(fill = Trt), color = "black") +  # Crear celdas coloreadas por tratamiento
  geom_text(aes(label = Trt), color = "white", size = 4) +  # Agregar etiquetas de tratamiento
  scale_fill_brewer(palette = "Set2") +  # Elegir una paleta de colores
  labs(title = "Diseño en Bloques Completos al Azar (DBCA)",
       x = "Tratamiento",
       y = "Bloque",
       fill = "Tratamiento") +
  theme_minimal() +  # Aplicar un tema minimalista
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "Calibri"),
    axis.title = element_text(size = 16, face = "bold", family = "Calibri"),
    axis.text = element_text(size = 14, family = "Calibri"),
    legend.title = element_text(size = 16, face = "bold", family = "Calibri"),
    legend.text = element_text(size = 14, family = "Calibri")
  )


# 4.3. Diseños Factoriales
# *Descripción:*
# - Este diseño permite estudiar simultáneamente los efectos de dos o más factores, y las interacciones entre ellos. Cada combinación de niveles de factores se considera un "tratamiento".
# - En el diseño factorial 2x2, cada factor tiene 2 niveles. Esto permite estudiar tanto los efectos individuales de los factores como sus interacciones.

# *Características:*
# - Adecuado para estudios en los que se quiere evaluar múltiples factores y sus posibles interacciones.
# - Es eficiente, ya que permite estudiar más de un factor en un solo experimento.

# *Puntos clave:*
# - Interacciones: Este diseño permite estudiar cómo la combinación de niveles de los factores afecta la respuesta, no solo los efectos individuales de los factores.
# - Eficiencia: Permite probar más de una variable a la vez, lo que lo convierte en una opción más eficiente en comparación con realizar varios experimentos independientes.

# Ejemplo de diseño factorial 2x2

trt <- c(2, 2)  # Esto indica un factorial 2x2 (dos factores con dos niveles cada uno)

# Generar el diseño factorial en bloques completos al azar (RCBD) con 3 repeticiones
set.seed(123)
outdesign <- design.ab(trt = trt, r = 3, serie = 2, design = "rcbd", seed = 123)

# Imprimir el libro de campo (fieldbook)
print("Diseño Factorial 2x2:")
print(outdesign$book)

str(outdesign$book)

# Renombrar las columnas A y B a "Factor_A" y "Factor_B" para mayor claridad
colnames(outdesign$book)[3:4] <- c("Factor_A", "Factor_B")
str(outdesign$book)

# Crear una nueva columna 'Tratamiento' combinando Factor_A y Factor_B
factorial_plot <- outdesign$book %>%
  mutate(Tratamiento = paste0(Factor_A, Factor_B))

# Asignar posiciones de fila y columna dentro de cada bloque
factorial_plot <- factorial_plot %>%
  group_by(block) %>%
  mutate(
    Fila = rep(1:2, each = 2),        # 2 filas por bloque
    Columna = rep(1:2, times = 2)     # 2 columnas por bloque
  ) %>%
  ungroup()

# Crear el gráfico de la distribución de tratamientos
ggplot(factorial_plot, aes(x = Columna, y = Fila)) +
  geom_tile(aes(fill = Tratamiento), color = "black") +    # Crear celdas coloreadas por tratamiento
  geom_text(aes(label = Tratamiento), color = "white", size = 5, fontface = "bold") +  # Agregar etiquetas de tratamiento
  scale_fill_brewer(palette = "Set1") +                    # Elegir una paleta de colores adecuada
  facet_wrap(~ block) +                                     # Crear paneles por bloque
  labs(title = "Diseño Factorial 2x2 en RCBD",
       x = "Columna",
       y = "Fila",
       fill = "Tratamiento") +
  theme_minimal() +                                        # Aplicar un tema minimalista
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5, family = "Calibri"),
    axis.title = element_text(size = 16, face = "bold", family = "Calibri"),
    axis.text = element_text(size = 14, family = "Calibri"),
    strip.text = element_text(size = 16, face = "bold", family = "Calibri"),
    legend.title = element_text(size = 16, face = "bold", family = "Calibri"),
    legend.text = element_text(size = 14, family = "Calibri")
  )


# 4.4. Cuadrados Latinos
# *Descripción:*
# - El diseño de Cuadrado Latino es útil cuando se deben controlar dos fuentes de variabilidad. En este diseño, tanto las filas como las columnas (los factores de confusión) se bloquean para reducir la variabilidad.
# - Se utiliza cuando se quiere estudiar un solo factor, pero se tienen dos fuentes de variación.

# *Características:*
# - Cada tratamiento aparece una vez en cada fila y en cada columna.
# - Ideal para controlar dos factores adicionales que no son de interés pero que podrían influir en los resultados.

# *Puntos clave:*
# - Control de dos factores: Permite controlar dos fuentes de variación que podrían afectar los resultados, como la ubicación y el tiempo.
# - Estructura cuadrada: Los tratamientos están distribuidos de manera que cada tratamiento aparece exactamente una vez en cada fila y cada columna, lo que minimiza la confusión con los efectos de fila y columna.

# Ejemplo simplificado de Cuadrado Latino
set.seed(101112)
latino <- design.lsd(trt = c("T1", "T2", "T3", "T4"),  # Tratamientos
                    serie = 2, 
                    seed = 101112)

# Imprimir el libro de campo (fieldbook)
print("Cuadrado Latino:")
print(latino$book)