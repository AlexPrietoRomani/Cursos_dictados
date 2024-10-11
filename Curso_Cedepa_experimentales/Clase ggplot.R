# ==========================================
# Clase Extra: Visualización de Datos con ggplot2 en Agronomía
# ==========================================

# -----------------------------
# 0. Preparación del Entorno
# -----------------------------

# Limpiar el entorno de trabajo
rm(list = ls())

# Instalar y cargar paquetes necesarios
# Descomenta las líneas de instalación si no tienes los paquetes instalados
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("tidyr")

library(ggplot2)   # Visualización de datos
library(dplyr)     # Manipulación de datos
library(readr)     # Lectura de datos
library(tidyr)     # Transformación de datos

# -----------------------------
# 1. Introducción a ggplot2
# -----------------------------

# ggplot2 es un paquete de R para la creación de gráficos de alta calidad.
# Se basa en la "Gramática de los Gráficos", lo que permite construir gráficos
# de manera modular y consistente.

# -----------------------------
# 2. Datos de Ejemplo en Agronomía
# -----------------------------

# Para nuestros ejemplos, utilizaremos un conjunto de datos simulado
# relacionado con el rendimiento de cultivos bajo diferentes tratamientos.

# Simularemos datos de un experimento factorial con dos factores:
# - Tipo de Fertilizante (A, B, C)
# - Variedad de Cultivo (V1, V2, V3)

set.seed(123)
datos <- expand.grid(
  Fertilizante = c("A", "B", "C"),
  Variedad = c("V1", "V2", "V3"),
  Repetición = 1:10
)

datos <- datos %>%
  mutate(
    Rendimiento = rnorm(
      n(),
      mean = ifelse(Fertilizante == "A", 50,
                    ifelse(Fertilizante == "B", 55, 60)) +
        ifelse(Variedad == "V1", 5,
               ifelse(Variedad == "V2", 0, -5)),
      sd = 5
    )
  )

# Mostrar los primeros registros
head(datos)

# -----------------------------
# 3. Gráficos Básicos con ggplot2
# -----------------------------

# La función principal de ggplot2 es ggplot(), a la que se le añaden capas
# utilizando el operador "+".

# 3.1. Gráfico de Dispersión (Scatter Plot)

# Queremos visualizar la relación entre el fertilizante y el rendimiento.

ggplot(data = datos, aes(x = Fertilizante, y = Rendimiento)) +
  geom_point()

# 3.2. Gráfico de Barras (Bar Plot)

# Visualizar el rendimiento promedio por tipo de fertilizante.

# Calcular medias
medias_fertilizante <- datos %>%
  group_by(Fertilizante) %>%
  summarise(Media_Rendimiento = mean(Rendimiento))

# Gráfico de barras
ggplot(data = medias_fertilizante, aes(x = Fertilizante, y = Media_Rendimiento)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Rendimiento Promedio por Fertilizante",
       x = "Fertilizante",
       y = "Rendimiento Promedio")

# 3.3. Boxplot

# Visualizar la distribución del rendimiento por variedad.

ggplot(data = datos, aes(x = Variedad, y = Rendimiento)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Distribución del Rendimiento por Variedad",
       x = "Variedad",
       y = "Rendimiento")

# -----------------------------
# 4. Añadiendo Estética y Facetas
# -----------------------------

# 4.1. Colores y Formas

# Podemos mapear variables a atributos estéticos como color y forma.

ggplot(data = datos, aes(x = Fertilizante, y = Rendimiento, color = Variedad)) +
  geom_point(size = 3) +
  labs(title = "Rendimiento por Fertilizante y Variedad",
       x = "Fertilizante",
       y = "Rendimiento")

# 4.2. Facetas

# Las facetas permiten crear paneles separados para cada nivel de una variable.

ggplot(data = datos, aes(x = Fertilizante, y = Rendimiento)) +
  geom_boxplot(fill = "orange") +
  facet_wrap(~ Variedad) +
  labs(title = "Rendimiento por Fertilizante y Variedad",
       x = "Fertilizante",
       y = "Rendimiento")

# -----------------------------
# 5. Gráficos Avanzados
# -----------------------------

# 5.1. Gráfico de Interacción

# Visualizar la interacción entre fertilizante y variedad.

ggplot(data = datos, aes(x = Fertilizante, y = Rendimiento, group = Variedad, color = Variedad)) +
  stat_summary(fun = mean, geom = "line", size = 1) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  labs(title = "Interacción entre Fertilizante y Variedad",
       x = "Fertilizante",
       y = "Rendimiento Promedio")

# 5.2. Gráfico de Densidad

# Comparar la distribución del rendimiento entre variedades.

ggplot(data = datos, aes(x = Rendimiento, fill = Variedad)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución del Rendimiento por Variedad",
       x = "Rendimiento",
       y = "Densidad")

# 5.3. Gráfico de Calor (Heatmap)

# Supongamos que tenemos datos de rendimiento en diferentes parcelas.

# Simulamos datos de rendimiento en una cuadrícula de parcelas.

datos_parcelas <- expand.grid(
  Fila = 1:10,
  Columna = 1:10
)

datos_parcelas <- datos_parcelas %>%
  mutate(
    Rendimiento = rnorm(n(), mean = 50, sd = 5)
  )

# Gráfico de calor del rendimiento en el campo

ggplot(data = datos_parcelas, aes(x = Columna, y = Fila, fill = Rendimiento)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Mapa de Calor del Rendimiento en el Campo",
       x = "Columna",
       y = "Fila")

# -----------------------------
# 6. Personalización de Gráficos
# -----------------------------

# 6.1. Temas

# ggplot2 ofrece temas predefinidos para cambiar la apariencia de los gráficos.

ggplot(data = datos, aes(x = Fertilizante, y = Rendimiento, color = Variedad)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Rendimiento por Fertilizante y Variedad (Tema Minimal)",
       x = "Fertilizante",
       y = "Rendimiento")

# 6.2. Etiquetas y Leyendas (Ampliado)

# Personalización avanzada de etiquetas, leyendas y temas en ggplot2.

# El paquete ggplot2 permite una amplia personalización de los gráficos mediante la función `theme()` y elementos como `element_text()`, `element_line()`, `element_rect()`, etc.

# A continuación, mostraremos cómo personalizar diferentes aspectos del gráfico, incluyendo:

#- Cambiar el estilo de texto (negrita, cursiva).
#- Cambiar la fuente.
#- Ajustar el tamaño y la posición de los títulos y etiquetas.
#- Modificar la apariencia de la leyenda.
#- Personalizar el fondo y las cuadrículas.

## Código Base

#Partimos del gráfico básico:

ggplot(data = datos, aes(x = Fertilizante, y = Rendimiento, shape = Variedad, color = Variedad)) +
  geom_point(size = 3) +
  labs(
    title = "Rendimiento por Fertilizante y Variedad",
    x = "Tipo de Fertilizante",
    y = "Rendimiento (kg/ha)",
    color = "Variedad de Cultivo",
    shape = "Variedad de Cultivo"
  ) +
  theme(legend.position = "bottom")

# Modificamos el tema pre-definido
## ggplot2 incluye temas predefinidos que puedes utilizar directamente, como theme_bw(), theme_minimal(), theme_classic(), etc.
# install.packages("ggthemes")
library(ggthemes)

ggplot(data = datos, aes(x = Fertilizante, y = Rendimiento, shape = Variedad, color = Variedad)) +
  geom_point(size = 3) +
  labs(
    title = "Rendimiento por Fertilizante y Variedad",
    x = "Tipo de Fertilizante",
    y = "Rendimiento (kg/ha)",
    color = "Variedad de Cultivo",
    shape = "Variedad de Cultivo"
  ) +
  theme_economist() +
  scale_color_economist() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )


# Personalización Completa del Gráfico
ggplot(data = datos, aes(x = Fertilizante, y = Rendimiento, shape = Variedad, color = Variedad)) +
  geom_point(size = 3) +
  labs(
    title = "Rendimiento por Fertilizante y Variedad",
    subtitle = "Datos Simulados",
    x = "Tipo de Fertilizante",
    y = "Rendimiento (kg/ha)",
    color = "Variedad de Cultivo",
    shape = "Variedad de Cultivo",
    caption = "Fuente: Simulación de Datos"
  ) +
  theme(
    # Título del gráfico
    plot.title = element_text(
      family = "Arial",        # Tipo de letra
      face = "bold",           # Estilo (bold, italic, bold.italic)
      size = 16,               # Tamaño del texto
      hjust = 0.5              # Alineación horizontal (0 izquierda, 0.5 centro, 1 derecha)
    ),
    # Subtítulo
    plot.subtitle = element_text(
      family = "Arial",
      face = "italic",
      size = 12,
      hjust = 0.5
    ),
    # Etiquetas de los ejes
    axis.title.x = element_text(
      family = "Times New Roman",
      face = "bold.italic",
      size = 12,
      color = "blue"
    ),
    axis.title.y = element_text(
      family = "Times New Roman",
      face = "bold.italic",
      size = 12,
      color = "blue"
    ),
    # Texto de los ejes (ticks)
    axis.text.x = element_text(
      family = "Calibri",
      face = "bold",
      size = 10,
      angle = 0,
      hjust = 0.5,
      vjust = 0.5
    ),
    axis.text.y = element_text(
      family = "Calibri",
      face = "bold",
      size = 10
    ),
    # Título de la leyenda
    legend.title = element_text(
      family = "Arial",
      face = "bold",
      size = 12
    ),
    # Texto de la leyenda
    legend.text = element_text(
      family = "Arial",
      size = 10
    ),
    # Posición de la leyenda
    legend.position = "bottom",  # También puede ser "top", "left", "right", "none"
    legend.box = "horizontal",
    # Fondo del gráfico
    panel.background = element_rect(fill = "white"),  # Fondo blanco
    panel.grid.major = element_line(color = "gray90"),  # Líneas de cuadrícula principales
    panel.grid.minor = element_blank(),  # Eliminar líneas de cuadrícula menores
    # Fondo general del gráfico
    plot.background = element_rect(fill = "white", color = NA),
    # Margen del gráfico
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10)
  )

# Ejemplo Final Combinando Todo
ggplot(data = datos, aes(x = Fertilizante, y = Rendimiento, shape = Variedad, color = Variedad)) +
  geom_point(size = 4) +
  labs(
    title = "Análisis del Rendimiento por Fertilizante y Variedad",
    subtitle = "Experimento en Campo - Temporada 2023",
    x = "Tipo de Fertilizante",
    y = "Rendimiento (kg/ha)",
    color = "Variedad",
    shape = "Variedad",
    caption = "Fuente: Datos Reales del Experimento"
  ) +
  scale_color_manual(
    values = c("V1" = "#1b9e77", "V2" = "#d95f02", "V3" = "#7570b3")
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(face = "italic", size = 14, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 1),
    axis.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 12),
    legend.title = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 10),
    legend.position = "bottom",
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )

# 6.3. Guardar Gráficos

# Puedes guardar tus gráficos utilizando ggsave()

ggsave("rendimiento_fertilizante_variedad.png", width = 8, height = 6)

# -----------------------------
# 7. Gráficos con Datos Reales
# -----------------------------

# 7.1. Importar Datos Reales

# Supongamos que tenemos un archivo CSV con datos reales.

# data_real <- read_csv("datos_agronomia.csv")

# Para este ejemplo, continuaremos usando los datos simulados.

# 7.2. Gráfico de Barras

# Visualizar la contribución de cada variedad al rendimiento total por fertilizante.

datos_resumen <- datos %>%
  group_by(Fertilizante, Variedad) %>%
  summarise(Rendimiento_Total = sum(Rendimiento))

# Gráfico de barras apiladas
ggplot(data = datos_resumen, aes(x = Fertilizante, y = Rendimiento_Total, fill = Variedad)) +
  geom_bar(stat = "identity") +
  labs(title = "Rendimiento Total por Fertilizante y Variedad",
       x = "Fertilizante",
       y = "Rendimiento Total",
       fill = "Variedad") +
  theme_classic()

# Gráfico de barras agrupadas
ggplot(data = datos_resumen, aes(x = Fertilizante, y = Rendimiento_Total, fill = Variedad)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Rendimiento Total por Fertilizante y Variedad",
       x = "Fertilizante",
       y = "Rendimiento Total",
       fill = "Variedad") +
  theme_classic()

# Gráfico de barras apiladas proporcionales
# Calcular el porcentaje de rendimiento por fertilizante
datos_resumen_porcentaje <- datos_resumen %>%
  group_by(Fertilizante) %>%
  mutate(Porcentaje = Rendimiento_Total / sum(Rendimiento_Total) * 100)

ggplot(data = datos_resumen_porcentaje, aes(x = Fertilizante, y = Porcentaje, fill = Variedad)) +
  geom_bar(stat = "identity") +
  labs(title = "Porcentaje de Rendimiento por Fertilizante y Variedad",
       x = "Fertilizante",
       y = "Porcentaje de Rendimiento",
       fill = "Variedad") +
  theme_minimal()

# Gráfico de Barras Apiladas con Etiquetas
ggplot(data = datos_resumen, aes(x = Fertilizante, y = Rendimiento_Total, fill = Variedad)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(Rendimiento_Total, 1)), 
            position = position_stack(vjust = 0.5), 
            color = "white", size = 3) +
  labs(title = "Rendimiento Total por Fertilizante y Variedad (Con Etiquetas)",
       x = "Fertilizante",
       y = "Rendimiento Total",
       fill = "Variedad") +
  theme_classic()

# Gráfico de Barras Agrupadas con Etiquetas
ggplot(data = datos_resumen, aes(x = Fertilizante, y = Rendimiento_Total, fill = Variedad)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = round(Rendimiento_Total, 1)),
            position = position_dodge(0.9), vjust = -0.5, size = 3) +
  labs(title = "Rendimiento Total por Fertilizante y Variedad (Etiquetas Arriba)",
       x = "Fertilizante",
       y = "Rendimiento Total",
       fill = "Variedad") +
  theme_minimal()

# Gráfico de Barras Apiladas Horizontal
ggplot(data = datos_resumen, aes(x = Fertilizante, y = Rendimiento_Total, fill = Variedad)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Rendimiento Total por Fertilizante y Variedad (Barras Horizontales)",
       x = "Rendimiento Total",
       y = "Fertilizante",
       fill = "Variedad") +
  theme_bw()

# Gráfico de barras con barras de error
# Calcular el rendimiento promedio y el error estándar
datos_error <- datos %>%
  group_by(Fertilizante, Variedad) %>%
  summarise(
    Rendimiento_Promedio = mean(Rendimiento),
    Error_Estandar = sd(Rendimiento) / sqrt(n())
  )

ggplot(data = datos_error, aes(x = Fertilizante, y = Rendimiento_Promedio, fill = Variedad)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Rendimiento_Promedio - Error_Estandar, ymax = Rendimiento_Promedio + Error_Estandar),
                width = 0.2, position = position_dodge(0.9)) +
  labs(title = "Rendimiento Promedio por Fertilizante y Variedad (Con Barras de Error)",
       x = "Fertilizante",
       y = "Rendimiento Promedio",
       fill = "Variedad") +
  theme_minimal()

# Gráfico de Barras Apiladas con Facetas por Variedad
ggplot(data = datos_resumen, aes(x = Fertilizante, y = Rendimiento_Total, fill = Fertilizante)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ Variedad) +
  labs(title = "Rendimiento Total por Fertilizante y Variedad (Facetas por Variedad)",
       x = "Fertilizante",
       y = "Rendimiento Total") +
  theme_light()

# Gráfico de Barras Apiladas con Orden Personalizado
# Ordenar las variedades según el rendimiento total
datos_resumen$Variedad <- factor(datos_resumen$Variedad, levels = c("V3", "V2", "V1"))

ggplot(data = datos_resumen, aes(x = Fertilizante, y = Rendimiento_Total, fill = Variedad)) +
  geom_bar(stat = "identity") +
  labs(title = "Rendimiento Total por Fertilizante y Variedad (Orden Personalizado)",
       x = "Fertilizante",
       y = "Rendimiento Total",
       fill = "Variedad") +
  theme_classic()

# Instalación y carga del paquete ggpattern
# install.packages("ggpattern")
library(ggpattern)

ggplot(data = datos_resumen, aes(x = Fertilizante, y = Rendimiento_Total, fill = Variedad, pattern = Variedad)) +
  geom_bar_pattern(stat = "identity",
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.02) +
  labs(title = "Rendimiento Total por Fertilizante y Variedad (Con Patrones)",
       x = "Fertilizante",
       y = "Rendimiento Total",
       fill = "Variedad",
       pattern = "Variedad") +
  theme_classic()


# -----------------------------
# 8. Ejemplos Adicionales en Agronomía
# -----------------------------

# 8.1. Gráfico de Línea para Crecimiento de Plantas

# Simular datos de crecimiento en función del tiempo.

datos_crecimiento <- data.frame(
  Dias = rep(1:30, times = 3),
  Altura = c(
    5 + 0.8 * (1:30) + rnorm(30, sd = 1),
    5 + 0.6 * (1:30) + rnorm(30, sd = 1),
    5 + 0.9 * (1:30) + rnorm(30, sd = 1)
  ),
  Variedad = rep(c("V1", "V2", "V3"), each = 30)
)

# Gráfico de crecimiento por variedad

ggplot(data = datos_crecimiento, aes(x = Dias, y = Altura, color = Variedad)) +
  geom_line(size = 1) +
  labs(title = "Crecimiento de Plantas por Variedad",
       x = "Días",
       y = "Altura (cm)")

# 8.2. Gráfico de Superficie (3D) - Necesita paquetes adicionales

# Si deseas crear gráficos 3D, puedes utilizar paquetes como plotly o rgl.
# Aquí mostramos un ejemplo simple usando ggplot2.

# Simular datos de respuesta de rendimiento a dosis de fertilizante y agua

datos_superficie <- expand.grid(
  Dosis_Fertilizante = seq(0, 100, by = 10),
  Dosis_Agua = seq(0, 200, by = 20)
)

datos_superficie <- datos_superficie %>%
  mutate(
    Rendimiento = 50 + 0.4 * Dosis_Fertilizante + 0.3 * Dosis_Agua - 0.002 * Dosis_Fertilizante * Dosis_Agua + rnorm(n(), sd = 5)
  )

# Gráfico de contorno (Contour Plot)

ggplot(data = datos_superficie, aes(x = Dosis_Fertilizante, y = Dosis_Agua, z = Rendimiento)) +
  geom_contour_filled() +
  labs(title = "Respuesta del Rendimiento a Dosis de Fertilizante y Agua",
       x = "Dosis de Fertilizante (kg/ha)",
       y = "Dosis de Agua (mm)",
       fill = "Rendimiento")

# -----------------------------
# 9. Conclusiones y Recomendaciones
# -----------------------------

# - ggplot2 es una herramienta poderosa para visualizar datos de manera efectiva.
# - La comprensión de la gramática de los gráficos te permitirá crear visualizaciones personalizadas.
# - Siempre explora y visualiza tus datos antes de realizar análisis estadísticos.
# - Personaliza tus gráficos para mejorar la comunicación de tus resultados.

# -----------------------------
# 10. Recursos Adicionales
# -----------------------------

# - Documentación oficial de ggplot2: https://ggplot2.tidyverse.org/
# - Libro "ggplot2: Elegant Graphics for Data Analysis" por Hadley Wickham
# - Tutoriales y ejemplos en línea:
#   - https://r4ds.had.co.nz/data-visualisation.html
#   - https://www.r-graph-gallery.com/
