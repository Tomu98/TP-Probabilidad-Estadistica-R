# ==============================================================================
# TRABAJO PRÁCTICO INTEGRADOR - PROBABILIDAD Y ESTADÍSTICA
# Alumno: Romero, Abel Tomás
# Comisión: 5
# ==============================================================================




# ----- Preparativos para el análisis -----

# Limpiar entorno de trabajo
rm(list = ls())

# Cargar librerías necesarias
if (!require(readxl)) install.packages("readxl")
if (!require(ggplot2)) install.packages("ggplot2")
library(readxl)
library(ggplot2)

# Seleccionar archivo y cargar datos
archivo <- file.choose()
datos <- read_excel(archivo)




# ==============================================================================
# CONSIGNA 8: Suponiendo que los datos corresponden a una población, a través
# de un Muestreo Aleatorio Simple, seleccione 6 muestras de 20 estudiantes y
# calcule para cada una de ellas el peso promedio.
# ==============================================================================

# ----- 1. Definir la población -----

# Extraer la variable peso de la población (sin valores NA)
poblacion <- datos$`PESO KG.`[!is.na(datos$`PESO KG.`)]

# Calcular parámetros poblacionales
tamanio_poblacion <- length(poblacion)
media_poblacional <- mean(poblacion)
desvio_poblacional <- sd(poblacion)

# Imprimir resultados poblacionales
cat("\nAnálisis Poblacional:\n")
cat("- Tamaño de la población (N):", tamanio_poblacion, "\n")
cat("- Media poblacional (μ):", round(media_poblacional, 2), "kg\n")
cat("- Desvío estándar poblacional (σ):", round(desvio_poblacional, 2), "kg\n\n")



# ----- 2. Configurar el muestreo -----

# Fijar semilla para reproducibilidad
set.seed(123)

# Definir parámetros del muestreo
cantidad_muestras <- 6
tamanio_muestra <- 20

# Imprimir parámetros del muestreo
cat("\nParámetros del Muestreo:\n")
cat("- Cantidad de muestras:", cantidad_muestras, "\n")
cat("- Tamaño de cada muestra (n):", tamanio_muestra, "\n\n")



# ----- 3. Extraer las 6 muestras aleatorias simples -----

# Guardar todo en una lista
lista_muestras <- list()
medias_muestrales <- numeric(cantidad_muestras)

cat("\nExtracción de Muestras:\n")

for (i in 1:cantidad_muestras) {
  # Muestreo aleatorio simple SIN reposición
  muestra_i <- sample(poblacion, tamanio_muestra, replace = FALSE)
  
  # Guardar muestra y calcular su media
  lista_muestras[[i]] <- muestra_i
  medias_muestrales[i] <- mean(muestra_i)
  
  cat("Muestra", i, "- Media:", round(medias_muestrales[i], 4), "kg\n")
}



# ----- 4. Construir tabla resumen -----

# Construcción de la tabla
resultado <- data.frame(
  Muestra = paste0("Muestra_", 1:cantidad_muestras),
  Media_Muestral = round(medias_muestrales, 2),
  Diferencia_vs_Poblacion = round(medias_muestrales - media_poblacional, 2),
  Diferencia_Porcentual = round(((medias_muestrales - media_poblacional) / media_poblacional) * 100, 2)
)

# Imprimir la tabla
cat("\nTabla Resumen de Resultados:\n")
print(resultado)



# ----- 5. Estadísticas descriptivas de las medias muestrales -----

# Calcular las medias muestrales
media_de_medias <- mean(medias_muestrales)
desvio_de_medias <- sd(medias_muestrales)
min_media <- min(medias_muestrales)
max_media <- max(medias_muestrales)
rango_medias <- max_media - min_media

# Calcular la diferencia entre media de medias y parámetro poblacional
diferencia_media_medias <- media_poblacional - media_de_medias

# Imprimir resultados
cat("\nEstadísticas de las Medias Muestrales:\n")
cat("- Media de las medias muestrales:", round(media_de_medias, 2), "kg\n")
cat("- Diferencia vs media poblacional:", round(diferencia_media_medias, 2), "kg\n")
cat("- Desvío estándar de las medias:", round(desvio_de_medias, 2), "kg\n")
cat("- Media mínima observada:", round(min_media, 2), "kg\n")
cat("- Media máxima observada:", round(max_media, 2), "kg\n")
cat("- Rango de variación:", round(rango_medias, 2), "kg\n")



# ----- 6. Error estándar teórico vs observado -----

# Calculamos errores
error_estandar_teorico <- desvio_poblacional / sqrt(tamanio_muestra)
error_estandar_observado <- desvio_de_medias

# Imprimir resultados
cat("\nComparación Error Estándar:\n")
cat("- Error estándar teórico (σ/√n):", round(error_estandar_teorico, 2), "kg\n")
cat("- Error estándar observado:", round(error_estandar_observado, 2), "kg\n")
cat("- Diferencia:", round(abs(error_estandar_teorico - error_estandar_observado), 2), "kg\n")



# ----- 7. Análisis de proximidad al parámetro -----

# Calculamos la proximidad
diferencias_absolutas <- abs(medias_muestrales - media_poblacional)
muestra_mas_cercana <- which.min(diferencias_absolutas)
muestra_mas_lejana <- which.max(diferencias_absolutas)

# Imprimir resultados
cat("Análisis de Proximidad:\n")
cat("- Muestra más cercana al parámetro:", muestra_mas_cercana, 
    "- diferencia:", round(diferencias_absolutas[muestra_mas_cercana], 2), "kg\n")
cat("- Muestra más alejada del parámetro:", muestra_mas_lejana, 
    "- diferencia:", round(diferencias_absolutas[muestra_mas_lejana], 2), "kg\n")



# ----- 8. Visualización gráfica -----

# GRÁFICO 1: Medias Muestrales vs Media Poblacional
cat("Generando Gráfico 1 con recuadro de media poblacional...\n")

resultado$Etiqueta <- paste0("M", 1:cantidad_muestras)

grafico1 <- ggplot(resultado, aes(x = Etiqueta, y = Media_Muestral)) +
  geom_col(
    fill = "steelblue", color = "black", width = 0.7
  ) +
  geom_hline(
    yintercept = media_poblacional,
    color = "red", linewidth = 1, linetype = "dashed"
  ) +
  geom_text(
    aes(label = round(Media_Muestral, 2)),
    vjust = -0.5, size = 4
  ) +
  annotate(
    "label",
    x = Inf, y = Inf,
    label = paste("--- μ =", round(media_poblacional, 2), "kg"),
    hjust = 1.1, vjust = 1.5,
    fill = "white", color = "red", fontface = "bold", size = 4,
    label.size = 0.8,
  ) +
  labs(
    title = "Medias Muestrales vs Media Poblacional",
    x = "Muestras",
    y = "Peso promedio (kg)"
  ) +
  coord_cartesian(ylim = c(min(poblacion) - 2, max(medias_muestrales) + 5)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

print(grafico1)



# GRÁFICO 2: Boxplot de todas las muestras comparadas
cat("Generando Gráfico 2: Boxplot de las 6 muestras...\n")

datos_largo <- data.frame(
  Peso = unlist(lista_muestras),
  Muestra = rep(paste0("M", 1:cantidad_muestras), each = tamanio_muestra)
)

lim_inf <- quantile(poblacion, 0.01)
lim_sup <- quantile(poblacion, 1)

grafico2 <- ggplot(datos_largo, aes(x = Muestra, y = Peso)) +
  geom_boxplot(
    fill = "lightblue", color = "darkblue", 
    outlier.color = "red", outlier.shape = 16, outlier.size = 2.5
  ) +
  geom_hline(
    yintercept = media_poblacional, 
    color = "red", linewidth = 1, linetype = "dashed"
  ) +
  annotate(
    "label",
    x = Inf, y = Inf,
    label = paste("--- μ =", round(media_poblacional, 2), "kg"),
    hjust = 1.1, vjust = 1.5,
    fill = "white", color = "red", fontface = "bold", size = 4,
    label.size = 0.8
  ) +
  labs(
    title = "Distribución de las 6 Muestras",
    x = "Muestras",
    y = "Peso (kg)"
  ) +
  coord_cartesian(ylim = c(lim_inf, lim_sup)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )

print(grafico2)



