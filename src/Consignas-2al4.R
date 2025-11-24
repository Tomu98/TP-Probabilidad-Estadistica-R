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
# CONSIGNA 2: Construir la/s Tabla/s de Frecuencias y calcular todas
# las frecuencias de las siguientes variables: "Tiempo en horas semanales
# dedicadas al estudio" y "Nivel de satisfacción con la Carrera"
# ==============================================================================

# ==============================================================================
# 2.a) Tabla de la variable "Tiempo semanal en horas dedicadas al estudio"
# ==============================================================================

variable_tiempo <- "TIEMPO SEMANAL en HS. DEDIC. EST."

# Parámetros de Sturges
n <- length(datos[[variable_tiempo]])   # n (número de elementos): 214
c <- ceiling(1 + 3.322 * log10(n))      # c (número de clases): 9

# Amplitud, intervalos y clases
rango <- range(datos[[variable_tiempo]])                                     # Min: 5, Max: 22
amplitud <- ceiling((rango[2] - rango[1]) / c)                               # Amplitud: 2
breaks <- seq(floor(rango[1]), ceiling(rango[2]) + amplitud, by = amplitud)  # Cortes: 5,7,9,11,13,15,17,19,21,23
clases_a <- cut(datos[[variable_tiempo]], breaks = breaks, right = FALSE)    # Intervalos definidos

# Frecuencias
tabla_a <- table(clases_a)         # Frecuencia absoluta
f_abs_acum_a <- cumsum(tabla_a)    # Frecuencia absoluta acumulada
f_rel_a <- prop.table(tabla_a)     # Frecuencia relativa
f_rel_acum_a <- cumsum(f_rel_a)    # Frecuencia relativa acumulada

# Tabla A final
tabla_a_final <- data.frame(
  Intervalos = levels(clases_a),
  Frec_Absoluta = as.vector(tabla_a),
  Frec_Abs_Acumulada = as.vector(f_abs_acum_a),
  Frec_Relativa = round(as.vector(f_rel_a), 4),
  Frec_Rel_Acumulada = round(as.vector(f_rel_acum_a), 4)
)

# Imprimir la tabla A en consola
cat("\n")
cat("=======================================================================\n")
cat("TABLA DE FRECUENCIAS - TIEMPO SEMANAL EN HORAS DEDICADAS AL ESTUDIO\n")
cat("=======================================================================\n")
print(tabla_a_final)



# ==============================================================================
# 2.b) Tabla de la variable "Nivel de satisfacción con la Carrera"
# ==============================================================================

variable_ordinal <- "SATISFACCIÓN CON LA CARRERA"

# Factor ordenado
datos[[variable_ordinal]] <- factor(
  datos$`SATISFACCIÓN CON LA CARRERA`,
  levels = c(1,2,3,4),
  labels = c("Muy satisfecho","Satisfecho","Insatisfecho","Muy insatisfecho"),
  ordered = TRUE
)

# Frecuencias
tabla_b <- table(datos[[variable_ordinal]])    # Frecuencia absoluta
f_abs_acum_b <- cumsum(tabla_b)                # Frecuencia absoluta acumulada
f_rel_b <- prop.table(tabla_b)                 # Frecuencia relativa
f_rel_acum_b <- cumsum(f_rel_b)                # Frecuencia relativa acumulada

# Tabla B final
tabla_b_final <- data.frame(
  Categoria = names(tabla_b),
  Frec_Absoluta = as.vector(tabla_b),
  Frec_Abs_Acumulada = as.vector(f_abs_acum_b),
  Frec_Relativa = round(as.vector(f_rel_b), 4),
  Frec_Rel_Acumulada = round(as.vector(f_rel_acum_b), 4)
)

# Imprimir la tabla B en consola
cat("\n")
cat("=======================================================================\n")
cat("TABLA DE FRECUENCIAS - NIVEL DE SATISFACCIÓN CON LA CARRERA\n")
cat("=======================================================================\n")
print(tabla_b_final)




# ==============================================================================
# CONSIGNA 3: Calcular medidas descriptivas de tendencia central,
# posición y dispersión para las variables definidas en el punto 2.
# ==============================================================================

# ==============================================================================
# 3.a) Variable: "Tiempo semanal en horas dedicadas al estudio"
# ==============================================================================

# Marcas de clase
marca_clase <- (head(breaks, -1) + tail(breaks, -1)) / 2
frecuencias <- as.vector(tabla_a)
n_total <- sum(frecuencias)

# Media
media_continua <- sum(marca_clase * frecuencias) / n_total

# Moda
i_modal <- which.max(frecuencias)
li_modal <- breaks[i_modal]
f_m <- frecuencias[i_modal]
f_1 <- ifelse(i_modal == 1, 0, frecuencias[i_modal - 1])
f_2 <- ifelse(i_modal == length(frecuencias), 0, frecuencias[i_modal + 1])
moda_continua <- li_modal + ((f_m - f_1) / ((f_m - f_1) + (f_m - f_2))) * amplitud

# Mediana
n_2 <- n_total / 2
f_acum <- cumsum(frecuencias)
clase_mediana_index <- which(f_acum >= n_2)[1]
L_mediana <- breaks[clase_mediana_index]
F_anterior <- ifelse(clase_mediana_index == 1, 0, f_acum[clase_mediana_index - 1])
f_mediana <- frecuencias[clase_mediana_index]
mediana_continua <- L_mediana + ((n_2 - F_anterior) / f_mediana) * amplitud

# Varianza, desvío y coeficiente de variación
varianza_continua <- sum(frecuencias * (marca_clase - media_continua)^2) / (n_total - 1)
desvio_continua <- sqrt(varianza_continua)
coef_var_continua <- (desvio_continua / media_continua) * 100

# Tabla de resultados
continua_stats <- data.frame(
  Media = round(media_continua, 4),
  Moda = round(moda_continua, 4),
  Mediana = round(mediana_continua, 4),
  Varianza = round(varianza_continua, 4),
  Desvio_Estandar = round(desvio_continua, 4),
  Coef_Variacion_pct = round(coef_var_continua, 4)
)

# Imprimir resultados
cat("\n")
cat("=======================================================================\n")
cat("MEDIDAS DESCRIPTIVAS - TIEMPO SEMANAL EN HORAS DE ESTUDIO\n")
cat("=======================================================================\n")
print(continua_stats, row.names = FALSE)



# ==============================================================================
# 3.b) Variable: "Nivel de satisfacción con la Carrera"
# ==============================================================================

# Moda
moda_ordinal <- names(tabla_b[tabla_b == max(tabla_b)])

# Mediana
valores_ordinal <- as.numeric(datos[[variable_ordinal]])  # Convertimos a numeric para cálculos
mediana_ordinal <- median(valores_ordinal, na.rm = TRUE)

# Cuartiles
cuartiles_ordinal <- quantile(valores_ordinal, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# Tabla de resultados
ordinal_stats <- data.frame(
  Moda = moda_ordinal,
  Mediana = mediana_ordinal,
  Q1 = cuartiles_ordinal[1],
  Q2 = cuartiles_ordinal[2],
  Q3 = cuartiles_ordinal[3]
)

# Imprimir resultados
cat("\n")
cat("=======================================================================\n")
cat("MEDIDAS DESCRIPTIVAS - NIVEL DE SATISFACCIÓN CON LA CARRERA\n")
cat("=======================================================================\n")
print(ordinal_stats, row.names = FALSE)




# ==============================================================================
# CONSIGNA 4: Representar gráficamente las variables definidas 
# en el punto 2 y realizar el correspondiente análisis
# ==============================================================================

# ==============================================================================
# 4.a) Elegir una frecuencia (Absoluta o Relativa) para la variable "Tiempo en 
#      horas semanales dedicadas al estudio" y construir un Histograma.
# ==============================================================================

# Histograma con ggplot2
ggplot(datos, aes(x = .data[[variable_tiempo]])) +
  geom_histogram(breaks = breaks,
                 fill = "steelblue",
                 color = "white",
                 alpha = 0.8,
                 closed = "left") +
  labs(title = "Distribución del Tiempo Semanal Dedicado al Estudio",
       subtitle = "Histograma de frecuencias absolutas",
       x = "Horas semanales",
       y = "Número de estudiantes",
       caption = "Fuente: Encuesta a estudiantes de Tecnicatura en Programación - Universidad INNOVA XXII") +
  scale_x_continuous(
    breaks = seq(5, 23, by = 2),
    limits = c(5, 23)
  ) +
  scale_y_continuous(
    breaks = seq(0, 60, by = 5),
    minor_breaks = seq(0, 60, by = 1),
    expand = c(0, 0, 0.1, 0)
  ) +
  theme_minimal() +
  theme(
    # FONDO Y BORDES
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 12, margin = margin(b = 15)),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0.5),
    # EJES Y ETIQUETAS
    axis.title.x = element_text(face = "bold", size = 12, margin = margin(t = 10)),
    axis.title.y = element_text(face = "bold", size = 12, margin = margin(r = 10)),
    axis.text = element_text(size = 10),
    # GRILLA PERSONALIZADA
    panel.grid.major.x = element_line(color = "gray75", size = 0.4, linetype = "solid"),
    panel.grid.minor.x = element_line(color = "gray85", size = 0.2, linetype = "dotted"),
    panel.grid.major.y = element_line(color = "gray75", size = 0.4, linetype = "solid"),
    panel.grid.minor.y = element_line(color = "gray85", size = 0.2, linetype = "dotted"),
    # FONDO
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )



# ==============================================================================
# 4.b) Construir un Diagrama Circular que represente porcentualmente
#      el "Nivel de satisfacción con la carrera".
# ==============================================================================

# Asegurar que la columna de categorías sea factor ordenado
tabla_b_final$Categoria <- factor(
  tabla_b_final$Categoria,
  levels = c("Muy satisfecho","Satisfecho","Insatisfecho","Muy insatisfecho"),
  ordered = TRUE
)

# Gráfico circular con ggplot2
ggplot(tabla_b_final, aes(x = "", y = Frec_Relativa, fill = Categoria)) +
  geom_bar(stat = "identity", width = 1, color = "black", linewidth = 0.6) +
  coord_polar("y") +
  labs(title = "Nivel de satisfacción con la carrera",
       subtitle = "Distribución porcentual de estudiantes",
       fill = "Categorías",
       caption = "Fuente: Encuesta a estudiantes de Tecnicatura en Programación - Universidad INNOVA XXII") +
  scale_fill_manual(values = c("darkcyan", "yellowgreen", "orange", "red")) +
  theme_void() +
  theme(
    # TÍTULOS Y SUBTÍTULOS
    plot.title = element_text(hjust = 0.5, vjust = -3.6, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, vjust = -4.5, size = 12),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0.5, vjust = 8),
    # LEYENDA
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    # FONDO
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  geom_text(
    aes(label = paste0(round(Frec_Relativa * 100, 2), "%")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3.5,
    fontface = "bold"
  )



