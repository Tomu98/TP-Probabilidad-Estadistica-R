# ==============================================================================
# TRABAJO PRÁCTICO INTEGRADOR - PROBABILIDAD Y ESTADÍSTICA
# Alumno: Romero, Abel Tomás
# Comisión: 5
# ==============================================================================




# ----- Preparativos para el análisis -----

# Limpiar entorno de trabajo
rm(list = ls())

# Cargar librería necesaria
library(readxl)

# Seleccionar archivo y cargar datos
archivo <- file.choose()
datos <- read_excel(archivo)




# ==============================================================================
# CONSIGNA 5: Suponiendo que los datos corresponden toda la población y
# son seleccionados 16 estudiantes, calcular las siguientes probabilidades.
# ==============================================================================

# ----- Extracción de datos desde el archivo -----

# Factor ordenado para niveles de satisfacción
datos$`Nivel de satisfacción` <- factor(
  datos$`SATISFACCIÓN CON LA CARRERA`,
  levels = c(1,2,3,4),
  labels = c("Muy satisfecho","Satisfecho","Insatisfecho","Muy insatisfecho"),
  ordered = TRUE
)

# Variable temporal para evitar repetición
satisfaccion <- datos$`Nivel de satisfacción`

# Extraer cantidades de cada categoría
muy_satisfecho <- sum(satisfaccion == "Muy satisfecho", na.rm = TRUE)     # 121
satisfecho <- sum(satisfaccion == "Satisfecho", na.rm = TRUE)             # 66
insatisfecho <- sum(satisfaccion == "Insatisfecho", na.rm = TRUE)         # 14
muy_insatisfecho <- sum(satisfaccion == "Muy insatisfecho", na.rm = TRUE) # 13
total_estudiantes <- nrow(datos)    # 214

# Parámetro n: Número de estudiantes seleccionados
n <- 16

# Cálculo de probabilidades poblacionales
p_muy_satisfecho <- muy_satisfecho / total_estudiantes
p_satisfecho <- satisfecho / total_estudiantes
p_insatisfecho <- insatisfecho / total_estudiantes
p_muy_insatisfecho <- muy_insatisfecho / total_estudiantes

# Imprimir resultados de las probabilidades poblacionales
cat("\nProbabilidades poblacionales:\n")
cat("P(Muy satisfecho):", round(p_muy_satisfecho, 4), "\n")        # 0.5654
cat("P(Satisfecho):", round(p_satisfecho, 4), "\n")                # 0.3084
cat("P(Insatisfecho):", round(p_insatisfecho, 4), "\n")            # 0.0654
cat("P(Muy insatisfecho):", round(p_muy_insatisfecho, 4), "\n")    # 0.0607



# ==============================================================================
# 5.a) Más de 9 estudiantes estén muy satisfechos con la carrera.
# ==============================================================================

cat("\n- a) P(X > 9) con X ~ Binomial(16, p_muy_satisfecho)\n")
prob_5a <- 1 - pbinom(9, size = n, prob = p_muy_satisfecho)
cat("P(X > 9) =", round(prob_5a, 4), "\n")    # 0.4143  



# ==============================================================================
# 5.b) Entre 4 y 8 estudiantes estén satisfechos con la carrera.
# ==============================================================================

cat("\n- b) P(4 ≤ X ≤ 8) con X ~ Binomial(16, p_satisfecho)\n")
prob_5b <- pbinom(8, size = n, prob = p_satisfecho) - pbinom(3, size = n, prob = p_satisfecho)
cat("P(4 ≤ X ≤ 8) =", round(prob_5b, 4), "\n")    # 0.7456



# ==============================================================================
# 5.c) Menos de 5 estudiantes estén insatisfechos con la carrera.
# ==============================================================================

cat("\n- c) P(X < 5) con X ~ Binomial(16, p_insatisfecho)\n")
prob_5c <- pbinom(4, size = n, prob = p_insatisfecho)
cat("P(X < 5) =", round(prob_5c, 4), "\n")    # 0.9972



# ==============================================================================
# 5.d) Exactamente 10 estudiantes estén muy insatisfechos con la carrera.
# ==============================================================================

cat("\n- d) P(X = 10) con X ~ Binomial(16, p_muy_insatisfecho)\n")
prob_5d <- dbinom(10, size = n, prob = p_muy_insatisfecho)
cat("P(X = 10) =", round(prob_5d, 4), "\n")    # 0.000




# ==============================================================================
# CONSIGNA 6: En el horario de consultas de cierta materia se reciben en 
# promedio 15 consultas en media hora. Calcular las siguientes probabilidades.
# ==============================================================================

# ----- Dato inicial -----

# Tasa promedio en 30 minutos
tasa_promedio <- 15



# ==============================================================================
# 6.a) Que lleguen por lo menos 6 consultas en 20 minutos.
# ==============================================================================

cat("\n- a) P(X ≥ 6) en 20 minutos\n")
lambda_20min <- tasa_promedio * (20/30)       # Ajuste de lambda para 20 minutos
cat("Lambda (20 min):", lambda_20min, "\n")
prob_6a <- 1 - ppois(5, lambda = lambda_20min)
cat("P(X ≥ 6) =", round(prob_6a, 4), "\n")    # 0.9329



# ==============================================================================
# 6.b) Que lleguen a lo sumo 12 consultas en 40 minutos.
# ==============================================================================

cat("\n- b) P(X ≤ 12) en 40 minutos\n")
lambda_40min <- tasa_promedio * (40/30)       # Ajuste de lambda para 40 minutos
cat("Lambda (40 min):", lambda_40min, "\n")
prob_6b <- ppois(12, lambda = lambda_40min)
cat("P(X ≤ 12) =", round(prob_6b, 4), "\n")   # 0.0390



# ==============================================================================
# 6.c) Más de 7 y menos de 10 consultas en 30 minutos.
# ==============================================================================

cat("\n- c) P(7 < X < 10) en 30 minutos\n")
lambda_30min <- tasa_promedio    # Lambda base
cat("Lambda (30 min):", lambda_30min, "\n")
prob_6c <- ppois(9, lambda = lambda_30min) - ppois(7, lambda = lambda_30min)
cat("P(7 < X < 10) =", round(prob_6c, 4), "\n")    # 0.0519




# ==============================================================================
# CONSIGNA 7: Utilizando el modelo normal:
# ==============================================================================

# ----- Cálculo de parámetros desde los datos -----

# Calcular media y desviación estándar de la estatura
media_estatura <- mean(datos$`ESTATURA CM.`, na.rm = TRUE)
sd_estatura <- sd(datos$`ESTATURA CM.`, na.rm = TRUE)

cat("\nParámetros de la distribución:\n")
cat("Media de estatura:", round(media_estatura, 2), "cm\n")    # 160.55cm
cat("Desviación estándar:", round(sd_estatura, 2), "cm\n")     # 9.48cm



# ==============================================================================
# 7.a) Calcular la probabilidad de que un estudiante seleccionado
#      aleatoriamente tenga una estatura mayor o igual que 179 cm.
# ==============================================================================

cat("\n- a) P(X ≥ 179)\n")
prob_7a <- 1 - pnorm(179, mean = media_estatura, sd = sd_estatura)
cat("P(X ≥ 179) =", round(prob_7a, 4), "\n")  # 0.0259



# ==============================================================================
# 7.b) Calcular la probabilidad de que un estudiante seleccionado
#      aleatoriamente tenga una estatura comprendida entre 147 cm. y 172 cm.
# ==============================================================================

cat("\n- b) P(147 ≤ X ≤ 172)\n")
prob_7b <- pnorm(172, mean = media_estatura, sd = sd_estatura) - 
           pnorm(147, mean = media_estatura, sd = sd_estatura)
cat("P(147 ≤ X ≤ 172) =", round(prob_7b, 4), "\n")    # 0.8099



# ==============================================================================
# 7.c) Hallar el valor que excede al 97,5% de las estaturas.
# ==============================================================================

cat("\n- c) Percentil 97.5\n")
percentil_975 <- qnorm(0.975, mean = media_estatura, sd = sd_estatura)
cat("Valor que excede al 97.5%:", round(percentil_975, 2), "cm\n")  # 179.14cm



