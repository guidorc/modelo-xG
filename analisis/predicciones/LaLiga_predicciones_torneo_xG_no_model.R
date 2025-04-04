# Dependencias
library(jsonlite)
library(readxl)

rm(list = ls())
setwd("~/facultad/tesis")

# Carga de datos
dta <- read.csv("datos/la_liga/la_liga_ultimas_temporadas.csv")

fecha_modelo <- "2022-08-12"
base <- dta[dta$Fecha < fecha_modelo, ] # Base de conocimiento
temporada <- dta[dta$Fecha >= fecha_modelo, ] # Temporada a analizar
equipos <- unique(c(as.character(dta$Local), as.character(dta$Visitante))) # Todos los equipos del dataset

# Agregamos columnas para las probabilidades al data frame
temporada$Prob_Local <- NA
temporada$Prob_Empate  <- NA
temporada$Prob_Visitante <- NA

# ----- Probabilidad de Poisson en rango ----- #
probaPoissonRango <- function (lambda) {
  scaled_lambda <- lambda * 10
  buckets <- c(0, 5, 15, 25, 35, 45, 55)
  sapply(buckets, function(i) {
    pmin <- i
    pmax <- i + 1
    # Probabilidad de caer en el rango [i, i+1]
    pbucket <- ppois(pmax, scaled_lambda) - ppois(pmin, scaled_lambda)
    return(pbucket)
  })
}

# ----- Probabilidad de cada resultado para el partido X_ij, Y_ij ----- #
probaPartido <- function(xG_local, xG_visitante) {
  # Discretizar los xG de cada equipo en buckets: [0, 5), [5, 15), ..., [45, 55)
  probaLocal <- probaPoissonRango(xG_local)
  probaVisitante <- probaPoissonRango(xG_visitante)
  
  # Matriz de probabilidades conjuntas
  probaMatrix <- outer(probaLocal, probaVisitante, `*`)
  
  # Normalizar las probabilidades
  probaMatrix <- probaMatrix / sum(probaMatrix)
  
  return(probaMatrix)
}

# ===== Probabilidad de cada resultado (V, E, D) ===== #
probabilidad_resultados <- function(mat) {
  # Initialize sums
  diag_sum <- 0
  lower_sum <- 0
  upper_sum <- 0
  
  n <- nrow(mat)
  for (i in 1:n) {
    for (j in 1:n) {
      if (i == j) {
        diag_sum <- diag_sum + mat[i, j]  # Empate
      } else if (i > j) {
        lower_sum <- lower_sum + mat[i, j]  # Victoria Local
      } else if (i < j) {
        upper_sum <- upper_sum + mat[i, j]  # Victoria Visitante
      }
    }
  }
  
  return(c(V = lower_sum, E = diag_sum, D = upper_sum))
}

# Iteramos cada partido de la temporada
for (i in 1:nrow(temporada)) {
  partido <- temporada[i, ]
  print(sprintf("partido %s: %s - %s", i, partido$Local, partido$Visitante))
  
  # Calcular probabilidad de cada resultado en base a los xG
  prob <- probaPartido(partido$xG_local, partido$xG_visitante)
  resultados <- probabilidad_resultados(prob)
  
  # Agregar datos al resultado
  temporada$Prob_Local[i] <- resultados[1]
  temporada$Prob_Empate[i]  <- resultados[2]
  temporada$Prob_Visitante[i] <- resultados[3]
}

write.csv(temporada, file = "resultados/predicciones/resultados_temporada_xG_no_model.csv", row.names = FALSE)