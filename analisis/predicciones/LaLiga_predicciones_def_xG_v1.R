# Dependencias
library(jsonlite)
library(readxl)

rm(list = ls())
setwd("~/facultad/tesis")

# Carga de datos
dta <- read.csv("datos/la_liga/la_liga_ultimas_temporadas.csv")
equipos <- (read.csv("datos/la_liga/equipos.csv", sep = "\t"))$equipos

# Entrenamos el modelo con los resultados hasta 12/05/2023 (Fecha Barcelona campeón)
dta <- dta[dta$Fecha < "2023-05-12", ]
fechaModelo <- "2023-05-14"

# ----- Dataframe del modelo ----- #
dta.indep <- data.frame(
  Equipo = as.factor(c(as.character(dta$Local), as.character(dta$Visitante))),
  Rival = as.factor(c(as.character(dta$Visitante), as.character(dta$Local))),
  PlusLocal = as.factor(c(paste("AT", as.character(dta$Local)), paste("DF", as.character(dta$Local)))),
  xG = c(as.double(dta$xG_local), as.double(dta$xG_visitante)),
  Fecha = as.factor(c(as.integer(gsub("-", "", format(as.Date(dta$Fecha)))), as.integer(gsub("-", "", format(as.Date(dta$Fecha)))))),
  DifFecha = c(as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks")), as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks"))),
  Fade = exp(-0.0065 * (c(as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks")), as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks"))))) # Ajuste por actualidad
)

# ----- MODELO ----- #
# Calculamos el EMV. Donde la probabilidad de que A tenga un xG de n frente B es una poisson
# Cuyo parametro depende de ambos equipos y quien fue local
modelo <- glm(xG ~ 0 + Equipo + Rival + PlusLocal, data = dta.indep, family = poisson(), weights = dta.indep$Fade) # Calibración

# ----- Probabilidad de Poisson en rango ----- #
probaPoissonRango <- function (rango, lambda) {
  sapply(rango, function(i) {
    pmin <- i
    pmax <- i + 1
    # Probabilidad de caer en el rango [i, i+1]
    pbucket <- ppois(pmax, lambda) - ppois(pmin, lambda)
    return(pbucket)
  })
}

# ----- Probabilidad de cada xG para el partido X_ij, Y_ij ----- #
probaPartido <- function(equipoLocal, equipoVisitante) {
  # Calcular media de xG para el equipo local
  lambda <- predict(
    modelo, 
    data.frame(
      Equipo = equipoLocal, 
      Rival = equipoVisitante, 
      PlusLocal = paste("AT", equipoLocal)
    ), 
    type = "response"
  )
  
  # Calcular media de xG para el equipo visitante
  mu <- predict(
    modelo, 
    data.frame(
      Equipo = equipoVisitante, 
      Rival = equipoLocal, 
      PlusLocal = paste("DF", equipoLocal)
    ), 
    type = "response"
  )
  
  # Discretizar los xG de cada equipo en buckets: [0, 1), [1, 2), ..., [4, 5)
  probaLocal <- probaPoissonRango(0:4, lambda)
  probaVisitante <- probaPoissonRango(0:4, mu)
  
  # Matriz de probabilidades conjuntas
  probaMatrix <- outer(probaLocal, probaVisitante, `*`)
  
  # Normalizar las probabilidades
  probaMatrix <- probaMatrix / sum(probaMatrix)
  
  return(probaMatrix)
}


# ----- Probabilidad de cada resultado para el partido X_ij, Y_ij, considerando la capacidad defensiva del rival ----- #
probaDoblePartido <- function(equipoLocal, equipoVisitante) {
  # Calcular media de xG para el equipo local
  lambda1 <- predict(modelo, data.frame(Equipo = equipoLocal, Rival = equipoVisitante, PlusLocal = paste("AT", equipoLocal)), type = "response")
  lambda2 <- predict(modelo, data.frame(Equipo = equipoLocal, Rival = equipoVisitante, PlusLocal = paste("DF", equipoVisitante)), type = "response")
  lambda <- lambda1 + lambda2
  
  # Calcular media de xG para el equipo visitante
  mu1 <- predict(modelo, data.frame(Equipo = equipoVisitante, Rival = equipoLocal, PlusLocal = paste("DF", equipoLocal)), type = "response")
  mu2 <- predict(modelo, data.frame(Equipo = equipoVisitante, Rival = equipoLocal, PlusLocal = paste("AT", equipoVisitante)), type = "response")
  mu <- mu1 + mu2

  # Discretizar los xG de cada equipo en buckets: [0, 1), [1, 2), ..., [4, 5)
  probaLocal <- probaPoissonRango(0:4, lambda)
  probaVisitante <- probaPoissonRango(0:4, mu)
  
  # Matriz de probabilidades conjuntas
  probaMatrix <- outer(probaLocal, probaVisitante, `*`)
  
  # Normalizar las probabilidades
  probaMatrix <- probaMatrix / sum(probaMatrix)
  
  return(probaMatrix)
}

# ===== Matriz de Probabilidad Acumulada ===== #
probabilidad_acumulada <- function(mat) {
  # Matriz de probabilidad acumulada
  n_rows <- nrow(mat)
  n_cols <- ncol(mat)
  cum_mat <- matrix(0, nrow = n_rows, ncol = n_cols)
  
  for (i in 1:n_rows) {
    for (j in 1:n_cols) {
      cum_mat[i, j] <- mat[i, j] + 
        ifelse(i > 1, cum_mat[i - 1, j], 0) + 
        ifelse(j > 1, cum_mat[i, j - 1], 0) - 
        ifelse(i > 1 && j > 1, cum_mat[i - 1, j - 1], 0)
    }
  }
  
  return(cum_mat)
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

# ----- Calcular predicciones para cada equipo ----- #
estimations_puntual <- list()
estimations_acumulada <- list()
estimations_puntual_ganador <- list()
estimations_puntual_reducido <- list()

for (equipoLocal in equipos) {
  estimations_puntual[[equipoLocal]] <- list()
  estimations_acumulada[[equipoLocal]] <- list()
  estimations_puntual_ganador[[equipoLocal]] <- list()
  estimations_puntual_reducido[[equipoLocal]] <- list()

  for (equipoVisitante in equipos) {
    if (equipoLocal != equipoVisitante) {
      prob <- probaPartido(equipoLocal, equipoVisitante)

      # Probabilidad de cada resultado i,j
      estimations_puntual[[equipoLocal]][[equipoVisitante]] <- prob

      # Probabilidad de que el resultado sea <= i,j
      estimations_acumulada[[equipoLocal]][[equipoVisitante]] <- probabilidad_acumulada(prob)

      # Probabilidad de: Victoria, Empate, Derrota
      estimations_puntual_ganador[[equipoLocal]][[equipoVisitante]] <- probabilidad_resultados(prob)

      # Probabilidad de Derrota/Empate, Victoria, considerando capacidad defensiva
      prob_doble <- probaDoblePartido(equipoLocal, equipoVisitante)
      prob_doble_acum <- probabilidad_resultados(prob_doble)
      estimations_puntual_reducido[[equipoLocal]][[equipoVisitante]] <- c(prob_doble_acum[2] + prob_doble_acum[3], prob_doble_acum[1])
    }
  }
}

# ----- Escribir Resultados ----- #
write_json(estimations_puntual, "resultados/estimacion_xG_puntual.json", pretty = TRUE, auto_unbox = TRUE, digits = NA)
write_json(estimations_acumulada, "resultados/estimacion_xG_acumulada.json", pretty = TRUE, auto_unbox = TRUE, digits = NA)
write_json(estimations_puntual_ganador, "resultados/estimacion_xG_puntual_ganador.json", pretty = TRUE, auto_unbox = TRUE, digits = NA)
write_json(estimations_puntual_reducido, "resultados/estimacion_xG_puntual_reducido.json", pretty = TRUE, auto_unbox = TRUE)
