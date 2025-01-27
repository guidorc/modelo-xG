# Dependencias
library(jsonlite)
library(readxl)

rm(list = ls())
setwd("~/facultad/tesis")

# Carga de datos
dta <- read_excel("datos/DatosLigaEspanola_xG.xlsx")
equipos <- read_excel("datos/DatosLigaEspanola_xG.xlsx", sheet = "Equipos")$Equipos

# Nos quedamos con los partidos desde 2021
dta <- dta[dta$Fecha > "2021-07-01", ]

pesoLocalia <- as.double(!dta$EstadioNeutral)
# n <- dim(dta)
# n <- n[1]
# pesoLocalia <- pesoLocalia * (rep(1, n) - as.double((dta$Fecha > inicioPandemia) & (dta$Fecha < finPandemia)) * 0.5)

# Armamos un dataframe
# Asignamos una fecha (puede ser el dia de hoy o una fecha fija)
fechaModelo <- Sys.Date() + 3
# fechaModelo <- "2022-10-22"

# ----- Variables independientes ----- #
dta.indep <- data.frame(
  Equipo = as.factor(c(as.character(dta$Local), as.character(dta$Visitante))),
  Rival = as.factor(c(as.character(dta$Visitante), as.character(dta$Local))),
  PlusLocal = as.factor(c(paste("AT", as.character(dta$Local)), paste("DF", as.character(dta$Local)))),
  PesoLocalia = as.double(c(pesoLocalia, pesoLocalia)),
  xG = as.double(c(dta$xG_Local, dta$xG_visitante)),
  GolesAFavor = c(dta$GolesLocal, dta$GolesVisitante),
  Fecha = as.factor(c(as.integer(gsub("-", "", format(as.Date(dta$Fecha)))), as.integer(gsub("-", "", format(as.Date(dta$Fecha)))))),
  DifFecha = c(as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks")), as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks"))),
  Fade = exp(-0.0065 * (c(as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks")), as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks"))))) # Ajuste por actualidad
)

# ----- MODELO ----- #
# Calculamos el EMV. Donde la probabilidad de que A le haga n goles a B es una poisson
# Cuyo parametro depende de ambos equipos y quien fue local
modelo <- glm(xG ~ 0 + Equipo + Rival + PlusLocal, data = dta.indep, family = poisson(), weights = dta.indep$Fade) # Calibración

# ----- Aux para obtener xG ----- #
expected_goals <- function (equipoLocal, equipoVisitante) {
  result <- (dta.indep$xG[dta.indep$Equipo == equipoLocal & dta.indep$Rival == equipoVisitante])
  if (length(result) == 0) {
    message(paste("No se encontraron datos para xG_Local:", equipoLocal, "vs", equipoVisitante))
    return(0)
  }
  return(result)
}

# ----- Probabilidad de cada resultado para el partido X_ij, Y_ij ----- #
probaPartido <- function(equipoLocal, equipoVisitante) {
  # expected goals de local y visitante
  xG_Local <- expected_goals(equipoLocal, equipoVisitante)
  xG_visitante <- expected_goals(equipoVisitante, equipoLocal)
  
  # Tasa de goles del Local. X_{i,j}
  # lambda = exp( Equipo(equipoLocal) + PlusLocal(AT equipoLocal) + Rival(equipoVisitante) )
  lambda <- predict(modelo, data.frame(Equipo = equipoLocal, Rival = equipoVisitante, PlusLocal = paste("AT", equipoLocal)), type = "response")
  
  # Tasa de goles del Visitante. Y_{i,j}
  # mu = exp( Equipo(equipoVisitante) + Rival(equipoLocal) + PlusLocal(DF equipoLocal) )
  mu <- predict(modelo, data.frame(Equipo = equipoVisitante, Rival = equipoLocal, PlusLocal = paste("DF", equipoLocal)), type = "response")

  maxgol <- 5 # Máxima cantidad de goles que puede meter un equipo
  # dpois da la probabilidad puntual.
  # Matriz con la probabilidad de que el partido termine con el resultado i-j
  proba <- dpois(0:maxgol, lambda) %*% t(dpois(0:maxgol, mu))
  
  # Ajustamos viendo en promedio los resultados anteriores
  proba[2, 1] <- proba[2, 1] * 0.95
  proba[3, 1] <- proba[3, 1] * 0.94
  proba[1, 2] <- proba[1, 2] * 0.88
  proba[2, 3] <- proba[2, 3] * 0.93
  # sobrante <- 1 - sum(proba)
  # #sobrante <- 0
  proba[1, 1] <- proba[1, 1] * 1.16
  proba[2, 2] <- proba[2, 2] * 1.06
  proba[3, 2] <- proba[3, 2] * 1.04
  
  # normalizar la matriz
  proba <- proba / sum(proba)
  # c combines its arguments to form a vector
  probaPartido <- rep(0, 36)
  # probaPartido = (5-0,5-1,5-2,5-3,5-4,4-0,4-1,4-2,4-3,3-0,3-1,3-2,2-0,2-1,1-0,0-0,1-1,2-2,3-3,4-4,5-5,0-1,1-2,0-2,2-3,1-3,0-3,3-4,2-4,1-4,0-4,4-5,3-5,2-5,1-5,0-5)
  for (i in 0:maxgol + 1) {
    i <- i - 1
    for (j in 0:maxgol + 1) {
      j <- j - 1
      if (i == j) {
        if (i < 5) {
          # Empate
          probaPartido[16 + i] <- proba[i + 1, j + 1]
        } else {
          # Empate 5-5
          probaPartido[21] <- probaPartido[21] + proba[i + 1, j + 1]
        }
      } else if ((0 < (i - j)) & ((i - j) < 5)) { # Si el local gana por menos de 5 goles
        if (i < 5) {
          probaPartido[j - sum(1:i) + 16] <- proba[i + 1, j + 1]
        } else {
          probaPartido[j - i + 6] <- probaPartido[j - i + 6] + proba[i + 1, j + 1]
        }
      } else if ((0 < (j - i)) & ((j - i) < 5)) { # Si el visitante gana por menos de 5 goles
        if (j < 5) {
          probaPartido[-i + sum(1:j) + 21] <- proba[i + 1, j + 1]
        } else {
          probaPartido[j - i + 31] <- probaPartido[j - i + 31] + proba[i + 1, j + 1]
        }
      } else if (i > j) {
        # Local Gana 5-0
        probaPartido[1] <- probaPartido[1] + proba[i + 1, j + 1]
      } else if (j > i) {
        # Visitante Gana 0-5
        probaPartido[36] <- probaPartido[36] + proba[i + 1, j + 1]
      }
    }
  }
  # cat('(\'',paste(equipoLocal,equipoVisitante, sep = '\', \''),'\',',paste(probaPartido, collapse = ','),"),\n", sep = '')
  # return(paste(probaPartido, collapse = ','))
  # return(proba)
  return(probaPartido)
}

# ----- Probabilidad de cada resultado para el partido X_ij, Y_ij, considerando la capacidad defensiva del rival ----- #
probaDoblePartido <- function(equipoLocal, equipoVisitante) {
  # expected goals de local y visitante
  xG_Local <- expected_goals(equipoLocal, equipoVisitante)
  xG_visitante <- expected_goals(equipoVisitante, equipoLocal)

  # Tasa de goles del Local. X_{i,j}
  # lambda = exp( Equipo(equipoLocal) + PlusLocal(AT equipoLocal) + Rival(equipoVisitante) )
  lambda1 <- predict(modelo, data.frame(Equipo = equipoLocal, Rival = equipoVisitante, xG = xG_Local, PlusLocal = paste("AT", equipoLocal)), type = "response")
  lambda2 <- predict(modelo, data.frame(Equipo = equipoLocal, Rival = equipoVisitante, xG = xG_Local, PlusLocal = paste("DF", equipoVisitante)), type = "response")
  lambda <- lambda1 + lambda2
  
  # Tasa de goles del Visitante. Y_{i,j}
  # mu = exp( Equipo(equipoVisitante) + Rival(equipoLocal) + PlusLocal(DF equipoLocal) )
  mu1 <- predict(modelo, data.frame(Equipo = equipoVisitante, Rival = equipoLocal, xG = xG_visitante, PlusLocal = paste("DF", equipoLocal)), type = "response")
  mu2 <- predict(modelo, data.frame(Equipo = equipoVisitante, Rival = equipoLocal, xG = xG_visitante, PlusLocal = paste("AT", equipoVisitante)), type = "response")
  mu <- mu1 + mu2

  maxgol <- 7 # Máxima cantidad de goles que puede meter un equipo
  # dpois da la probabilidad puntual.
  # Matriz con la probabilidad de que el partido termine con el resultado i-j
  proba <- dpois(0:maxgol, lambda) %*% t(dpois(0:maxgol, mu))
  # print(proba)
  # Ajustamos viendo en promedio los resultados anteriores
  # proba[2,1] <- proba[2,1] * 0.95
  # proba[3,1] <- proba[3,1] * 0.94
  # proba[1,2] <- proba[1,2] * 0.88
  # proba[2,3] <- proba[2,3] * 0.93
  # sobrante <- 1 - sum(proba)
  
  # #sobrante <- 0
  # proba[1,1] <- proba[1,1] * 1.16
  # proba[2,2] <- proba[2,2] * 1.06
  # proba[3,2] <- proba[3,2] * 1.04
  
  proba <- proba / sum(proba)
  # c combines its arguments to form a vector
  probaPartido <- rep(0, 2)
  # probaPartido = (5-0,5-1,5-2,5-3,5-4,4-0,4-1,4-2,4-3,3-0,3-1,3-2,2-0,2-1,1-0,0-0,1-1,2-2,3-3,4-4,5-5,0-1,1-2,0-2,2-3,1-3,0-3,3-4,2-4,1-4,0-4,4-5,3-5,2-5,1-5,0-5)
  for (i in 1:(maxgol + 1)) {
    for (j in 1:(1 + maxgol)) {
      if (i <= j) {
        probaPartido[1] <- probaPartido[1] + proba[i, j]
      } else {
        probaPartido[2] <- probaPartido[2] + proba[i, j]
      }
    }
  }
  return(probaPartido)
}

# ----- Calcular predicciones para cada equipo ----- #
estimations_puntual <- list()
estimations_acumulada <- list()
estimations_puntual_ganador <- list()
estimations_puntual_reducido <- list()

for (equipoLocal in equipos) {
  print(estimations_puntual)
  estimations_puntual[[equipoLocal]] <- list()
  estimations_acumulada[[equipoLocal]] <- list()
  estimations_puntual_ganador[[equipoLocal]] <- list()
  estimations_puntual_reducido[[equipoLocal]] <- list()
  
  for (equipoVisitante in equipos) {
    if (equipoLocal != equipoVisitante) {
      prob <- probaPartido(equipoLocal, equipoVisitante)

      acumProb <- cumsum(prob)
      acumProb <- acumProb / tail(acumProb, 1)

      # Probabilidad de cada resultado i, j
      estimations_puntual[[equipoLocal]][[equipoVisitante]] <- prob

      # Probabilidad de que el resultado sea <= i,j
      estimations_acumulada[[equipoLocal]][[equipoVisitante]] <- acumProb

      # Probabilidad de: Victoria, Empate, Derrota
      resultado <- c(
        sum(prob[1:15]),   # Victoria
        sum(prob[16:21]),  # Empate
        sum(prob[22:36])   # Derrota
      ) / sum(prob)
      estimations_puntual_ganador[[equipoLocal]][[equipoVisitante]] <- resultado

      # Probabilidad de: Derrota/Empate, Victoria
      prob_red <- probaDoblePartido(equipoLocal, equipoVisitante)
      estimations_puntual_reducido[[equipoLocal]][[equipoVisitante]] <- prob_red
      
    }
  }
}

# ----- Escribir Resultados ----- #
write_json(estimations_puntual, "resultados/estimacion_puntual.json", pretty = TRUE, auto_unbox = TRUE, digits = NA)
write_json(estimations_acumulada, "resultados/estimacion_acumulada.json", pretty = TRUE, auto_unbox = TRUE, digits = NA)
write_json(estimations_puntual_ganador, "resultados/estimacion_puntual_ganador.json", pretty = TRUE, auto_unbox = TRUE, digits = NA)
write_json(estimations_puntual_reducido, "resultados/estimacion_puntual_reducido.json", pretty = TRUE, auto_unbox = TRUE)
