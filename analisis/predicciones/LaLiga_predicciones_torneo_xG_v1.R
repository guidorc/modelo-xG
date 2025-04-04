# Dependencias
library(jsonlite)
library(readxl)

rm(list = ls())
setwd("~/facultad/tesis")

# Carga de datos
dta <- read.csv("datos/la_liga/agregado_temporadas/temporadas_18_24.csv")

temporada <- dta[dta$Fecha >= "2023-08-11", ] # Temporada a analizar
equipos <- unique(c(as.character(dta$Local), as.character(dta$Visitante))) # Todos los equipos del dataset

# Agregamos columnas para las probabilidades al data frame
temporada$xG_local_pred <- NA
temporada$xG_visitante_pred <- NA
temporada$Prob_Local <- NA
temporada$Prob_Empate  <- NA
temporada$Prob_Visitante <- NA

# ---------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------------------- #

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

# ----- Predecir xG en base al modelo ----- #
expectedGoals <- function(modelo, equipo, rival, localia) {
  return(predict(
    modelo, 
    data.frame(
      Equipo = factor(equipo, levels = modelo$xlevels$Equipo), 
      Rival = factor(rival, levels = modelo$xlevels$Equipo), 
      PlusLocal = factor(paste(localia, local), modelo$xlevels$PlusLocal)
    ), 
    type = "response"
  )
  )
}

# ----- Probabilidad de cada xG para el partido X_ij, Y_ij ----- #
probaPartido <- function(xG_local, xG_visitante, modelo) {
  # Discretizar los xG de cada equipo en buckets: [0, 1), [1, 2), ..., [4, 5)
  probaLocal <- probaPoissonRango(0:4, xG_local)
  probaVisitante <- probaPoissonRango(0:4, xG_visitante)
  
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

# ---------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------------------- #

# Iteramos cada partido de la temporada
for (i in 1:nrow(temporada)) {
  partido <- temporada[i, ]
  fecha_partido <- partido$Fecha
  local <- partido$Local
  visitante <- partido$Visitante
  dta_aux <- dta[dta$Fecha < fecha_partido, ]
  
  print(sprintf("partido %s: %s - %s", i, local, visitante))
  
  # Entrenamos el modelo con los datos hasta la fecha del partido
  dta.indep <- data.frame(
    Equipo = factor(c(as.character(dta_aux$Local), as.character(dta_aux$Visitante)), levels = equipos),
    Rival = factor(c(as.character(dta_aux$Visitante), as.character(dta_aux$Local)), levels = equipos),
    PlusLocal = as.factor(c(paste("AT", as.character(dta_aux$Local)), paste("DF", as.character(dta_aux$Local)))),
    xG = c(as.double(dta_aux$xG_local), as.double(dta_aux$xG_visitante)),
    Fecha = as.factor(c(as.integer(gsub("-", "", format(as.Date(dta_aux$Fecha)))), as.integer(gsub("-", "", format(as.Date(dta_aux$Fecha)))))),
    DifFecha = c(as.integer(difftime(fecha_partido, dta_aux$Fecha, units = "weeks")), as.integer(difftime(fecha_partido, dta_aux$Fecha, units = "weeks"))),
    Fade = exp(-0.0065 * (c(as.integer(difftime(fecha_partido, dta_aux$Fecha, units = "weeks")), as.integer(difftime(fecha_partido, dta_aux$Fecha, units = "weeks")))))
  )
  modelo <- glm(xG ~ 0 + Equipo + Rival + PlusLocal, data=dta.indep, family=poisson(), weights=dta.indep$Fade)
  
  # Predecir xG de cada equipo para el partido actual
  xG_local <- expectedGoals(modelo, local, visitante, "AT")
  xG_visitante <- expectedGoals(modelo, visitante, local, "DF")
  
  # Calcular probabilidad de cada resultado en base a los xG
  prob <- probaPartido(xG_local, xG_visitante, modelo)
  resultados <- probabilidad_resultados(prob)
  
  # Agregar datos al resultado
  temporada$xG_local_pred[i] <- xG_local
  temporada$xG_visitante_pred[i] <- xG_visitante
  temporada$Prob_Local[i] <- resultados[1]
  temporada$Prob_Empate[i]  <- resultados[2]
  temporada$Prob_Visitante[i] <- resultados[3]
}

write.csv(temporada, file = "resultados/predicciones/resultados_temporada_xG_v1.csv", row.names = FALSE)