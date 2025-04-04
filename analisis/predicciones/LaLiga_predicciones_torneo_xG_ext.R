# Dependencias
library(jsonlite)
library(readxl)

rm(list = ls())
setwd("~/facultad/tesis")

# Carga de datos
dta <- read.csv("datos/la_liga/agregado_temporadas/temporadas_19_23_ext.csv")

temporada_aux <- dta[dta$Fecha >= "2019-08-17", ]
temporada <- temporada_aux[temporada_aux$Fecha <= "2020-07-19", ] # Temporada a analizar
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

# ----- Predecir xG en base al modelo ----- #
expectedGoals <- function(modelo, local, rival, localia, posesion, tiros, al_arco, prec_tiros, pases, pases_comp, prec_pases) {
  return(predict(
      modelo, 
      data.frame(
        Equipo = factor(local, levels = modelo$xlevels$Equipo), 
        Rival = factor(rival, levels = modelo$xlevels$Equipo), 
        PlusLocal = factor(paste(localia, local), levels = modelo$xlevels$PlusLocal),
        Posesion = as.double(posesion),
        Tiros = as.integer(tiros), 
        Tiros_al_arco = as.integer(al_arco), 
        Precision_tiros = as.double(prec_tiros), 
        Pases = as.integer(pases), 
        Pases_completos = as.integer(pases_comp), 
        Precision_pases = as.double(prec_pases)
      ), 
      type = "response"
    )
  )
}

# ----- Probabilidad de cada xG para el partido X_ij, Y_ij ----- #
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
    Posesion = c(as.double(dta_aux$posesión_home), as.double(dta_aux$posesión_away)),
    Tiros = c(as.integer(dta_aux$home_shots), as.integer(dta_aux$away_shots)),
    Tiros_al_arco = c(as.integer(dta_aux$home_shots_on_target), as.integer(dta_aux$away_shots_on_target)),
    Precision_tiros = c(as.double(dta_aux$home_shot_accuracy), as.double(dta_aux$away_shot_accuracy)),
    Pases = c(as.integer(dta_aux$home_passes), as.integer(dta_aux$away_passes)),
    Pases_completos = c(as.integer(dta_aux$home_completed_passes), as.integer(dta_aux$away_completed_passes)),
    Precision_pases = c(as.double(dta_aux$home_passing_accuracy), as.double(dta_aux$away_passing_accuracy)),
    Fecha = as.factor(c(as.integer(gsub("-", "", format(as.Date(dta_aux$Fecha)))), as.integer(gsub("-", "", format(as.Date(dta_aux$Fecha)))))),
    DifFecha = c(as.integer(difftime(fecha_partido, dta_aux$Fecha, units = "weeks")), as.integer(difftime(fecha_partido, dta_aux$Fecha, units = "weeks"))),
    Fade = exp(-0.0065 * (c(as.integer(difftime(fecha_partido, dta_aux$Fecha, units = "weeks")), as.integer(difftime(fecha_partido, dta_aux$Fecha, units = "weeks")))))
  )
  
  # Agregar parámetros
  modelo <- glm(
    xG ~ 0 + Equipo + Rival + PlusLocal + Posesion + Tiros + Tiros_al_arco + Precision_tiros + Pases + Pases_completos + Precision_pases,
    data = dta.indep,
    family = poisson(),
    weights = dta.indep$Fade
  )
  
  # Predecir xG de cada equipo para el partido actual
  xG_local <- expectedGoals(modelo, local, visitante, "AT", partido$posesión_home, partido$home_shots, partido$home_shots_on_target, partido$home_shot_accuracy, partido$home_passes, partido$home_completed_passes, partido$home_passing_accuracy)
  xG_visitante <- expectedGoals(modelo, visitante, local, "DF", partido$posesión_away, partido$away_shots, partido$away_shots_on_target, partido$away_shot_accuracy, partido$away_passes, partido$away_completed_passes, partido$away_passing_accuracy)
  
  # Calcular probabilidad de cada resultado en base a los xG
  prob <- probaPartido(xG_local, xG_visitante)
  resultados <- probabilidad_resultados(prob)
  
  # Agregar datos al resultado
  temporada$xG_local_pred[i] <- xG_local
  temporada$xG_visitante_pred[i] <- xG_visitante
  temporada$Prob_Local[i] <- resultados[1]
  temporada$Prob_Empate[i]  <- resultados[2]
  temporada$Prob_Visitante[i] <- resultados[3]
}

write.csv(temporada, file = "resultados/predicciones/resultados_temporada_xG_ext.csv", row.names = FALSE)