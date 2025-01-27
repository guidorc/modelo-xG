library(jsonlite)

escribirResultados <- function (equipos, prob, probRed) {
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
        acumProb <- cumsum(prob)
        acumProb <- acumProb / tail(acumProb, 1)
        
        # Probabilidad de cada resultado i, j
        estimations_puntual[[equipoLocal]][[equipoVisitante]] <- prob
        
        # Probabilidad de que el resultado sea <= i,j
        estimations_acumulada[[equipoLocal]][[equipoVisitante]] <- acumProb
        
        # Probabilidad de: Victoria, Empate, Derrota
        resultados <- c(
          sum(prob[1:15]),   # Victoria
          sum(prob[16:21]),  # Empate
          sum(prob[22:36])   # Derrota
        ) / sum(prob)
        estimations_puntual_ganador[[equipoLocal]][[equipoVisitante]] <- resultados
        
        # Probabilidad de: Derrota/Empate, Victoria
        estimations_puntual_reducido[[equipoLocal]][[equipoVisitante]] <- prob_red
        
      }
    }
  }

  write_json(estimations_puntual, "estimations_puntual.json", pretty = TRUE, auto_unbox = TRUE, digits = NA)
  write_json(estimations_acumulada, "estimations_acumulada.json", pretty = TRUE, auto_unbox = TRUE, digits = NA)
  write_json(estimations_puntual_ganador, "estimations_puntual_ganador.json", pretty = TRUE, auto_unbox = TRUE, digits = NA)
  write_json(estimations_puntual_reducido, "estimations_puntual_reducido.json", pretty = TRUE, auto_unbox = TRUE) 
}