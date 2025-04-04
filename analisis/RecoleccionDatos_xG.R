rm(list=ls())
setwd("~/facultad/tesis")
library(readxl)

# ----- Carga de datos ----- #
dta <- read.csv('datos/la_liga/la_liga_ultimas_temporadas.csv')

# ----- Variables ----- #
cant_partidos <- dim(dta)[1]
resultados <- matrix(data=0, nrow=cant_partidos,ncol = 2+25+25)
scores <- expand.grid(home = 4:0, away = 0:4)
posibles_resultados <- c()
nombre_col <- c("xG_Local","xG_Visitante",posibles_resultados,paste("R",posibles_resultados,sep = ""))
colnames(resultados) <- nombre_col
part_hasta <- "2000-01-01"
part_desde <- "2000-01-01"

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
probaPartido <- function(equipoLocal, equipoVisitante, modelo) {
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

# ----- Efectuar analisis de partidos ----- #
for (i in 1:cant_partidos){
  part <- dta[i,]
  part_hasta_aux <- part$`Partidos hasta`
  part_desde_aux <- part$`Partidos desde`
  if(part_hasta != part_hasta_aux){
    part_desde <- part_desde_aux
    part_hasta <- part_hasta_aux
    dta_aux <- dta[dta$Fecha > part_desde,]
    dta_aux <- dta_aux[dta_aux$Fecha < part_hasta,]
    dta.indep <- data.frame(
      Equipo = as.factor(c(as.character(dta_aux$Local), as.character(dta_aux$Visitante))),
      Rival =  as.factor(c(as.character(dta_aux$Visitante), as.character(dta_aux$Local))),
      PlusLocal = as.factor(c(paste('AT',as.character(dta_aux$Local)),paste('DF',as.character(dta_aux$Local)))),
      GolesAFavor =  c(dta_aux$GolesLocal, dta_aux$GolesVisitante),
      Localia = c(rep(1, dim(dta_aux)[1]), rep(0, dim(dta_aux)[1])),
      Fecha = as.factor(c(as.integer(gsub("-", "", format(as.Date(dta_aux$Fecha)))), as.integer(gsub("-", "", format(as.Date(dta_aux$Fecha)))))),
      DifFecha = c(as.integer(difftime(part_hasta, dta_aux$Fecha, units = "weeks")),as.integer(difftime(part_hasta, dta_aux$Fecha, units = "weeks"))),
      Fade = exp(-0.0065* (c(as.integer(difftime(part_hasta, dta_aux$Fecha, units = "weeks")),as.integer(difftime(part_hasta, dta_aux$Fecha, units = "weeks")))) )
    )
    modelo <- glm(xG ~ 0 + Equipo + Rival + PlusLocal, data=dta.indep, family=poisson(), weights= dta.indep$Fade)
  }
  prob <- probaPartido(part$Local,part$Visitante,modelo)
  resultados[i,1:38] <- c(part$GolesLocal,part$GolesVisitante,prob)
  str_res <- paste(part$GolesLocal,"-",part$GolesVisitante,sep = "")
  if (sum(nombre_col==str_res)==1){
    j <- which(nombre_col==str_res) + 36
    resultados[i,j] <- 1
  }
}

library(openxlsx)
write.xlsx(resultados,"Resultados.xlsx")

