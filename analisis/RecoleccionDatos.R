rm(list=ls())
setwd("~/UBA/301060/Paper")
library(readxl)

# Carga de la base de datos
dta <- read_excel('DatosTorneosLocales.xlsx')
dta <- dta[,2:10]
dta_analizar <- dta[dta$`Partidos hasta`>"2018-07-01",]
dta <- dta[,1:7]
# # Nos quedamos con los partidos desde 2017
cant_partidos <- dim(dta_analizar)[1]
resultados <- matrix(data=0, nrow=cant_partidos,ncol = 2+36+36)
posibles_resultados <- c("5-0","5-1","5-2","5-3","5-4","4-0","4-1","4-2","4-3","3-0","3-1","3-2","2-0","2-1","1-0","0-0","1-1","2-2","3-3","4-4","5-5","0-1","1-2","0-2","2-3","1-3","0-3","3-4","2-4","1-4","0-4","4-5","3-5","2-5","1-5","0-5")
nombre_col <- c("GolLocal","GolVisitante",posibles_resultados,paste("R",posibles_resultados,sep = ""))
colnames(resultados) <- nombre_col
part_hasta <- "2000-01-01"
part_desde <- "2000-01-01"

probaPartido <- function(equipoLocal, equipoVisitante, modelo) {
  # Tasa de goles del Local. X_{i,j}
  # lambda = exp( Equipo(equipoLocal) + PlusLocal(AT equipoLocal) + Rival(equipoVisitante) )
  lambda <- predict(modelo, data.frame(Equipo = equipoLocal, Rival = equipoVisitante, PlusLocal = paste('AT',equipoLocal)), type = 'response')
  # Tasa de goles del Visitante. Y_{i,j}
  # mu = exp( Equipo(equipoVisitante) + Rival(equipoLocal) + PlusLocal(DF equipoLocal) )
  mu <- predict(modelo, data.frame(Equipo = equipoVisitante, Rival = equipoLocal, PlusLocal = paste('DF',equipoLocal)), type = 'response')
  maxgol <- 5 # Máxima cantidad de goles que puede meter un equipo
  # dpois da la probabilidad puntual.
  # Matriz con la probabilidad de que el partido termine con el resultado i-j
  proba <- dpois(0:maxgol, lambda) %*% t(dpois(0:maxgol, mu))
  #print(proba)
  # Ajustamos viendo en promedio los resultados anteriores
  proba[1,1] <- proba[1,1] * 0.96
  proba[1,2] <- proba[1,2] * 0.79
  proba[2,3] <- proba[2,3] * 0.97
  proba[1,4] <- proba[1,4] * 0.86
  # proba[3,4] <- proba[3,4] * 0.64
  # proba[2,5] <- proba[2,5] * 0.88
  proba[3,5] <- proba[3,5] * 0.81
  proba[4,5] <- proba[4,5] * 0.74
  proba[1,6] <- proba[1,6] * 0.85
  # proba[2,6] <- proba[2,6] * 0.40
  # proba[3,6] <- proba[3,6] * 0.10
  # proba[4,6] <- proba[4,6] * 0.25
  proba[2,1] <- proba[2,1] * 0.96
  # proba[4,3] <- proba[4,3] * 0.99
  proba[5,1] <- proba[5,1] * 0.72
  # proba[5,2] <- proba[5,2] * 0.90
  proba[5,4] <- proba[5,4] * 0.92
  proba[6,1] <- proba[6,1] * 0.65
  proba[6,2] <- proba[6,2] * 0.38
  proba[6,3] <- proba[6,3] * 0.76
  proba[6,4] <- proba[6,4] * 0.19
  sobrante <- 1 - sum(proba)
  
  # #sobrante <- 0
  proba[3,2] <- proba[3,2] + sobrante*0.28
  proba[3,1] <- proba[3,1] + sobrante*0.03
  # proba[2,1] <- proba[2,1] + sobrante*0.07
  proba[4,1] <- proba[4,1] + sobrante*0.10
  proba[4,2] <- proba[4,2] + sobrante*0.03
  proba[4,3] <- proba[4,3] + sobrante*0.04
  # proba[5,1] <- proba[5,1] + sobrante*0.02
  proba[5,2] <- proba[5,2] + sobrante*0.01
  proba[5,3] <- proba[5,3] + sobrante*0.04
  # proba[1,1] <- proba[1,1] + sobrante*0.07
  proba[2,2] <- proba[2,2] + sobrante*0.02
  proba[3,3] <- proba[3,3] + sobrante*0.10
  proba[4,4] <- proba[4,4] + sobrante*0.07
  proba[5,5] <- proba[5,5] + sobrante*0.02
  proba[2,4] <- proba[2,4] + sobrante*0.19
  # proba[1,3] <- proba[1,3] + sobrante*0.03
  proba[1,5] <- proba[1,5] + sobrante*0.03
  proba[2,5] <- proba[2,5] + sobrante*0.02
  proba[2,6] <- proba[2,6] + sobrante*0.02
  
  # c combines its arguments to form a vector
  probaPartido <- rep(0,36)
  # probaPartido = (5-0,5-1,5-2,5-3,5-4,4-0,4-1,4-2,4-3,3-0,3-1,3-2,2-0,2-1,1-0,0-0,1-1,2-2,3-3,4-4,5-5,0-1,1-2,0-2,2-3,1-3,0-3,3-4,2-4,1-4,0-4,4-5,3-5,2-5,1-5,0-5)
  for (i in 0:maxgol+1) {
    i <- i-1
    for (j in 0:maxgol+1) {
      j <- j-1
      if (i==j){
        if(i<5){
          probaPartido[16+i] <- proba[i+1,j+1]
        }
        else{
          probaPartido[21] <- probaPartido[21] + proba[i+1,j+1]
        }
      }
      else if ((0 < (i-j)) &  ((i-j) < 5)){ # Si el local gana por menos de 5 goles
        if(i<5){
          probaPartido[j-sum(1:i)+16] <- proba[i+1,j+1]
        }
        else{
          probaPartido[j-i+6] <- probaPartido[j-i+6] + proba[i+1,j+1]
        }
      }
      else if ((0 < (j-i)) &  ((j-i) < 5)){ # Si el visitante gana por menos de 5 goles
        if(j<5){
          probaPartido[-i+sum(1:j)+21] <- proba[i+1,j+1]
        }
        else{
          probaPartido[j-i+31] <- probaPartido[j-i+31] + proba[i+1,j+1]
        }
      }
      else if (i > j){
        probaPartido[1] <- probaPartido[1] + proba[i+1,j+1]
      }
      else if (j > i){
        probaPartido[36] <- probaPartido[36] + proba[i+1,j+1]
      }
    }
  }
  #cat('(\'',paste(equipoLocal,equipoVisitante, sep = '\', \''),'\',',paste(probaPartido, collapse = ','),"),\n", sep = '')
  #return(paste(probaPartido, collapse = ','))
  #return(proba)
  return(probaPartido)
}

for (i in 1:cant_partidos){
  part <- dta_analizar[i,]
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
    modelo <- glm(GolesAFavor ~ 0 + Equipo + Rival + PlusLocal, data=dta.indep, family=poisson(), weights= dta.indep$Fade)
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

