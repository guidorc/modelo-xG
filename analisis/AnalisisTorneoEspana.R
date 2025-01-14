# Copyright (c) 2018 by Instituto de C?lculo, http://www.ic.fcen.uba.ar/

# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:

# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.

# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

# Coded by Ivan Monardo for project 301060 (https://301060.exactas.uba.ar/)



rm(list=ls())
setwd("~/UBA/301060/ModeloTorneoLocal/espana")
library(readxl)

# Carga de la base de datos
dta <- read_excel('DatosLigaEspanola.xlsx')


# # Nos quedamos con los partidos desde 2021
dta <- dta[dta$Fecha > "2021-07-01",]

n <- dim(dta)
n <- n[1]

pesoLocalia <- as.double(!dta$EstadioNeutral)
#pesoLocalia <- pesoLocalia * (rep(1,n)-as.double((dta$Fecha>inicioPandemia)&(dta$Fecha<finPandemia))*0.5)

# Armamos un dataframe
# Asignamos una fecha (puede ser el dia de hoy o una fecha fija)
fechaModelo <- Sys.Date()+3
#fechaModelo <- "2022-10-22"
dta.indep <- data.frame(
      Equipo = as.factor(c(as.character(dta$Local), as.character(dta$Visitante))),
			Rival =  as.factor(c(as.character(dta$Visitante), as.character(dta$Local))),
      PlusLocal = as.factor(c(paste('AT',as.character(dta$Local)),paste('DF',as.character(dta$Local)))),
			PesoLocalia = as.double(c(pesoLocalia,pesoLocalia)),
			GolesAFavor =  c(dta$GolesLocal, dta$GolesVisitante),
			Fecha = as.factor(c(as.integer(gsub("-", "", format(as.Date(dta$Fecha)))), as.integer(gsub("-", "", format(as.Date(dta$Fecha)))))),
      DifFecha = c(as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks")),as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks"))),
			Fade = exp(-0.0065* (c(as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks")),as.integer(difftime(fechaModelo, dta$Fecha, units = "weeks")))) )
			)

# Modelo: Calculamos el EMV. Donde la probabilidad de que A le haga n goles a B es una poisson
# Cuyo par?metro depende de ambos equipos y quien fue local
modelo <- glm(GolesAFavor ~ 0 + Equipo + Rival + PlusLocal, data=dta.indep, family=poisson(), weights= dta.indep$Fade)

probaPartido <- function(equipoLocal, equipoVisitante) {
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
  proba[2,1] <- proba[2,1] * 0.95
  proba[3,1] <- proba[3,1] * 0.94
  proba[1,2] <- proba[1,2] * 0.88
  proba[2,3] <- proba[2,3] * 0.93
  #sobrante <- 1 - sum(proba)
   
  # #sobrante <- 0
  proba[1,1] <- proba[1,1] * 1.16
  proba[2,2] <- proba[2,2] * 1.06
  proba[3,2] <- proba[3,2] * 1.04
  
  proba <- proba / sum(proba)
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



probaDoblePartido <- function(equipoLocal, equipoVisitante) {
  # Tasa de goles del Local. X_{i,j}
  # lambda = exp( Equipo(equipoLocal) + PlusLocal(AT equipoLocal) + Rival(equipoVisitante) )
  lambda1 <- predict(modelo, data.frame(Equipo = equipoLocal, Rival = equipoVisitante, PlusLocal = paste('AT',equipoLocal)), type = 'response')
  mu2 <- predict(modelo, data.frame(Equipo = equipoVisitante, Rival = equipoLocal, PlusLocal = paste('AT',equipoVisitante)), type = 'response')
  # Tasa de goles del Visitante. Y_{i,j}
  # mu = exp( Equipo(equipoVisitante) + Rival(equipoLocal) + PlusLocal(DF equipoLocal) )
  mu1 <- predict(modelo, data.frame(Equipo = equipoVisitante, Rival = equipoLocal, PlusLocal = paste('DF',equipoLocal)), type = 'response')
  lambda2 <- predict(modelo, data.frame(Equipo = equipoLocal, Rival = equipoVisitante, PlusLocal = paste('DF',equipoVisitante)), type = 'response')
  lambda <- lambda1 + lambda2
  mu <- mu1 + mu2
  maxgol <- 7 # Máxima cantidad de goles que puede meter un equipo
  # dpois da la probabilidad puntual.
  # Matriz con la probabilidad de que el partido termine con el resultado i-j
  proba <- dpois(0:maxgol, lambda) %*% t(dpois(0:maxgol, mu))
  #print(proba)
  # Ajustamos viendo en promedio los resultados anteriores
  #proba[2,1] <- proba[2,1] * 0.95
  #proba[3,1] <- proba[3,1] * 0.94
  #proba[1,2] <- proba[1,2] * 0.88
  #proba[2,3] <- proba[2,3] * 0.93
  #sobrante <- 1 - sum(proba)
  
  # #sobrante <- 0
  #proba[1,1] <- proba[1,1] * 1.16
  #proba[2,2] <- proba[2,2] * 1.06
  #proba[3,2] <- proba[3,2] * 1.04
  
  proba <- proba / sum(proba)
  # c combines its arguments to form a vector
  probaPartido <- rep(0,2)
  # probaPartido = (5-0,5-1,5-2,5-3,5-4,4-0,4-1,4-2,4-3,3-0,3-1,3-2,2-0,2-1,1-0,0-0,1-1,2-2,3-3,4-4,5-5,0-1,1-2,0-2,2-3,1-3,0-3,3-4,2-4,1-4,0-4,4-5,3-5,2-5,1-5,0-5)
  for (i in 1:(maxgol+1)) {
    for (j in 1:(1+maxgol)) {
      if (i<=j){
          probaPartido[1] <- probaPartido[1] + proba[i,j]
      }
      else {
        probaPartido[2] <- probaPartido[2] + proba[i,j]
      }
  }}
  return(probaPartido)
}

equipos <- c('Albacete',
             'Almeria',
             'Burgos-Cf',
             'Cadiz',
             'Cartagena',
             'Cd-Castellon',
             'Cordoba-Cf',
             'Deportivo',
             'Eibar',
             'Elche',
             'Eldense',
             'Granada',
             'Huesca',
             'Levante',
             'Malaga',
             'Mirandes',
             'Racing',
             'Racing-Club-Ferrol',
             'Real-Oviedo',
             'Real-Zaragoza',
             'Sporting-Gijon',
             'Tenerife')

write('probaPuntual = {', file = "estimations_puntual.py", append = FALSE)
write('probaAcumulada = {', file = "estimations_acumulada.py", append = FALSE)
write('probaPuntualGanador = {', file = "estimations_puntual_ganador.py", append = FALSE)
write('probaReducido = {', file = "estimations_puntual_reducido.py", append = FALSE)

for (equipoLocal in equipos) {
  write(paste("\t'",equipoLocal,"':{", sep = ''), file = "estimations_puntual.py", append = TRUE)
  write(paste("\t'",equipoLocal,"':{", sep = ''), file = "estimations_acumulada.py", append = TRUE)
  write(paste("\t'",equipoLocal,"':{", sep = ''), file = "estimations_puntual_ganador.py", append = TRUE)
  write(paste("\t'",equipoLocal,"':{", sep = ''), file = "estimations_puntual_reducido.py", append = TRUE)
  for (equipoVisitante in equipos){
    if (equipoLocal != equipoVisitante){
      prob <- probaPartido(equipoLocal,equipoVisitante)
      acumProb <- cumsum(prob)
      acumProb <- acumProb / tail(acumProb,1)
      write(paste0("\t\t'",equipoVisitante,"':","[", paste(prob, collapse = ','),"],"), file = "estimations_puntual.py", append = TRUE)
      write(paste0("\t\t'",equipoVisitante,"':","[", paste(acumProb, collapse = ','),"],"), file = "estimations_acumulada.py", append = TRUE)
      resultado <- c(sum(prob[1:15]),sum(prob[16:21]),sum(prob[22:36])) / sum(prob)
      write(paste0("\t\t'",equipoVisitante,"':","[", paste(resultado, collapse = ','),"],"), file = "estimations_puntual_ganador.py", append = TRUE)
      prob_red <- probaDoblePartido(equipoLocal,equipoVisitante)
      write(paste0("\t\t'",equipoVisitante,"':","[", paste(prob_red, collapse = ','),"],"), file = "estimations_puntual_reducido.py", append = TRUE)
    }
  }  
  write("\t},", file = "estimations_puntual.py", append = TRUE)
  write("\t},", file = "estimations_acumulada.py", append = TRUE)
  write("\t},", file = "estimations_puntual_ganador.py", append = TRUE)
  write("\t},", file = "estimations_puntual_reducido.py", append = TRUE)
}
write("}", file = "estimations_puntual.py", append = TRUE)
write("}", file = "estimations_acumulada.py", append = TRUE)
write("}", file = "estimations_puntual_ganador.py", append = TRUE)
write("}", file = "estimations_puntual_reducido.py", append = TRUE)
