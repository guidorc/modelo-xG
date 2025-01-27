import os
import pandas as pd
import numpy as np
import statsmodels.api as sm
from scipy.stats import poisson
from datetime import datetime, timedelta

# Limpiar el entorno y configurar el directorio de trabajo
# os.chdir("~/UBA/301060/ModeloTorneoLocal/espana")

# Carga de la base de datos
dta = pd.read_excel("DatosLigaEspanola.xlsx")

# Nos quedamos con los partidos desde 2021
dta['Fecha'] = pd.to_datetime(dta['Fecha'], errors='coerce')
dta = dta[dta['Fecha'] > "2021-07-01"]

n = dta.shape[0]

maxGol = 5

pesoLocalia = (~dta['EstadioNeutral']).astype(float)
# Consideraciones para localia durante la pandemia
# pesoLocalia *= (1 - ((dta['Fecha'] > inicioPandemia) & (dta['Fecha'] < finPandemia)).astype(float) * 0.5)

# Armamos un dataframe
# Asignamos una fecha (puede ser el dia de hoy o una fecha fija)
fechaModelo = datetime.today() + timedelta(days=3)
# fechaModelo = datetime.strptime("2022-10-22", "%Y-%m-%d")

# ----- DATAFRAME -----
dta_indep = pd.DataFrame({
  "Equipo": pd.Categorical(pd.concat([dta["Local"], dta["Visitante"]], ignore_index=True)),
  "Rival": pd.Categorical(pd.concat([dta["Visitante"], dta["Local"]], ignore_index=True)),
  "PlusLocal": pd.Categorical(pd.concat([
      "AT " + dta["Local"].astype(str), 
      "DF " + dta["Local"].astype(str)
  ], ignore_index=True)),
  "PesoLocalia": np.concatenate([pesoLocalia, pesoLocalia]),
  "GolesAFavor": pd.concat([dta["GolesLocal"], dta["GolesVisitante"]], ignore_index=True),
  "Fecha": pd.Categorical(pd.concat([
      dta["Fecha"].dt.strftime("%Y%m%d").astype(int), 
      dta["Fecha"].dt.strftime("%Y%m%d").astype(int)
  ], ignore_index=True)),
  "DifFecha": np.concatenate([
      (fechaModelo - dta["Fecha"]).dt.days // 7, 
      (fechaModelo - dta["Fecha"]).dt.days // 7
  ]),
  "Fade": np.exp(-0.0065 * np.concatenate([
      (fechaModelo - dta["Fecha"]).dt.days // 7, 
      (fechaModelo - dta["Fecha"]).dt.days // 7
  ]))
})

# ----- MODELO -----
# Calculamos el EMV. Donde la probabilidad de que A le haga n goles a B es una poisson
# Cuyo parametro depende de ambos equipos y quien fue local
X = pd.DataFrame({
    'Equipo': pd.Categorical(dta_indep['Equipo']).codes,
    'Rival': pd.Categorical(dta_indep['Rival']).codes,
    'PlusLocal': pd.Categorical(dta_indep['PlusLocal']).codes
})
X = sm.add_constant(X, has_constant='add')
# Ajuste del modelo GLM con distribución Poisson y pesos (calibración)
modelo = sm.GLM(dta_indep['GolesAFavor'], X, family=sm.families.Poisson(), freq_weights=dta_indep['Fade']).fit()

# ----- PREDICCION -----
def probaPartido(equipoLocal, equipoVisitante):
  # Calcular la tasa de goles del equipo local (lambda)
  input_lambda = pd.DataFrame({
    'Equipo': pd.Categorical([equipoLocal], categories=equipos),
    'Rival': pd.Categorical([equipoVisitante], categories=equipos),
    'PlusLocal': pd.Categorical(["AT " + equipoLocal], categories=["AT " + equipo for equipo in equipos])
  })
  # Realizar la predicción
  lambda_ = modelo.predict(input_lambda)[0]
  
  # Calcular la tasa de goles del equipo visitante (mu)
  mu = modelo.predict(pd.DataFrame({
    'Equipo': [equipoVisitante],
    'Rival': [equipoLocal],
    'PlusLocal': ["DF " + equipoLocal]
  }))[0]
  
  # Matriz con la probabilidad de que el partido termine con el resultado i-j
  # probabilidad de que el equipo local haga 0 a maxgol goles
  proba_local = poisson.pmf(np.arange(maxgol + 1), lambda_)
  # Calcular la probabilidad de que el equipo visitante haga 0 a maxgol goles
  proba_visitante = poisson.pmf(np.arange(maxgol + 1), mu)
  # Crear la matriz de probabilidades combinada (producto de las probabilidades de goles del local y visitante)
  proba = np.outer(proba_local, proba_visitante)
  
  # Ajustes específicos a las probabilidades
  proba[1, 0] *= 0.95
  proba[2, 0] *= 0.94
  proba[0, 1] *= 0.88
  proba[1, 2] *= 0.93
  # sobrante <- 1 - sum(proba)
  # sobrante <- 0
  proba[0, 0] *= 1.16
  proba[1, 1] *= 1.06
  proba[2, 1] *= 1.04
  
  # Normalizar la matriz
  proba /= np.sum(proba)
  
  # Crear el vector de probabilidades del partido
  probaPartido = np.zeros(36)
  
  for i in range(maxgol + 1):
    for j in range(maxgol + 1):
      if i == j: 
        # Empate
        if i < maxgol:
          probaPartido[15 + i] = proba[i, j]
        else:
          probaPartido[20] += proba[i, j]
      elif (0 < (i - j)) & ((i - j) < 5):
        # Local gana por menos de 5 goles
        if i < maxgol:
          probaPartido[j - np.sum(range(i+1)) + 15] = proba[i, j]
        else:
          probaPartido[j - i + 5] += proba[i, j]
      elif (0 < (j - i)) & ((j - i) < 5):
        # Visitante gana por menos de 5 goles
        if j < maxgol:
          probaPartido[-i + np.sum(range(j+1)) + 20] = proba[i, j]
        else:
          probaPartido[j - i + 30] += proba[i, j]
      elif i > j:
        # Local gana por mas de 5 goles
        probaPartido[0] += proba[i, j]
      elif j > i:
        # Visitante gana por mas de 5 goles
        probaPartido[35] += proba[i, j]
  
  return probaPartido

import numpy as np

def probaDoblePartido(equipoLocal, equipoVisitante):
  # Calculando tasas de goles para los dos equipos
  lambda1 = modelo.predict(pd.DataFrame({'Equipo': [equipoLocal], 'Rival': [equipoVisitante], 'PlusLocal': [f"AT {equipoLocal}"]}))[0]
  lambda2 = modelo.predict(pd.DataFrame({'Equipo': [equipoLocal], 'Rival': [equipoVisitante], 'PlusLocal': [f"DF {equipoVisitante}"]}))[0]
  
  mu1 = modelo.predict(pd.DataFrame({'Equipo': [equipoVisitante], 'Rival': [equipoLocal], 'PlusLocal': [f"DF {equipoLocal}"]}))[0]
  mu2 = modelo.predict(pd.DataFrame({'Equipo': [equipoVisitante], 'Rival': [equipoLocal], 'PlusLocal': [f"AT {equipoVisitante}"]}))[0]
  
  # Sumando tasas de goles
  lambda_sum = lambda1 + lambda2
  mu_sum = mu1 + mu2
  
  maxgol = 7  # Máxima cantidad de goles posibles
  # Probabilidad de cada posible resultado
  proba = np.outer(np.poisson(0, lambda_sum), np.poisson(0, mu_sum))
  
  proba /= np.sum(proba)  # Normalizamos la probabilidad
  
  probaPartido = [0, 0]  # Inicializamos el vector de probabilidades
  for i in range(maxgol + 1):
    for j in range(maxgol + 1):
      if i <= j:
        probaPartido[0] += proba[i, j]
      else:
        probaPartido[1] += proba[i, j]
  
  return probaPartido

# ----- RESULTADOS -----
equipos = [
    "Albacete", "Almeria", "Burgos-Cf", "Cadiz", "Cartagena", "Cd-Castellon",
    "Cordoba-Cf", "Deportivo", "Eibar", "Elche", "Eldense", "Granada", "Huesca",
    "Levante", "Malaga", "Mirandes", "Racing", "Racing-Club-Ferrol", "Real-Oviedo",
    "Real-Zaragoza", "Sporting-Gijon", "Tenerife"
]

# Escribir encabezados en los archivos
with open("estimations_puntual.py", "w") as f:
  f.write("probaPuntual = {\n")
with open("estimations_acumulada.py", "w") as f:
  f.write("probaAcumulada = {\n")
with open("estimations_puntual_ganador.py", "w") as f:
  f.write("probaPuntualGanador = {\n")
with open("estimations_puntual_reducido.py", "w") as f:
  f.write("probaReducido = {\n")

# Calcular las probabilidades para cada combinación de equipos
for equipoLocal in equipos:
  with open("estimations_puntual.py", "a") as f:
    f.write(f"\t'{equipoLocal}':{{\n")
  with open("estimations_acumulada.py", "a") as f:
    f.write(f"\t'{equipoLocal}':{{\n")
  with open("estimations_puntual_ganador.py", "a") as f:
    f.write(f"\t'{equipoLocal}':{{\n")
  with open("estimations_puntual_reducido.py", "a") as f:
    f.write(f"\t'{equipoLocal}':{{\n")

  for equipoVisitante in equipos:
    if equipoLocal != equipoVisitante:
      prob = probaPartido(equipoLocal, equipoVisitante)
      acumProb = np.cumsum(prob)
      acumProb /= acumProb[-1]
      
      with open("estimations_puntual.py", "a") as f:
        f.write(f"\t\t'{equipoVisitante}':[{','.join(map(str, prob))}],\n")
      with open("estimations_acumulada.py", "a") as f:
        f.write(f"\t\t'{equipoVisitante}':[{','.join(map(str, acumProb))}],\n")
      
      resultado = [
        sum(prob[0:15]), sum(prob[15:21]), sum(prob[21:36])
      ] / sum(prob)
      with open("estimations_puntual_ganador.py", "a") as f:
        f.write(f"\t\t'{equipoVisitante}':[{','.join(map(str, resultado))}],\n")
      
      prob_red = probaDoblePartido(equipoLocal, equipoVisitante)
      with open("estimations_puntual_reducido.py", "a") as f:
        f.write(f"\t\t'{equipoVisitante}':[{','.join(map(str, prob_red))}],\n")

  with open("estimations_puntual.py", "a") as f:
    f.write("\t},\n")
  with open("estimations_acumulada.py", "a") as f:
    f.write("\t},\n")
  with open("estimations_puntual_ganador.py", "a") as f:
    f.write("\t},\n")
  with open("estimations_puntual_reducido.py", "a") as f:
    f.write("\t},\n")

# Escribir el cierre de los diccionarios
with open("estimations_puntual.py", "a") as f:
  f.write("}\n")
with open("estimations_acumulada.py", "a") as f:
  f.write("}\n")
with open("estimations_puntual_ganador.py", "a") as f:
  f.write("}\n")
with open("estimations_puntual_reducido.py", "a") as f:
  f.write("}\n")
