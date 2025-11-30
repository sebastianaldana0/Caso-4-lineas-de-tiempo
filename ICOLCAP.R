# Librerias

library(readxl)
library(tidyverse)
library(xts)
library(fpp2)
library(tseries)
library(gridExtra)

# Base de datos

Base_datos=read_excel("ICOLCAP.xlsx") %>%
  rename(Fecha=1,Cierre=2)

Serie<-xts(Base_datos$Cierre,order.by = Base_datos$Fecha)

autoplot(Serie)

accion<-Serie %>% na.omit()

length(accion)

ventana<-window(accion, end = "2024-06-06") # ventana de entrenamiento
ventana2<-window(accion, start = "2024-06-07") # ventana de prueba

# Graficos de ACF Y test de 

autoplot(ventana)
ggAcf(ventana)
adf.test(ventana)

# Se diferencia la serie

dif1serie<-diff(ventana) %>% na.omit()
autoplot(dif1serie)

# Grafico de la nueva seria diferenciada

ggAcf(dif1serie)
adf.test(dis1serie)

# ACF VS PACF

grid.arrange(ggAcf(dif1serie),
             ggPacf(dif1serie),
             nrow=1
)

# Autoarima

auto.arima(ventana)
# (2,1,1)

# Comparaciones de modelos

modelo1 <- Arima(ventana, order = c(2,1,1)) # Modelo de autoarima

modelo2 <- Arima(ventana, order = c(0,1,2))

modelo3 <- Arima(ventana, order = c(2,1,0))

# Graficos de residuales

checkresiduals(modelo1)

checkresiduals(modelo2)

checkresiduals(modelo3)

# Hipotesis de la prueba Ljung-box test
#La prueba de Ljung-Box tiene una hipótesis nula H0 de que los residuos
#de una serie temporal son independientes y no están correlacionados, y una 
#hipótesis alternativa H1 de que los residuos exhiben autocorrelación.

# Indicadores del modelo

accuracy(modelo1)
accuracy(modelo2)
accuracy(modelo3)

#ME (Mean Error): Promedio de los errores de predicción. Indica si el modelo 
#tiende a sobrestimar (positivo) o subestimar (negativo) los valores reales.

#RMSE (Root Mean Square Error) indica el error típico de tus predicciones en las mismas unidades
#de tu variable original - un valor más bajo significa que tus predicciones caen 
#más cerca de los valores reales. 

#MAE (Mean Absolute Error) es similar pero menos sensible a valores atípicos, dándote una visión 
#más robusta del error promedio

#MPE (Mean Percentage Error) Error porcentual promedio. Indica el sesgo direccional en términos porcentuales.

#MAPE es particularmente útil porque expresa el error en términos porcentuales, 
#lo que te permite entender fácilmente que tu modelo, por ejemplo, tiene un error
#promedio del 5%, lo que significa que tus predicciones se desvían typicalmente un 5% de los valores reales.

#MASE es quizás uno de los indicadores más reveladores, ya que te dice cómo se 
#desempeña tu modelo comparado con un pronóstico simple "naïve" que solo usa el 
#valor anterior. Un MASE menor que 1 indica que tu modelo ARIMA es mejor que este
#benchmark simple, mientras que un valor mayor que 1 sugiere que incluso un modelo básico podría superarlo

#ACF1 (Autocorrelation Function at lag 1): Mide la correlación de los residuos 
#con su primer retardo. Evalúa si quedan patrones no capturados en los residuos.

# Valores optimos

#ME: 0 - Indica que no hay sesgo sistemático (ni sobrestimación ni subestimación)

#RMSE: Más cercano a 0 posible - Error mínimo en las predicciones

#MAE: Más cercano a 0 posible - Error absoluto mínimo

#MPE: 0% - Sin sesgo porcentual direccional

#MAPE:
  
 # < 10%: Excelente

#10-20%: Bueno

#20-50%: Aceptable

#> 50%: Pobre

#MASE: < 1 - Mejor que el modelo naïve de referencia

#ACF1: 0 - Residuos no autocorrelacionados (ruido blanco)

#Pronosticos
modelo1 %>% 
  forecast(h=5,level = 0.95)%>%  
  autoplot(include=80)

modelo2 %>% 
  forecast(h=5,level = 0.95)%>%  
  autoplot(include=80)

modelo3 %>% 
  forecast(h=5,level = 0.95) %>%  
  autoplot(include=80)



