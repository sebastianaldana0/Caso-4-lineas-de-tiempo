library(quantmod)

# Descargo la serie de interes desde yahoo

serie <- getSymbols('NFLX',src='yahoo',auto.assign=FALSE,
                    from="2015-01-01")

View(serie)
names(serie)

plot(serie$`NFLX.Close`)



# Actualmente se puede utilizar esta otra librerC-a
library(yahoofinancer)

maxDate = "2005-01-01"

#Serie a descargar
tick<-"NFLX"


accion_down <- Ticker$new(tick)

# Cargo los datos desde la API
prices<- accion_down$get_history(start = maxDate, interval = '1d')


View(prices)


# Esta libreria me permite convertir la data a serie de tiempo
library(xts)

# Tomar la columna 'close' y convertirla a xts
close_prices <- xts(prices$close, order.by = as.Date(prices$date))


# Grafico
plot(close_prices, main="Precio de cierre NFLX")


accion<-close_prices

length(accion)

ventana<-window(accion, end = "2024-06-06") # ventana de entrenamiento
ventana2<-window(accion, start = "2024-06-07") # ventana de prueba


library(fpp2)

autoplot(ventana)


ggAcf(ventana)

library(tseries)
adf.test(ventana)


miserie<-diff(ventana) %>% na.omit()
autoplot(miserie)

library(tseries)
ggAcf(miserie)
adf.test(miserie)

library(gridExtra)

grid.arrange(ggAcf(miserie),
             ggPacf(miserie),
             nrow=1
)



auto.arima(ventana)  # PELIGRO


modelo1 <- Arima(ventana, order = c(0,1,0)) # Importante comparar el AICc con el autoarima

modelo2 <- Arima(ventana, order = c(5,1,5))


checkresiduals(modelo1)

checkresiduals(modelo2)

accuracy(modelo1)
accuracy(modelo2)


library(tidyverse)
#Pronostico
modelo2 %>% 
  forecast(h=5,level = 0.95)  # (Realizo 5 pronósticos)

#Miro los valores verdaderos
accion[4891:4895]


#Gráfico
modelo2 %>% 
  forecast(h=5) %>%  # (Realizo 5 pronósticos)
  autoplot(include=80)   # Gráfico los últimos 80 valores + pronóstico (se puede cambiar el 80)


# Con la función xts(serie, order=date) se cargan los datos




library(readxl)
Data <- read_excel("Datos/BaseEjemplo.xlsx", col_types = c("date", 
                                                           "numeric"))
View(Data)


Serie<-xts(Data$Y,order.by = Data$fecha)

autoplot(Serie)




