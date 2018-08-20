# Myst_lab1_CeciliaR
## Laboratorio 1

# Myst_lab1_Cecilia

#remover environment
rm(list=ls())

#0s aceptados antes de expresar una cifra en notacion cientifica
options("scipen"=100, "digits"=4)

#cargar librerias
suppressMessages(library(plotly)) #graficas interactivas
suppressMessages(library(Quandl)) #descargar precios
suppressMessages(library(PortfolioAnalytics)) #teoria moderna portafolios
suppressMessages(library(ROI))  #Optimizacion portafolios
suppressMessages(library(kableExtra)) #tablas html

#Cargar token QUANDL
Quandl.api_key("wPSZaujksYoP9nN2szov_i")

#Funcion descargar precios
Bajar_Precios <- function(Columns, Ticker, Fecha_In, Fecha_Fn) {
  
  #Columns: c("date", "adj_close",...)
  #Tickers: character: "TSLA"
  #Fecha_In: "2017-01-02"
  #Fecha_Fn: "2017-08-02"
  
#Peticion para descargar precios
  Datos<- Quandl.datatable(code= "WIKI/PRICES", qopts.columns=Columns,ticker=Ticker, date.gte=Fecha_In, date.lte=Fecha_Fn)
  
  return(Datos)
}

#Tickers QUANDL

tk<- c("TSLA", "BBY", "HD")
cs <- c("date", "adj_close")

#Fechas
fs <- c("2015-08-01", "2016-08-01")

#capital inicial
Capital_Inicial <- 100000
Comision <- 0.005

#descargar precios y calcular rend
Datos<-list()

for(i in 1:length(tk)) {
  Datos [[i]] <- Bajar_Precios(Columns = cs, Ticker=tk[i],Fecha_In = fs[1], Fecha_Fn = fs[2] )
  
}
  
names(Datos) <-tk
