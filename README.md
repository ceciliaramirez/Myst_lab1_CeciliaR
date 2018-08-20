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
