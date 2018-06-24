#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Geografía"),
      sliderInput("tiendas", "Numero Tiendas:",20,2000,100),
      sliderInput("lado","Cara del Cuadrado",0,100,20),
      numericInput("semilla","semilla random",1,step = 1),
      h3("Logistica"),
      sliderInput("restock_bodega","Periodo Restock Bodega",1,120,30),
      sliderInput("restock_tienda","Periodo Restock Tienda",1,120,7),
      numericInput("costo_km","Costo por Kilometro",5,0),
      numericInput("costo_despacho","Costo fijo Despacho",50,0),
      h3("Ventas"),
      numericInput("sd_ventas_global","Desvest Ventas Global",100,0),
      sliderInput("nivel_seguridad","Nivel de Seguridad",0,1,0.95,0.01),
      numericInput("costo_unidad","Costo_Unidad",100,0),
      numericInput("sku","Numero Sku",10,1),
      sliderInput("factor_dcto","Factor Descuento Anual",0,1,0.1,0.01)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Geolicalización de Tiendas"),
      sliderInput("bodegas","Numero de Bodegas",1,10,5),
      plotOutput("geo",width = 550,height = 500),
      h3("Costos"),
      plotOutput("costos",width = 550,height = 500),
      formattableOutput("tabla_costos",width = 550)
    )
  )
))
