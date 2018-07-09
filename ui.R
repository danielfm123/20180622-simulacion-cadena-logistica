#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shinydashboard)

# Define UI for application that draws a histogram
sidebar = dashboardSidebar(
  hr(),
  menuItem("Geografia",
           sliderInput("tiendas", "Numero Tiendas:",20,2000,100),
           sliderInput("lado","Cara del Cuadrado",0,100,20),
           sliderInput("semilla","Escenario",0,100,1)
           ),
  menuItem("Logistica Bodega",
           sliderInput("bodegas","Numero de Bodegas",1,10,3),
           sliderInput("restock_bodega","Periodo Restock Bodega",1,120,30),
           numericInput("costo_bodega","Costo Fijo Mensual Bodega",3000,0),
           numericInput("costo_restock","Costo fijo restock bodega",300,0),
           numericInput("costo_importar","Costo Importacion",5000,0),
           sliderInput("nivel_seguridad_bodega","Nivel de Seguridad bodega",0.01,0.99,0.95,0.01)
           ),
  menuItem("Logistica Tienda",
           sliderInput("restock_tienda","Periodo Restock Tienda",1,120,7),
           numericInput("costo_km","Costo por Kilometro",5,0),
           numericInput("costo_despacho_tienda","Costo fijo Despacho",50,0),
           numericInput("costo_despacho_cliente","Costo despacho a Cliente",5,0),
           sliderInput("nivel_seguridad_tienda","Nivel de Seguridad Tienda",0.01,0.99,0.95,0.01)
           ),
  menuItem("Ventas",
           numericInput("mean_ventas_global","Promedio Ventas Diarias",1000,0),
           numericInput("sd_ventas","Desvest Ventas SKU x Tienda",1,0),
           numericInput("costo_unidad","Costo Unidad",100,0),
           numericInput("sku","Numero Sku",100,1),
           sliderInput("factor_dcto","Factor Descuento Anual",0,1,0.1,0.01)
           )
)

body = dashboardBody(
  box(
    title = "Geolicalización de Tiendas",
    solidHeader = T,
    collapsible = T,
    collapsed = T,
    plotOutput("geo"),
    width = "12"
  ),
  box(
    title = "Costos",
    solidHeader = T,
    collapsible = T,
    valueBoxOutput("costo_total",12),
    formattableOutput("tabla_costos"),
    width = "12"
  )
)

dashboardPage(
  dashboardHeader(title = "Logistica"),
  sidebar,
  body
)

