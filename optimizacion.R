library(tidyverse)

# Parametros
costo_despacho = 30
tasa_dcto_anual = 0.1
costo_motoboy = 10

# Data se ventas
precio = c(50,100,200,400,800)
venta_dia = c(0.1,0.5,1,5)
porcentaje_margen = c(0.25,0.30,0.35)

productos = crossing(precio,venta_dia,porcentaje_margen) %>% 
  mutate(costo = (1-porcentaje_margen)*precio,
         margen = precio * porcentaje_margen)

#Variables desicion
dias_restock = 7
motoboy = F

getSimulation = function(nivel_servicio = 0.95){
  
  productos %>% 
    mutate(venta_esperada = venta_dia * dias_restock,
           stock_provicionado = qpois(nivel_servicio,venta_esperada),
           venta_perdida = map())
  
}


pgamma(1,1,10)
pexp(1,100)
