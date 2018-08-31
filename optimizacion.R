# Setup
library(tidyverse)
library(future)
library(tictoc)
plan(multiprocess)
options(dplyr.width = Inf)

# Parametros
costo_despacho = 100
tasa_dcto_anual = 0.2
costo_motoboy = 10
periodo = 30 #dia

# Data se ventas
precio = c(50,100,200,400,800)
venta_dia = c(0.01,0.1,0.5,1,5)
porcentaje_margen = c(0.25,0.30,0.35)

productos = crossing(precio,venta_dia,porcentaje_margen) %>% 
  mutate(costo_unidad = (1-porcentaje_margen)*precio,
         margen_unidad = precio * porcentaje_margen)


getSimulation = function(nivel_servicio = 0.95,producto){
  
  # producto %>% mutate(
    with(producto,{
      nivel_servicio = nivel_servicio
      venta_esperada = venta_dia * dias_restock
      stock_provicionado = qpois(nivel_servicio, venta_esperada)
      nivel_servicio_real = ppois(stock_provicionado,venta_esperada)
      venta_tienda = map2_dbl(venta_esperada, nivel_servicio, function(v,n){
        mean(qpois(seq(from = 0, to = nivel_servicio_real, length.out = 1000),v))
      })
      venta_motoboy = ifelse(motoboy,venta_esperada - venta_tienda,0)
      costo_motoboy = venta_motoboy * costo_motoboy
      venta_real = venta_motoboy + venta_tienda
      margen_venta_esperado = margen_unidad * venta_real
      inventario_promedio = max(0,stock_provicionado - venta_esperada / 2)
      valor_inventario_promedio = inventario_promedio * costo_unidad
      costo_inventario = valor_inventario_promedio * ((1 + tasa_dcto_anual) ^ (dias_restock / 360) - 1)
      margen_global = margen_venta_esperado - costo_inventario - costo_motoboy
    }) 
    data.frame(producto,nivel_servicio,venta_esperada,stock_provicionado,nivel_servicio_real,venta_tienda)
}
tic()
getSimulation(0.95,productos[1,])
toc()
# map_dbl(seq(from = 0,to = .99, by = 0.01),margenAux,productos[61,])
#Variables desicion

dias_restock = 7
motoboy = F

optimo_por_producto = map(1:nrow(productos),function(p){
  producto = productos[p,]
  future({
    simulaciones = map_dfr(seq(from = 0,to = .99, by = 0.01),getSimulation,producto)
    simulaciones[which.max(simulaciones$margen_global),]
  })
})
optimo_por_producto = map_dfr(optimo_por_producto,value)

(sum(optimo_por_producto$margen_global) - costo_despacho) * (periodo/dias_restock)
