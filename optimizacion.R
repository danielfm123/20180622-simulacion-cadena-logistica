# Setup
library(tidyverse)
library(future.apply)
library(tictoc)
library(compiler)
plan(multiprocess)
options(dplyr.width = Inf)

# Parametros
costo_despacho = 1.5 # por equipo
tasa_dcto_anual = 0.2
costo_motoboy = 6

# Data se ventas
par_precio = c(50,100,200,400,800)
par_venta_dia = c(0.01,0.1,1,3)
par_porcentaje_margen = c(0.25,0.30,0.35)

productos = crossing(precio = par_precio,
                     venta_dia = par_venta_dia,
                     porcentaje_margen = par_porcentaje_margen) %>% 
  mutate(costo_unidad = (1-porcentaje_margen)*precio,
         margen_unidad = precio * porcentaje_margen)


getSimulation = function(nivel_servicio = 0.95,producto, dias_ciclo = 7, motoboy = 'auto' ){
  # producto = productos[1,]
  # attach(producto)
  # producto %>% mutate(
  with(producto,{
    nivel_servicio = nivel_servicio
    venta_esperada_ciclo = venta_dia * dias_ciclo
    stock_provicionado_ciclo = qpois(nivel_servicio, venta_esperada_ciclo)
    nivel_servicio_real = ppois(stock_provicionado_ciclo,venta_esperada_ciclo)
    # venta
    ## tienda
    venta_tienda_ciclo = map2_dbl(venta_esperada_ciclo, nivel_servicio_real, function(v,n){
      mean(qpois(seq(from = 0, to = nivel_servicio_real, length.out = 1000),v))
    })
    venta_tienda = venta_tienda_ciclo * 360/dias_ciclo
    margen_venta_tienda_ciclo = venta_tienda_ciclo * margen_unidad
    margen_venta_tienda = margen_venta_tienda_ciclo * 360/dias_ciclo
    ## motoboy
    venta_motoboy_ciclo = venta_esperada_ciclo - venta_tienda_ciclo
    venta_motoboy = venta_motoboy_ciclo * 360/dias_ciclo
    margen_venta_motoboy_ciclo = venta_motoboy_ciclo * (margen_unidad - costo_motoboy)
    margen_venta_motoboy = margen_venta_motoboy_ciclo * 360/dias_ciclo
    
    conMotoboy = if(motoboy == 'auto'){margen_venta_motoboy > 0}else{motoboy}
    
    venta_total = venta_tienda + conMotoboy*venta_motoboy
    margen_venta_total = margen_venta_tienda + conMotoboy*margen_venta_motoboy
    # inventario
    inventario_promedio = stock_provicionado_ciclo - venta_tienda_ciclo / 2
    valor_inventario_promedio = inventario_promedio * costo_unidad
    costo_inventario = valor_inventario_promedio * tasa_dcto_anual
    # logistica
    costo_logistica = venta_tienda * costo_despacho
    
    margen_global = margen_venta_total - costo_inventario - costo_logistica
    
    resumen = data.frame(producto,
                         venta_tienda,
                         stock_provicionado_ciclo,
                         nivel_servicio_real,
                         margen_venta_tienda,
                         conMotoboy,
                         venta_motoboy = conMotoboy*venta_motoboy,
                         margen_venta_motoboy = conMotoboy*margen_venta_motoboy,
                         venta_total,
                         margen_venta_total,
                         costo_inventario,
                         costo_logistica,
                         margen_global)
    return(resumen)
  }) 
}
getSimulation = cmpfun(getSimulation)
# tic()
# getSimulation(0.95,productos[1,],7,F)
# toc()

getOptimServiceLevel = function(productos,periodo,motoboy){
  optimo = map_dfr(split(productos,1:nrow(productos)),function(producto){
      simulaciones = map_dfr(seq(from = 0,to = .99, by = 0.01),getSimulation,producto,periodo,motoboy)
      simulaciones[which.max(simulaciones$margen_global),]
  })
  return(optimo)
} 

getMarginCurve = function(productos,periodos = 1:30,motoboy){
    curva = future_lapply(periodos,function(periodo){
      simulacion = getOptimServiceLevel(productos,periodo,motoboy)
      data.frame(
        periodo,
        motoboy,
        margen_anual = sum(simulacion$margen_global)
      )
    })
    return(map_dfr(curva,function(x) x))
}


curva = rbind(
  getMarginCurve(productos,1:30,T),
  getMarginCurve(productos,1:30,F),
  getMarginCurve(productos,1:30,'auto')
)
ggplot(curva,aes(periodo,margen_anual,color=motoboy)) + geom_line()
anaysis_case = getOptimServiceLevel(productos,3,F)
sum(anaysis_case$margen_global)
# optimo en 3 dias de despacho?
# costo despacho fijo 6000,1930878
# costo despacho fijo 1500,2022461
