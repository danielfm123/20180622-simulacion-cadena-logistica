library(tidyverse)

## Parametros
# geografía
tiendas = 100
# bodegas = 5
lado = 20
#logistica
costo_despacho = 100
costo_km = 2
restock_tienda = 7
restock_bodega = 30
periodo = 30
#Ventas
sd_ventas_global = 100
nivel_servicio = 0.99
# Costo stock
costo_unidad = 100
factor_dcto_anual = 0.2
sku = 10
# Otros
semilla = 123

get_costs = function(bodegas){
  # costo tienda
  sd_tienda_sku = sd_ventas_global/sqrt(tiendas*sku)
  stock_seguriad_tienda = sku * sqrt(restock_tienda) * qnorm(nivel_servicio,0,sd_tienda_sku)
  valor_stock_seguriad_tienda = stock_seguriad_tienda * costo_unidad
  costo_stock_seguriad_tienda = valor_stock_seguriad_tienda * factor_dcto_anual^(periodo/365)
  
  # Costo bodega
  sd_bodega_sku = sd_ventas_global/sqrt(bodegas*sku)
  stock_seguriad_bodega = sqrt(restock_bodega) * qnorm(nivel_servicio,0,sd_bodega_sku)
  valor_stock_seguriad_bodega = stock_seguriad_bodega * costo_unidad
  costo_stock_seguriad_bodega = valor_stock_seguriad_bodega * factor_dcto_anual^(periodo/365)
  
  set.seed(semilla)
  coord = data.frame(x = runif(tiendas,0,lado),y = runif(tiendas,0,lado))
  
  fit = kmeans(coord,bodegas,nstart = 10)
  zona = as.factor(fit$cluster)
  center = data.frame(fit$centers, zona = as.factor(1:bodegas))
  # 
  # ggplot() +
  #   geom_point(aes(x,y, color = zona), coord) +
  #   geom_point(aes(x,y),center, size = 3)
  
  dataset_tienda = mutate(coord,zona = zona) %>% 
    left_join(center,by=c("zona"),suffix = c("","_bodega")) %>% 
    mutate(dist_bodega = sqrt((x-x_bodega)^2 + (y-y_bodega)^2),
           costo_logistico = (dist_bodega * costo_km + costo_despacho) * periodo/restock_tienda,
           costo_stock_seguridad = costo_stock_seguriad_tienda)
  
  dataset_bodega = mutate(center,
                          costo_stock_seguridad = costo_stock_seguriad_bodega)
  
  data.frame(bodegas = bodegas,
             costo_logistica = sum(dataset_tienda$costo_logistico),
             costo_stock_seguridad_tienda = sum(dataset_tienda$costo_stock_seguridad),
             costo_stock_seguridad_bodega = sum(dataset_bodega$costo_stock_seguridad))
}

map_dfr(1:10,get_costs)
