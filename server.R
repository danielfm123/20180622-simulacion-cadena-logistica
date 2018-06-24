#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(formattable)

periodo = 30

shinyServer(function(input, output) {
   
  values = reactiveValues()

  observe({
    set.seed(input$semilla)
    values$coord = data.frame(x = runif(input$tiendas,0,input$lado),y = runif(input$tiendas,0,input$lado))
  })
  
  output$geo = renderPlot({
    fit = kmeans(values$coord,input$bodegas,nstart = 10)
    zona = as.factor(fit$cluster)
    center = data.frame(fit$centers, zona = as.factor(1:input$bodegas))

    ggplot() +
      geom_point(aes(x,y, color = zona), values$coord) +
      geom_point(aes(x,y),center, size = 3)
  })
  
  get_costs = function(bodegas){
    # costo tienda
    sd_tienda_sku = input$sd_ventas_global/sqrt(input$tiendas*input$sku)
    stock_seguriad_tienda = input$sku * sqrt(input$restock_tienda) * qnorm(1-(1-input$nivel_seguridad)/2,0,sd_tienda_sku)
    valor_stock_seguriad_tienda = stock_seguriad_tienda * input$costo_unidad
    costo_stock_seguriad_tienda = valor_stock_seguriad_tienda * input$factor_dcto^(periodo/365)
    
    
    # Costo bodega
    sd_bodega_sku = input$sd_ventas_global/sqrt(bodegas*input$sku)
    stock_seguriad_bodega = sqrt(input$restock_bodega) * qnorm(1-(1-input$nivel_seguridad)/2,0,sd_bodega_sku)
    valor_stock_seguriad_bodega = stock_seguriad_bodega * input$costo_unidad
    costo_stock_seguriad_bodega = valor_stock_seguriad_bodega * input$factor_dcto^(periodo/365)
    # print(costo_stock_seguriad_bodega)
    

    set.seed(input$semilla)
    coord = data.frame(x = runif(input$tiendas,0,input$lado),y = runif(input$tiendas,0,input$lado))

    fit = kmeans(coord,bodegas,nstart = 10)
    zona = as.factor(fit$cluster)
    center = data.frame(fit$centers, zona = as.factor(1:bodegas))
    # print(center)
    
    dataset_tienda = mutate(coord,zona = zona) %>%
      left_join(center,by=c("zona"),suffix = c("","_bodega")) %>%
      mutate(dist_bodega = sqrt((x-x_bodega)^2 + (y-y_bodega)^2),
             costo_logistico = (dist_bodega * input$costo_km + input$costo_despacho) * periodo/input$restock_tienda,
             costo_stock_seguridad = costo_stock_seguriad_tienda)
    # print(dataset_tienda)
    
    dataset_bodega = mutate(center,
                            costo_stock_seguridad = costo_stock_seguriad_bodega)
    
    data.frame(bodegas = bodegas,
               costo_logistica = sum(dataset_tienda$costo_logistico),
               costo_stock_seguridad_tienda = sum(dataset_tienda$costo_stock_seguridad),
               costo_stock_seguridad_bodega = sum(dataset_bodega$costo_stock_seguridad))
  }
  
  simulacion = reactive({
    dataset = map_dfr(1:10,get_costs) 
  })
  
  output$costos = renderPlot({
    dataset = simulacion() %>% 
      gather(linea,costo,-bodegas)
    gr = ggplot(dataset,aes(bodegas,costo,fill=linea)) + geom_bar(stat="identity")+
      scale_fill_discrete(guide = guide_legend())+
      theme(legend.position="bottom")
    # ggplotly(gr)
    gr
  })
  
  output$tabla_costos = renderFormattable({
    dataset = mutate_at(simulacion(),vars(-bodegas),
                        accounting,digits= 0L
                     )
    print(dataset)
    formattable(dataset,
                list(
                  costo_logistica = color_bar("pink"),
                  costo_stock_seguridad_tienda = color_bar("lightgreen"),
                  costo_stock_seguridad_bodega = color_bar("lightblue")
                  )
                )
  })
})