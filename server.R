#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {
   
  values = reactiveValues()


  output$geo = renderPlot({
    set.seed(input$semilla)
    values$coord = data.frame(x = runif(input$tiendas,0,input$lado),y = runif(input$tiendas,0,input$lado))
    fit = kmeans(values$coord,input$bodegas,nstart = 10)
        
    isolate({
      values$zona = as.factor(fit$cluster)
      values$center = data.frame(fit$centers, zona = as.factor(1:input$bodegas))
      
      ggplot() +
        geom_point(aes(x,y, color = values$zona), values$coord) +
        geom_point(aes(x,y),values$center, size = 3)
    })
  })
  
  observe({
    dcto_periodo = ((1+input$factor_dcto)^(periodo/365)-1)
    
    # costo tienda
    sd_tienda_sku = input$sd_ventas
    stock_seguriad_tienda = input$sku * sqrt(input$restock_tienda) * qnorm(input$nivel_seguridad,0,sd_tienda_sku)
    valor_stock_seguriad_tienda = stock_seguriad_tienda * input$costo_unidad
    costo_stock_seguriad_tienda = valor_stock_seguriad_tienda * dcto_periodo
    
    # Costo bodega
    sd_bodega_sku = input$sd_ventas
    stock_seguriad_bodega = input$sku * sqrt(input$restock_bodega) * qnorm(input$nivel_seguridad,0,sd_bodega_sku)
    valor_stock_seguriad_bodega = stock_seguriad_bodega * input$costo_unidad
    costo_stock_seguriad_bodega = valor_stock_seguriad_bodega * dcto_periodo
    # print(costo_stock_seguriad_bodega)
    
    dataset_tienda = mutate(values$coord,zona = values$zona) %>%
      left_join(values$center,by=c("zona"),suffix = c("","_bodega")) %>%
      mutate(dist_bodega = sqrt((x-x_bodega)^2 + (y-y_bodega)^2),
             costo_logistico = (dist_bodega * input$costo_km + input$costo_despacho) * periodo/input$restock_tienda)
    # print(dataset_tienda)

    dataset_bodega = dataset_tienda %>% 
      group_by(zona) %>% 
      summarise(tiendas_zona = n()) %>% 
      mutate(costo_stock_seguriad_bodega = costo_stock_seguriad_bodega * sqrt(tiendas_zona))
    
    costos = data.frame(
      logistica_tiendas =      sum(dataset_tienda$costo_logistico),
      logistica_bodega =       input$costo_restock * periodo / input$restock_bodega * input$bodegas,
      operacion_bodegas =      input$costo_bodega * input$bodegas,
      stock_venta_tienda =     input$mean_ventas_global * (input$restock_tienda / 2) *  input$costo_unidad * dcto_periodo,
      stock_venta_bodega =     input$mean_ventas_global * (input$restock_bodega / 2) *  input$costo_unidad * dcto_periodo,
      stock_seguridad_tienda = costo_stock_seguriad_tienda*input$tiendas,
      stock_seguridad_bodega = sum(dataset_bodega$costo_stock_seguriad_bodega))
    
    values$costos = gather(costos, costo, monto)
    
    print(values$costos)
    
  })
  
  output$costo_total = renderText({
    paste("Costo Total:", accounting(sum(values$costos$monto),digits = 0L) )
  })
  
  # output$costos = renderPlot({
  #   gr = ggplot(values$costos,aes(costo,monto)) + geom_bar(stat="identity") + 
  #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
  #   # ggplotly(gr)
  #   gr
  # })

  output$tabla_costos = renderFormattable({
    dataset = mutate(values$costos,monto = accounting(monto,digits= 0L))
    formattable(dataset,align = c("l","r"),
                list(
                  monto = color_bar("pink")
                  )
                )
  })
})
