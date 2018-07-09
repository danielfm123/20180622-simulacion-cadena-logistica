#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#


shinyServer(function(input, output) {
   
  values = reactiveValues()

  observe({
    set.seed(input$semilla)
    values$coord = data.frame(x = runif(input$tiendas,0,input$lado),y = runif(input$tiendas,0,input$lado))
    fit = kmeans(values$coord,input$bodegas,nstart = 10)
    
    isolate({
      values$zona = as.factor(fit$cluster)
      values$center = data.frame(fit$centers, zona = as.factor(1:input$bodegas))
    })
  })
  
  output$geo = renderPlot({
      ggplot() +
        geom_point(aes(x,y, color = values$zona), values$coord) +
        geom_point(aes(x,y),values$center, size = 3) +
        scale_color_discrete("zona\nbodega")
  })

  observe({
    dcto_periodo = ((1+input$factor_dcto)^(periodo/365)-1)

    # costo tienda
    sd_tienda_sku =               input$sd_ventas
    sd_tienda_sku_restock =       sd_tienda_sku * sqrt(input$restock_tienda) 
    stock_seguriad_tienda_sku =   qnorm(input$nivel_seguridad_tienda,0,sd_tienda_sku_restock)
    stock_seguriad_tienda =       input$sku * stock_seguriad_tienda_sku
    valor_stock_seguriad_tienda = stock_seguriad_tienda * input$costo_unidad
    costo_stock_seguriad_tienda = valor_stock_seguriad_tienda * dcto_periodo

    
    dataset_tienda = mutate(values$coord,zona = values$zona) %>%
      left_join(values$center,by = c("zona"),suffix = c("","_bodega")) %>%
      mutate(dist_bodega =               sqrt((x-x_bodega)^2 + (y-y_bodega)^2),
             costo_logistico =           (dist_bodega * input$costo_km + input$costo_despacho_tienda) * periodo/input$restock_tienda,
             venta_perdida_condicional = E_p_mayor(input$nivel_seguridad_tienda,-stock_seguriad_tienda_sku,sd_tienda_sku_restock),
             ventas_sin_stock_esperada = venta_perdida_condicional * (1-input$nivel_seguridad_tienda) * input$sku * periodo/input$restock_tienda)
    # browser()
    # Costo bodega


    dataset_bodega = dataset_tienda %>%
      group_by(zona) %>%
      summarise(tiendas_zona = n()) %>%
      mutate(
        sd_bodega_sku =               input$sd_ventas * sqrt(tiendas_zona) * sqrt(input$restock_bodega),
        stock_seguriad_bodega =       input$sku * qnorm(input$nivel_seguridad_bodega,0,sd_bodega_sku),
        valor_stock_seguriad_bodega = stock_seguriad_bodega * input$costo_unidad,
        costo_stock_seguriad_bodega = valor_stock_seguriad_bodega * dcto_periodo
      )

    # browser()
    
    costos = data.frame(
      logistica_tiendas =      sum(dataset_tienda$costo_logistico),
      logistica_importacion =  input$costo_importar * periodo / input$restock_bodega,
      logistica_bodega =       input$costo_restock * periodo / input$restock_bodega * input$bodegas,
      operacion_bodegas =      input$costo_bodega * input$bodegas,
      stock_venta_tienda =     input$mean_ventas_global * (input$restock_tienda / 2) *  input$costo_unidad * dcto_periodo,
      stock_venta_bodega =     input$mean_ventas_global * (input$restock_bodega / 2) *  input$costo_unidad * dcto_periodo,
      stock_seguridad_tienda = costo_stock_seguriad_tienda*input$tiendas,
      stock_seguridad_bodega = sum(dataset_bodega$costo_stock_seguriad_bodega),
      despacho_gratis =        sum(dataset_tienda$ventas_sin_stock_esperada) * input$costo_despacho_cliente)

    values$costos = gather(costos, costo, monto)

    print(values$costos)

  })

  output$costo_total = renderValueBox({
    valueBox(
      "Costo Total",
      accounting(sum(values$costos$monto),digits = 0L) 
    )
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
