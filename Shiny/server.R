
shinyServer(function(input, output) {
  
  ### SECCIÓN MUNDIAL ###
  
  base_reactiva1 <- reactive({
    selecc <- owid_m_casos %>% 
      filter(Variable1 == input$var_inter_casos)
  })
  
  output$plot_inter_casos <- renderPlotly({
    plot_ly(base_reactiva1()) %>% 
      add_trace(base_reactiva1(), x = ~Fecha, y = ~Casos, type = "scatter", mode = "lines", name = "", color = I("#541388")) %>%
      layout(title = paste0("\n", input$var_inter_casos))
    
  })
  
  base_reactiva2 <- reactive({
    selecc <- owid_m_muertes %>% 
      filter(Variable2 == input$var_inter_muertes)
  })
  
  output$plot_inter_muertes <- renderPlotly({
    plot_ly(base_reactiva2()) %>% 
      add_trace(base_reactiva2(), x = ~Fecha, y = ~Muertes, type = "scatter", mode = "lines", name = "", color = I("#D90368")) %>%
      layout(title = paste0("\n", input$var_inter_muertes))
    
  })
  
  output$plot_inter_vacunas <- renderPlotly({
    plot_ly(owid_m, x = ~fecha, y = ~people_vaccinated,  
            type = 'scatter', mode = 'lines',
            line = list(color = "#84165B", width = 3), name="Parcialmente (1 dosis)") %>%
      add_lines(y= ~people_fully_vaccinated, line = list(color = "#2E294E", width = 3), 
                name="Totalmente") %>%
      layout(legend=list(x=100, y=0.5, title=list(text="Vacunados")),
             font=list(size=14),
             xaxis = list(title = "Fecha"), 
             yaxis = list(title = "Personas vacunadas"))
    
  })
  
  base_reactiva3 <- reactive({
    selecc <- owid_cont %>% 
      filter(Variable3 == input$var_inter_continentes)
  })
  
  output$plot_inter_continentes <- renderPlotly({
    plot_ly(base_reactiva3()) %>% 
      add_trace(base_reactiva3(), x = ~Fecha, y = ~ Africa, type = "scatter", mode = "lines", name = "Africa", color = I("#50514F")) %>%
      add_trace(base_reactiva3(), x = ~Fecha, y = ~ Asia, type = "scatter", mode = "lines", name = "Asia", color = I("#F25F5C")) %>%
      add_trace(base_reactiva3(), x = ~Fecha, y = ~ Europa, type = "scatter", mode = "lines", name = "Europa", color = I("#FFE066")) %>%
      add_trace(base_reactiva3(), x = ~Fecha, y = ~ Norteamerica, type = "scatter", mode = "lines", name = "Norteamerica", color = I("#247BA0")) %>%
      add_trace(base_reactiva3(), x = ~Fecha, y = ~ Oceania, type = "scatter", mode = "lines", name = "Ocenia", color = I("#70C1B3")) %>%
      add_trace(base_reactiva3(), x = ~Fecha, y = ~ Sudamerica, type = "scatter", mode = "lines", name = "Sudamerica", color = I("#F9A061")) %>%
      layout(title = paste0("\n", input$var_inter_continentes), yaxis = list(title = 'Valor'))
  })
  
  base_reactiva4 <- reactive({
    selecc <- owid_barras_casos %>% 
      filter(Continente == input$var_cont_casos)
  })
  
  output$plot_casos_continentes <- renderPlotly({
    plot_ly(base_reactiva4()) %>% 
      add_trace(base_reactiva4(), x = ~Fecha, y = ~ NuevosCasos, type = "bar", mode = "lines", name = "", color = I("#ae2012")) %>%
      layout(title = paste0("\n", input$var_cont_casos), yaxis = list(title = 'Casos'))
  })
  
  base_reactiva5 <- reactive({
    selecc <- owid_barras_muertes %>% 
      filter(Continente == input$var_cont_muertes)
  })
  
  output$plot_muertes_continentes <- renderPlotly({
    plot_ly(base_reactiva5()) %>% 
      add_trace(base_reactiva5(), x = ~Fecha, y = ~ NuevasMuertes, type = "bar", mode = "lines", name = "", color = I("#0a9396")) %>%
      layout(title = paste0("\n", input$var_cont_muertes), yaxis = list(title = 'Muertes'))
  })
  
  # Mapas #
  
  # Casos por millón
  
  output$mapamundial1 <- renderPlotly({
    plot_geo(owidmapas, locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code,
                z = ~total_cases_per_million, 
                color = ~total_cases_per_million, 
                colorscale="Viridis",
                text=~location) %>%
      colorbar(title = "Casos totales por millon")
    
  })
  
  
  output$mapaNA1 <- renderPlotly({
    subset(owidmapas2, continent == "North America") %>% 
    plot_geo(locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code, 
                z = ~total_cases_per_million, 
                color = ~total_cases_per_million, 
                colorscale="Viridis", 
                text=~location) %>%
      layout(geo = list(scope="north america")) %>% 
      layout(autosize = F, width = 900, height = 600) %>%
      colorbar(title = "Casos totales por millón")
  })
  
  output$mapaSA1 <- renderPlotly({
    subset(owidmapas2, continent == "South America") %>% 
    plot_geo(locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code, 
                z = ~total_cases_per_million, 
                color = ~total_cases_per_million, 
                colorscale="Viridis", 
                text=~location) %>%
      layout(geo = list(scope="south america")) %>% 
      layout(autosize = F, width = 900, height = 600) %>%
      colorbar(title = "Casos totales por millón")
  })
  
  output$mapaEUR1 <- renderPlotly({
    subset(owidmapas2, continent == "Europe") %>% 
    plot_geo(locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code, 
                z = ~total_cases_per_million, 
                color = ~total_cases_per_million, 
                colorscale="Viridis", 
                text=~location) %>%
      layout(geo = list(scope="europe")) %>% 
      layout(autosize = F, width = 900, height = 600) %>%
      colorbar(title = "Casos totales por millón")
  })
  
  output$mapaASIA1 <- renderPlotly({
    subset(owidmapas2, continent == "Asia") %>% 
    plot_geo(locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code, 
                z = ~total_cases_per_million, 
                color = ~total_cases_per_million, 
                colorscale="Viridis", 
                text=~location) %>%
      layout(geo = list(scope="asia")) %>% 
      layout(autosize = F, width = 900, height = 600) %>%
      colorbar(title = "Casos totales por millón")
  })
  
  output$mapaAFR1 <- renderPlotly({
    subset(owidmapas2, continent == "Africa") %>% 
    plot_geo(locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code, 
                z = ~total_cases_per_million, 
                color = ~total_cases_per_million, 
                colorscale="Viridis", 
                text=~location) %>%
      layout(geo = list(scope="africa")) %>% 
      layout(autosize = F, width = 900, height = 600) %>%
      colorbar(title = "Casos totales por millón")
  })
  

  
  
  
  # Muertes por millón
  
  output$mapamundial2 <- renderPlotly({
    plot_geo(owidmapas, locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code,
                z = ~total_deaths_per_million, 
                color = ~total_deaths_per_million, 
                colorscale="Viridis",
                text=~location) %>%
      colorbar(title = "Muertes totales por millon")
    
  })
  
  
  output$mapaNA2 <- renderPlotly({
    subset(owidmapas2, continent == "North America") %>% 
    plot_geo(locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code, 
                z = ~total_deaths_per_million, 
                color = ~total_deaths_per_million, 
                colorscale="Viridis", 
                text=~location) %>%
      layout(geo = list(scope="north america")) %>% 
      layout(autosize = F, width = 900, height = 600) %>%
      colorbar(title = "MUertes totales por millón")
  })
  
  output$mapaSA2 <- renderPlotly({
    subset(owidmapas2, continent == "South America") %>% 
    plot_geo(locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code, 
                z = ~total_deaths_per_million, 
                color = ~total_deaths_per_million, 
                colorscale="Viridis", 
                text=~location) %>%
      layout(geo = list(scope="south america")) %>% 
      layout(autosize = F, width = 900, height = 600) %>%
      colorbar(title = "Muertes totales por millón")
  })
  
  output$mapaEUR2 <- renderPlotly({
    subset(owidmapas2, continent == "Europe") %>% 
    plot_geo(locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code, 
                z = ~total_deaths_per_million, 
                color = ~total_deaths_per_million, 
                colorscale="Viridis", 
                text=~location) %>%
      layout(geo = list(scope="europe")) %>% 
      layout(autosize = F, width = 900, height = 600) %>%
      colorbar(title = "Muertes totales por millón")
  })
  
  output$mapaASIA2 <- renderPlotly({
    subset(owidmapas2, continent == "Asia") %>% 
    plot_geo(locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code, 
                z = ~total_deaths_per_million, 
                color = ~total_deaths_per_million, 
                colorscale="Viridis", 
                text=~location) %>%
      layout(geo = list(scope="asia")) %>% 
      layout(autosize = F, width = 900, height = 600) %>%
      colorbar(title = "Muertes totales por millón")
  })
  
  output$mapaAFR2 <- renderPlotly({
    subset(owidmapas2, continent == "Africa") %>% 
    plot_geo(locationmode="ISO-3", frame=~datemonth) %>% 
      add_trace(locations = ~iso_code, 
                z = ~total_deaths_per_million, 
                color = ~total_deaths_per_million, 
                colorscale="Viridis", 
                text=~location) %>%
      layout(geo = list(scope="africa")) %>% 
      layout(autosize = F, width = 900, height = 600) %>%
      colorbar(title = "Muertes totales por millón")
  })
  
  ### SECCIÓN LATINOAMÉRICA ###
  
  base_reactiva6 <- reactive({
    selecc <- owidLA %>% 
      filter(Variable4 == input$var_la_paises)
  })
  
  output$plot_la_paises <- renderPlotly({
    plot_ly(base_reactiva6()) %>% 
      add_trace(base_reactiva6(), x = ~Fecha, y = ~ Argentina, type = "scatter", mode = "lines", name = "Argentina", color = I("#235789")) %>%
      add_trace(base_reactiva6(), x = ~Fecha, y = ~ Brazil, type = "scatter", mode = "lines", name = "Brasil", color = I("#72405C")) %>%
      add_trace(base_reactiva6(), x = ~Fecha, y = ~ Colombia, type = "scatter", mode = "lines", name = "Colombia", color = I("#C1292E")) %>%
      add_trace(base_reactiva6(), x = ~Fecha, y = ~ Mexico, type = "scatter", mode = "lines", name = "Mexico", color = I("#D97E18")) %>%
      add_trace(base_reactiva6(), x = ~Fecha, y = ~ Peru, type = "scatter", mode = "lines", name = "Peru", color = I("#847614")) %>%
      layout(title = paste0("\n", input$var_la_paises), yaxis = list(title = 'Valor'))
  })
  
  # Barras #
  
  base_reactiva7 <- reactive({
    selecc <- owid_barras_casos_la %>% 
      filter(Pais == input$var_la_paises_casos)
  })
  
  output$plot_la_paises_casos <- renderPlotly({
    plot_ly(base_reactiva7()) %>% 
      add_trace(base_reactiva7(), x = ~Fecha, y = ~ NuevosCasos, type = "bar", mode = "lines", name = "", color = I("#ae2012")) %>%
      layout(title = paste0("\n", input$var_la_paises_casos), yaxis = list(title = 'Casos'))
  })
  
  base_reactiva8 <- reactive({
    selecc <- owid_barras_muertes_la %>% 
      filter(Pais == input$var_la_paises_muertes)
  })
  
  output$plot_la_paises_muertes <- renderPlotly({
    plot_ly(base_reactiva8()) %>% 
      add_trace(base_reactiva8(), x = ~Fecha, y = ~ NuevasMuertes, type = "bar", mode = "lines", name = "", color = I("#213d65")) %>%
      layout(title = paste0("\n", input$var_la_paises_muertes), yaxis = list(title = 'Muertes'))
  })
  
  base_reactiva9 <- reactive({
    selecc <- owid_barras_vacunas_la %>% 
      filter(Pais == input$var_la_paises_vacunas)
  })
  
  output$plot_la_paises_vacunas <- renderPlotly({
    plot_ly(base_reactiva9()) %>% 
      add_trace(base_reactiva9(), x = ~Fecha, y = ~ NuevasVacunas, type = "bar", mode = "lines", name = "", color = I("#ee9b00")) %>%
      layout(title = paste0("\n", input$var_la_paises_vacunas), yaxis = list(title = 'Vacunas'))
  })
    
    
})
