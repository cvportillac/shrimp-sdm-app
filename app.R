library(shiny)
library(leaflet)
library(terra)
library(sf)
library(viridis)
library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets)
library(DT)

# ============================================================================
# INTERFAZ DE USUARIO
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Modelos de Idoneidad Ambiental para camarones en el Océano Pacífico colombiano",
    titleWidth = 750
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Análisis para el Pacífico colombiano", tabName = "maps", icon = icon("map")),
      menuItem("Análisis por Unidad Ambiental Costera - UAC", tabName = "uacs", icon = icon("layer-group"))
    ),
    
    hr(),
    
    selectInput("species", 
                "Seleccionar Especie:",
                choices = c(
                  "Litopenaeus occidentalis" = "L.occidentalis",
                  "Xiphopenaeus riveti" = "X.rivetti",
                  "Solenocera agassizii" = "S.agassizii",
                  "Penaeus brevirostris" = "P.brevirostris",
                  "Penaeus californiensis" = "P.californiensis"
                ),
                selected = "L.occidentalis"),
    
    hr(),
    
    selectInput("month", 
                "Seleccionar Mes:",
                choices = c("Enero" = "jan", "Febrero" = "feb", "Marzo" = "mar",
                            "Abril" = "apr", "Mayo" = "may", "Junio" = "jun",
                            "Julio" = "jul", "Agosto" = "aug", "Septiembre" = "sep",
                            "Octubre" = "oct", "Noviembre" = "nov", "Diciembre" = "dec"),
                selected = "jan"),
    
    hr(),
    
    radioButtons("scenario",
                 "Escenario Futuro 2050:",
                 choices = c("Optimista (RCP 2.6)" = "26",
                             "Pesimista (RCP 8.5)" = "85"),
                 selected = "26"),
    
    hr(),
    
    sliderInput("threshold",
                "Umbral de presencia (%):",
                min = 10, max = 70, value = 50, step = 5),
    
    hr(),
    
    sliderInput("opacity",
                "Opacidad del raster:",
                min = 0.3, max = 1, value = 0.8, step = 0.1),
    
    pickerInput("palette",
                "Paleta de colores:",
                choices = c("viridis", "plasma", "inferno", "magma", "turbo"),
                selected = "viridis")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #ecf0f5; }
        .box { border-top: 3px solid #3c8dbc; }
        .leaflet-container { background: #ffffff; }
      "))
    ),
    
    tabItems(
      tabItem(tabName = "maps",
              fluidRow(
                infoBoxOutput("area_actual", width = 3),
                infoBoxOutput("area_futura", width = 3),
                infoBoxOutput("area_perdida", width = 3),
                infoBoxOutput("area_ganada", width = 3)
              ),
              
              fluidRow(
                box(title = textOutput("title_present"), status = "primary",
                    solidHeader = TRUE, width = 4, height = 600,
                    leafletOutput("map_present", height = 520)),
                
                box(title = textOutput("title_future"), status = "warning",
                    solidHeader = TRUE, width = 4, height = 600,
                    leafletOutput("map_future", height = 520)),
                
                box(title = "Cambios en Idoneidad Ambiental", status = "info",
                    solidHeader = TRUE, width = 4, height = 600,
                    leafletOutput("map_changes", height = 520))
              ),
              
              fluidRow(
                box(title = "Análisis Cuantitativo de Cambios", status = "info",
                    solidHeader = TRUE, width = 12,
                    plotOutput("changes_barplot", height = 300))
              )
      ),
      
      tabItem(tabName = "uacs",
              fluidRow(
                box(title = "Mapa con UACs", status = "primary",
                    solidHeader = TRUE, width = 12, height = 600,
                    leafletOutput("map_uacs", height = 520))
              ),
              
              fluidRow(
                box(title = "Análisis Detallado por UAC", status = "info",
                    solidHeader = TRUE, width = 12,
                    DTOutput("table_uac_complete"))
              ),
              
              fluidRow(
                box(title = "Comparación Visual por UAC", status = "primary",
                    solidHeader = TRUE, width = 12,
                    plotOutput("uac_comparison_plot", height = 400))
              )
      )
    )
  )
)

# ============================================================================
# SERVIDOR
# ============================================================================

server <- function(input, output, session) {
  
  # Ruta base y shapefile
  base_path <- "F:/App_camaron"
  uac_path <- "F:/App_camaron/Shapefiles/UACs_fixed.shp"
  
  # Nombres completos de especies para display
  species_names <- c(
    "L.occidentalis" = "Litopenaeus occidentalis",
    "X.rivetti" = "Xiphopenaeus riveti",
    "S.agassizii" = "Solenocera agassizii",
    "P.brevirostris" = "Penaeus brevirostris",
    "P.californiensis" = "Penaeus californiensis"
  )
  
  raster_data <- reactiveValues(
    present_raw = NULL,
    future_raw = NULL,
    present_prob = NULL,
    future_prob = NULL,
    present_binary = NULL,
    future_binary = NULL,
    changes = NULL,
    uacs = NULL,
    loaded = FALSE
  )
  
  load_raster_data <- function() {
    tryCatch({
      withProgress(message = 'Cargando datos...', value = 0, {
        
        # Construir rutas con la especie seleccionada
        species_path <- file.path(base_path, input$species)
        present_file <- file.path(species_path, paste0(input$month, "_pres.tif"))
        future_file <- file.path(species_path, paste0(input$month, "_2050_", input$scenario, ".tif"))
        
        incProgress(0.1, detail = "Verificando archivos...")
        
        if (!file.exists(present_file) || !file.exists(future_file)) {
          showNotification("Archivos raster no encontrados", type = "error", duration = 5)
          return(FALSE)
        }
        
        if (!file.exists(uac_path)) {
          showNotification("Shapefile UACs no encontrado", type = "error", duration = 5)
          return(FALSE)
        }
        
        incProgress(0.2, detail = "Cargando shapefile UACs...")
        uacs <- st_read(uac_path, quiet = TRUE)
        uacs <- st_transform(uacs, crs = 4326)
        raster_data$uacs <- uacs
        
        incProgress(0.3, detail = "Cargando rasters...")
        present_raw <- rast(present_file)
        future_raw <- rast(future_file)
        
        raster_data$present_raw <- present_raw
        raster_data$future_raw <- future_raw
        
        incProgress(0.4, detail = "Escalando a 0-100%...")
        present_prob <- (present_raw / 1000) * 100
        future_prob <- (future_raw / 1000) * 100
        
        raster_data$present_prob <- present_prob
        raster_data$future_prob <- future_prob
        
        incProgress(0.5, detail = "Binarizando...")
        pres_vals <- values(present_prob)
        fut_vals <- values(future_prob)
        
        pres_bin_vals <- ifelse(is.na(pres_vals), NA,
                                ifelse(pres_vals >= input$threshold, 1, 0))
        
        fut_bin_vals <- ifelse(is.na(fut_vals), NA,
                               ifelse(fut_vals >= input$threshold, 1, 0))
        
        present_bin <- present_prob
        values(present_bin) <- pres_bin_vals
        
        future_bin <- future_prob
        values(future_bin) <- fut_bin_vals
        
        raster_data$present_binary <- present_bin
        raster_data$future_binary <- future_bin
        
        incProgress(0.7, detail = "Clasificando cambios...")
        changes_vals <- rep(NA, length(pres_bin_vals))
        valid <- !is.na(pres_bin_vals) & !is.na(fut_bin_vals)
        
        changes_vals[valid & pres_bin_vals == 0 & fut_bin_vals == 1] <- 3
        changes_vals[valid & pres_bin_vals == 1 & fut_bin_vals == 0] <- 1
        changes_vals[valid & pres_bin_vals == 1 & fut_bin_vals == 1] <- 2
        changes_vals[valid & pres_bin_vals == 0 & fut_bin_vals == 0] <- 0
        
        changes <- present_bin
        values(changes) <- changes_vals
        
        raster_data$changes <- changes
        
        incProgress(1, detail = "¡Completado!")
        raster_data$loaded <- TRUE
        
        showNotification("Datos cargados exitosamente", type = "message", duration = 3)
        return(TRUE)
      })
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 10)
      raster_data$loaded <- FALSE
      return(FALSE)
    })
  }
  
  # Cargar datos automáticamente al iniciar
  observe({
    load_raster_data()
  })
  
  observeEvent(input$species, { if (raster_data$loaded) load_raster_data() }, ignoreInit = TRUE)
  observeEvent(input$month, { if (raster_data$loaded) load_raster_data() }, ignoreInit = TRUE)
  observeEvent(input$scenario, { if (raster_data$loaded) load_raster_data() }, ignoreInit = TRUE)
  observeEvent(input$threshold, { if (raster_data$loaded) load_raster_data() }, ignoreInit = TRUE)
  
  # TÍTULOS
  month_names <- c("jan"="Enero", "feb"="Febrero", "mar"="Marzo",
                   "apr"="Abril", "may"="Mayo", "jun"="Junio",
                   "jul"="Julio", "aug"="Agosto", "sep"="Septiembre",
                   "oct"="Octubre", "nov"="Noviembre", "dec"="Diciembre")
  
  output$title_present <- renderText({
    paste("Distribución Presente -", species_names[input$species], "-", month_names[input$month])
  })
  
  output$title_future <- renderText({
    paste("Proyección 2050 (RCP", input$scenario, ") -", species_names[input$species], "-", month_names[input$month])
  })
  
  # MAPAS PRINCIPALES
  output$map_present <- renderLeaflet({
    req(raster_data$present_prob)
    
    pal <- colorNumeric(
      palette = input$palette,
      domain = c(0, 100),
      na.color = "transparent"
    )
    
    m <- leaflet() %>%
      setView(lng = -78.5, lat = 3, zoom = 7) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addRasterImage(raster_data$present_prob, colors = pal, opacity = input$opacity) %>%
      addLegend(
        pal = pal,
        values = c(0, 25, 50, 75, 100),
        title = "Probabilidad (%)",
        position = "bottomright",
        opacity = 1
      ) %>%
      addScaleBar(position = "bottomleft")
    
    m %>% onRender("
      function(el, x) {
        var map = this;
        map.on('moveend', function() {
          Shiny.setInputValue('sync_present', {
            center: map.getCenter(),
            zoom: map.getZoom(),
            timestamp: Date.now()
          });
        });
      }
    ")
  })
  
  output$map_future <- renderLeaflet({
    req(raster_data$future_prob)
    
    pal <- colorNumeric(
      palette = input$palette,
      domain = c(0, 100),
      na.color = "transparent"
    )
    
    m <- leaflet() %>%
      setView(lng = -78.5, lat = 3, zoom = 7) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addRasterImage(raster_data$future_prob, colors = pal, opacity = input$opacity) %>%
      addLegend(
        pal = pal,
        values = c(0, 25, 50, 75, 100),
        title = "Probabilidad (%)",
        position = "bottomright",
        opacity = 1
      ) %>%
      addScaleBar(position = "bottomleft")
    
    m %>% onRender("
      function(el, x) {
        var map = this;
        map.on('moveend', function() {
          Shiny.setInputValue('sync_future', {
            center: map.getCenter(),
            zoom: map.getZoom(),
            timestamp: Date.now()
          });
        });
      }
    ")
  })
  
  output$map_changes <- renderLeaflet({
    req(raster_data$changes)
    
    pal <- colorFactor(
      palette = c("#CCCCCC", "#E74C3C", "#F39C12", "#27AE60"),
      domain = 0:3,
      na.color = "transparent"
    )
    
    m <- leaflet() %>%
      setView(lng = -78.5, lat = 3, zoom = 7) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addRasterImage(raster_data$changes, colors = pal, opacity = 1,
                     project = FALSE, method = "ngb") %>%
      addLegend(
        colors = c("#E74C3C", "#F39C12", "#27AE60", "#CCCCCC"),
        labels = c("Cambios Negativos", "Sin Cambios", "Cambios Positivos", "Ausencia"),
        title = "Categorías",
        position = "bottomright"
      ) %>%
      addScaleBar(position = "bottomleft")
    
    m %>% onRender("
      function(el, x) {
        var map = this;
        map.on('moveend', function() {
          Shiny.setInputValue('sync_changes', {
            center: map.getCenter(),
            zoom: map.getZoom(),
            timestamp: Date.now()
          });
        });
      }
    ")
  })
  
  # MAPA CON UACs
  output$map_uacs <- renderLeaflet({
    req(raster_data$changes, raster_data$uacs)
    
    pal <- colorFactor(
      palette = c("#CCCCCC", "#E74C3C", "#F39C12", "#27AE60"),
      domain = 0:3,
      na.color = "transparent"
    )
    
    leaflet() %>%
      setView(lng = -78.5, lat = 3, zoom = 7) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addRasterImage(raster_data$changes, colors = pal, opacity = 0.7,
                     project = FALSE, method = "ngb") %>%
      addPolygons(
        data = raster_data$uacs,
        color = "#000000",
        weight = 2,
        fillOpacity = 0,
        label = ~nombre,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#FFFF00",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        colors = c("#E74C3C", "#F39C12", "#27AE60", "#CCCCCC"),
        labels = c("Cambios Negativos", "Sin Cambios", "Cambios Positivos", "Ausencia"),
        title = "Categorías",
        position = "bottomright"
      ) %>%
      addScaleBar(position = "bottomleft")
  })
  
  # SINCRONIZACIÓN
  last_sync_time <- reactiveVal(0)
  last_sync_source <- reactiveVal("")
  
  observeEvent(input$sync_present, {
    current_time <- as.numeric(Sys.time())
    
    if (current_time - last_sync_time() > 0.5 && last_sync_source() != "present") {
      last_sync_time(current_time)
      last_sync_source("present")
      
      v <- input$sync_present
      
      leafletProxy("map_future") %>% 
        setView(lng = v$center$lng, lat = v$center$lat, zoom = v$zoom)
      
      leafletProxy("map_changes") %>% 
        setView(lng = v$center$lng, lat = v$center$lat, zoom = v$zoom)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$sync_future, {
    current_time <- as.numeric(Sys.time())
    
    if (current_time - last_sync_time() > 0.5 && last_sync_source() != "future") {
      last_sync_time(current_time)
      last_sync_source("future")
      
      v <- input$sync_future
      
      leafletProxy("map_present") %>% 
        setView(lng = v$center$lng, lat = v$center$lat, zoom = v$zoom)
      
      leafletProxy("map_changes") %>% 
        setView(lng = v$center$lng, lat = v$center$lat, zoom = v$zoom)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$sync_changes, {
    current_time <- as.numeric(Sys.time())
    
    if (current_time - last_sync_time() > 0.5 && last_sync_source() != "changes") {
      last_sync_time(current_time)
      last_sync_source("changes")
      
      v <- input$sync_changes
      
      leafletProxy("map_present") %>% 
        setView(lng = v$center$lng, lat = v$center$lat, zoom = v$zoom)
      
      leafletProxy("map_future") %>% 
        setView(lng = v$center$lng, lat = v$center$lat, zoom = v$zoom)
    }
  }, ignoreInit = TRUE)
  
  observe({
    invalidateLater(2000)
    if (as.numeric(Sys.time()) - last_sync_time() > 2) {
      last_sync_source("")
    }
  })
  
  # CÁLCULO DE ÁREAS TOTALES
  calculate_areas <- reactive({
    req(raster_data$changes)
    res_x <- res(raster_data$changes)[1]
    res_y <- res(raster_data$changes)[2]
    pixel_area <- abs(res_x * res_y) * 111 * 111
    vals <- values(raster_data$changes)
    
    list(
      negativos = round(sum(vals == 1, na.rm = TRUE) * pixel_area, 2),
      sin_cambios = round(sum(vals == 2, na.rm = TRUE) * pixel_area, 2),
      positivos = round(sum(vals == 3, na.rm = TRUE) * pixel_area, 2),
      area_actual = round((sum(vals == 1, na.rm = TRUE) + sum(vals == 2, na.rm = TRUE)) * pixel_area, 2),
      area_futura = round((sum(vals == 2, na.rm = TRUE) + sum(vals == 3, na.rm = TRUE)) * pixel_area, 2)
    )
  })
  
  # CÁLCULO DE ÁREAS POR UAC
  calculate_uac_areas <- reactive({
    req(raster_data$changes, raster_data$uacs, raster_data$present_binary, raster_data$future_binary)
    
    uacs <- raster_data$uacs
    changes_rast <- raster_data$changes
    present_bin <- raster_data$present_binary
    future_bin <- raster_data$future_binary
    
    # Convertir UACs a SpatVector para terra
    uacs_vect <- vect(uacs)
    
    # Calcular resolución
    res_x <- res(changes_rast)[1]
    res_y <- res(changes_rast)[2]
    pixel_area <- abs(res_x * res_y) * 111 * 111
    
    results <- list()
    
    for (i in 1:nrow(uacs)) {
      uac_name <- uacs$nombre[i]
      uac_poly <- uacs_vect[i]
      
      # Extraer valores dentro del polígono
      changes_extract <- extract(changes_rast, uac_poly, fun = NULL)
      present_extract <- extract(present_bin, uac_poly, fun = NULL)
      future_extract <- extract(future_bin, uac_poly, fun = NULL)
      
      # Calcular áreas
      changes_vals <- changes_extract[[2]]
      present_vals <- present_extract[[2]]
      future_vals <- future_extract[[2]]
      
      negativos <- sum(changes_vals == 1, na.rm = TRUE) * pixel_area
      sin_cambios <- sum(changes_vals == 2, na.rm = TRUE) * pixel_area
      positivos <- sum(changes_vals == 3, na.rm = TRUE) * pixel_area
      
      area_presente <- sum(present_vals == 1, na.rm = TRUE) * pixel_area
      area_futura <- sum(future_vals == 1, na.rm = TRUE) * pixel_area
      
      results[[uac_name]] <- list(
        UAC = uac_name,
        Area_Presente_km2 = round(area_presente, 2),
        Area_Futura_km2 = round(area_futura, 2),
        Cambio_Total_km2 = round(area_futura - area_presente, 2),
        Cambio_Porcentaje = round(((area_futura - area_presente) / area_presente) * 100, 2),
        Perdida_km2 = round(negativos, 2),
        Sin_Cambios_km2 = round(sin_cambios, 2),
        Ganancia_km2 = round(positivos, 2)
      )
    }
    
    df_result <- do.call(rbind, lapply(results, as.data.frame))
    
    # Ordenar según el orden especificado
    orden_uacs <- c("Norte_Choco", "Baudo_San Juan", "Malaga_Buenaventura", "Llanura_Aluvial_Sur")
    df_result$UAC <- factor(df_result$UAC, levels = orden_uacs)
    df_result <- df_result[order(df_result$UAC), ]
    df_result$UAC <- as.character(df_result$UAC)
    
    # Reemplazar nombres con tildes
    df_result$UAC <- gsub("Norte_Choco", "Norte_Chocó", df_result$UAC)
    df_result$UAC <- gsub("Baudo_San Juan", "Baudó_San Juan", df_result$UAC)
    df_result$UAC <- gsub("Malaga_Buenaventura", "Málaga_Buenaventura", df_result$UAC)
    
    return(df_result)
  })
  
  # INFOBOXES
  output$area_actual <- renderInfoBox({
    areas <- calculate_areas()
    infoBox("Área Potencial Actual", paste(format(areas$area_actual, big.mark=","), "km²"),
            icon = icon("map-marked"), color = "blue", fill = TRUE)
  })
  
  output$area_futura <- renderInfoBox({
    areas <- calculate_areas()
    infoBox("Área Potencial Futura", paste(format(areas$area_futura, big.mark=","), "km²"),
            icon = icon("map-marked-alt"), color = "yellow", fill = TRUE)
  })
  
  output$area_perdida <- renderInfoBox({
    areas <- calculate_areas()
    infoBox("Área con Pérdida", paste(format(areas$negativos, big.mark=","), "km²"),
            icon = icon("arrow-down"), color = "red", fill = TRUE)
  })
  
  output$area_ganada <- renderInfoBox({
    areas <- calculate_areas()
    infoBox("Área con Ganancia", paste(format(areas$positivos, big.mark=","), "km²"),
            icon = icon("arrow-up"), color = "green", fill = TRUE)
  })
  
  # TABLA ÚNICA COMPLETA POR UAC
  output$table_uac_complete <- renderDT({
    df <- calculate_uac_areas()
    
    # Seleccionar y ordenar columnas en el orden solicitado
    df_display <- data.frame(
      UAC = gsub("_", " ", df$UAC),
      Area_Presente = df$Area_Presente_km2,
      Area_Futura = df$Area_Futura_km2,
      Perdida = df$Perdida_km2,
      Ganancia = df$Ganancia_km2,
      Sin_Cambios = df$Sin_Cambios_km2
    )
    
    datatable(df_display,
              options = list(
                pageLength = 10, 
                dom = 't',
                scrollX = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = 1:5)
                )
              ),
              rownames = FALSE,
              colnames = c("UAC", 
                           "Área Presente (km²)", 
                           "Área Futura (km²)", 
                           "Pérdida (km²)", 
                           "Ganancia (km²)",
                           "Sin Cambios (km²)")) %>%
      formatRound(columns = 2:6, digits = 2) %>%
      formatStyle('Area_Presente',
                  backgroundColor = '#E3F2FD',
                  fontWeight = 'bold') %>%
      formatStyle('Area_Futura',
                  backgroundColor = '#FFF3E0',
                  fontWeight = 'bold') %>%
      formatStyle('Perdida',
                  backgroundColor = '#FFEBEE',
                  color = '#C62828') %>%
      formatStyle('Ganancia',
                  backgroundColor = '#E8F5E9',
                  color = '#2E7D32') %>%
      formatStyle('Sin_Cambios',
                  backgroundColor = '#F5F5F5')
  })
  
  # GRÁFICO GENERAL
  output$changes_barplot <- renderPlot({
    areas <- calculate_areas()
    valores <- c(areas$negativos, areas$sin_cambios, areas$positivos)
    par(mar = c(5, 5, 3, 2))
    bp <- barplot(valores,
                  names.arg = c("Cambios\nNegativos", "Sin\nCambios", "Cambios\nPositivos"),
                  col = c("#E74C3C", "#F39C12", "#27AE60"), border = "white",
                  main = "Distribución de Cambios por Categoría",
                  ylab = "Área (km²)", ylim = c(0, max(valores) * 1.2), las = 1)
    text(bp, valores, labels = format(round(valores, 0), big.mark = ","),
         pos = 3, cex = 0.9, font = 2)
    grid(nx = NA, ny = NULL, col = "gray90", lty = 1)
  })
  
  # GRÁFICO COMPARACIÓN UACs
  output$uac_comparison_plot <- renderPlot({
    df <- calculate_uac_areas()
    
    # Aplicar orden y quitar guiones bajos para visualización
    df$UAC_display <- gsub("_", " ", df$UAC)
    orden_display <- c("Norte Chocó", "Baudó San Juan", "Málaga Buenaventura", "Llanura Aluvial Sur")
    df$UAC_display <- factor(df$UAC_display, levels = orden_display)
    
    par(mfrow = c(1, 2), mar = c(10, 5, 3, 2))
    
    # Gráfico 1: Áreas presente vs futuro (colores modernos distintos)
    areas_matrix <- rbind(df$Area_Presente_km2, df$Area_Futura_km2)
    colnames(areas_matrix) <- as.character(df$UAC_display)
    
    max_y1 <- max(areas_matrix) * 1.3
    
    bp1 <- barplot(areas_matrix, beside = TRUE,
                   col = c("#5E35B1", "#FF6F00"),  # Púrpura y naranja oscuro
                   main = "Área Presente vs Futura por UAC",
                   ylab = "Área (km²)",
                   ylim = c(0, max_y1),
                   las = 2,
                   cex.names = 0.9,
                   border = NA)
    
    legend("top", 
           legend = c("Presente", "Futuro 2050"),
           fill = c("#5E35B1", "#FF6F00"),
           bty = "n",
           horiz = TRUE,
           xpd = TRUE)
    
    # Gráfico 2: Cambios por categoría (mantener colores originales: rojo, gris, verde)
    cambios_matrix <- rbind(df$Perdida_km2, df$Sin_Cambios_km2, df$Ganancia_km2)
    colnames(cambios_matrix) <- as.character(df$UAC_display)
    
    max_y2 <- max(cambios_matrix) * 1.3
    
    bp2 <- barplot(cambios_matrix, beside = TRUE,
                   col = c("#E74C3C", "#95A5A6", "#27AE60"),
                   main = "Cambios por Categoría y UAC",
                   ylab = "Área (km²)",
                   ylim = c(0, max_y2),
                   las = 2,
                   cex.names = 0.9,
                   border = NA)
    
    legend("top",
           legend = c("Pérdida", "Sin Cambios", "Ganancia"),
           fill = c("#E74C3C", "#95A5A6", "#27AE60"),
           bty = "n",
           horiz = TRUE,
           xpd = TRUE)
  })
}

shinyApp(ui = ui, server = server)