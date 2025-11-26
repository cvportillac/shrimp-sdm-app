library(shiny)
library(leaflet)
library(terra)
library(sf)
library(viridis)
library(shinydashboard)
library(shinyWidgets)
library(htmlwidgets)
library(DT)
library(fmsb)

# ============================================================================
# DESCARGAR DATOS DESDE GITHUB CON MANEJO ROBUSTO
# ============================================================================

github_raw <- "https://github.com/cvportillac/shrimp-sdm-app/raw/main"

download_with_retry <- function(url, destfile, max_attempts = 3) {
  for (attempt in 1:max_attempts) {
    result <- try({
      download.file(url, destfile, mode = "wb", quiet = TRUE)
      TRUE
    }, silent = TRUE)
    
    if (!inherits(result, "try-error") && file.exists(destfile) && file.size(destfile) > 0) {
      return(TRUE)
    }
    
    if (attempt < max_attempts) {
      Sys.sleep(1)
    }
  }
  return(FALSE)
}

if (!dir.exists("data")) {
  message("========================================")
  message("INICIANDO DESCARGA DE DATOS DESDE GITHUB")
  message("========================================")
  
  dir.create("data/Shapefiles", recursive = TRUE, showWarnings = FALSE)
  especies <- c("L.occidentalis", "P.brevirostris", "P.californiensis", 
                "S.agassizii", "X.rivetti")
  meses <- c("jan", "feb", "mar", "apr", "may", "jun", 
             "jul", "aug", "sep", "oct", "nov", "dec")
  
  for (sp in especies) {
    dir.create(file.path("data", sp), recursive = TRUE, showWarnings = FALSE)
  }
  
  message("\n>>> DESCARGANDO SHAPEFILES (PRIORIDAD ALTA) <<<")
  shp_components <- c("shp", "shx", "dbf", "prj")
  shp_success <- TRUE
  
  for (ext in shp_components) {
    filename <- paste0("UACs_fixed.", ext)
    url <- paste0(github_raw, "/data/Shapefiles/", filename)
    dest <- file.path("data/Shapefiles", filename)
    
    message("Descargando: ", filename, " ... ", appendLF = FALSE)
    
    if (download_with_retry(url, dest)) {
      size <- file.size(dest)
      if (size > 0) {
        message("OK (", size, " bytes)")
      } else {
        message("FALLO - archivo vacío")
        shp_success <- FALSE
      }
    } else {
      message("FALLO - no se pudo descargar")
      shp_success <- FALSE
    }
  }
  
  if (shp_success) {
    message("\n>>> VERIFICANDO INTEGRIDAD DEL SHAPEFILE <<<")
    shp_path <- "data/Shapefiles/UACs_fixed.shp"
    
    test_read <- try({
      suppressWarnings(st_read(shp_path, quiet = TRUE))
    }, silent = TRUE)
    
    if (!inherits(test_read, "try-error")) {
      message("✓ Shapefile VERIFICADO - ", nrow(test_read), " features encontradas")
      message("  Columnas disponibles: ", paste(names(test_read), collapse = ", "))
    } else {
      message("✗ ERROR: Shapefile no se puede leer")
      shp_success <- FALSE
    }
  }
  
  if (!shp_success) {
    stop("ERROR CRÍTICO: No se pudo descargar o verificar el shapefile UACs.")
  }
  
  message("\n>>> DESCARGANDO RASTERS <<<")
  total_files <- length(especies) * length(meses) * 3
  file_count <- 0
  failed_count <- 0
  
  for (sp in especies) {
    message("Procesando especie: ", sp)
    for (mes in meses) {
      url <- paste0(github_raw, "/data/", sp, "/", mes, "_pres.tif")
      dest <- file.path("data", sp, paste0(mes, "_pres.tif"))
      if (!download_with_retry(url, dest)) failed_count <- failed_count + 1
      file_count <- file_count + 1
      
      url <- paste0(github_raw, "/data/", sp, "/", mes, "_2050_26.tif")
      dest <- file.path("data", sp, paste0(mes, "_2050_26.tif"))
      if (!download_with_retry(url, dest)) failed_count <- failed_count + 1
      file_count <- file_count + 1
      
      url <- paste0(github_raw, "/data/", sp, "/", mes, "_2050_85.tif")
      dest <- file.path("data", sp, paste0(mes, "_2050_85.tif"))
      if (!download_with_retry(url, dest)) failed_count <- failed_count + 1
      file_count <- file_count + 1
      
      if (file_count %% 30 == 0) {
        message("  Progreso: ", file_count, "/", total_files, " (fallidos: ", failed_count, ")")
      }
    }
  }
  
  message("\n========================================")
  message("DESCARGA COMPLETADA")
  message("Total archivos procesados: ", file_count)
  message("Archivos fallidos: ", failed_count)
  message("========================================\n")
  
} else {
  message("Datos ya disponibles localmente")
}

base_path <- "data"
uac_path <- file.path(base_path, "Shapefiles", "UACs_fixed.shp")

if (!file.exists(uac_path)) {
  stop("FATAL: Shapefile UACs no encontrado en: ", uac_path)
}

message("Configuración completada. Iniciando aplicación Shiny...")

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
      menuItem("Análisis por Unidad Ambiental Costera - UAC", tabName = "uacs", icon = icon("layer-group")),
      menuItem("Análisis Temporal por Especie", tabName = "temporal", icon = icon("chart-line"))
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
                 choices = c("Optimista (SSP 1-2.6)" = "26",
                             "Pesimista (SSP 5-8.5)" = "85"),
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
      ),
      
      tabItem(tabName = "temporal",
              fluidRow(
                box(title = "Variación Mensual del Área de Distribución",
                    status = "primary", solidHeader = TRUE, width = 12,
                    plotOutput("spider_plot", height = 650))
              ),
              fluidRow(
                box(title = "Resumen de Resultados por Especie",
                    status = "success", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("species_summary"))
              ),
              fluidRow(
                box(title = "Tabla de Datos Mensuales",
                    status = "info", solidHeader = TRUE, width = 12,
                    DTOutput("spider_data_table"))
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
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
    loaded = FALSE,
    name_column = NULL
  )
  
  detect_name_column <- function(sf_object) {
    possible_cols <- c("nombre", "NOMBRE", "Nombre", "name", "NAME", "Name", 
                      "UAC", "uac", "id", "ID", "region", "REGION")
    
    for (col in possible_cols) {
      if (col %in% names(sf_object)) {
        message("Columna de nombres detectada: ", col)
        return(col)
      }
    }
    
    non_geom_cols <- setdiff(names(sf_object), attr(sf_object, "sf_column"))
    if (length(non_geom_cols) > 0) {
      message("Usando primera columna disponible: ", non_geom_cols[1])
      return(non_geom_cols[1])
    }
    
    return(NULL)
  }
  
  calculate_total_study_area <- function(species, base_path) {
    tryCatch({
      especies_path <- file.path(base_path, species)
      pres_file <- file.path(especies_path, "jan_pres.tif")
      
      if (!file.exists(pres_file)) {
        return(NULL)
      }
      
      rast_ref <- rast(pres_file)
      res_x <- res(rast_ref)[1]
      res_y <- res(rast_ref)[2]
      pixel_area <- abs(res_x * res_y) * 111 * 111
      
      total_pixels <- sum(!is.na(values(rast_ref)))
      total_area <- total_pixels * pixel_area
      
      return(total_area)
      
    }, error = function(e) {
      message("Error calculando área total: ", e$message)
      return(NULL)
    })
  }
  
  calculate_monthly_areas <- function(species, base_path, threshold) {
    
    meses <- c("jan", "feb", "mar", "apr", "may", "jun", 
               "jul", "aug", "sep", "oct", "nov", "dec")
    
    meses_nombres <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun",
                       "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
    
    especies_path <- file.path(base_path, species)
    
    total_area <- calculate_total_study_area(species, base_path)
    
    if (is.null(total_area)) {
      return(NULL)
    }
    
    resultados <- data.frame(
      Mes = meses_nombres,
      Presente = numeric(12),
      Futuro_2050_SSP126 = numeric(12),
      Futuro_2050_SSP585 = numeric(12),
      Presente_km2 = numeric(12),
      Futuro_2050_SSP126_km2 = numeric(12),
      Futuro_2050_SSP585_km2 = numeric(12),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(meses)) {
      mes <- meses[i]
      
      tryCatch({
        pres_file <- file.path(especies_path, paste0(mes, "_pres.tif"))
        fut_26_file <- file.path(especies_path, paste0(mes, "_2050_26.tif"))
        fut_85_file <- file.path(especies_path, paste0(mes, "_2050_85.tif"))
        
        if (!file.exists(pres_file)) {
          next
        }
        
        pres_rast <- rast(pres_file)
        pres_prob <- (pres_rast / 1000) * 100
        pres_bin <- pres_prob >= threshold
        
        res_x <- res(pres_rast)[1]
        res_y <- res(pres_rast)[2]
        pixel_area <- abs(res_x * res_y) * 111 * 111
        
        area_pres <- sum(values(pres_bin) == 1, na.rm = TRUE) * pixel_area
        resultados$Presente[i] <- (area_pres / total_area) * 100
        resultados$Presente_km2[i] <- area_pres
        
        if (file.exists(fut_26_file)) {
          fut_26_rast <- rast(fut_26_file)
          fut_26_prob <- (fut_26_rast / 1000) * 100
          fut_26_bin <- fut_26_prob >= threshold
          area_26 <- sum(values(fut_26_bin) == 1, na.rm = TRUE) * pixel_area
          resultados$Futuro_2050_SSP126[i] <- (area_26 / total_area) * 100
          resultados$Futuro_2050_SSP126_km2[i] <- area_26
        }
        
        if (file.exists(fut_85_file)) {
          fut_85_rast <- rast(fut_85_file)
          fut_85_prob <- (fut_85_rast / 1000) * 100
          fut_85_bin <- fut_85_prob >= threshold
          area_85 <- sum(values(fut_85_bin) == 1, na.rm = TRUE) * pixel_area
          resultados$Futuro_2050_SSP585[i] <- (area_85 / total_area) * 100
          resultados$Futuro_2050_SSP585_km2[i] <- area_85
        }
        
      }, error = function(e) {
        message("Error procesando ", mes, ": ", e$message)
      })
    }
    
    attr(resultados, "total_area") <- total_area
    
    return(resultados)
  }
  
  generate_species_summary <- function(datos, species_name) {
    if (is.null(datos)) return("No hay datos disponibles")
    
    total_area <- attr(datos, "total_area")
    area_uac <- 27965
    
    prom_presente_km2 <- mean(datos$Presente_km2, na.rm = TRUE)
    prom_presente_porc <- (prom_presente_km2 / area_uac) * 100
    
    prom_ssp126_km2 <- mean(datos$Futuro_2050_SSP126_km2, na.rm = TRUE)
    prom_ssp126_porc <- (prom_ssp126_km2 / area_uac) * 100
    
    prom_ssp585_km2 <- mean(datos$Futuro_2050_SSP585_km2, na.rm = TRUE)
    prom_ssp585_porc <- (prom_ssp585_km2 / area_uac) * 100
    
    cambio_ssp126_km2 <- prom_ssp126_km2 - prom_presente_km2
    cambio_ssp126_porc <- ((prom_ssp126_km2 - prom_presente_km2) / prom_presente_km2) * 100
    
    cambio_ssp585_km2 <- prom_ssp585_km2 - prom_presente_km2
    cambio_ssp585_porc <- ((prom_ssp585_km2 - prom_presente_km2) / prom_presente_km2) * 100
    
    if (cambio_ssp126_km2 > 0) {
      texto_ssp126 <- paste0("una expansión de ", 
                             format(round(cambio_ssp126_km2, 2), big.mark = ","), 
                             " km² (+", round(abs(cambio_ssp126_porc), 2), "% de ganancia)")
    } else {
      texto_ssp126 <- paste0("una contracción de ", 
                             format(round(abs(cambio_ssp126_km2), 2), big.mark = ","), 
                             " km² (", round(cambio_ssp126_porc, 2), "% de pérdida)")
    }
    
    if (cambio_ssp585_km2 > 0) {
      texto_ssp585 <- paste0("una expansión significativa a ", 
                             format(round(prom_ssp585_km2, 2), big.mark = ","), 
                             " km² (", round(prom_ssp585_porc, 2), "%), equivalente a una ganancia de ", 
                             format(round(cambio_ssp585_km2, 2), big.mark = ","), 
                             " km² (+", round(abs(cambio_ssp585_porc), 2), "% respecto al presente)")
    } else {
      texto_ssp585 <- paste0("una reducción drástica a ", 
                             format(round(prom_ssp585_km2, 2), big.mark = ","), 
                             " km² (", round(prom_ssp585_porc, 2), "%), equivalente a una pérdida de ", 
                             format(round(abs(cambio_ssp585_km2), 2), big.mark = ","), 
                             " km² (", round(cambio_ssp585_porc, 2), "% respecto al presente)")
    }
    
    parrafo <- paste0(
      "El análisis del área idónea para la distribución potencial de ", species_name, 
      " reveló un área promedio de ", format(round(prom_presente_km2, 2), big.mark = ","), 
      " km² (", round(prom_presente_porc, 2), 
      "% de las Unidades Ambientales Costeras, ", format(area_uac, big.mark = ","), 
      " km²) en el período presente. ",
      "Las proyecciones bajo escenarios climáticos contrastantes muestran tendencias divergentes para 2050: ",
      "bajo el escenario optimista SSP1-2.6, se proyecta un área de ", 
      format(round(prom_ssp126_km2, 2), big.mark = ","), 
      " km² (", round(prom_ssp126_porc, 2), "%), representando ", 
      texto_ssp126, ". ",
      "En contraste, el escenario pesimista SSP5-8.5 proyecta ", 
      texto_ssp585, "."
    )
    
    return(parrafo)
  }
  
  load_raster_data <- function() {
    tryCatch({
      withProgress(message = 'Cargando datos...', value = 0, {
        
        species_path <- file.path(base_path, input$species)
        present_file <- file.path(species_path, paste0(input$month, "_pres.tif"))
        future_file <- file.path(species_path, paste0(input$month, "_2050_", input$scenario, ".tif"))
        
        incProgress(0.1, detail = "Verificando archivos...")
        
        if (!file.exists(present_file) || !file.exists(future_file)) {
          showNotification("Archivos raster no encontrados", type = "error", duration = 5)
          return(FALSE)
        }
        
        incProgress(0.2, detail = "Cargando shapefile UACs...")
        
        if (!file.exists(uac_path)) {
          message("ERROR CRÍTICO: Shapefile no encontrado en: ", uac_path)
          showNotification(
            "Shapefile UACs no encontrado. Reinicie la aplicación.", 
            type = "error", 
            duration = NULL
          )
          return(FALSE)
        }
        
        uacs <- tryCatch({
          shp <- suppressWarnings(st_read(uac_path, quiet = TRUE))
          message("Shapefile cargado con éxito")
          message("Columnas disponibles: ", paste(names(shp), collapse = ", "))
          shp
        }, error = function(e) {
          message("ERROR al leer shapefile: ", e$message)
          showNotification(
            paste("Error crítico al cargar shapefile:", e$message), 
            type = "error", 
            duration = NULL
          )
          return(NULL)
        })
        
        if (is.null(uacs)) {
          return(FALSE)
        }
        
        if (nrow(uacs) == 0) {
          message("ERROR: Shapefile sin features")
          showNotification(
            "El shapefile UACs no contiene datos válidos.", 
            type = "error", 
            duration = NULL
          )
          return(FALSE)
        }
        
        name_col <- detect_name_column(uacs)
        if (is.null(name_col)) {
          message("ERROR: No se pudo detectar columna de nombres")
          showNotification(
            "No se pudo identificar la columna de nombres en el shapefile.", 
            type = "error", 
            duration = NULL
          )
          return(FALSE)
        }
        
        raster_data$name_column <- name_col
        message("Usando columna: '", name_col, "' para nombres de UACs")
        
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
      message("ERROR GENERAL en load_raster_data: ", e$message)
      showNotification(
        paste("Error al cargar datos:", e$message), 
        type = "error", 
        duration = NULL
      )
      raster_data$loaded <- FALSE
      return(FALSE)
    })
  }
  
  observe({
    load_raster_data()
  })
  
  observeEvent(input$species, { if (raster_data$loaded) load_raster_data() }, ignoreInit = TRUE)
  observeEvent(input$month, { if (raster_data$loaded) load_raster_data() }, ignoreInit = TRUE)
  observeEvent(input$scenario, { if (raster_data$loaded) load_raster_data() }, ignoreInit = TRUE)
  observeEvent(input$threshold, { if (raster_data$loaded) load_raster_data() }, ignoreInit = TRUE)
  
  month_names <- c("jan"="Enero", "feb"="Febrero", "mar"="Marzo",
                   "apr"="Abril", "may"="Mayo", "jun"="Junio",
                   "jul"="Julio", "aug"="Agosto", "sep"="Septiembre",
                   "oct"="Octubre", "nov"="Noviembre", "dec"="Diciembre")
  
  output$title_present <- renderText({
    paste("Distribución Presente -", species_names[input$species], "-", month_names[input$month])
  })
  
  output$title_future <- renderText({
    paste("Proyección 2050 (SSP", gsub("26", "1-2.6", gsub("85", "5-8.5", input$scenario)), ") -", 
          species_names[input$species], "-", month_names[input$month])
  })
  
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
  
  output$map_uacs <- renderLeaflet({
    req(raster_data$changes, raster_data$uacs, raster_data$name_column)
    
    tryCatch({
      pal <- colorFactor(
        palette = c("#CCCCCC", "#E74C3C", "#F39C12", "#27AE60"),
        domain = 0:3,
        na.color = "transparent"
      )
      
      uacs <- raster_data$uacs
      name_col <- raster_data$name_column
      
      uac_labels <- as.character(st_drop_geometry(uacs)[[name_col]])
      
      message("Renderizando mapa con ", nrow(uacs), " UACs")
      message("Labels: ", paste(uac_labels, collapse = ", "))
      
      leaflet() %>%
        setView(lng = -78.5, lat = 3, zoom = 7) %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        addRasterImage(raster_data$changes, colors = pal, opacity = 0.7,
                       project = FALSE, method = "ngb") %>%
        addPolygons(
          data = uacs,
          color = "#000000",
          weight = 2,
          fillOpacity = 0,
          label = uac_labels,
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
      
    }, error = function(e) {
      message("ERROR en map_uacs: ", e$message)
      showNotification(
        paste("Error al renderizar mapa UACs:", e$message),
        type = "error",
        duration = 10
      )
      return(leaflet() %>% setView(lng = -78.5, lat = 3, zoom = 7) %>% addProviderTiles(providers$Esri.OceanBasemap))
    })
  })
  
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
  
  calculate_areas <- reactive({
    req(raster_data$changes)
    
    tryCatch({
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
    }, error = function(e) {
      message("ERROR en calculate_areas: ", e$message)
      list(negativos = 0, sin_cambios = 0, positivos = 0, area_actual = 0, area_futura = 0)
    })
  })
  
  calculate_uac_areas <- reactive({
    req(raster_data$changes, raster_data$uacs, raster_data$present_binary, 
        raster_data$future_binary, raster_data$name_column)
    
    tryCatch({
      uacs <- raster_data$uacs
      changes_rast <- raster_data$changes
      present_bin <- raster_data$present_binary
      future_bin <- raster_data$future_binary
      name_col <- raster_data$name_column
      
      message("Calculando áreas por UAC usando columna: ", name_col)
      
      uacs_vect <- vect(uacs)
      
      res_x <- res(changes_rast)[1]
      res_y <- res(changes_rast)[2]
      pixel_area <- abs(res_x * res_y) * 111 * 111
      
      results <- list()
      
      for (i in 1:nrow(uacs)) {
        uac_name <- as.character(uacs[[name_col]][i])
        
        if (is.na(uac_name) || uac_name == "") {
          uac_name <- paste0("UAC_", i)
        }
        
        message("Procesando: ", uac_name)
        
        uac_poly <- uacs_vect[i]
        
        changes_extract <- extract(changes_rast, uac_poly, fun = NULL)
        present_extract <- extract(present_bin, uac_poly, fun = NULL)
        future_extract <- extract(future_bin, uac_poly, fun = NULL)
        
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
          Cambio_Porcentaje = round(ifelse(area_presente > 0, 
                                           ((area_futura - area_presente) / area_presente) * 100, 
                                           0), 2),
          Perdida_km2 = round(negativos, 2),
          Sin_Cambios_km2 = round(sin_cambios, 2),
          Ganancia_km2 = round(positivos, 2)
        )
      }
      
      df_result <- do.call(rbind, lapply(results, as.data.frame))
      
      tryCatch({
        orden_uacs <- c("Norte_Choco", "Baudo_San Juan", "Malaga_Buenaventura", "Llanura_Aluvial_Sur",
                       "Norte_Chocó", "Baudó_San Juan", "Málaga_Buenaventura")
        
        df_result$UAC <- factor(df_result$UAC, 
                               levels = intersect(orden_uacs, df_result$UAC))
        df_result <- df_result[order(df_result$UAC), ]
        df_result$UAC <- as.character(df_result$UAC)
      }, error = function(e) {
        message("No se pudo ordenar UACs, usando orden original")
      })
      
      df_result$UAC <- gsub("Norte_Choco", "Norte_Chocó", df_result$UAC)
      df_result$UAC <- gsub("Baudo_San Juan", "Baudó_San Juan", df_result$UAC)
      df_result$UAC <- gsub("Malaga_Buenaventura", "Málaga_Buenaventura", df_result$UAC)
      
      message("Cálculo de áreas por UAC completado exitosamente")
      return(df_result)
      
    }, error = function(e) {
      message("ERROR CRÍTICO en calculate_uac_areas: ", e$message)
      showNotification(
        paste("Error al calcular áreas por UAC:", e$message),
        type = "error",
        duration = 10
      )
      return(NULL)
    })
  })
  
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
  
  output$table_uac_complete <- renderDT({
    df <- calculate_uac_areas()
    
    if (is.null(df)) {
      return(datatable(data.frame(Error = "No se pudieron cargar los datos de UACs.")))
    }
    
    req(!is.null(df))
    
    tryCatch({
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
      
    }, error = function(e) {
      message("ERROR en table_uac_complete: ", e$message)
      datatable(data.frame(Error = paste("Error:", e$message)))
    })
  })
  
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
  
  output$uac_comparison_plot <- renderPlot({
    df <- calculate_uac_areas()
    
    if (is.null(df)) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "No se pudieron cargar los datos", cex = 1.5)
      return()
    }
    
    req(!is.null(df))
    
    tryCatch({
      df$UAC_display <- gsub("_", " ", df$UAC)
      
      tryCatch({
        orden_display <- c("Norte Chocó", "Baudó San Juan", "Málaga Buenaventura", "Llanura Aluvial Sur")
        matching_order <- intersect(orden_display, df$UAC_display)
        if (length(matching_order) > 0) {
          df$UAC_display <- factor(df$UAC_display, levels = matching_order)
        }
      }, error = function(e) {
        message("Usando orden original")
      })
      
      par(mfrow = c(1, 2), mar = c(10, 5, 3, 2))
      
      areas_matrix <- rbind(df$Area_Presente_km2, df$Area_Futura_km2)
      colnames(areas_matrix) <- as.character(df$UAC_display)
      
      max_y1 <- max(areas_matrix) * 1.3
      
      bp1 <- barplot(areas_matrix, beside = TRUE,
                     col = c("#5E35B1", "#FF6F00"),
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
      
    }, error = function(e) {
      message("ERROR en gráfico: ", e$message)
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, "Error al generar gráfico", cex = 1.5)
    })
  })
  
  monthly_data <- reactive({
    req(input$species, input$threshold)
    
    withProgress(message = 'Calculando datos mensuales...', value = 0, {
      datos <- calculate_monthly_areas(input$species, base_path, input$threshold)
      incProgress(1)
      return(datos)
    })
  })
  
  output$species_summary <- renderText({
    datos <- monthly_data()
    req(!is.null(datos))
    
    resumen <- generate_species_summary(datos, species_names[input$species])
    return(resumen)
  })
  
  output$spider_plot <- renderPlot({
    datos <- monthly_data()
    req(!is.null(datos), nrow(datos) > 0)
    
    total_area <- attr(datos, "total_area")
    
    max_val <- 100
    min_val <- 0
    
    spider_data <- rbind(
      rep(max_val, 12),
      rep(min_val, 12),
      datos$Presente,
      datos$Futuro_2050_SSP126,
      datos$Futuro_2050_SSP585
    )
    
    colnames(spider_data) <- datos$Mes
    spider_data <- as.data.frame(spider_data)
    
    rango_presente <- paste0(round(min(datos$Presente), 1), " - ", round(max(datos$Presente), 1), "%")
    rango_ssp126 <- paste0(round(min(datos$Futuro_2050_SSP126), 1), " - ", round(max(datos$Futuro_2050_SSP126), 1), "%")
    rango_ssp585 <- paste0(round(min(datos$Futuro_2050_SSP585), 1), " - ", round(max(datos$Futuro_2050_SSP585), 1), "%")
    
    par(mar = c(4, 2, 4, 2), mfrow = c(1, 1))
    
    radarchart(
      spider_data,
      axistype = 1,
      
      pcol = c("#2E86C1", "#27AE60", "#E74C3C"),
      pfcol = c(rgb(0.18, 0.53, 0.76, 0.3),
                rgb(0.15, 0.68, 0.38, 0.3),
                rgb(0.91, 0.30, 0.24, 0.3)),
      plwd = 3,
      plty = 1,
      
      cglcol = "grey70",
      cglty = 1,
      cglwd = 0.8,
      axislabcol = "grey30",
      
      vlcex = 1.4,
      calcex = 1.1,
      seg = 5,
      
      title = ""
    )
    
    species_title <- switch(input$species,
                            "L.occidentalis" = expression("Variación mensual del área de distribución potencial de " * italic("Litopenaeus occidentalis")),
                            "X.rivetti" = expression("Variación mensual del área de distribución potencial de " * italic("Xiphopenaeus riveti")),
                            "S.agassizii" = expression("Variación mensual del área de distribución potencial de " * italic("Solenocera agassizii")),
                            "P.brevirostris" = expression("Variación mensual del área de distribución potencial de " * italic("Penaeus brevirostris")),
                            "P.californiensis" = expression("Variación mensual del área de distribución potencial de " * italic("Penaeus californiensis")))
    
    mtext(species_title, side = 3, line = 1.5, cex = 1.2)
    
    legend(
      x = "topright",
      legend = c("Presente", "Futuro 2050 SSP 1-2.6", "Futuro 2050 SSP 5-8.5"),
      col = c("#2E86C1", "#27AE60", "#E74C3C"),
      lty = 1,
      lwd = 3,
      bty = "n",
      cex = 1.2
    )
    
    legend(
      x = "topright",
      y = NULL,
      inset = c(0, 0.19),
      legend = c("Rango:", rango_presente, rango_ssp126, rango_ssp585),
      col = c("black", "#2E86C1", "#27AE60", "#E74C3C"),
      pch = c(NA, 15, 15, 15),
      bty = "n",
      cex = 1.2,
      pt.cex = 1.8
    )
    
    mtext(
      paste0("Porcentaje del área de estudio (Área total = ",
             format(round(total_area, 0), big.mark = ","), 
             " km²) | Umbral: ", input$threshold, "%"),
      side = 1, 
      line = 2.5, 
      cex = 1.0,
      col = "grey20"
    )
  })
  
  output$spider_data_table <- renderDT({
    datos <- monthly_data()
    req(!is.null(datos))
    
    datos_display <- data.frame(
      Mes = datos$Mes,
      Presente_Porc = datos$Presente,
      Presente_km2 = datos$Presente_km2,
      Futuro_SSP126_Porc = datos$Futuro_2050_SSP126,
      Futuro_SSP126_km2 = datos$Futuro_2050_SSP126_km2,
      Futuro_SSP585_Porc = datos$Futuro_2050_SSP585,
      Futuro_SSP585_km2 = datos$Futuro_2050_SSP585_km2
    )
    
    colnames(datos_display) <- c(
      "Mes", 
      "Presente (%)", 
      "Presente (km²)",
      "Futuro SSP 1-2.6 (%)", 
      "Futuro SSP 1-2.6 (km²)",
      "Futuro SSP 5-8.5 (%)", 
      "Futuro SSP 5-8.5 (km²)"
    )
    
    datatable(
      datos_display,
      options = list(
        pageLength = 12,
        dom = 't',
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = 1:6)
        )
      ),
      rownames = FALSE
    ) %>%
      formatRound(columns = 2:7, digits = 2) %>%
      formatStyle(c('Presente (%)', 'Presente (km²)'),
                  backgroundColor = '#E3F2FD',
                  fontWeight = 'bold') %>%
      formatStyle(c('Futuro SSP 1-2.6 (%)', 'Futuro SSP 1-2.6 (km²)'),
                  backgroundColor = '#E8F5E9',
                  fontWeight = 'bold') %>%
      formatStyle(c('Futuro SSP 5-8.5 (%)', 'Futuro SSP 5-8.5 (km²)'),
                  backgroundColor = '#FFEBEE',
                  fontWeight = 'bold')
  })
}

shinyApp(ui = ui, server = server)
