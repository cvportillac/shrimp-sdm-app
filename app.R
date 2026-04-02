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
library(rmapshaper)

Sys.setlocale("LC_ALL", "C")
github_raw <- "https://github.com/cvportillac/shrimp-sdm-app/raw/main"

download_with_retry <- function(url, destfile, max_attempts = 3, timeout = 300) {
  old_timeout <- getOption("timeout")
  options(timeout = timeout)
  for (attempt in 1:max_attempts) {
    result <- try({
      download.file(url, destfile, mode = "wb", quiet = FALSE, method = "libcurl")
      TRUE
    }, silent = FALSE)
    if (!inherits(result, "try-error") && file.exists(destfile) && file.size(destfile) > 0) {
      options(timeout = old_timeout)
      return(TRUE)
    }
    if (attempt < max_attempts) Sys.sleep(3)
  }
  options(timeout = old_timeout)
  return(FALSE)
}

base_path    <- "data"
uac_path     <- file.path(base_path, "Shapefiles", "UACs_fixed.shp")
som_path     <- file.path(base_path, "Shapefiles", "SOM_shape.shp")
som_csv_path <- file.path(base_path, "SOM_variables.csv")

if (!file.exists(uac_path)) {
  cat("STARTING DOWNLOAD\n")
  dir.create("data/Shapefiles", recursive = TRUE, showWarnings = FALSE)
  especies <- c("L.occidentalis","P.brevirostris","P.californiensis","S.agassizii","X.rivetti")
  meses    <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
  for (sp in especies) dir.create(file.path("data", sp), recursive = TRUE, showWarnings = FALSE)
  shp_components <- c("shp","shx","dbf","prj")
  ok <- TRUE
  for (ext in shp_components) {
    fn <- paste0("UACs_fixed.", ext)
    if (!download_with_retry(paste0(github_raw,"/data/Shapefiles/",fn),
                             file.path("data/Shapefiles",fn), 5, 300)) ok <- FALSE
  }
  if (!ok) stop("ERROR: Could not download UACs shapefile")
  for (ext in shp_components) {
    fn <- paste0("SOM_shape.", ext)
    download_with_retry(paste0(github_raw,"/data/Shapefiles/",fn),
                        file.path("data/Shapefiles",fn), 5, 300)
  }
  download_with_retry(paste0(github_raw,"/data/SOM_variables.csv"),
                      file.path("data","SOM_variables.csv"), 5, 300)
  for (sp in especies)
    for (mes in meses)
      for (suffix in c("_pres.tif","_2050_26.tif","_2050_85.tif"))
        download_with_retry(paste0(github_raw,"/data/",sp,"/",mes,suffix),
                            file.path("data",sp,paste0(mes,suffix)), 3, 180)
  cat("DOWNLOAD COMPLETED\n")
}

# ==============================================================================
# UI
# ==============================================================================
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(
    title = "Environmental Suitability Models for Colombian Pacific Shrimp",
    titleWidth = 750
  ),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Colombian Pacific Analysis",           tabName = "maps",     icon = icon("map")),
      menuItem("Coastal Environmental Units Analysis", tabName = "uacs",     icon = icon("layer-group")),
      menuItem("Benthic Units Analysis",               tabName = "benthic",  icon = icon("th")),
      menuItem("Temporal Analysis by Species",         tabName = "temporal", icon = icon("chart-line")),
      menuItem("Environmental Conditions by Habitat",  tabName = "habitat",  icon = icon("water"))
    ),
    hr(),
    selectInput("species","Select Species:",
                choices = c("Litopenaeus occidentalis"="L.occidentalis",
                            "Xiphopenaeus riveti"     ="X.rivetti",
                            "Solenocera agassizii"    ="S.agassizii",
                            "Penaeus brevirostris"    ="P.brevirostris",
                            "Penaeus californiensis"  ="P.californiensis"),
                selected = "L.occidentalis"),
    hr(),
    selectInput("month","Select Month:",
                choices = c("January"="jan","February"="feb","March"="mar","April"="apr",
                            "May"="may","June"="jun","July"="jul","August"="aug",
                            "September"="sep","October"="oct","November"="nov","December"="dec"),
                selected = "jan"),
    hr(),
    radioButtons("scenario","Future Scenario 2050:",
                 choices  = c("Optimistic (SSP 1-2.6)"="26","Pessimistic (SSP 5-8.5)"="85"),
                 selected = "26"),
    hr(),
    sliderInput("threshold","Presence Threshold (%):", min=10, max=70, value=50, step=5),
    hr(),
    sliderInput("opacity","Raster Opacity:", min=0.3, max=1, value=0.8, step=0.1),
    hr(),
    tags$div(style="padding:10px 15px;color:#ccc;",
             tags$p(tags$strong("Developed by:"), style="margin-bottom:8px;color:#fff;font-size:13px;"),
             tags$p(tags$span("Iván Felipe Benavides-Martinez",
                              style="display:block;font-size:12px;color:#eee;"),
                    tags$a(href="mailto:fbenavides@gmri.org","fbenavides@gmri.org",
                           style="font-size:11px;color:#7EC8E3;")),
             tags$p(tags$span("John Josephraj Selvaraj",
                              style="display:block;font-size:12px;color:#eee;margin-top:6px;"),
                    tags$a(href="mailto:jojselvaraj@unal.edu.co","jojselvaraj@unal.edu.co",
                           style="font-size:11px;color:#7EC8E3;")),
             tags$p(tags$span("Cristiam Victoriano Portilla-Cabrera",
                              style="display:block;font-size:12px;color:#eee;margin-top:6px;"),
                    tags$a(href="mailto:cvportillac@unal.edu.co","cvportillac@unal.edu.co",
                           style="font-size:11px;color:#7EC8E3;"))
    )
  ),
  dashboardBody(
    tags$head(tags$style(HTML(
      ".content-wrapper{background-color:#ecf0f5;}
       .box{border-top:3px solid #3c8dbc;}
       .leaflet-container{background:#fff;}"
    ))),
    tabItems(
      tabItem(tabName="maps",
              fluidRow(infoBoxOutput("area_actual",width=3), infoBoxOutput("area_futura",width=3),
                       infoBoxOutput("area_perdida",width=3), infoBoxOutput("area_ganada",width=3)),
              fluidRow(
                box(title=textOutput("title_present"), status="primary", solidHeader=TRUE, width=4, height=600,
                    leafletOutput("map_present", height=520)),
                box(title=textOutput("title_future"),  status="warning", solidHeader=TRUE, width=4, height=600,
                    leafletOutput("map_future",  height=520)),
                box(title="Environmental Suitability Changes", status="info", solidHeader=TRUE, width=4, height=600,
                    leafletOutput("map_changes", height=520))
              ),
              fluidRow(box(title="Quantitative Change Analysis", status="info", solidHeader=TRUE, width=12,
                           plotOutput("changes_barplot", height=300)))
      ),
      tabItem(tabName="uacs",
              fluidRow(box(title="Map with Coastal Environmental Units", status="primary", solidHeader=TRUE, width=12, height=600,
                           leafletOutput("map_uacs", height=520))),
              fluidRow(box(title="Detailed Analysis by Coastal Environmental Unit", status="info", solidHeader=TRUE, width=12,
                           DTOutput("table_uac_complete"))),
              fluidRow(box(title="Visual Comparison by Coastal Environmental Unit", status="primary", solidHeader=TRUE, width=12,
                           plotOutput("uac_comparison_plot", height=400)))
      ),
      tabItem(tabName="benthic",
              fluidRow(box(title="Map with Benthic Units", status="primary", solidHeader=TRUE, width=12, height=600,
                           leafletOutput("map_benthic", height=520))),
              fluidRow(box(title="Detailed Analysis by Benthic Unit", status="info", solidHeader=TRUE, width=12,
                           DTOutput("table_benthic_complete"))),
              fluidRow(box(title="Visual Comparison by Benthic Unit", status="primary", solidHeader=TRUE, width=12,
                           plotOutput("benthic_comparison_plot", height=400)))
      ),
      tabItem(tabName="temporal",
              fluidRow(box(title="Monthly Variation of Distribution Area", status="primary", solidHeader=TRUE, width=12,
                           plotOutput("spider_plot", height=550))),
              fluidRow(box(title="Results Summary by Species", status="success", solidHeader=TRUE, width=12,
                           htmlOutput("species_summary"))),
              fluidRow(box(title="Monthly Data Table", status="info", solidHeader=TRUE, width=12,
                           DTOutput("spider_data_table")))
      ),
      tabItem(tabName="habitat",
              fluidRow(
                column(width=9,
                       box(title="Benthic Units Map - Click on a polygon to view its characteristics",
                           status="primary", solidHeader=TRUE, width=12, height=700,
                           leafletOutput("map_habitat", height=630))
                ),
                column(width=3,
                       box(title="Settings", status="info", solidHeader=TRUE, width=12,
                           selectInput("habitat_variable","Environmental Variable:",
                                       choices=c("Phosphate"="Phosphate","Iron"="Iron","Bottom Light"="Bottom.light",
                                                 "Nitrate"="Nitrate","Dissolved Oxygen"="Dissolved.oxygen","pH"="pH",
                                                 "Salinity"="Salinity","Silicate"="Silicate","Temperature"="Temperature",
                                                 "PAR"="PAR","Organic Matter"="Organic.matter","Chlorophyll a"="Chlorophyll.a",
                                                 "Phytoplankton"="Phytoplankton","Bottom Utilized Oxygen"="Bottom.utilized.oxygen",
                                                 "Primary Productivity"="Primary.productivity","Asp Bat"="Asp_bat",
                                                 "East Bat"="East_bat","River Influence"="River.influence",
                                                 "Benthic Current Velocity"="Benthic.current.velocity","Runoff"="Runoff",
                                                 "Wind"="Wind","Land Distance"="Land.distance","Mean Bat"="Mean_bat",
                                                 "North Bat"="Nort_bat","Rdmv Bat"="Rdmv_bat","Slope Bat"="Slop_bat",
                                                 "Stde Bat"="Stde_bat","Surface Bat"="Surf_bat",
                                                 "Substrate Hardness"="Substrate.hardness","Particle Size"="Particle.size",
                                                 "Euphotic Layer Depth"="Euphotic.layer.depth",
                                                 "Light Attenuation Coefficient"="Light.attenuation.coefficient","IBMR"="IBMR"),
                                       selected="Salinity")),
                       box(title="Selected Benthic Unit", status="success", solidHeader=TRUE, width=12,
                           h4(textOutput("selected_habitat_name"), style="color:#3c8dbc;font-weight:bold;text-align:center;"),
                           hr(),
                           h5("Variable:", style="color:#666;font-weight:bold;"),
                           h4(textOutput("selected_variable_name"), style="color:#27AE60;text-align:center;"),
                           hr(),
                           h5("Units:", style="color:#666;font-weight:bold;"),
                           htmlOutput("selected_variable_units"),
                           hr(),
                           h5("Range [Min - Max]:", style="color:#666;font-weight:bold;"),
                           h3(textOutput("selected_range_value"), style="color:#E74C3C;font-weight:bold;text-align:center;"))
                )
              )
      )
    )
  )
)

# ==============================================================================
# SERVER
# ==============================================================================
server <- function(input, output, session) {
  
  species_names <- c("L.occidentalis"="Litopenaeus occidentalis","X.rivetti"="Xiphopenaeus riveti",
                     "S.agassizii"="Solenocera agassizii","P.brevirostris"="Penaeus brevirostris",
                     "P.californiensis"="Penaeus californiensis")
  
  rv <- reactiveValues(
    present_prob=NULL, future_prob=NULL,
    present_binary=NULL, future_binary=NULL,
    changes=NULL, uacs=NULL, benthic=NULL,
    loaded=FALSE, name_column=NULL,
    som_data=NULL, selected_habitat=NULL
  )
  
  pal_prob    <- colorNumeric("viridis", c(0,100), na.color="transparent")
  pal_changes <- colorFactor(c("#CCCCCC","#E74C3C","#F39C12","#27AE60"),
                             domain=0:3, na.color="transparent")
  
  observe({
    if (!file.exists(som_csv_path)) return()
    tryCatch({
      df <- read.csv(som_csv_path, sep=";", stringsAsFactors=FALSE, fileEncoding="UTF-8")
      df$Clase_SOM <- as.numeric(df$Clase_SOM)
      rv$som_data  <- df
    }, error=function(e) cat("Error loading SOM CSV:", e$message, "\n"))
  })
  
  detect_name_column <- function(sf_obj) {
    candidates <- c("nombre","NOMBRE","Nombre","name","NAME","Name","UAC","uac","id","ID","region","REGION")
    for (col in candidates) if (col %in% names(sf_obj)) return(col)
    non_geom <- setdiff(names(sf_obj), attr(sf_obj,"sf_column"))
    if (length(non_geom) > 0) return(non_geom[1])
    NULL
  }
  
  calculate_total_study_area <- function(species, base_path) {
    tryCatch({
      r <- rast(file.path(base_path, species, "jan_pres.tif"))
      abs(res(r)[1]*res(r)[2]) * 111*111 * sum(!is.na(values(r)))
    }, error=function(e) NULL)
  }
  
  calculate_monthly_areas <- function(species, base_path, threshold) {
    meses  <- c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
    mnames <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
    sp_path <- file.path(base_path, species)
    total   <- calculate_total_study_area(species, base_path)
    if (is.null(total)) return(NULL)
    res_df <- data.frame(Mes=mnames,
                         Presente=numeric(12), Futuro_2050_SSP126=numeric(12), Futuro_2050_SSP585=numeric(12),
                         Presente_km2=numeric(12), Futuro_2050_SSP126_km2=numeric(12), Futuro_2050_SSP585_km2=numeric(12),
                         stringsAsFactors=FALSE)
    for (i in seq_along(meses)) {
      pf <- file.path(sp_path, paste0(meses[i],"_pres.tif"))
      if (!file.exists(pf)) next
      tryCatch({
        pr  <- rast(pf); pp <- (pr/1000)*100; pa <- abs(res(pr)[1]*res(pr)[2])*111*111
        ap  <- sum(values(pp>=threshold)==1, na.rm=TRUE)*pa
        res_df$Presente[i]     <- (ap/total)*100
        res_df$Presente_km2[i] <- ap
        for (sc in list(c("26","Futuro_2050_SSP126","Futuro_2050_SSP126_km2"),
                        c("85","Futuro_2050_SSP585","Futuro_2050_SSP585_km2"))) {
          ff <- file.path(sp_path, paste0(meses[i],"_2050_",sc[1],".tif"))
          if (!file.exists(ff)) next
          fr <- rast(ff); fp <- (fr/1000)*100
          af <- sum(values(fp>=threshold)==1, na.rm=TRUE)*pa
          res_df[[sc[2]]][i]  <- (af/total)*100
          res_df[[sc[3]]][i]  <- af
        }
      }, error=function(e) NULL)
    }
    attr(res_df,"total_area") <- total
    res_df
  }
  
  generate_species_summary <- function(datos, sname) {
    if (is.null(datos)) return("No data available")
    pm  <- mean(datos$Presente_km2,na.rm=TRUE)
    s26 <- mean(datos$Futuro_2050_SSP126_km2,na.rm=TRUE)
    s85 <- mean(datos$Futuro_2050_SSP585_km2,na.rm=TRUE)
    fmt <- function(val, base) {
      d <- val-base; p <- (d/base)*100
      if (d>0) paste0("an expansion of ",format(round(d,2),big.mark=",")," km<sup>2</sup> (+",round(abs(p),2),"% gain)")
      else      paste0("a contraction of ",format(round(abs(d),2),big.mark=",")," km<sup>2</sup> (",round(p,2),"% loss)")
    }
    paste0("<p style='font-size:14px;line-height:1.6;text-align:justify;'>",
           "Analysis of <em>",sname,"</em> revealed an average area of ",
           format(round(pm,2),big.mark=",")," km<sup>2</sup>. ",
           "SSP1-2.6 projects ",fmt(s26,pm),". SSP5-8.5 projects ",fmt(s85,pm),".</p>")
  }
  
  load_raster_data <- function() {
    tryCatch({
      withProgress(message='Loading data...', value=0, {
        sp_path  <- file.path(base_path, input$species)
        pres_f   <- file.path(sp_path, paste0(input$month,"_pres.tif"))
        fut_f    <- file.path(sp_path, paste0(input$month,"_2050_",input$scenario,".tif"))
        incProgress(0.1, detail="Checking files...")
        if (!file.exists(pres_f) || !file.exists(fut_f)) {
          showNotification("Raster files not found", type="error", duration=5); return(FALSE)
        }
        if (is.null(rv$uacs)) {
          incProgress(0.15, detail="Loading UACs...")
          uacs <- tryCatch({
            shp <- suppressWarnings(st_read(uac_path, quiet=TRUE))
            shp <- st_make_valid(shp); shp <- st_cast(shp,"MULTIPOLYGON"); shp <- st_buffer(shp,0)
            shp <- tryCatch(ms_simplify(shp,keep=0.8,keep_shapes=TRUE),
                            error=function(e) st_simplify(shp,preserveTopology=TRUE,dTolerance=0.0005))
            shp <- st_transform(shp, crs=4326)
            df_tmp <- st_drop_geometry(shp)
            for (col in names(df_tmp))
              if (is.character(df_tmp[[col]]))
                shp[[col]] <- iconv(df_tmp[[col]], from="", to="UTF-8", sub="")
            shp
          }, error=function(e) NULL)
          if (!is.null(uacs) && nrow(uacs)>0) {
            rv$name_column <- detect_name_column(uacs)
            rv$uacs        <- uacs
          }
        }
        if (is.null(rv$benthic) && file.exists(som_path)) {
          incProgress(0.2, detail="Loading Benthic...")
          rv$benthic <- tryCatch({
            shp <- suppressWarnings(st_read(som_path, quiet=TRUE))
            if (any(is.na(st_is_valid(shp))) || any(!st_is_valid(shp))) shp <- st_make_valid(shp)
            shp <- st_transform(shp, crs=4326)
            shp$gridcode <- as.numeric(shp$gridcode)
            shp
          }, error=function(e) NULL)
        }
        incProgress(0.35, detail="Loading rasters...")
        pr  <- rast(pres_f);  fr  <- rast(fut_f)
        pp  <- (pr/1000)*100; fp  <- (fr/1000)*100
        pv  <- values(pp);    fv  <- values(fp)
        pbv <- ifelse(is.na(pv),NA,ifelse(pv>=input$threshold,1,0))
        fbv <- ifelse(is.na(fv),NA,ifelse(fv>=input$threshold,1,0))
        pb  <- pp; values(pb) <- pbv
        fb  <- fp; values(fb) <- fbv
        incProgress(0.65, detail="Classifying changes...")
        cv   <- rep(NA,length(pbv))
        ok   <- !is.na(pbv) & !is.na(fbv)
        cv[ok & pbv==0 & fbv==1] <- 3
        cv[ok & pbv==1 & fbv==0] <- 1
        cv[ok & pbv==1 & fbv==1] <- 2
        cv[ok & pbv==0 & fbv==0] <- 0
        ch <- pb; values(ch) <- cv
        rv$present_prob   <- pp
        rv$future_prob    <- fp
        rv$present_binary <- pb
        rv$future_binary  <- fb
        rv$changes        <- ch
        rv$loaded         <- TRUE
        incProgress(1, detail="Done")
        showNotification("Data loaded successfully", type="message", duration=3)
        return(TRUE)
      })
    }, error=function(e) {
      showNotification(paste("Error:", e$message), type="error", duration=NULL)
      rv$loaded <- FALSE; FALSE
    })
  }
  
  observe({ load_raster_data() })
  observeEvent(input$species,   { if (rv$loaded) load_raster_data() }, ignoreInit=TRUE)
  observeEvent(input$month,     { if (rv$loaded) load_raster_data() }, ignoreInit=TRUE)
  observeEvent(input$scenario,  { if (rv$loaded) load_raster_data() }, ignoreInit=TRUE)
  observeEvent(input$threshold, { if (rv$loaded) load_raster_data() }, ignoreInit=TRUE)
  
  month_names <- c("jan"="January","feb"="February","mar"="March","apr"="April","may"="May",
                   "jun"="June","jul"="July","aug"="August","sep"="September",
                   "oct"="October","nov"="November","dec"="December")
  output$title_present <- renderText(paste("Present -",species_names[input$species],"-",month_names[input$month]))
  output$title_future  <- renderText(paste("2050 SSP",gsub("26","1-2.6",gsub("85","5-8.5",input$scenario)),
                                           "-",species_names[input$species],"-",month_names[input$month]))
  
  output$map_present <- renderLeaflet({
    leaflet() %>% setView(lng=-78.5, lat=3, zoom=7) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addScaleBar(position="bottomleft") %>%
      onRender("function(el,x){var map=this;map.on('moveend',function(){Shiny.setInputValue('sync_present',{center:map.getCenter(),zoom:map.getZoom(),ts:Date.now()});});}")
  })
  output$map_future <- renderLeaflet({
    leaflet() %>% setView(lng=-78.5, lat=3, zoom=7) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addScaleBar(position="bottomleft") %>%
      onRender("function(el,x){var map=this;map.on('moveend',function(){Shiny.setInputValue('sync_future',{center:map.getCenter(),zoom:map.getZoom(),ts:Date.now()});});}")
  })
  output$map_changes <- renderLeaflet({
    leaflet() %>% setView(lng=-78.5, lat=3, zoom=7) %>%
      addProviderTiles(providers$Esri.OceanBasemap) %>%
      addScaleBar(position="bottomleft") %>%
      onRender("function(el,x){var map=this;map.on('moveend',function(){Shiny.setInputValue('sync_changes',{center:map.getCenter(),zoom:map.getZoom(),ts:Date.now()});});}")
  })
  
  observeEvent(rv$present_prob, {
    req(rv$present_prob)
    leafletProxy("map_present") %>% clearImages() %>% clearControls() %>%
      addRasterImage(rv$present_prob, colors=pal_prob, opacity=input$opacity) %>%
      addLegend(pal=pal_prob, values=c(0,25,50,75,100), title="Probability (%)", position="bottomright")
  })
  observeEvent(rv$future_prob, {
    req(rv$future_prob)
    leafletProxy("map_future") %>% clearImages() %>% clearControls() %>%
      addRasterImage(rv$future_prob, colors=pal_prob, opacity=input$opacity) %>%
      addLegend(pal=pal_prob, values=c(0,25,50,75,100), title="Probability (%)", position="bottomright")
  })
  observeEvent(rv$changes, {
    req(rv$changes)
    leafletProxy("map_changes") %>% clearImages() %>% clearControls() %>%
      addRasterImage(rv$changes, colors=pal_changes, opacity=1, project=FALSE, method="ngb") %>%
      addLegend(colors=c("#E74C3C","#F39C12","#27AE60","#CCCCCC"),
                labels=c("Negative Changes","No Changes","Positive Changes","Absence"),
                title="Categories", position="bottomright")
  })
  observeEvent(input$opacity, {
    req(rv$present_prob, rv$future_prob)
    leafletProxy("map_present") %>% clearImages() %>%
      addRasterImage(rv$present_prob, colors=pal_prob, opacity=input$opacity)
    leafletProxy("map_future") %>% clearImages() %>%
      addRasterImage(rv$future_prob, colors=pal_prob, opacity=input$opacity)
  }, ignoreInit=TRUE)
  
  last_sync_t <- reactiveVal(0)
  last_sync_s <- reactiveVal("")
  observeEvent(input$sync_present, {
    ct <- as.numeric(Sys.time())
    if (ct - last_sync_t() > 0.5 && last_sync_s() != "present") {
      last_sync_t(ct); last_sync_s("present"); v <- input$sync_present
      leafletProxy("map_future")  %>% setView(v$center$lng, v$center$lat, v$zoom)
      leafletProxy("map_changes") %>% setView(v$center$lng, v$center$lat, v$zoom)
    }
  }, ignoreInit=TRUE)
  observeEvent(input$sync_future, {
    ct <- as.numeric(Sys.time())
    if (ct - last_sync_t() > 0.5 && last_sync_s() != "future") {
      last_sync_t(ct); last_sync_s("future"); v <- input$sync_future
      leafletProxy("map_present") %>% setView(v$center$lng, v$center$lat, v$zoom)
      leafletProxy("map_changes") %>% setView(v$center$lng, v$center$lat, v$zoom)
    }
  }, ignoreInit=TRUE)
  observeEvent(input$sync_changes, {
    ct <- as.numeric(Sys.time())
    if (ct - last_sync_t() > 0.5 && last_sync_s() != "changes") {
      last_sync_t(ct); last_sync_s("changes"); v <- input$sync_changes
      leafletProxy("map_present") %>% setView(v$center$lng, v$center$lat, v$zoom)
      leafletProxy("map_future")  %>% setView(v$center$lng, v$center$lat, v$zoom)
    }
  }, ignoreInit=TRUE)
  observe({ invalidateLater(2000); if (as.numeric(Sys.time())-last_sync_t()>2) last_sync_s("") })
  
  output$map_uacs <- renderLeaflet({
    req(rv$loaded, rv$uacs, rv$name_column)
    tryCatch({
      uacs      <- rv$uacs; nc <- rv$name_column
      uac_names <- tryCatch(
        iconv(as.character(st_drop_geometry(uacs)[[nc]]), from="", to="ASCII//TRANSLIT", sub="?"),
        error=function(e) paste0("UAC_", seq_len(nrow(uacs))))
      bad <- is.na(uac_names)|uac_names==""|uac_names=="NA"
      if (any(bad)) uac_names[bad] <- paste0("UAC_",which(bad))
      m <- leaflet() %>% setView(lng=-78.5,lat=3,zoom=7) %>% addProviderTiles(providers$Esri.OceanBasemap)
      if (!is.null(rv$changes))
        m <- tryCatch(m %>% addRasterImage(rv$changes,colors=pal_changes,opacity=0.7,project=FALSE,method="ngb"),
                      error=function(e) m)
      m %>%
        addPolygons(data=uacs, color="#000000", weight=2, fillOpacity=0,
                    label=uac_names, popup=paste0("<strong>",uac_names,"</strong>"),
                    highlightOptions=highlightOptions(weight=4,color="#FFFF00",bringToFront=TRUE)) %>%
        addLegend(colors=c("#E74C3C","#F39C12","#27AE60","#CCCCCC"),
                  labels=c("Negative Changes","No Changes","Positive Changes","Absence"),
                  title="Categories", position="bottomright") %>%
        addScaleBar(position="bottomleft")
    }, error=function(e)
      leaflet() %>% setView(lng=-78.5,lat=3,zoom=7) %>% addProviderTiles(providers$Esri.OceanBasemap))
  })
  
  output$map_benthic <- renderLeaflet({
    req(rv$loaded, rv$benthic)
    tryCatch({
      benthic <- rv$benthic; pal_b <- colorFactor("Set3", domain=unique(benthic$gridcode))
      m <- leaflet() %>% setView(lng=-78.5,lat=3,zoom=7) %>% addProviderTiles(providers$Esri.OceanBasemap)
      if (!is.null(rv$changes))
        m <- tryCatch(m %>% addRasterImage(rv$changes,colors=pal_changes,opacity=0.7,project=FALSE,method="ngb"),
                      error=function(e) m)
      m %>%
        addPolygons(data=benthic, color="#000000", weight=0.5,
                    fillColor=~pal_b(gridcode), fillOpacity=0.4,
                    label=~paste0("Benthic Unit: ",gridcode),
                    popup=~paste0("<strong>Benthic Unit:</strong> ",gridcode),
                    highlightOptions=highlightOptions(weight=2,color="#FFFF00",bringToFront=TRUE)) %>%
        addLegend(colors=c("#E74C3C","#F39C12","#27AE60","#CCCCCC"),
                  labels=c("Negative Changes","No Changes","Positive Changes","Absence"),
                  title="Categories", position="bottomright") %>%
        addScaleBar(position="bottomleft")
    }, error=function(e)
      leaflet() %>% setView(lng=-78.5,lat=3,zoom=7) %>% addProviderTiles(providers$Esri.OceanBasemap))
  })
  
  observeEvent(input$map_habitat_shape_click, {
    if (!is.null(input$map_habitat_shape_click$id))
      rv$selected_habitat <- as.numeric(input$map_habitat_shape_click$id)
  })
  output$map_habitat <- renderLeaflet({
    req(rv$benthic)
    tryCatch({
      b <- rv$benthic; b$gc <- as.numeric(b$gridcode)
      cls <- sort(unique(b$gc)); pal_h <- colorFactor("Spectral", domain=cls)
      leaflet() %>% setView(lng=-78.5,lat=3,zoom=7) %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        addPolygons(data=b, layerId=~gc, color="#000000", weight=1,
                    fillColor=~pal_h(gc), fillOpacity=0.6,
                    label=~paste0("Benthic Unit: ",gc),
                    highlightOptions=highlightOptions(weight=4,color="#FFFF00",fillOpacity=0.9,bringToFront=TRUE)) %>%
        addLegend(pal=pal_h, values=cls, title="Benthic Unit", position="bottomright",
                  labFormat=labelFormat(transform=function(x) sort(unique(x)))) %>%
        addScaleBar(position="bottomleft")
    }, error=function(e)
      leaflet() %>% setView(lng=-78.5,lat=3,zoom=7) %>% addProviderTiles(providers$Esri.OceanBasemap))
  })
  observeEvent(rv$selected_habitat, {
    req(rv$benthic, rv$selected_habitat)
    b <- rv$benthic; b$gc <- as.numeric(b$gridcode)
    cls <- sort(unique(b$gc)); pal_h <- colorFactor("Spectral", domain=cls)
    sel <- rv$selected_habitat
    b$bc <- ifelse(b$gc==sel,"#FFFF00","#000000")
    b$bw <- ifelse(b$gc==sel,4,1)
    b$fo <- ifelse(b$gc==sel,0.9,0.6)
    leafletProxy("map_habitat") %>% clearShapes() %>%
      addPolygons(data=b, layerId=~gc, color=~bc, weight=~bw,
                  fillColor=~pal_h(gc), fillOpacity=~fo,
                  label=~paste0("Benthic Unit: ",gc),
                  highlightOptions=highlightOptions(weight=4,color="#FFFF00",fillOpacity=0.9,bringToFront=TRUE))
  }, ignoreInit=TRUE)
  
  output$selected_habitat_name <- renderText({
    if (is.null(rv$selected_habitat)) "No unit selected"
    else paste0("Benthic Unit: ", rv$selected_habitat)
  })
  output$selected_variable_name <- renderText({
    vn <- c("Phosphate"="Phosphate","Iron"="Iron","Bottom.light"="Bottom Light",
            "Nitrate"="Nitrate","Dissolved.oxygen"="Dissolved Oxygen","pH"="pH",
            "Salinity"="Salinity","Silicate"="Silicate","Temperature"="Temperature",
            "PAR"="PAR","Organic.matter"="Organic Matter","Chlorophyll.a"="Chlorophyll a",
            "Phytoplankton"="Phytoplankton","Bottom.utilized.oxygen"="Bottom Utilized Oxygen",
            "Primary.productivity"="Primary Productivity","Asp_bat"="Asp Bat",
            "East_bat"="East Bat","River.influence"="River Influence",
            "Benthic.current.velocity"="Benthic Current Velocity","Runoff"="Runoff",
            "Wind"="Wind","Land.distance"="Land Distance","Mean_bat"="Mean Bat",
            "Nort_bat"="North Bat","Rdmv_bat"="Rdmv Bat","Slop_bat"="Slope Bat",
            "Stde_bat"="Stde Bat","Surf_bat"="Surface Bat",
            "Substrate.hardness"="Substrate Hardness","Particle.size"="Particle Size",
            "Euphotic.layer.depth"="Euphotic Layer Depth",
            "Light.attenuation.coefficient"="Light Attenuation Coefficient","IBMR"="IBMR")
    sv <- input$habitat_variable
    if (sv %in% names(vn)) vn[sv] else sv
  })
  output$selected_variable_units <- renderUI({
    vu <- c("Phosphate"="[mol&middot;m<sup>-3</sup>]","Iron"="[&mu;mol&middot;m<sup>-3</sup>]",
            "Bottom.light"="","Nitrate"="[mol&middot;m<sup>-3</sup>]",
            "Dissolved.oxygen"="[mmol&middot;m<sup>-3</sup>]","pH"="","Salinity"="[PSU]",
            "Silicate"="[mol&middot;m<sup>-3</sup>]","Temperature"="[&deg;C]",
            "PAR"="[Einstein&middot;m<sup>-2</sup>&middot;day<sup>-1</sup>]","Organic.matter"="[%]",
            "Chlorophyll.a"="[mg&middot;m<sup>-3</sup>]","Phytoplankton"="[&mu;mol&middot;m<sup>-3</sup>]",
            "Bottom.utilized.oxygen"="[ml&middot;l<sup>-1</sup>]",
            "Primary.productivity"="[g&middot;m<sup>-3</sup>&middot;day<sup>-1</sup>]",
            "Asp_bat"="","East_bat"="","River.influence"="",
            "Benthic.current.velocity"="[m&middot;s<sup>-1</sup>]","Runoff"="[m]",
            "Wind"="[m&middot;s<sup>-1</sup>]","Land.distance"="","Mean_bat"="","Nort_bat"="",
            "Rdmv_bat"="","Slop_bat"="","Stde_bat"="","Surf_bat"="",
            "Substrate.hardness"="[%]","Particle.size"="[phi]",
            "Euphotic.layer.depth"="[m]","Light.attenuation.coefficient"="[m<sup>-1</sup>]","IBMR"="")
    sv   <- input$habitat_variable
    unit <- if (sv %in% names(vu)) vu[sv] else ""
    if (unit=="") HTML("<h4 style='color:#9B59B6;text-align:center;font-size:16px;'>No units</h4>")
    else          HTML(paste0("<h4 style='color:#9B59B6;text-align:center;font-size:16px;'>",unit,"</h4>"))
  })
  output$selected_range_value <- renderText({
    req(rv$som_data)
    sv  <- input$habitat_variable; sel <- rv$selected_habitat
    if (is.null(sel)) return("Click on the map")
    som <- rv$som_data
    if (!(sv %in% names(som))) return("Variable not found")
    rows <- som[som$Clase_SOM==sel, ]
    if (nrow(rows)==0) return("Unit not found in CSV")
    vals <- as.numeric(rows[[sv]]); vals <- vals[!is.na(vals)]
    if (length(vals)==0) return("Data not available")
    mn <- round(min(vals),4); mx <- round(max(vals),4)
    if (abs(mx-mn)<0.0001) return(if(abs(mn)<0.0001) "≈ 0" else paste0("[",mn,"]"))
    paste0("[",mn," - ",mx,"]")
  })
  
  calculate_areas <- reactive({
    req(rv$changes)
    tryCatch({
      pa  <- abs(res(rv$changes)[1]*res(rv$changes)[2]) * 111*111
      v   <- values(rv$changes)
      list(negativos=round(sum(v==1,na.rm=TRUE)*pa,2),
           sin_cambios=round(sum(v==2,na.rm=TRUE)*pa,2),
           positivos=round(sum(v==3,na.rm=TRUE)*pa,2),
           area_actual=round((sum(v==1,na.rm=TRUE)+sum(v==2,na.rm=TRUE))*pa,2),
           area_futura=round((sum(v==2,na.rm=TRUE)+sum(v==3,na.rm=TRUE))*pa,2))
    }, error=function(e) list(negativos=0,sin_cambios=0,positivos=0,area_actual=0,area_futura=0))
  })
  
  calculate_uac_areas <- reactive({
    req(rv$changes, rv$uacs, rv$present_binary, rv$future_binary, rv$name_column)
    tryCatch({
      uacs <- rv$uacs; nc <- rv$name_column; uv <- vect(uacs)
      pa   <- abs(res(rv$changes)[1]*res(rv$changes)[2]) * 111*111
      results <- list()
      for (i in 1:nrow(uacs)) {
        nm  <- as.character(uacs[[nc]][i])
        if (is.na(nm)||nm=="") nm <- paste0("UAC_",i)
        cv  <- extract(rv$changes,        uv[i],fun=NULL)[[2]]
        pv  <- extract(rv$present_binary, uv[i],fun=NULL)[[2]]
        fv  <- extract(rv$future_binary,  uv[i],fun=NULL)[[2]]
        ap  <- sum(pv==1,na.rm=TRUE)*pa; af <- sum(fv==1,na.rm=TRUE)*pa
        results[[nm]] <- list(UAC=nm, Area_Present_km2=round(ap,2), Area_Future_km2=round(af,2),
                              Total_Change_km2=round(af-ap,2),
                              Change_Percentage=round(ifelse(ap>0,((af-ap)/ap)*100,0),2),
                              Loss_km2=round(sum(cv==1,na.rm=TRUE)*pa,2),
                              No_Change_km2=round(sum(cv==2,na.rm=TRUE)*pa,2),
                              Gain_km2=round(sum(cv==3,na.rm=TRUE)*pa,2))
      }
      df <- do.call(rbind, lapply(results,as.data.frame))
      df$UAC <- gsub("Norte_Choco","North Choco",gsub("Baudo_San Juan","Baudo San Juan",
                                                      gsub("Malaga_Buenaventura","Malaga Buenaventura",df$UAC)))
      df
    }, error=function(e) NULL)
  })
  
  calculate_benthic_areas <- reactive({
    req(rv$changes, rv$benthic, rv$present_binary, rv$future_binary)
    tryCatch({
      b  <- rv$benthic; bv <- vect(b)
      pa <- abs(res(rv$changes)[1]*res(rv$changes)[2]) * 111*111
      results <- list()
      for (i in 1:nrow(b)) {
        cb <- as.character(b$gridcode[i])
        cv <- extract(rv$changes,        bv[i],fun=NULL)[[2]]
        pv <- extract(rv$present_binary, bv[i],fun=NULL)[[2]]
        fv <- extract(rv$future_binary,  bv[i],fun=NULL)[[2]]
        ap <- sum(pv==1,na.rm=TRUE)*pa; af <- sum(fv==1,na.rm=TRUE)*pa
        results[[cb]] <- list(Benthic_Unit=cb, Area_Present_km2=round(ap,2), Area_Future_km2=round(af,2),
                              Total_Change_km2=round(af-ap,2),
                              Change_Percentage=round(ifelse(ap>0,((af-ap)/ap)*100,0),2),
                              Loss_km2=round(sum(cv==1,na.rm=TRUE)*pa,2),
                              No_Change_km2=round(sum(cv==2,na.rm=TRUE)*pa,2),
                              Gain_km2=round(sum(cv==3,na.rm=TRUE)*pa,2))
      }
      df <- do.call(rbind, lapply(results,as.data.frame))
      df[order(as.numeric(df$Benthic_Unit)),]
    }, error=function(e) NULL)
  })
  
  output$area_actual  <- renderInfoBox({
    a <- calculate_areas()
    infoBox(HTML("Current Potential Area"),HTML(paste0(format(a$area_actual,big.mark=",")," km<sup>2</sup>")),icon=icon("map-marked"),color="blue",fill=TRUE)
  })
  output$area_futura  <- renderInfoBox({
    a <- calculate_areas()
    infoBox(HTML("Future Potential Area"),HTML(paste0(format(a$area_futura,big.mark=",")," km<sup>2</sup>")),icon=icon("map-marked-alt"),color="yellow",fill=TRUE)
  })
  output$area_perdida <- renderInfoBox({
    a <- calculate_areas()
    infoBox(HTML("Lost Area"),HTML(paste0(format(a$negativos,big.mark=",")," km<sup>2</sup>")),icon=icon("arrow-down"),color="red",fill=TRUE)
  })
  output$area_ganada  <- renderInfoBox({
    a <- calculate_areas()
    infoBox(HTML("Gained Area"),HTML(paste0(format(a$positivos,big.mark=",")," km<sup>2</sup>")),icon=icon("arrow-up"),color="green",fill=TRUE)
  })
  
  output$table_uac_complete <- renderDT({
    df <- calculate_uac_areas()
    if (is.null(df)) return(datatable(data.frame(Error="Could not load UAC data")))
    tryCatch({
      dd <- data.frame(UAC=gsub("_"," ",df$UAC), Area_Present=df$Area_Present_km2,
                       Area_Future=df$Area_Future_km2, Loss=df$Loss_km2,
                       Gain=df$Gain_km2, No_Change=df$No_Change_km2)
      datatable(dd, options=list(pageLength=10,dom='t',scrollX=TRUE), rownames=FALSE,
                colnames=c("CEU","Present Area (km²)","Future Area (km²)","Loss (km²)","Gain (km²)","No Change (km²)")) %>%
        formatRound(2:6,2) %>%
        formatStyle('Area_Present',backgroundColor='#E3F2FD',fontWeight='bold') %>%
        formatStyle('Area_Future', backgroundColor='#FFF3E0',fontWeight='bold') %>%
        formatStyle('Loss',        backgroundColor='#FFEBEE',color='#C62828') %>%
        formatStyle('Gain',        backgroundColor='#E8F5E9',color='#2E7D32') %>%
        formatStyle('No_Change',   backgroundColor='#F5F5F5')
    }, error=function(e) datatable(data.frame(Error="Error loading data")))
  })
  
  output$table_benthic_complete <- renderDT({
    df <- calculate_benthic_areas()
    if (is.null(df)) return(datatable(data.frame(Error="Could not load Benthic data")))
    tryCatch({
      dd <- data.frame(Unit=df$Benthic_Unit, Area_Present=df$Area_Present_km2,
                       Area_Future=df$Area_Future_km2, Loss=df$Loss_km2,
                       Gain=df$Gain_km2, No_Change=df$No_Change_km2)
      datatable(dd, options=list(pageLength=15,dom='tp',scrollX=TRUE), rownames=FALSE,
                colnames=c("Benthic Unit","Present Area (km²)","Future Area (km²)","Loss (km²)","Gain (km²)","No Change (km²)")) %>%
        formatRound(2:6,2) %>%
        formatStyle('Area_Present',backgroundColor='#E3F2FD',fontWeight='bold') %>%
        formatStyle('Area_Future', backgroundColor='#FFF3E0',fontWeight='bold') %>%
        formatStyle('Loss',        backgroundColor='#FFEBEE',color='#C62828') %>%
        formatStyle('Gain',        backgroundColor='#E8F5E9',color='#2E7D32') %>%
        formatStyle('No_Change',   backgroundColor='#F5F5F5')
    }, error=function(e) datatable(data.frame(Error="Error loading data")))
  })
  
  output$changes_barplot <- renderPlot({
    a <- calculate_areas()
    v <- c(a$negativos, a$sin_cambios, a$positivos)
    par(mar=c(5,5,3,2))
    bp <- barplot(v, names.arg=c("Negative\nChanges","No\nChanges","Positive\nChanges"),
                  col=c("#E74C3C","#F39C12","#27AE60"), border="white",
                  main="Distribution of Changes by Category",
                  ylab=expression("Area (km"^2*")"), ylim=c(0,max(v)*1.2), las=1)
    text(bp, v, labels=format(round(v,0),big.mark=","), pos=3, cex=0.9, font=2)
    grid(nx=NA, ny=NULL, col="gray90", lty=1)
  })
  
  output$uac_comparison_plot <- renderPlot({
    df <- calculate_uac_areas()
    if (is.null(df)) { plot(1,type="n",axes=FALSE,xlab="",ylab=""); text(1,1,"Could not load data",cex=1.5); return() }
    tryCatch({
      df$lbl <- gsub("_"," ",df$UAC)
      par(mfrow=c(1,2), mar=c(10,5,3,2))
      am <- rbind(df$Area_Present_km2, df$Area_Future_km2); colnames(am) <- df$lbl
      barplot(am, beside=TRUE, col=c("#5E35B1","#FF6F00"), main="Present vs Future Area by CEU",
              ylab=expression("Area (km"^2*")"), ylim=c(0,max(am)*1.3), las=2, cex.names=0.9, border=NA)
      legend("top",legend=c("Present","Future 2050"),fill=c("#5E35B1","#FF6F00"),bty="n",horiz=TRUE,xpd=TRUE)
      cm <- rbind(df$Loss_km2, df$No_Change_km2, df$Gain_km2); colnames(cm) <- df$lbl
      barplot(cm, beside=TRUE, col=c("#E74C3C","#95A5A6","#27AE60"), main="Changes by Category and CEU",
              ylab=expression("Area (km"^2*")"), ylim=c(0,max(cm)*1.3), las=2, cex.names=0.9, border=NA)
      legend("top",legend=c("Loss","No Change","Gain"),fill=c("#E74C3C","#95A5A6","#27AE60"),bty="n",horiz=TRUE,xpd=TRUE)
    }, error=function(e) { plot(1,type="n"); text(1,1,"Error generating chart") })
  })
  
  output$benthic_comparison_plot <- renderPlot({
    df <- calculate_benthic_areas()
    if (is.null(df)) { plot(1,type="n",axes=FALSE,xlab="",ylab=""); text(1,1,"Could not load data",cex=1.5); return() }
    tryCatch({
      par(mfrow=c(1,2), mar=c(10,5,3,2))
      am <- rbind(df$Area_Present_km2, df$Area_Future_km2); colnames(am) <- paste("Unit",df$Benthic_Unit)
      barplot(am, beside=TRUE, col=c("#5E35B1","#FF6F00"), main="Present vs Future Area by Benthic Unit",
              ylab=expression("Area (km"^2*")"), ylim=c(0,max(am,na.rm=TRUE)*1.3), las=2, cex.names=0.7, border=NA)
      legend("top",legend=c("Present","Future 2050"),fill=c("#5E35B1","#FF6F00"),bty="n",horiz=TRUE,xpd=TRUE)
      cm <- rbind(df$Loss_km2, df$No_Change_km2, df$Gain_km2); colnames(cm) <- paste("Unit",df$Benthic_Unit)
      barplot(cm, beside=TRUE, col=c("#E74C3C","#95A5A6","#27AE60"), main="Changes by Category and Benthic Unit",
              ylab=expression("Area (km"^2*")"), ylim=c(0,max(cm,na.rm=TRUE)*1.3), las=2, cex.names=0.7, border=NA)
      legend("top",legend=c("Loss","No Change","Gain"),fill=c("#E74C3C","#95A5A6","#27AE60"),bty="n",horiz=TRUE,xpd=TRUE)
    }, error=function(e) { plot(1,type="n"); text(1,1,"Error generating chart") })
  })
  
  monthly_data <- reactive({
    req(input$species, input$threshold)
    withProgress(message='Calculating monthly data...', value=0, {
      d <- calculate_monthly_areas(input$species, base_path, input$threshold)
      incProgress(1); d
    })
  })
  
  output$species_summary <- renderUI({
    d <- monthly_data(); req(!is.null(d))
    HTML(generate_species_summary(d, species_names[input$species]))
  })
  
  # ==============================================================================
  # SPIDER PLOT
  # ==============================================================================
  output$spider_plot <- renderPlot({
    d <- monthly_data(); req(!is.null(d), nrow(d)>0)
    
    col_lines <- c("#2E86C1", "#27AE60", "#E74C3C")
    col_fill  <- c(rgb(0.18,0.53,0.76,0.20),
                   rgb(0.15,0.68,0.38,0.20),
                   rgb(0.91,0.30,0.24,0.20))
    leg_labels <- c("Present", "Future 2050 SSP 1-2.6", "Future 2050 SSP 5-8.5")
    
    fmt_comma <- function(x, digits=1)
      formatC(round(x, digits), format="f", digits=digits, decimal.mark=",")
    make_rng <- function(v) {
      vok <- v[!is.na(v)]
      if (length(vok)==0) return("0,0 - 0,0%")
      paste0(fmt_comma(min(vok)), " - ", fmt_comma(max(vok)), "%")
    }
    rng_vals <- c(make_rng(d$Presente),
                  make_rng(d$Futuro_2050_SSP126),
                  make_rng(d$Futuro_2050_SSP585))
    
    sd <- as.data.frame(rbind(
      rep(100,12), rep(0,12),
      d$Presente, d$Futuro_2050_SSP126, d$Futuro_2050_SSP585
    ))
    colnames(sd) <- d$Mes
    
    layout(matrix(c(1,2), nrow=1), widths=c(2.1, 1))
    
    par(mar=c(2, 2, 1, 0), xpd=TRUE)
    radarchart(sd, axistype=1,
               pcol=col_lines, pfcol=col_fill,
               plwd=1.8, plty=1,
               cglcol="grey65", cglty=1, cglwd=0.6,
               axislabcol="grey25", vlcex=1.1, calcex=0.8,
               seg=5, title="")
    
    par(mar=c(0, 0, 0, 0.5), xpd=TRUE)
    plot.new()
    plot.window(xlim=c(0,1), ylim=c(0,1))
    
    draw_item <- function(x0, x1, y, col, label, lwd=1.8, cex_txt=0.82) {
      segments(x0, y, x1, y, col=col, lwd=lwd)
      points((x0+x1)/2, y, pch=19, col=col, cex=0.9)
      text(x1+0.04, y, label, adj=c(0,0.5), cex=cex_txt, col="grey10")
    }
    
    y0 <- 0.87; dy <- 0.11; x_l <- 0.04; x_r <- 0.24
    
    for (k in seq_along(leg_labels))
      draw_item(x_l, x_r, y0-(k-1)*dy, col_lines[k], leg_labels[k])
    
    y_rng <- y0 - length(leg_labels)*dy - 0.06
    text(x_l, y_rng+0.03, "Range:", adj=c(0,0.5),
         cex=0.85, font=2, col="grey10")
    
    for (k in seq_along(rng_vals))
      draw_item(x_l, x_r, y_rng-(k-1)*dy-0.02,
                col_lines[k], rng_vals[k], cex_txt=0.78)
    
  }, height=500)
  
  # ==============================================================================
  # SPIDER DATA TABLE — FIX: colnames passed via datatable() parameter,
  # formatStyle uses column indices instead of display names with special chars
  # ==============================================================================
  output$spider_data_table <- renderDT({
    d <- monthly_data(); req(!is.null(d))
    dd <- data.frame(Month=d$Mes,
                     Pp=d$Presente,
                     Pk=d$Presente_km2,
                     S26p=d$Futuro_2050_SSP126,
                     S26k=d$Futuro_2050_SSP126_km2,
                     S85p=d$Futuro_2050_SSP585,
                     S85k=d$Futuro_2050_SSP585_km2)
    datatable(dd,
              options=list(pageLength=12, dom='t', scrollX=TRUE),
              rownames=FALSE,
              colnames=c("Month",
                         "Present (%)", "Present (km²)",
                         "Future SSP 1-2.6 (%)", "Future SSP 1-2.6 (km²)",
                         "Future SSP 5-8.5 (%)", "Future SSP 5-8.5 (km²)")) %>%
      formatRound(2:7, 2) %>%
      formatStyle(columns=c(2, 3), backgroundColor='#E3F2FD', fontWeight='bold') %>%
      formatStyle(columns=c(4, 5), backgroundColor='#E8F5E9', fontWeight='bold') %>%
      formatStyle(columns=c(6, 7), backgroundColor='#FFEBEE', fontWeight='bold')
  })
}

shinyApp(ui=ui, server=server)
