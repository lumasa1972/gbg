# =========================================================
# Dashboard epidemiológico GBG (SENASA)
# Shiny app para análisis de datos epidemiológicos
# =========================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readr)
library(stringi)
library(sf)
library(leaflet)
library(gbg)  # Load package functions

# ===============================
# Cargar datos
# ===============================
# Path relative to inst/shiny/
CSV_PATH <- system.file("extdata", "Base_datos_semana_50-25.csv", package = "gbg")
if (CSV_PATH == "" || !file.exists(CSV_PATH)) {
  # Fallback to local file if package data not found
  CSV_PATH <- "../../Base_datos_semana_50-25.csv"
}

datos <- read_delim(
  CSV_PATH,
  delim = ";",
  locale = locale(encoding = "UTF-8", decimal_mark = ","),
  trim_ws = TRUE,
  guess_max = 200000,
  show_col_types = FALSE
)

# Columnas esperadas (tras normalización)
# Semana epidemiológica -> Semana_epidemiologica
# Año -> Ano
# Animales muestreados -> Animales_muestreados
# Región -> Region
# Cantón -> Canton
# Longitud / Latitud -> Longitud / Latitud
# Casos -> Casos

# Coordenadas exactas del CSV
LAT_COL <- "Latitud"
LON_COL <- "Longitud"

# Aplicar normalización
datos <- normalizar_columnas(datos)

anios_disponibles <- sort(unique(datos$Ano))

# ===============================
# Shape cantones
# ===============================
# Ajusta este path si tu carpeta es distinta
MAP_PATH <- system.file("extdata", "Cantones_de_Costa_Rica", "Cantones_de_Costa_Rica.shp", package = "gbg")
if (MAP_PATH == "" || !file.exists(MAP_PATH)) {
  # Fallback to local path
  MAP_PATH <- "../../data/Cantones_de_Costa_Rica/Cantones_de_Costa_Rica.shp"
}

cr_cantones <- NULL
if (file.exists(MAP_PATH)) {
  cr_cantones <- tryCatch(
    {
      shp <- st_read(MAP_PATH, quiet = TRUE)
      # Intenta adivinar nombre del cantón del shp
      cand <- intersect(names(shp), c("NOM_CANT_1", "NOMBRE_CANTON", "CANTON", "CANTON_NOM"))
      if (length(cand) == 0) cand <- names(shp)[1]
      shp$CANTON_KEY <- norm_key(shp[[cand[1]]])
      st_transform(shp, 4326)
    },
    error = function(e) NULL
  )
}

# ===============================
# UI
# ===============================
ui <- dashboardPage(
  dashboardHeader(title = "Análisis epidemiológico – GBG"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Resumen", tabName = "resumen", icon = icon("chart-line")),
      hr(),
      selectInput("anio", "Año", choices = c("Todos", anios_disponibles), selected = "Todos"),
      selectInput("region", "Región", choices = c("Todas", sort(unique(datos$Region))), selected = "Todas"),
      selectInput("especie", "Especie", choices = c("Todas", sort(unique(datos$Especie))), selected = "Todas"),
      selectInput("tipo_caso", "Casos", choices = c("Todos", "NUEVOS", "ACUMULADOS"), selected = "Todos"),
      actionButton("procesar", "Procesar", icon = icon("play"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        /* Sidebar 25% más angosta aprox */
        .main-sidebar { width:150px; }
        .content-wrapper, .main-footer { margin-left:150px; }
        .main-header .logo { width:150px; }
        .main-header .navbar { margin-left:150px; }

        /* Leyendas Leaflet compactas */
        .leaflet-control.leaflet-legend {
          max-width:160px !important;
          padding:6px 8px !important;
          font-size:11px !important;
        }

        /* Boxes: títulos un poco más pequeños */
        .box .box-header .box-title { font-size: 16px; }
      "))
    ),

    tabItems(
      tabItem(
        tabName = "resumen",

        # KPI
        fluidRow(
          valueBoxOutput("kpi_registros", width = 3),
          valueBoxOutput("kpi_muestreados", width = 3),
          valueBoxOutput("kpi_nuevos", width = 3),
          valueBoxOutput("kpi_cantones", width = 3)
        ),

        # Tabs analíticos (como la imagen)
        fluidRow(
          box(
            title = "Tendencia temporal de casos nuevos",
            width = 12, status = "primary", solidHeader = TRUE,
            tabBox(
              width = 12,
              tabPanel("Casos nuevos", plotOutput("plot_nuevos", height = 280)),
              tabPanel("Detección de anomalías", plotOutput("plot_anom", height = 280)),
              tabPanel("Predicción de tendencia", plotOutput("plot_pred", height = 280))
            )
          )
        ),

        # Comparativos semana / mes (mismo tamaño)
        fluidRow(
          column(
            width = 6,
            box(
              title = "Comparativo semanal de animales muestreados por año",
              width = 12, status = "primary", solidHeader = TRUE,
              plotOutput("plot_semana", height = 260)
            )
          ),
          column(
            width = 6,
            box(
              title = "Comparativo mensual de animales muestreados por año",
              width = 12, status = "primary", solidHeader = TRUE,
              plotOutput("plot_mes", height = 260)
            )
          )
        ),

        # Mapas lado a lado
        fluidRow(
          column(
            width = 6,
            box(
              title = "Mapa coroplético – Casos por cantón",
              width = 12, status = "primary", solidHeader = TRUE,
              leafletOutput("map_cr", height = 460)
            )
          ),
          column(
            width = 6,
            box(
              title = "Mapa georreferenciado – Puntos (casos)",
              width = 12, status = "primary", solidHeader = TRUE,
              leafletOutput("map_pts", height = 460)
            )
          )
        )
      )
    )
  )
)

# ===============================
# SERVER
# ===============================
server <- function(input, output, session) {

  # 1) Base (sin tipo de caso)
  base_df <- eventReactive(input$procesar, {
    aplicar_filtros(datos, anio = input$anio, region = input$region, especie = input$especie)
  }, ignoreInit = FALSE)

  # 2) Aplicar tipo de caso
  df_ok <- eventReactive(input$procesar, {
    aplicar_filtros(datos, anio = input$anio, region = input$region, especie = input$especie, tipo_caso = input$tipo_caso)
  }, ignoreInit = FALSE)

  # ===============================
  # KPI
  # ===============================
  output$kpi_registros <- renderValueBox({
    df <- df_ok()
    valueBox(value = fmt_miles(nrow(df)), subtitle = "Registros", icon = icon("database"), color = "aqua")
  })

  output$kpi_muestreados <- renderValueBox({
    df <- df_ok()
    tot <- sum(df$Animales_muestreados, na.rm = TRUE)
    valueBox(value = fmt_miles(tot), subtitle = "Animales muestreados", icon = icon("vial"), color = "green")
  })

  output$kpi_nuevos <- renderValueBox({
    df <- base_df()
    df_n <- df %>% filter(Casos == "NUEVOS")
    tot <- sum(df_n$Animales_muestreados, na.rm = TRUE)
    valueBox(value = fmt_miles(tot), subtitle = "Casos NUEVOS", icon = icon("circle-plus"), color = "yellow")
  })

  output$kpi_cantones <- renderValueBox({
    df <- base_df()
    if (!"Canton" %in% names(df)) {
      valueBox(value = "ND", subtitle = "Cantones con casos", icon = icon("map"), color = "purple")
    } else {
      cant <- df %>% filter(!is.na(Canton), Canton != "") %>% distinct(Canton) %>% nrow()
      total_cant <- if (!is.null(cr_cantones)) nrow(cr_cantones) else NA_integer_
      pct <- if (!is.na(total_cant) && total_cant > 0) paste0(" (", round(100 * cant / total_cant, 1), "%)") else ""
      valueBox(value = paste0(fmt_miles(cant), pct), subtitle = "Cantones con casos", icon = icon("map"), color = "purple")
    }
  })

  # ===============================
  # Comparativo SEMANAL (sin puntos / sin etiquetas)
  # ===============================
  output$plot_semana <- renderPlot({
    df <- df_ok()
    if (nrow(df) == 0) return(ggplot() + theme_void())

    if (input$anio == "Todos") {
      dfp <- df %>%
        group_by(Ano, Semana_epidemiologica) %>%
        summarise(m = sum(Animales_muestreados, na.rm = TRUE), .groups = "drop")

      p <- crear_grafico(
        dfp, 
        x_var = "Semana_epidemiologica", 
        y_var = "m", 
        tipo = "linea",
        color_var = "Ano",
        titulo_x = "Semana epidemiológica", 
        titulo_y = "Número de Animales Muestreados"
      )

      # MM3 SOLO 2026
      df26 <- dfp %>% filter(Ano == 2026) %>% arrange(Semana_epidemiologica)
      if (nrow(df26) >= 3) {
        df26$mm3 <- movavg_k(df26$m, 3)
        p <- p + geom_line(
          data = df26,
          aes(Semana_epidemiologica, mm3),
          inherit.aes = FALSE,
          linetype = "dashed",
          color = "black"
        )
      }
      p
    } else {
      anio_sel <- as.integer(input$anio)
      dfp <- df %>%
        filter(Ano == anio_sel) %>%
        group_by(Semana_epidemiologica) %>%
        summarise(m = sum(Animales_muestreados, na.rm = TRUE), .groups = "drop") %>%
        arrange(Semana_epidemiologica)

      col <- if (anio_sel == 2023) "blue" else if (anio_sel == 2024) "orange" else if (anio_sel == 2025) "red" else "darkgreen"

      p <- ggplot(dfp, aes(Semana_epidemiologica, m)) +
        geom_line(color = col) +
        labs(x = "Semana epidemiológica", y = "Número de Animales Muestreados", caption = "Fuente: SENASA – SIVE-PNTrans, 2026") +
        theme_senasa()

      if (anio_sel == 2026 && nrow(dfp) >= 3) {
        dfp$mm3 <- movavg_k(dfp$m, 3)
        p <- p + geom_line(aes(y = mm3), linetype = "dashed", color = "black")
      }
      p
    }
  })

  # ===============================
  # Comparativo MENSUAL (con valores)
  # ===============================
  output$plot_mes <- renderPlot({
    df <- df_ok()
    if (nrow(df) == 0) return(ggplot() + theme_void())

    dfm <- df %>%
      filter(!is.na(Mes)) %>%
      group_by(Ano, Mes) %>%
      summarise(m = sum(Animales_muestreados, na.rm = TRUE), .groups = "drop")

    if (nrow(dfm) == 0) return(ggplot() + theme_void())

    crear_grafico(
      dfm,
      x_var = "Mes",
      y_var = "m",
      tipo = "linea",
      agregar_puntos = TRUE,
      agregar_etiquetas = TRUE,
      color_var = "Ano",
      titulo_x = "Mes",
      titulo_y = "Número de Animales Muestreados"
    ) + scale_x_continuous(breaks = 1:12)
  })

  # ===============================
  # Tabs analíticos (NUEVOS / ANOMALÍAS / PREDICCIÓN)
  # ===============================
  weekly_new <- reactive({
    df <- base_df() %>% filter(Casos == "NUEVOS")
    if (nrow(df) == 0) return(NULL)

    df %>%
      group_by(Ano, Semana_epidemiologica) %>%
      summarise(nuevos = sum(Animales_muestreados, na.rm = TRUE), .groups = "drop") %>%
      arrange(Ano, Semana_epidemiologica)
  })

  output$plot_nuevos <- renderPlot({
    w <- weekly_new()
    if (is.null(w) || nrow(w) == 0) return(ggplot() + theme_void())

    crear_grafico(
      w,
      x_var = "Semana_epidemiologica",
      y_var = "nuevos",
      tipo = "linea",
      agregar_puntos = TRUE,
      color_var = "Ano",
      titulo_x = "Semana",
      titulo_y = "Casos nuevos (suma)"
    )
  })

  output$plot_anom <- renderPlot({
    pie <- "Fuente: SENASA – SIVE-PNTrans, 2026"

    # Usar TODOS los registros filtrados (no solo NUEVOS)
    df <- df_ok()
    if (nrow(df) == 0) return(blank_plot("No hay datos para calcular anomalías con los filtros actuales."))

    # Elegir el año a analizar:
    # - Si "Todos": usar el año más reciente disponible en el set filtrado
    # - Si un año específico: usar ese
    y_sel <- if (input$anio == "Todos") max(df$Ano, na.rm = TRUE) else as.integer(input$anio)

    ww <- df %>%
      filter(Ano == y_sel) %>%
      group_by(Semana_epidemiologica) %>%
      summarise(valor = sum(Animales_muestreados, na.rm = TRUE), .groups = "drop") %>%
      arrange(Semana_epidemiologica)

    if (nrow(ww) < 5) {
      return(blank_plot(paste0("No hay suficientes observaciones semanales para el año ", y_sel, ".")))
    }

    # Z-score semanal:
    # z_t = (x_t - mu_t) / sigma_t
    # donde mu_t y sigma_t se calculan con el histórico acumulado (semanas 1..t)
    ww$mu_t <- sapply(seq_len(nrow(ww)), function(i) mean(ww$valor[1:i], na.rm = TRUE))
    ww$sig_t <- sapply(seq_len(nrow(ww)), function(i) stats::sd(ww$valor[1:i], na.rm = TRUE))
    ww$z <- (ww$valor - ww$mu_t) / ww$sig_t

    # Se considera anomalía cuando |Z| > 2
    ww$anom <- !is.na(ww$z) & is.finite(ww$z) & abs(ww$z) > 2

    ggplot(ww, aes(Semana_epidemiologica, valor)) +
      geom_ribbon(
        aes(ymin = mu_t - 2 * sig_t, ymax = mu_t + 2 * sig_t),
        inherit.aes = FALSE,
        fill = "grey70",
        alpha = 0.35
      ) +
      geom_line(color = "blue", linewidth = 1) +
      geom_point(color = "blue", size = 1.6) +
      geom_line(aes(y = mu_t), linetype = "dashed", color = "black", linewidth = 0.8) +
      geom_point(
        data = ww %>% filter(anom),
        aes(Semana_epidemiologica, valor),
        inherit.aes = FALSE,
        color = "red",
        size = 2.5
      ) +
      labs(
        x = "Semana",
        y = "Número de Animales Muestreados",
        caption = pie
      ) +
      theme_senasa()
  })


  output$plot_pred <- renderPlot({
    pie <- "Fuente: SENASA – SIVE-PNTrans, 2026"
    w <- weekly_new()
    if (is.null(w) || nrow(w) < 8) return(ggplot() + theme_void())

    y_sel <- if (input$anio == "Todos") max(w$Ano, na.rm = TRUE) else as.integer(input$anio)
    ww <- w %>% filter(Ano == y_sel) %>% arrange(Semana_epidemiologica)
    if (nrow(ww) < 8) return(ggplot() + theme_void())

    # Predicción simple: media móvil 4 semanas (solo como tendencia)
    ww$ma4 <- movavg_k(ww$nuevos, 4)

    ggplot(ww, aes(Semana_epidemiologica, nuevos)) +
      geom_line(color = "#2C3E50") +
      geom_point(size = 1.5, color = "#2C3E50") +
      geom_line(aes(y = ma4), linetype = "dashed", color = "darkgreen") +
      labs(x = "Semana", y = "Casos nuevos", caption = pie) +
      theme_senasa()
  })

  # ===============================
  # MAPA COROPLÉTICO (FIJO con setView)
  # ===============================
  output$map_cr <- renderLeaflet({
    if (is.null(cr_cantones)) {
      return(
        crear_mapa(
          tipo = "fijo",
          mensaje_error = "No se pudo leer el shapefile. Ajusta <b>MAP_PATH</b>."
        )
      )
    }

    df <- base_df()
    if (!"Canton" %in% names(df)) df$Canton <- NA_character_
    df <- df %>% mutate(CANTON_KEY = norm_key(Canton))
    if (input$tipo_caso != "Todos") df <- df %>% filter(Casos == input$tipo_caso)

    resumen <- df %>%
      group_by(CANTON_KEY) %>%
      summarise(valor = sum(Animales_muestreados, na.rm = TRUE), .groups = "drop")

    shp <- cr_cantones %>% left_join(resumen, by = "CANTON_KEY")
    pal <- colorNumeric("YlOrRd", domain = shp$valor, na.color = "#f0f0f0")

    crear_mapa(tipo = "fijo") %>%
      addPolygons(
        data = shp,
        fillColor = ~pal(valor),
        fillOpacity = 0.8,
        color = "#666666",
        weight = 0.5,
        label = ~paste0(CANTON_KEY, ": ", ifelse(is.na(valor), "0", fmt_miles(valor)))
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal,
        values = shp$valor,
        title = "Casos",
        opacity = 0.9,
        labFormat = labelFormat(digits = 0, big.mark = ","),
        na.label = "Sin dato",
        className = "leaflet-legend"
      )
  })

  # ===============================
  # MAPA PUNTOS (fitBounds SOLO aquí)
  # ===============================
  output$map_pts <- renderLeaflet({

    df <- base_df()
    if (input$tipo_caso != "Todos") df <- df %>% filter(Casos == input$tipo_caso)

    if (!(LAT_COL %in% names(df)) || !(LON_COL %in% names(df))) {
      return(
        crear_mapa(
          tipo = "fijo",
          mensaje_error = "No se encontraron columnas <b>Latitud</b> / <b>Longitud</b>."
        )
      )
    }

    # Asegurar columnas para popup
    if (!"Canton" %in% names(df)) df$Canton <- NA_character_
    if (!"Distrito" %in% names(df)) df$Distrito <- NA_character_
    if (!"Especie" %in% names(df)) df$Especie <- NA_character_
    if (!"Casos" %in% names(df)) df$Casos <- NA_character_
    if (!"Animales_muestreados" %in% names(df)) df$Animales_muestreados <- NA_real_

    df <- df %>%
      mutate(
        lat = suppressWarnings(as.numeric(gsub(",", ".", as.character(.data[[LAT_COL]])))),
        lon = suppressWarnings(as.numeric(gsub(",", ".", as.character(.data[[LON_COL]]))))
      ) %>%
      mutate(
        lat = ifelse(!is.na(lat) & abs(lat) > 90,  lat / 1e6, lat),
        lon = ifelse(!is.na(lon) & abs(lon) > 200, lon / 1e6, lon)
      )

    # Corregir signo de longitud si viniera positiva
    if (sum(!is.na(df$lon)) > 0 && stats::median(df$lon, na.rm = TRUE) > 0) df$lon <- -abs(df$lon)

    # Filtrar a CR (evita outliers)
    df <- df %>%
      filter(!is.na(lat), !is.na(lon), is.finite(lat), is.finite(lon)) %>%
      filter(lat >= 5 & lat <= 12.5, lon >= -86.5 & lon <= -82.0)

    if (nrow(df) == 0) {
      return(
        crear_mapa(
          tipo = "fijo",
          mensaje_error = "No hay puntos válidos con los filtros actuales."
        )
      )
    }

    pal_pts <- colorFactor(
      palette = c("NUEVOS" = "blue", "ACUMULADOS" = "red"),
      domain = c("NUEVOS", "ACUMULADOS")
    )

    df$popup <- paste0(
      "<b>Cantón:</b> ", ifelse(is.na(df$Canton), "ND", df$Canton), "<br>",
      "<b>Distrito:</b> ", ifelse(is.na(df$Distrito), "ND", df$Distrito), "<br>",
      "<b>Especie:</b> ", ifelse(is.na(df$Especie), "ND", df$Especie), "<br>",
      "<b>Tipo de caso:</b> ", ifelse(is.na(df$Casos), "ND", df$Casos), "<br>",
      "<b>Animales muestreados:</b> ", ifelse(is.na(df$Animales_muestreados), "0", fmt_miles(df$Animales_muestreados))
    )

    crear_mapa(tipo = "base") %>%
      fitBounds(
        lng1 = min(df$lon, na.rm = TRUE),
        lat1 = min(df$lat, na.rm = TRUE),
        lng2 = max(df$lon, na.rm = TRUE),
        lat2 = max(df$lat, na.rm = TRUE)
      ) %>%
      addCircleMarkers(
        data = df,
        lng = ~lon, lat = ~lat,
        radius = 4,
        stroke = TRUE, weight = 0.6, color = "#222222",
        fillOpacity = 0.8,
        fillColor = ~pal_pts(Casos),
        popup = ~popup
      ) %>%
      addLegend(
        position = "bottomleft",
        pal = pal_pts,
        values = c("NUEVOS", "ACUMULADOS"),
        title = "Tipo de caso",
        opacity = 1,
        className = "leaflet-legend"
      )
  })
}

shinyApp(ui, server)
