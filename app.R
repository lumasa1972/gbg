# ==============================
# APP SHINY – DASHBOARD COMPLETO (VIGILANCIA EPIDEMIOLÓGICA)
# - Indicador principal recomendado: "Animales muestreados"
# - CSV ; o , (auto-detección) + Excel
# - Diagnóstico + Vista previa
# - Selectores dinámicos
# - Mapa con auto-corrección de coordenadas + clusters
# ==============================

library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(readr)
library(leaflet)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# ------------------------------
# Lectura robusta
# ------------------------------
leer_datos <- function(path, filename = NULL) {
  ext <- tolower(tools::file_ext(filename %||% path))
  
  if (ext == "csv") {
    first_line <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
    delim <- if (grepl(";", first_line)) ";" else ","
    loc <- if (delim == ";") locale(encoding = "UTF-8", decimal_mark = ",") else locale(encoding = "UTF-8")
    
    return(read_delim(
      file = path,
      delim = delim,
      locale = loc,
      trim_ws = TRUE,
      guess_max = 200000,
      show_col_types = FALSE
    ))
  }
  
  if (ext %in% c("xls", "xlsx")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Falta el paquete 'readxl'. Instálalo con install.packages('readxl')")
    }
    return(readxl::read_excel(path))
  }
  
  stop("Formato no soportado. Use CSV o Excel.")
}

# ------------------------------
# Normalización + auto-guess
# ------------------------------
norm <- function(x) {
  x <- tolower(x)
  x <- gsub("á","a", x); x <- gsub("é","e", x); x <- gsub("í","i", x)
  x <- gsub("ó","o", x); x <- gsub("ú","u", x); x <- gsub("ñ","n", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

guess_col <- function(cols, patterns, fallback = cols[1]) {
  nc <- norm(cols)
  for (p in patterns) {
    idx <- which(grepl(p, nc, perl = TRUE))
    if (length(idx) > 0) return(cols[idx[1]])
  }
  fallback
}

# ------------------------------
# Limpieza robusta a número
# (sirve para "1.234,56", "1,234.56", "  123  ", etc.)
# ------------------------------
to_num_robusto <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  
  # intento 1: estilo CR (1.234,56)
  x1 <- gsub("\\.", "", x)
  x1 <- gsub(",", ".", x1)
  x1 <- gsub("[^0-9\\.-]", "", x1)
  n1 <- suppressWarnings(as.numeric(x1))
  
  # intento 2: estilo US (1,234.56)
  x2 <- gsub(",", "", x)
  x2 <- gsub("[^0-9\\.-]", "", x2)
  n2 <- suppressWarnings(as.numeric(x2))
  
  if (sum(!is.na(n1)) >= sum(!is.na(n2))) n1 else n2
}

# ------------------------------
# UI
# ------------------------------
ui <- page_sidebar(
  title = "Dashboard Shiny – Vigilancia Epidemiológica",
  
  sidebar = sidebar(
    fileInput("archivo", "Cargar base (CSV o Excel)", accept = c(".csv",".xls",".xlsx")),
    helpText("Tip: Si el CSV viene con ';' (Excel CR), la app lo detecta automáticamente."),
    hr(),
    
    uiOutput("ui_cols"),
    hr(),
    uiOutput("ui_filtros"),
    hr(),
    
    downloadButton("descargar", "Descargar datos filtrados")
  ),
  
  layout_column_wrap(
    width = 1,
    
    card(
      card_header("Diagnóstico + Vista previa"),
      verbatimTextOutput("diag"),
      uiOutput("aviso_valor"),
      tableOutput("preview")
    ),
    
    layout_column_wrap(
      width = 1/2,
      card(card_header("Resumen"), uiOutput("resumen")),
      card(card_header("Total por año"), plotOutput("plot_anio"))
    ),
    
    layout_column_wrap(
      width = 1/2,
      card(card_header("Serie semanal"), plotOutput("plot_semana")),
      card(card_header("Mapa (clusters)"), leafletOutput("mapa", height = 350))
    )
  )
)

# ------------------------------
# SERVER
# ------------------------------
server <- function(input, output, session) {
  
  # ---- Datos crudos ----
  datos_raw <- reactive({
    req(input$archivo)
    leer_datos(input$archivo$datapath, input$archivo$name)
  })
  
  columnas <- reactive({
    req(datos_raw())
    names(datos_raw())
  })
  
  # ---- Selectores dinámicos (por defecto: Animales muestreados) ----
  output$ui_cols <- renderUI({
    req(columnas())
    cols <- columnas()
    
    default_anio <- guess_col(cols, c("^ano$", "^año$", "year$"), fallback = cols[1])
    default_sem  <- guess_col(cols, c("semana epidemiologica", "^semana$|^semana "), fallback = cols[min(2, length(cols))])
    
    # Preferir Animales muestreados como indicador principal
    default_val <- if ("Animales muestreados" %in% cols) {
      "Animales muestreados"
    } else {
      guess_col(cols, c("animales muestreados", "muestras", "total"), fallback = cols[min(3, length(cols))])
    }
    
    lat_exists <- any(grepl("^latitud$|^lat$|latitud", norm(cols)))
    lon_exists <- any(grepl("^longitud$|^lon$|longitud", norm(cols)))
    default_lat <- if (lat_exists) guess_col(cols, c("^latitud$|^lat$|latitud"), fallback = "") else ""
    default_lon <- if (lon_exists) guess_col(cols, c("^longitud$|^lon$|longitud"), fallback = "") else ""
    
    tagList(
      selectInput("col_anio", "Columna Año", choices = cols, selected = default_anio),
      selectInput("col_semana", "Columna Semana", choices = cols, selected = default_sem),
      selectInput("col_valor", "Indicador (recomendado: Animales muestreados)", choices = cols, selected = default_val),
      selectInput("col_lat", "Latitud (opcional)", choices = c("Ninguna" = "", cols), selected = default_lat),
      selectInput("col_lon", "Longitud (opcional)", choices = c("Ninguna" = "", cols), selected = default_lon)
    )
  })
  
  # ---- Diagnóstico ----
  output$diag <- renderPrint({
    req(datos_raw())
    df <- datos_raw()
    cat("Filas:", nrow(df), "\n")
    cat("Columnas:", ncol(df), "\n\n")
    cat("Primeros nombres de columnas:\n")
    print(head(names(df), 25))
    cat("\nSelección actual:\n")
    cat("Año   :", input$col_anio, "\n")
    cat("Semana:", input$col_semana, "\n")
    cat("Valor :", input$col_valor, "\n")
    cat("Lat   :", input$col_lat, "\n")
    cat("Lon   :", input$col_lon, "\n")
  })
  
  # ---- Vista previa SIEMPRE ----
  output$preview <- renderTable({
    req(datos_raw())
    head(datos_raw(), 10)
  })
  
  # ---- Datos estandarizados + validación suave del valor ----
  datos_std <- reactive({
    req(datos_raw(), input$col_anio, input$col_semana, input$col_valor)
    df <- datos_raw()
    
    validate(
      need(input$col_anio %in% names(df), "Selecciona una columna válida de Año."),
      need(input$col_semana %in% names(df), "Selecciona una columna válida de Semana."),
      need(input$col_valor %in% names(df), "Selecciona una columna válida de Indicador.")
    )
    
    df <- df %>%
      mutate(
        anio   = suppressWarnings(as.integer(.data[[input$col_anio]])),
        semana = suppressWarnings(as.integer(.data[[input$col_semana]])),
        valor  = to_num_robusto(.data[[input$col_valor]])
      )
    
    # validación suave: NO rompe la app, solo marca si no es numérico
    attr(df, "valor_ok") <- !all(is.na(df$valor))
    
    df
  })
  
  # ---- Aviso si el indicador no es numérico ----
  output$aviso_valor <- renderUI({
    req(datos_std())
    ok <- isTRUE(attr(datos_std(), "valor_ok"))
    if (ok) return(NULL)
    
    div(
      class = "alert alert-warning",
      "⚠️ El indicador seleccionado no se pudo convertir a número. ",
      "Elige una columna numérica (recomendado: 'Animales muestreados')."
    )
  })
  
  # ---- UI filtros (solo si valor es numérico) ----
  output$ui_filtros <- renderUI({
    req(datos_std())
    if (!isTRUE(attr(datos_std(), "valor_ok"))) return(NULL)
    
    df <- datos_std()
    years <- sort(unique(df$anio))
    wmin <- min(df$semana, na.rm = TRUE)
    wmax <- max(df$semana, na.rm = TRUE)
    
    tagList(
      selectizeInput(
        "anio_sel", "Año",
        choices = years, selected = years,
        multiple = TRUE,
        options = list(plugins = list("remove_button"))
      ),
      sliderInput(
        "semana_sel", "Rango de semanas",
        min = wmin, max = wmax,
        value = c(wmin, wmax),
        step = 1
      )
    )
  })
  
  # ---- Filtrado (solo si valor es numérico) ----
  datos_filtrados <- reactive({
    req(datos_std())
    df <- datos_std()
    if (!isTRUE(attr(df, "valor_ok"))) return(NULL)
    
    if (is.null(input$anio_sel) || is.null(input$semana_sel)) return(df)
    
    df %>%
      filter(anio %in% as.integer(input$anio_sel)) %>%
      filter(semana >= input$semana_sel[1], semana <= input$semana_sel[2])
  })
  
  # ---- Resumen ----
  output$resumen <- renderUI({
    df <- datos_filtrados()
    if (is.null(df)) return(NULL)
    
    tagList(
      p(strong("Registros:"), nrow(df)),
      p(strong("Total:"), format(sum(df$valor, na.rm = TRUE), big.mark = ",", scientific = FALSE))
    )
  })
  
  # ---- Total por año ----
  output$plot_anio <- renderPlot({
    df <- datos_filtrados()
    if (is.null(df)) return(NULL)
    
    df %>%
      group_by(anio) %>%
      summarise(total = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = factor(anio), y = total)) +
      geom_col() +
      labs(x = "Año", y = paste0("Total (", input$col_valor, ")")) +
      theme_minimal()
  })
  
  # ---- Serie semanal ----
  output$plot_semana <- renderPlot({
    df <- datos_filtrados()
    if (is.null(df)) return(NULL)
    
    df %>%
      group_by(anio, semana) %>%
      summarise(total = sum(valor, na.rm = TRUE), .groups = "drop") %>%
      ggplot(aes(x = semana, y = total)) +
      geom_line() +
      geom_point() +
      facet_wrap(~ anio, scales = "free_y") +
      labs(x = "Semana epidemiológica", y = paste0("Total (", input$col_valor, ")")) +
      theme_minimal()
  })
  
  # ---- Mapa (auto-corrección coordenadas + clusters) ----
  output$mapa <- renderLeaflet({
    df <- datos_filtrados()
    if (is.null(df)) {
      return(leaflet() %>% addTiles() %>% setView(lng = -84.1, lat = 9.9, zoom = 7))
    }
    
    if (is.null(input$col_lat) || is.null(input$col_lon) || input$col_lat == "" || input$col_lon == "") {
      return(leaflet() %>% addTiles() %>% setView(lng = -84.1, lat = 9.9, zoom = 7))
    }
    
    dfm <- df %>%
      mutate(
        lat_raw = suppressWarnings(as.numeric(.data[[input$col_lat]])),
        lon_raw = suppressWarnings(as.numeric(.data[[input$col_lon]]))
      ) %>%
      mutate(
        lat = case_when(
          is.finite(lat_raw) & abs(lat_raw) > 90 ~ lat_raw / 10000,
          TRUE ~ lat_raw
        ),
        lon = case_when(
          is.finite(lon_raw) & abs(lon_raw) > 180 ~ lon_raw / 10000,
          TRUE ~ lon_raw
        )
      ) %>%
      filter(is.finite(lat), is.finite(lon)) %>%
      filter(lat >= -90, lat <= 90, lon >= -180, lon <= 180)
    
    if (nrow(dfm) == 0) {
      return(
        leaflet() %>% addTiles() %>% setView(lng = -84.1, lat = 9.9, zoom = 7) %>%
          addPopups(lng = -84.1, lat = 9.9, popup = "No hay coordenadas válidas con el filtro actual.")
      )
    }
    
    # tamaño por valor (Animales muestreados)
    v <- dfm$valor
    v[is.na(v)] <- 0
    dfm$radio <- pmax(3, pmin(15, sqrt(v + 1)))
    
    leaflet(dfm) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~radio,
        stroke = FALSE,
        fillOpacity = 0.75,
        popup = ~paste0(
          "<b>Año:</b> ", anio,
          "<br><b>Semana:</b> ", semana,
          "<br><b>", input$col_valor, ":</b> ", format(valor, big.mark = ",", scientific = FALSE)
        ),
        clusterOptions = markerClusterOptions()
      ) %>%
      fitBounds(
        lng1 = min(dfm$lon, na.rm = TRUE),
        lat1 = min(dfm$lat, na.rm = TRUE),
        lng2 = max(dfm$lon, na.rm = TRUE),
        lat2 = max(dfm$lat, na.rm = TRUE)
      )
  })
  
  # ---- Descargar ----
  output$descargar <- downloadHandler(
    filename = function() paste0("datos_filtrados_", Sys.Date(), ".csv"),
    content = function(file) {
      df <- datos_filtrados()
      if (is.null(df)) {
        write.csv(data.frame(), file, row.names = FALSE, fileEncoding = "UTF-8")
      } else {
        write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    }
  )
}

shinyApp(ui, server)
