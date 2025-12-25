#' Normalize Column Names and Data Types
#'
#' Normalizes column names, converts data types, and filters invalid data
#'
#' @param df Data frame to normalize
#' @return Data frame with normalized columns
#' @export
#' @importFrom stringi stri_trans_general
#' @importFrom readr parse_number locale
#' @importFrom dplyr filter
#' @examples
#' \dontrun{
#' datos_normalized <- normalizar_columnas(datos_raw)
#' }
normalizar_columnas <- function(df) {
  # Normalizar nombres de columnas
  names(df) <- stringi::stri_trans_general(names(df), "Latin-ASCII")
  names(df) <- gsub("[^A-Za-z0-9 ]", "", names(df))
  names(df) <- gsub("\\s+", "_", trimws(names(df)))
  
  # Tipos
  if ("Semana_epidemiologica" %in% names(df)) {
    df$Semana_epidemiologica <- suppressWarnings(as.integer(df$Semana_epidemiologica))
  }
  if ("Ano" %in% names(df)) {
    df$Ano <- suppressWarnings(as.integer(df$Ano))
  }
  if ("Mes" %in% names(df)) {
    df$Mes <- suppressWarnings(as.integer(df$Mes))
  }
  
  # Numérico (acepta comas/puntos)
  if ("Animales_muestreados" %in% names(df)) {
    df$Animales_muestreados <- readr::parse_number(
      as.character(df$Animales_muestreados),
      locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
    )
  }
  
  # Texto a mayúsculas
  for (c in c("Especie", "Region", "Provincia", "Canton", "Distrito", "Casos")) {
    if (c %in% names(df)) df[[c]] <- norm_key(df[[c]])
  }
  
  # Filtrar datos inválidos
  df <- df %>%
    dplyr::filter(!is.na(Ano), !is.na(Semana_epidemiologica)) %>%
    dplyr::filter(!is.na(Animales_muestreados), is.finite(Animales_muestreados))
  
  return(df)
}

#' Apply Dynamic Filters
#'
#' Apply filters to epidemiological data based on year, region, species, and case type
#'
#' @param df Data frame to filter
#' @param anio Year filter (default: "Todos")
#' @param region Region filter (default: "Todas")
#' @param especie Species filter (default: "Todas")
#' @param tipo_caso Case type filter (default: "Todos")
#' @return Filtered data frame
#' @export
#' @importFrom dplyr filter
#' @examples
#' \dontrun{
#' datos_filtrados <- aplicar_filtros(datos, anio = "2024", region = "CENTRAL")
#' }
aplicar_filtros <- function(df, anio = "Todos", region = "Todas", especie = "Todas", tipo_caso = "Todos") {
  if (anio != "Todos") df <- df %>% dplyr::filter(Ano == as.integer(anio))
  if (region != "Todas") df <- df %>% dplyr::filter(Region == region)
  if (especie != "Todas") df <- df %>% dplyr::filter(Especie == especie)
  if (tipo_caso != "Todos") df <- df %>% dplyr::filter(Casos == tipo_caso)
  return(df)
}
