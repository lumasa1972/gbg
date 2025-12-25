#' Create Base Map
#'
#' Create a base Leaflet map with standard configuration
#'
#' @param tipo Map type: "fijo" (fixed view) or "base" (dynamic)
#' @param lng Longitude for center (default: -84)
#' @param lat Latitude for center (default: 9.9)
#' @param zoom Zoom level (default: 8)
#' @param mensaje_error Error message to display (default: NULL)
#' @return A leaflet map object
#' @export
#' @importFrom leaflet leaflet addProviderTiles providers setView addControl
#' @examples
#' \dontrun{
#' crear_mapa(tipo = "fijo")
#' }
crear_mapa <- function(tipo = "base", lng = -84, lat = 9.9, zoom = 8, mensaje_error = NULL) {
  m <- leaflet::leaflet() %>% 
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron)
  
  if (tipo == "fijo") {
    # Mapa con vista fija (no fitBounds)
    m <- m %>% leaflet::setView(lng = lng, lat = lat, zoom = zoom)
  }
  
  if (!is.null(mensaje_error)) {
    m <- m %>% leaflet::addControl(html = mensaje_error, position = "topright")
  }
  
  return(m)
}
