#' SENASA Theme for ggplot2
#'
#' Custom theme for epidemiological charts following SENASA style guidelines
#'
#' @return A ggplot2 theme object
#' @export
#' @importFrom ggplot2 theme_minimal theme element_blank element_text margin
#' @examples
#' \dontrun{
#' library(ggplot2)
#' ggplot(data, aes(x, y)) + geom_line() + theme_senasa()
#' }
theme_senasa <- function() {
  ggplot2::theme_minimal(base_family = "sans") +
    ggplot2::theme(
      plot.title   = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0, size = 9),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(size = 9),
      legend.text  = ggplot2::element_text(size = 8),
      axis.title   = ggplot2::element_text(size = 10),
      axis.text    = ggplot2::element_text(size = 9),
      plot.margin  = ggplot2::margin(6, 10, 2, 10)
    )
}

#' Create Blank Plot with Message
#'
#' Create a blank plot with a centered text message
#'
#' @param msg Message to display
#' @return A ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot annotate theme_void
#' @examples
#' blank_plot("No data available")
blank_plot <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg, size = 5) +
    ggplot2::theme_void()
}

#' Create Standardized Chart
#'
#' Create a chart with standardized SENASA configuration
#'
#' @param df Data frame
#' @param x_var X-axis variable name
#' @param y_var Y-axis variable name
#' @param tipo Chart type ("linea")
#' @param agregar_puntos Add points to line chart (default: FALSE)
#' @param agregar_etiquetas Add value labels (default: FALSE)
#' @param color_var Variable for color grouping (default: NULL)
#' @param titulo_x X-axis title (default: "")
#' @param titulo_y Y-axis title (default: "")
#' @return A ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_point geom_text scale_color_manual labs
#' @importFrom rlang .data
#' @examples
#' \dontrun{
#' crear_grafico(df, "Semana", "casos", titulo_x = "Semana", titulo_y = "Casos")
#' }
crear_grafico <- function(df, x_var, y_var, tipo = "linea", agregar_puntos = FALSE, 
                          agregar_etiquetas = FALSE, color_var = NULL, titulo_x = "", 
                          titulo_y = "") {
  pie <- "Fuente: SENASA – SIVE-PNTrans, 2026"
  
  # Colores por año estandarizados
  colores_anio <- c("2023" = "blue", "2024" = "orange", "2025" = "red", "2026" = "darkgreen")
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]]))
  
  # Aplicar color si se especifica
  if (!is.null(color_var)) {
    p <- p + ggplot2::aes(color = factor(.data[[color_var]]), group = .data[[color_var]])
  }
  
  # Tipo de gráfico
  if (tipo == "linea") {
    p <- p + ggplot2::geom_line()
  }
  
  if (agregar_puntos) {
    p <- p + ggplot2::geom_point()
  }
  
  if (agregar_etiquetas) {
    p <- p + ggplot2::geom_text(ggplot2::aes(label = fmt_miles(.data[[y_var]])), 
                                 vjust = -0.4, size = 2.5, show.legend = FALSE)
  }
  
  # Escala de colores
  if (!is.null(color_var)) {
    p <- p + ggplot2::scale_color_manual(values = colores_anio, drop = FALSE, name = "Año")
  }
  
  # Etiquetas
  p <- p + ggplot2::labs(x = titulo_x, y = titulo_y, caption = pie) + theme_senasa()
  
  return(p)
}
