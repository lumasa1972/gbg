# GBG Dashboard Launcher
# 
# Para ejecutar el dashboard, usa una de estas opciones:
#
# Opción 1: Desde el paquete instalado (recomendado)
# library(gbg)
# run_dashboard()
#
# Opción 2: Desarrollo local (sin instalar)
# shiny::runApp("inst/shiny")
#
# Opción 3: Cargar funciones y ejecutar
# devtools::load_all()
# run_dashboard()

# Ejecutar directamente en modo desarrollo
if (interactive()) {
  cat("Ejecutando dashboard en modo desarrollo...\n")
  cat("Ruta: inst/shiny/\n\n")
  shiny::runApp("inst/shiny", launch.browser = TRUE)
} else {
  cat("Para ejecutar el dashboard:\n")
  cat("  library(gbg)\n")
  cat("  run_dashboard()\n")
  cat("\nO en modo desarrollo:\n")
  cat("  shiny::runApp('inst/shiny')\n")
}
