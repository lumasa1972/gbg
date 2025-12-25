# gbg: Dashboard GBG Epidemiological Analysis

Paquete R para análisis de datos epidemiológicos de GBG (SENASA).

## Instalación

```r
# Instalar desde GitHub
# install.packages("devtools")
devtools::install_github("lumasa1972/gbg")
```

## Uso

### Ejecutar el dashboard

**Opción 1: Desde el paquete instalado (recomendado)**
```r
library(gbg)
run_dashboard()
```

**Opción 2: Modo desarrollo (sin instalar)**
```r
# Desde el directorio raíz del proyecto
source("run_app.R")

# O directamente
shiny::runApp("inst/shiny")
```

**Opción 3: Desarrollo con devtools**
```r
devtools::load_all()  # Carga el paquete
run_dashboard()       # Ejecuta el dashboard
```

### Usar las funciones de análisis

```r
library(gbg)

# Normalizar datos
datos_normalizados <- normalizar_columnas(datos_raw)

# Aplicar filtros
datos_filtrados <- aplicar_filtros(datos_normalizados, 
                                    anio = "2024", 
                                    region = "CENTRAL")

# Crear gráficos
crear_grafico(datos_filtrados, 
              x_var = "Semana_epidemiologica", 
              y_var = "Animales_muestreados",
              titulo_x = "Semana", 
              titulo_y = "Casos")
```

## Funciones principales

- `run_dashboard()`: Lanza el dashboard interactivo de Shiny
- `normalizar_columnas()`: Normaliza nombres de columnas y tipos de datos
- `aplicar_filtros()`: Aplica filtros dinámicos a los datos
- `crear_grafico()`: Crea gráficos estandarizados con tema SENASA
- `crear_mapa()`: Crea mapas base para visualizaciones geográficas
- `theme_senasa()`: Tema ggplot2 personalizado para SENASA

## Estructura del paquete

```
gbg/
├── R/                     # Funciones del paquete
│   ├── utils.R           # Utilidades generales
│   ├── data_processing.R # Procesamiento de datos
│   ├── charts.R          # Funciones de gráficos
│   ├── maps.R            # Funciones de mapas
│   └── run_dashboard.R   # Launcher del dashboard
├── inst/
│   └── shiny/            # Aplicación Shiny
│       └── app.R
├── man/                   # Documentación
├── tests/                 # Tests unitarios
└── DESCRIPTION           # Metadatos del paquete
```

## Desarrollo

### Configuración inicial
```r
# Instalar dependencias de desarrollo
install.packages(c("devtools", "roxygen2", "testthat"))

# Clonar el repositorio
git clone https://github.com/lumasa1972/gbg.git
cd gbg
```

### Ejecutar en modo desarrollo
```r
# Opción 1: Usar el launcher
source("run_app.R")

# Opción 2: Cargar y ejecutar
devtools::load_all()
run_dashboard()

# Opción 3: Ejecutar directamente
shiny::runApp("inst/shiny")
```

### Flujo de trabajo
```r
# Documentar funciones
devtools::document()

# Ejecutar tests
devtools::test()

# Verificar el paquete
devtools::check()
```

## Solución de problemas

### Error: "Loading R/ subdirectory for Shiny application"
Este error ocurre si intentas ejecutar `shiny::runApp()` desde el directorio raíz del proyecto. 

**Solución:**
- Usa `run_dashboard()` después de cargar el paquete con `library(gbg)` o `devtools::load_all()`
- O ejecuta `shiny::runApp("inst/shiny")` especificando la ruta correcta
- O usa el script launcher: `source("run_app.R")`

## Licencia

MIT

## Contacto

Mariano Arroyo - marianoarroyo@gmail.com
