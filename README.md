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

```r
library(gbg)
run_dashboard()
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

```r
# Instalar dependencias de desarrollo
install.packages(c("devtools", "roxygen2", "testthat"))

# Documentar funciones
devtools::document()

# Ejecutar tests
devtools::test()

# Verificar el paquete
devtools::check()
```

## Licencia

MIT

## Contacto

Mariano Arroyo - marianoarroyo@gmail.com
