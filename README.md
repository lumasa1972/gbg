# Dashboard GBG - Vigilancia Epidemiológica

Dashboard interactivo desarrollado con R Shiny para el análisis y visualización de datos de vigilancia epidemiológica en Costa Rica.

## Descripción

Este proyecto proporciona una aplicación web interactiva que permite cargar, analizar y visualizar datos epidemiológicos con las siguientes características:

- **Carga flexible de datos**: Soporta archivos CSV (con detección automática de delimitadores ; o ,) y Excel (.xls, .xlsx)
- **Diagnóstico automático**: Vista previa de datos y detección de la estructura de columnas
- **Visualizaciones interactivas**:
  - Resumen de registros y totales
  - Gráficos de serie temporal por año y semana epidemiológica
  - Mapa interactivo con clusters geográficos
- **Filtros dinámicos**: Por año y rango de semanas epidemiológicas
- **Exportación de datos**: Descarga de datos filtrados en formato CSV

## Características Técnicas

### Indicador Principal
El dashboard está optimizado para trabajar con el indicador **"Animales muestreados"**, que es detectado automáticamente si existe en los datos.

### Estructura de Datos Esperada
La aplicación espera datos con las siguientes columnas (con detección automática):
- **Año**: Año de la observación
- **Semana epidemiológica**: Número de semana (1-52)
- **Indicador numérico**: Valor a analizar (ej. Animales muestreados)
- **Latitud/Longitud** (opcionales): Para visualización en mapa

### Auto-corrección de Coordenadas
El sistema incluye corrección automática para coordenadas que vienen en formato incorrecto (ej. dividiendo por 10000 cuando el valor excede los rangos válidos).

## Instalación

### Requisitos Previos
- R (versión 4.0 o superior)
- RStudio (recomendado)

### Dependencias
Instale los paquetes necesarios ejecutando:

```r
install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "ggplot2",
  "readr",
  "leaflet",
  "readxl"
))
```

## Uso

### Iniciar la Aplicación
Desde R o RStudio:

```r
shiny::runApp("app.R")
```

O desde la línea de comandos:

```bash
R -e "shiny::runApp('app.R')"
```

### Pasos de Uso
1. **Cargar datos**: Haga clic en "Cargar base (CSV o Excel)" y seleccione su archivo
2. **Verificar diagnóstico**: Revise la vista previa y el diagnóstico automático de columnas
3. **Ajustar selecciones**: Si es necesario, modifique las columnas seleccionadas para Año, Semana e Indicador
4. **Aplicar filtros**: Use los selectores de año y rango de semanas para filtrar los datos
5. **Explorar visualizaciones**: Revise los gráficos y el mapa interactivo
6. **Descargar resultados**: Use el botón "Descargar datos filtrados" para exportar

## Datos de Ejemplo

El repositorio incluye:
- `Base_datos_semana_50-25.csv`: Datos de ejemplo de vigilancia epidemiológica (semana 50 de 2024 a semana inicial de 2025)
- `data/Cantones_de_Costa_Rica/`: Shapefiles de cantones de Costa Rica

## Estructura del Proyecto

```
gbg/
├── app.R                           # Aplicación principal Shiny
├── DESCRIPTION                     # Metadatos del paquete R
├── NAMESPACE                       # Namespace del paquete
├── README.md                       # Este archivo
├── Base_datos_semana_50-25.csv    # Datos de ejemplo
└── data/                          # Datos adicionales
    └── Cantones_de_Costa_Rica/    # Shapefiles
```

## Funcionalidades Principales

### Normalización de Texto
- Normalización automática de nombres de columnas (elimina acentos, espacios múltiples)
- Detección inteligente de columnas por patrones de texto

### Conversión Robusta de Números
- Soporta formato costarricense (1.234,56) y estadounidense (1,234.56)
- Manejo automático de caracteres no numéricos

### Mapas Interactivos
- Visualización con Leaflet
- Clusters automáticos para mejor rendimiento
- Círculos proporcionales al valor del indicador
- Popups informativos con detalles de cada punto

## Contribuciones

Este es un proyecto de análisis epidemiológico. Para contribuir:
1. Fork el repositorio
2. Cree una rama para su funcionalidad
3. Envíe un Pull Request

## Licencia

MIT License

## Contacto

Mantenedor: mariano (marianoarroyo@gmail.com)

## Notas Técnicas

### Detección de Delimitadores CSV
La aplicación detecta automáticamente si el CSV usa punto y coma (;) o coma (,) como delimitador, común en archivos exportados desde Excel en diferentes configuraciones regionales.

### Validación de Datos
- Validación suave: La aplicación muestra advertencias pero no bloquea el flujo si encuentra datos no numéricos
- Visualizaciones se generan solo cuando los datos son válidos

### Rendimiento
- Optimizado para datasets grandes (hasta 200,000 filas)
- Clustering en mapas para mejorar rendimiento visual
- Carga perezosa de datos (LazyData: true)
