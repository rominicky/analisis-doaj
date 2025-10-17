# Análisis cualitativo y cuantitivo de la base de datos de DOAJ con R

#### [Romina De León](rdeleon@conicet.gov.ar) y Dra. [Gimena del Rio Riande](gdelrio.riande@conicet.gov.ar), investigadora independiente del IIBICRIT-CONICET y embajadora en América Latina para el Directory of Open Access Journals
#### Este proyecto es parte de las actividedes del ([HDLAB CONICET](https://hdlab.space/))
#### Mantenido por Romina De León 

[![DOI](https://zenodo.org/badge/660713633.svg)](https://doi.org/10.5281/zenodo.17244368)

[![MyBinder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/rominicky/analisis-doaj/main?urlpath=%2Fdoc%2Ftree%2Fdoaj-analisis.ipynb)

Este repositorio contiene un análisis exploratorio y visual de datos sobre revistas académicas latinoamericanas incluidas en el [*Directory of Open Access Journals* (DOAJ)](https://doaj.org/). Desarrollado con la base de datos disponible públicamente desde el DOAJ, procesada con lenguaje de programación R que ofrece análisis y visualizaciones en tiempo real, que facilita su exploración y reutilización.

## Objetivos

- Explorar la presencia de revistas de América Latina en DOAJ.
- Analizar su distribución por país, idioma, disciplina, identificadores persistentes y política de acceso.
- Ofrecer visualizaciones accesibles y reutilizables que apoyen investigaciones sobre publicación científica, acceso abierto, políticas científicas regionales, multilingüismo, etc.

## Contenido del repositorio

├── datos/ # Archivos CSV utilizados para el análisis

├── notebooks/ # Cuadernos Jupyter con el procesamiento y visualización

├── scripts/ # Scripts auxiliares para limpieza y transformación de datos

├── visualizaciones/ # Gráficos exportados en formato PNG/SVG/HTML

├── README.md # Este archivo

└── licencia.txt # Términos de uso de los contenidos del repositorio

## Datos utilizados

Los datos originales se descargan desde la sección [DOAJ CSV](https://doaj.org/CSV/). Se realizaron visualizaciones a nivel mundial, y luego limpieza y filtrado para conservar únicamente revistas registradas en América Latina.

**Campos principales analizados:**

- `country` (país de la revista)
- `language` (idioma de publicación)
- `subject` (área temática)
- `IDs` (Identificadores persistentes)
- `publisher` (editorial o institución responsable)
- `APC` (Article Processing Charge)

## Herramientas y tecnologías

- Python 3.x
- Jupyter Notebooks
- Pandas, Matplotlib, Seaborn 

Si reutilizás este trabajo, por favor citá de la siguiente manera:

    De León, Romina  y Gimena del Rio Riande (2025). Análisis de revistas latinoamericanas en DOAJ [Repositorio GitHub]. https://doi.org/10.5281/zenodo.17080220
