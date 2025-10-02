# Análisis cualitativo y cuantitivo de la base de datos de DOAJ con R

#### [Romina De León](rdeleon@conicet.gov.ar) y Dra. [Gimena del Rio Riande](gdelrio@conicet.gov.ar), investigadora independiente del IIBICRIT-CONICET y embajadora en América Latina para el Directory of Open Access Journals
#### Este proyecto es parte de las actividedes del ([HDLAB CONICET](https://hdlab.space/))
#### Diseñado por Gimena del Rio Riande y Romina De León. Mantenido por Romina De León

[![DOI](https://zenodo.org/badge/660713633.svg)](https://doi.org/10.5281/zenodo.17244368)

[![MyBinder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/rominicky/analisis-doaj/main?urlpath=%2Fdoc%2Ftree%2Fdoaj-analisis.ipynb)

Esta notebook ofrece los datos actualizados cosechados automáticamente del base de datos pública de [*Directory of Open Access Journals* (DOAJ)](https://doaj.org/) a través de un análisis exploratorio de revistas de investigación latinoamericanas incluidas en el directorio. Utiliza el lenguaje de programación R para ofrecer el análisis y las visualizaciones en tiempo real.

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

** Las notebooks y scripts no están publicados, pero los datos de entrada y visualizaciones están disponibles para consulta y citación.**

## Datos utilizados

Los datos originales se descargan desde la sección [DOAJ CSV](https://doaj.org/CSV/). Se realizó una limpieza y filtrado para conservar únicamente revistas registradas con país de publicación en América Latina.

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
- LibreOffice / CSVKit para revisión de datos

Si reutilizás este trabajo, por favor citá de la siguiente manera:

    De León, Romina  y Gimena del Rio Riande (2025). Análisis de revistas latinoamericanas en DOAJ [Repositorio GitHub]. https://doi.org/10.5281/zenodo.17080220
