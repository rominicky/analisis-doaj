# Análisis de revistas latinoamericanas en DOAJ

## Diseñado y mantenido por Romina De León (CONICET)

[![DOI](https://zenodo.org/badge/660713633.svg)](https://doi.org/10.5281/zenodo.17080220)

[![MyBinder](https://mybinder.org/badge_logo.svg)](https://hub.2i2c.mybinder.org/user/rominicky-analisis-doaj-vacrhqwc/lab/tree/doaj-analisis.ipynb)

Este repositorio contiene un análisis exploratorio y visual de datos sobre revistas académicas latinoamericanas incluidas en el [*Directory of Open Access Journals* (DOAJ)](https://doaj.org/).El trabajo se basa en la base de datos disponible públicamente desde el DOAJ, procesada con herramientas libres para facilitar su exploración y reutilización.

## Objetivos

- Explorar la presencia de revistas de América Latina en DOAJ.
- Analizar su distribución por país, idioma, disciplina, identificadores persistentes y política de acceso.
- Ofrecer visualizaciones accesibles y reutilizables que apoyen investigaciones bibliométricas, estudios de acceso abierto y políticas científicas regionales.

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

## Herramientas y tecnologías

- Python 3.x
- Jupyter Notebooks
- Pandas, Matplotlib, Seaborn
- LibreOffice / CSVKit para revisión de datos

Si reutilizás este trabajo, por favor citá de la siguiente manera:

    De León, Romina (2025). Análisis de revistas latinoamericanas en DOAJ [Repositorio GitHub]. https://doi.org/10.5281/zenodo.17080220
