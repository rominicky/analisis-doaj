# Análisis de revistas latinoamericanas en DOAJ
## Diseñado y mantenido por Romina De León (CONICET)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.1234567.svg)](https://doi.org/10.5281/zenodo.1234567)

Este repositorio contiene un análisis exploratorio y visual de datos sobre revistas académicas latinoamericanas incluidas en el [*Directory of Open Access Journals* (DOAJ)](https://doaj.org/).El trabajo se basa en la base de datos disponible públicamente desde el DOAJ, procesada con herramientas libres para facilitar su exploración y reutilización.

## Objetivos

- Explorar la presencia de revistas de América Latina en DOAJ.
- Analizar su distribución por país, idioma, disciplina, identificadores persistentes y política de acceso.
- Ofrecer visualizaciones accesibles y reutilizables que apoyen investigaciones bibliométricas, estudios de acceso abierto y políticas científicas regionales.

## Contenido del repositorio

.
├── datos/ # Archivos CSV utilizados para el análisis
├── notebooks/ # Cuadernos Jupyter con el procesamiento y visualización
├── visualizaciones/ # Gráficos exportados en formato PNG/SVG
├── scripts/ # Scripts auxiliares para limpieza y transformación de datos
├── README.md # Este archivo
└── licencia.txt # Términos de uso de los contenidos del repositorio

** Las notebooks y scripts no están publicados, pero los datos de entrada y visualizaciones están disponibles para consulta y citación.**

## Datos utilizados

Los datos originales fueron descargados desde la sección [DOAJ Public Data Dump](https://doaj.org/docs/public-data-dump/) en 2024. Se realizó una limpieza y filtrado para conservar únicamente revistas registradas con país de publicación en América Latina.

**Campos principales analizados:**

- `country` (país de la revista)
- `language` (idioma de publicación)
- `subject` (área temática)
- `apc` (Article Processing Charges: si cobra o no a autores)
- `publisher` (editorial o institución responsable)

## Herramientas y tecnologías

- Python 3.x
- Jupyter Notebooks
- Pandas, Matplotlib, Seaborn
- LibreOffice / CSVKit para revisión de datos

## Cómo reutilizar este repositorio

1. Clonar o descargar el repositorio:
   ```bash
   git clone https://github.com/rominicky/analisis-doaj.git
2. Instalar dependencias necesarias (si se usan los notebooks).

3. Abrir los notebooks desde JupyterLab o VS Code.

4. Usar, modificar o ampliar los gráficos y tablas disponibles para tus propios fines de investigación.

Cómo citar este repositorio

Si reutilizás este trabajo, por favor citá de la siguiente manera:

    Rominicky (2025). Análisis de revistas latinoamericanas en DOAJ [Repositorio GitHub]. https://doi.org/10.5281/zenodo.1234567

También podés importar la cita desde Zenodo en formatos BibTeX, EndNote o CSL JSON.
Licencia

Este repositorio se distribuye bajo una licencia abierta (ver archivo licencia.txt), que permite su reutilización siempre que se reconozca la fuente.
Créditos

Este análisis fue desarrollado por rominicky como parte de un proyecto de investigación sobre acceso abierto en América Latina.
Enlaces relacionados

    DOAJ - Directory of Open Access Journals

    Repositorio con recursos complementarios