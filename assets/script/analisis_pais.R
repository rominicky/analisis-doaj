#mejorado con IA
analizar_pais <- function(pais, data = journal.amlat, dir_output = "graficos") {
  # Cargar librerías
  suppressPackageStartupMessages({
    library(dplyr)
    library(ggplot2)
    library(forcats)
  })
  
  # Verificar/crear directorio de salida
  if (!dir.exists(dir_output)) {
    dir.create(dir_output)
  }
  
  # Verificar estructura de los datos
  if (!"country" %in% names(data)) {
    stop("El dataframe no contiene una columna 'country'")
  }
  
  # Filtrar datos por país
  journal.pais <- data %>% 
    filter(country == pais)
  
  if (nrow(journal.pais) == 0) {
    stop("No se encontraron revistas para el país especificado: ", pais)
  }
  
  # Mostrar resumen estadístico
  total.pais <- nrow(journal.pais)
  message("\n=== Análisis para: ", pais, " ===")
  message("Total de revistas: ", total.pais)
  
  # Función helper para guardar gráficos
  guardar_grafico <- function(plot, nombre, ancho = 30, alto = 15) {
    nombre_archivo <- paste0(nombre, tolower(gsub(" ", "_", pais)), ".jpg")
    ruta <- file.path(dir_output, nombre_archivo)
    
    ggsave(
      filename = ruta,
      plot = plot,
      width = ancho,
      height = alto,
      units = "cm",
      dpi = 300
    )
    message("Gráfico guardado: ", nombre_archivo)
  }
  
  # Función para crear gráficos de barras horizontales simplificados
  crear_grafico_simple <- function(var, titulo, filtro_min = 5) {
    # Verificar si la variable existe
    if (!var %in% names(journal.pais)) {
      warning("La columna '", var, "' no existe en los datos. Omitiendo gráfico.")
      return(NULL)
    }
    
    # Preparar datos
    plot_data <- journal.pais %>%
      count(!!sym(var)) %>%
      filter(n >= filtro_min)
    
    if (nrow(plot_data) == 0) {
      warning("No hay suficientes datos (n >= ", filtro_min, ") para '", var, "'. Omitiendo gráfico.")
      return(NULL)
    }
    
    # Crear gráfico minimalista
    plot_data %>%
      mutate(categoria = fct_reorder(!!sym(var), n)) %>%
      ggplot(aes(x = categoria, y = n, fill = categoria)) +
      geom_col(show.legend = FALSE) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        plot.caption = element_text(size = 8, color = "gray30"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10)
      ) +
      labs(
        y = "Cantidad de revistas",
        x = NULL,
        title = paste(titulo, "en", pais),
        caption = "Citar como: Romina De León y Gimena del Rio, 2025. Análisis de revistas latinoamericanas en DOAJ."
      ) +
      coord_flip()
  }
  
  # Variables a graficar (ajusta según tu dataframe)
  variables_grafico <- c(
    "language" = "Idiomas de publicación",
    "Publisher" = "Editoriales",
    "license" = "Licencias de publicación",
    "Review" = "Procesos de revisión",
    "Subjects" = "Temas de publicación"
  )
  
  # Filtrar solo variables que existen
  variables_grafico <- variables_grafico[names(variables_grafico) %in% names(journal.pais)]
  
  # Generar gráficos
  graficos <- list()
  for (var in names(variables_grafico)) {
    g <- crear_grafico_simple(var, variables_grafico[var])
    if (!is.null(g)) {
      print(g)
      guardar_grafico(g, paste0(var, "_"))
      graficos[[var]] <- g
    }
  }
  
  # Retornar resultados
  invisible(list(
    datos = journal.pais,
    total = total.pais,
    graficos = graficos
  ))
}