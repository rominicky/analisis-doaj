#- almacenamos en el objeto "path_wd" la ruta de mi directorio de trabajo
rm(list = ls())

getwd()

path_wd <- getwd()
#

setwd(path_wd)
packages <- c("tidytext", "gapminder", "tidyverse", "dplyr", "ggplot2")
install.packages(packages, INSTALL_opts = '--no-lock') #si tira error de /00LOCK 
#lo eliminé C:\Users\RominaSoledadDeLeon\AppData\Local\R\win-library desde esa carpeta busque folder /00LOCK 
install.packages("treemapify", dependencies = TRUE, INSTALL_opts = '--no-lock')
install.packages('magritter')
install.packages("ggplot2")
install.packages('rio')
install.packages('xlsx')
library(treemapify)
library(tidyverse)
library(tidytext)
library(dplyr)
library(ggplot2)
library(gapminder)
library(magrittr) #algunos los vuelvo a llamar por error al correr el script
library(tidyselect)
library(purrr)
library(readr)
library(rio)
library(xlsx)
library(stringr)
library(highcharter)
library(webshot)
library(htmlwidgets)

#creamos un data.frame con la base de datos: mis_datos
#Gimena quiere que se haga una jupyter para levantar en vivo y generar los gráficos

#journal <- read.csv(url("https://raw.githubusercontent.com/rominicky/analisis-doaj/main/journalcsv__doaj_20240404_1721_utf8.csv?token=GHSAT0AAAAAACPRMIVAKVUR2TLJYTL5B4QOZQVK33A"))
journal <- read.csv("2025_journalcsv.csv")
as_tibble(journal)
#nueva tabla con datos de interes 

colnames(journal)

journal.select <- journal%>% select(Journal.title, Country.of.publisher, Languages.in.which.the.journal.accepts.manuscripts, Journal.license, Publisher, Review.process, Subjects, APC, Persistent.article.identifiers, Keywords)


journal.select <- journal.select %>%
  rename(title = Journal.title) %>%
  rename(country = Country.of.publisher) %>%
  rename(language = Languages.in.which.the.journal.accepts.manuscripts) %>%
  rename (license = Journal.license) %>%
  rename (Review = Review.process) %>%
  rename (Ids = Persistent.article.identifiers)


#dejo solo las primeras palabras de subjects

journal.select$Subjects <- str_extract(journal.select$Subjects, "\\w+(?:[^\\w]+\\w+){0,1}")

#elimino los signos de puntuación
journal.select$Subjects <- gsub("[[:punct:]]", "", journal.select$Subjects)

journal.select <- journal.select %>%
  mutate(Subjects = str_trim(Subjects)) %>%
  mutate(Subjects = sapply(Subjects, function(x) {
    words <- str_split(x, "\\s{2,}|,|\\s*\\band\\b\\s*|\\s+")[[1]]  # dividir por palabras, "and", comas, o espacios
    words <- unique(words[words != ""])  # eliminar vacíos y duplicados
    paste(words, collapse = " ")
  }))

# Crear una función para ordenar los idiomas en una lista
sort_languages <- function(language_list) {
  sorted_languages <- sort(unlist(strsplit(language_list, ", ")))
  #sorted_languages <- unique(sorted_languages)  # Eliminar duplicados
  return(paste(sorted_languages, collapse = ", "))
}

# Aplicar la función a cada celda en la columna 'language'
journal.select$language <- sapply(journal.select$language, sort_languages)

# Crear una función para ordenar los ids
sort_ids <- function(ids_list) {
  sorted_ids <- sort(unlist(strsplit(ids_list, ", ")))
  #sorted_languages <- unique(sorted_languages)  # Eliminar duplicados
  return(paste(sorted_ids, collapse = ", "))
}

# Aplicar la función a cada celda en la columna 'language'
journal.select$Ids <- sapply(journal.select$Ids, sort_ids)



 #journal.select <- journal.select[order(journal.select$language), ]

porcen_journal <- journal.select %>%
  group_by(country)%>%
  count()%>%
  ungroup()%>%
  mutate(percentage= n /sum(n)*100) %>%
  bind_rows(data.frame(country = "Total", n = NA, percentage = sum(.$percentage)))

#grafico con porcentajes 


# Filtrar los países de América Latina
selected_countries <- c("Brazil", "Argentina", "Mexico", "Colombia", "Ecuador", "Costa Rica", "Bolivia, Plurinational State of", "Dominican Republic", "El Salvador", "Guatemala", "	
Honduras", "Nicaragua", "Panama", "Chile", "Paraguay", "Peru", "Uruguay", "Venezuela, Bolivarian Republic of")
filtered_data <- porcen_journal %>% filter(country %in% selected_countries)



paises_total <- hchart(
 filtered_data,
  type = "pie",
  hcaes(x = country, y = percentage), 
  dataLabels = list(enabled = TRUE),  # Deshabilitar etiquetas de datos
  showInLegend = TRUE
) %>%
  hc_title(text = "<b>Porcentaje de países de América Latina</b>") %>%
  hc_subtitle(text = "<i>Países de América Latina respecto al total de los países del mundo</i>") %>%
  hc_exporting(
    enabled = TRUE, # Habilitar exportación
    filename = "paises_total"
  ) %>%
  hc_plotOptions(
    pie = list(
      allowPointSelect = TRUE,
      cursor = "pointer"
    )
  ) %>%
  hc_legend(
    enabled = TRUE, 
    layout = "horizontal",
    align = "center",
    verticalAlign = "bottom",
    y = 10,  # Ajustar la posición vertical de la leyenda
    itemStyle = list(fontSize = "12px", fontWeight = "normal"),
    labelFormat = "{name}: {percentage:.1f}%"
  )
saveWidget(paises_total, file = "paises_total-interactivo_amlat_2024.html")

p <- ggplot(filtered_data, aes(x = "", y = percentage, fill = country)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Porcentaje de países América Latina",
       fill = "País",
       x = NULL,
       y = NULL) +
  theme_void() +
  scale_fill_discrete(name = "Países",
                      labels = function(x) {
                        index <- match(x, filtered_data$country)
                        paste(x, round(filtered_data$percentage[index], 2), "%", sep = "\n")
                      }) +
  theme(legend.position = "right")
p + theme(plot.title=element_text(size=25, face='bold', color='purple'))
ggsave("paises-total.png", width = 3000, height = 1500, units = "px", dpi = 300)

total.journal <- length(journal.select$title)
total.journal

#en todo el mundo 
journal.select %>%
  group_by(language)%>%
  count()%>%
  filter(n >= 100 )%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  #scale_fill_manual(values = rainbow(length(unique(datos$language)))) +
  scale_fill_viridis_d(option = "C" ) +
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en todo el mundo") +
  coord_flip()
ggsave("idiomas-total.jpg", width = 3000, height = 1500, units = "px", dpi = 300)

#idioma por pais
journal.select %>%
  group_by(language, country) %>%
  count() %>%
  filter(n >= 50 & n <= 750) %>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = language, y = n, fill = country)) +  # Mapear 'country' al color
  geom_col() +
  theme_minimal() + # Colocar la leyenda en la parte superior
  theme(legend.position = "none") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación por países") +
  coord_flip()

# por continente 
# Instalar la librería si no está instalada
install.packages("countrycode")

# Cargar las librerías 
library(countrycode)
library(ggplot2)
library(dplyr)
library(viridis)
# 
journal.select <- journal.select %>%
  mutate(continent = countrycode(country, "country.name", "continent"))

# Por continente
journal.select %>%
  group_by(language, country, continent) %>%
  count() %>%
  filter(n >= 100 ) %>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = language, y = n, fill = country)) +  
  geom_col(show.legend = FALSE) +  
  #geom_text(aes(label = n), vjust = -0.5, size = 3, color = "black") +
  theme_minimal() +
  ylab("Número de publicaciones") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en todo el mundo por país") +
  coord_flip() +
  facet_wrap(~ continent) +  # Usar facetas por continente
  scale_fill_viridis_d(option = "H") +  # Usar una paleta con más colores
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave("idiomas-continente_min100.jpg", width = 3000, height = 1500, units = "px", dpi = 300)
class(journal.select)


journal.select %>%
  group_by(language, country, continent) %>%
  count() %>%
  filter(n >= 25 & n<= 150) %>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = language, y = n, fill = country)) +  
  geom_col(show.legend = FALSE) +  # Eliminamos la leyenda de los países
  #geom_text(aes(label = n), vjust = -0.5, size = 3, color = "black") +  
  theme_minimal() +
  ylab("Número de publicaciones") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en todo el mundo por país") +
  coord_flip() +
  facet_wrap(~ continent, scales = "free_y") +  # Ajusta las escalas de cada faceta por continente
  scale_fill_viridis_d(option = "H") +  # Paleta viridis con más colores, ajustada para una mejor visualización
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Ajusta la rotación para los textos en el eje X
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    plot.margin = margin(10, 10, 10, 10),
    strip.text = element_text(size = 10),  # Títulos de las facetas más grandes
    strip.background = element_rect(fill = "lightgray")  # Fondo gris claro para las facetas
  )
ggsave("idiomas-cont_min25_mx150.jpg", width = 3000, height = 1500, units = "px", dpi = 300)


##AMERICA LATINA

journal.amlat <- journal.select[journal.select$country %in% selected_countries, ]

total.pais <- length(journal.amlat$title)
total.pais
#total <- c("Total de pa?ses", total.journal)
#pais <- c("América Latina", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.amlat ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.amlat %>%
  filter(Subjects == "Social Sciences") %>%  # Cambié "=" a "==" para la comparación
  group_by(language) %>%
  count() %>%
  filter(n >= 1) %>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +  # Cambié "n" a "none" para ocultar la leyenda
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicaciones sobre Historia en América Latina") +
  coord_flip()
ggsave("area_social_sciences_idioma_AmLatina.png", width = 3000, height = 1500, units = "px", dpi = 300)


journal.amlat %>%
  filter(Subjects %in% c("History", "Education History", "Education Social", "History General", "History America", "Language", "Philosophy", "Political Science", "Education Theory", "Bibliography Library", "Auxiliary sciences", "Education Special", "Education", "Social Sciences")) %>%
  group_by(language) %>%
  count() %>%
  filter(n >= 3) %>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, n), y = n, fill = language)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +  # Cambié "n" a "none" para ocultar la leyenda
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicaciones sobre Ciencias Sociales y Humanidades en América Latina") +
  coord_flip()  
ggsave("area_cssoc_idioma_AmLatina.png", width = 3000, height = 1500, units = "px", dpi = 300)  
  
#############################################################
#Voy a unificar las social sciences en una 
# Crear un gráfico de barras apiladas
journal.amlat %>%
  group_by(language, country) %>%
  count() %>%
  filter(n >= 25) %>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = language, y = n, fill = country)) +
  geom_col() +
  #geom_text(aes(label = n), hjust = -0.1, size = 3) +
  scale_fill_viridis_d(option = "D") +
  #scale_y_continuous(labels = comma) +
  theme_minimal() +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 10)
  ) +
  ylab("Número de publicaciones") +
  xlab("Idioma") +
  ggtitle(
    "Distribución de idiomas en publicaciones académicas de América Latina",
    subtitle = "Solo se muestran combinaciones con al menos 25 registros"
  ) +
  coord_flip()
ggsave("idiomas-amlat.jpg", width = 3000, height = 1500, units = "px", dpi = 300)

english <- journal.amlat[journal.amlat$language == "English", ]
colnames(english)

install.packages("scico")
library(scico)
install.packages("Cairo")
library(Cairo)

# Crear el gráfico de dispersión con etiquetas de conteo
plot1 <- ggplot(journal.amlat %>% 
         group_by(language, country, Subjects) %>% 
         summarise(count = n()) %>% 
         filter(count >= 3 ) %>% 
         mutate(language = reorder(language, count)), 
       aes(x = country, y = language, color = Subjects)) +
  geom_point(size = 5, alpha = 0.7) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 1 , size = 16),  # Espacio bajo el título
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "left",
    legend.box = "horizontal",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.spacing.y = unit(0.1, "cm"),  # Espacio entre filas de leyenda
    plot.margin = margin(t = 20, r = 20, b = 20, l = 80)
  ) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  ylab("Idiomas") +
  xlab("Países") +
  ggtitle("Relación entre países, idiomas y áreas en publicaciones de América Latina") +
  scale_color_scico_d(palette = "berlin")  # También probá: "lajolla", "tokyo", "batlow"
  #scale_color_viridis_d(option = "D")  # Utilizar una paleta de colores como Viridis
ggsave("relacion_idiomas_temas-amlat.jpg",
       plot = plot1,
       width = 3800, height = 1900, units = "px", dpi = 300)


library(viridis)

ggplot(english, aes(x = country, fill = Subjects)) +
  geom_bar(position = "stack") +
  scale_fill_viridis_d(option = "D") +  # Usar la paleta viridis
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Número de registros") +
  xlab("Países") +
  ggtitle("Distribución de temas por países de América Latina con publicaciones unicamente en Inglés")

#otro gráfico
journal.amlat %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>12)%>%
  mutate(Publisher = reorder(Publisher, n)) %>%
  ggplot(aes(x = reorder(Publisher, n), y = n, fill = Publisher)) +  # Ordenar x según la frecuencia -n
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  scale_color_scico_d(palette = "bilbao") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en América Latina") +
  coord_flip()
ggsave("editorial_América Latina_min12.jpg", width = 3000, height = 1500, units = "px", dpi = 300)

journal.amlat %>%
  group_by(license) %>%
  count() %>%
  filter(n >= 5) %>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, n), y = n, fill = license)) +
  geom_col() +
  scale_fill_scico_d(palette = "berlin") +
  theme_minimal() +
  theme(legend.position = "none") +  # debe ser "none", no "n"
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en América Latina") +
  coord_flip()
ggsave("licencia_AmLatina_min5.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.amlat %>%
  group_by(Review)%>%
  count()%>%
  filter(n>=10)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  scale_fill_scico_d(palette = "berlin") +
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en América Latina") +
  scale_color_scico_d(palette = "lajolla") +
  coord_flip()
ggsave("revision_AmLatina_min10.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.amlat %>%
  group_by(Subjects) %>%
  count() %>%
  filter(n > 15) %>%
  mutate(Subjects = fct_reorder(Subjects, n)) %>%
  ggplot(aes(x = reorder(Subjects, n), y = n, fill = Subjects)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en América Latina") +
  scale_fill_scico_d(palette = "lajolla") +
  coord_flip()
ggsave("temas_América Latina.png", width = 3000, height = 1500, units = "px", dpi = 300)



analizar_pais <- function(pais, data = journal.amlat) {
  library(dplyr)
  library(ggplot2)
  
  # Filtrar datos por país
  journal.pais <- data %>% filter(country == pais)
  
  # Mostrar total de revistas
  total.pais <- length(journal.pais$title)
  print(paste("Total de revistas en", pais, ":", total.pais))
  # Helper para guardar
  guardar_grafico <- function(plot, nombre, pais) {
    ruta <- file.path(path_wd, paste0(nombre, "_", pais, ".jpg"))
    ggsave(ruta, plot = plot, width = 10, height = 6)
  }
  
  # Gráfico por idioma
  g1 <- journal.pais %>%
    group_by(language) %>%
    count() %>%
    filter(n > 5) %>%
    mutate(language = reorder(language, n)) %>%
    ggplot(aes(x = language, y = n, fill = language)) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none") +
    ylab("Número de veces que aparecen") +
    xlab(NULL) +
    ggtitle(paste("Idiomas de publicación de revistas en", pais)) +
    coord_flip()
  print(g1)
  guardar_grafico(g1, "idiomas_", pais)
  
  # Gráfico por editorial
  g2 <- journal.pais %>%
    group_by(Publisher) %>%
    count() %>%
    filter(n > 5) %>%
    mutate(Publisher = reorder(Publisher, n)) %>%
    ggplot(aes(x = Publisher, y = n, fill = Publisher)) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none") +
    ylab("Cantidad de revistas publicadas por") +
    xlab(NULL) +
    ggtitle(paste("Editorial de publicación de revistas en", pais)) +
    coord_flip()
  print(g2)
  guardar_grafico(g2, "editorial_", pais)
  
  # Gráfico por licencias
  g3 <- journal.pais %>%
    group_by(license) %>%
    count() %>%
    filter(n > 1) %>%
    mutate(license = reorder(license, n)) %>%
    ggplot(aes(x = license, y = n, fill = license)) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none") +
    ylab("Tipo de licencias") +
    xlab(NULL) +
    ggtitle(paste("Licencias de publicación de revistas en", pais)) +
    coord_flip()
  print(g3)
  guardar_grafico(g3, "licencias_", pais)
  
  # Gráfico por proceso de revisión
  g4 <- journal.pais %>%
    group_by(Review) %>%
    count() %>%
    filter(n > 5) %>%
    mutate(Review = reorder(Review, n)) %>%
    ggplot(aes(x = Review, y = n, fill = Review)) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none") +
    ylab("Proceso de revisión") +
    xlab(NULL) +
    ggtitle(paste("Proceso de revisión de revistas en", pais)) +
    coord_flip()
  print(g4)
  guardar_grafico(g4, "revision_", pais)
  
  # Gráfico por temas
  g5 <- journal.pais %>%
    group_by(Subjects) %>%
    count() %>%
    filter(n > 5) %>%
    mutate(Subjects = reorder(Subjects, n)) %>%
    ggplot(aes(x = Subjects, y = n, fill = Subjects)) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none") +
    ylab("Temas") +
    xlab(NULL) +
    ggtitle(paste("Temas de publicación de revistas en", pais)) +
    coord_flip()
  print(g5)
  guardar_grafico(g5, "temas_", pais)
  
  # Gráfico por temas
  g6 <- journal.pais %>%
    group_by(id) %>%
    count() %>%
    filter(n > 5) %>%
    mutate(id = reorder(id, n)) %>%
    ggplot(aes(x = id, y = n, fill = id)) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none") +
    ylab("Identificadores persistentes") +
    xlab(NULL) +
    ggtitle(paste("Identificadores persistentes de revistas en", pais)) +
    coord_flip()
  print(g5)
  guardar_grafico(g6, "identificadores_", pais)
}

walk(selected_countries, analizar_pais)
 
selected_countries
analizar_pais("Colombia")



##################################################################

##COLOMBIA
journal.colombia <- journal.select %>% filter(country == "Colombia")

total.pais <- length(journal.colombia$title)
total.pais
#total <- c("Total de pa?ses", total.journal)
#pais <- c("Colombia", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.colombia ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.colombia %>%
  group_by(language)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en Colombia") +
  coord_flip()
ggsave("idioma_colombia.png", width = 3000, height = 1500, units = "px", dpi = 300)

#otro gráfico
journal.colombia %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>3)%>%
  mutate(Publisher = reorder(Publisher, desc(n))) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n, fill = Publisher)) + 
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en Colombia") +
  coord_flip()
ggsave("editorial_colombia.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.colombia %>%
  group_by(license)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, -n), y = n, fill = license)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en Colombia") +
  coord_flip()
ggsave("licencia_colombia.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.colombia %>%
  group_by(Review)%>%
  count()%>%
  filter(n>=2)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, -n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en Colombia") +
  coord_flip()
ggsave("revision_colombia.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.colombia %>%
  ggplot(aes(x = reorder(Subjects, -table(Subjects)[Subjects]), fill = Subjects)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en Colombia") +
  coord_flip()
ggsave("temas_colombia.png", width = 3000, height = 1500, units = "px", dpi = 300)

##Chile
journal.Chile <- journal.select %>% filter(country == "Chile")

total.pais <- length(journal.Chile$title)
total.pais
#total <- c("Total de pa?ses", total.journal)
#pais <- c("Chile", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.Chile ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.Chile %>%
  group_by(language)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en Chile") +
  coord_flip()
ggsave("idioma_Chile.png", width = 3000, height = 1500, units = "px", dpi = 300)

#otro gráfico
journal.Chile %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>2)%>%
  mutate(Publisher = reorder(Publisher, desc(n))) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n, fill = Publisher)) + 
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en Chile") +
  coord_flip()
ggsave("editorial_Chile.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Chile %>%
  group_by(license)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, -n), y = n, fill = license)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en Chile") +
  coord_flip()
ggsave("licencia_Chile.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Chile %>%
  group_by(Review)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, -n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en Chile") +
  coord_flip()
ggsave("revision_Chile.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Chile %>%
  ggplot(aes(x = reorder(Subjects, -table(Subjects)[Subjects]), fill = Subjects)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en Chile") +
  coord_flip()
ggsave("temas_Chile.png", width = 3000, height = 1500, units = "px", dpi = 300)

##Bolivia
journal.Bolivia <- journal.select %>% filter(country == "Bolivia, Plurinational State of")

total.pais <- length(journal.Bolivia$title)
total.pais
#total <- c("Total de países", total.journal)
#pais <- c("Bolivia", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.Bolivia ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.Bolivia %>%
  group_by(language)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en Bolivia") +
  coord_flip()
ggsave("idioma_Bolivia.png", width = 3000, height = 1500, units = "px", dpi = 300)

#otro gráfico
journal.Bolivia %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(Publisher = reorder(Publisher, desc(n))) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n, fill = Publisher)) + 
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en Bolivia") +
  coord_flip()
ggsave("editorial_Bolivia.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Bolivia %>%
  group_by(license)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, -n), y = n, fill = license)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en Bolivia") +
  coord_flip()
ggsave("licencia_Bolivia.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Bolivia %>%
  group_by(Review)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, -n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en Bolivia") +
  coord_flip()
ggsave("revision_Bolivia.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Bolivia %>%
  ggplot(aes(x = reorder(Subjects, -table(Subjects)[Subjects]), fill = Subjects)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en Bolivia") +
  coord_flip()
ggsave("temas_Bolivia.png", width = 3000, height = 1500, units = "px", dpi = 300)


##Paraguay
journal.Paraguay <- journal.select %>% filter(country == "Paraguay")

total.pais <- length(journal.Paraguay$title)
total.pais
#total <- c("Total de países", total.journal)
#pais <- c("Paraguay", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.Paraguay ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.Paraguay %>%
  group_by(language)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en Paraguay") +
  coord_flip()
ggsave("idioma_Paraguay.png", width = 3000, height = 1500, units = "px", dpi = 300)

#otro gráfico
journal.Paraguay %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(Publisher = reorder(Publisher, desc(n))) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n, fill = Publisher)) + 
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en Paraguay") +
  coord_flip()
ggsave("editorial_Paraguay.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Paraguay %>%
  group_by(license)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, -n), y = n, fill = license)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en Paraguay") +
  coord_flip()
ggsave("licencia_Paraguay.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Paraguay %>%
  group_by(Review)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, -n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en Paraguay") +
  coord_flip()
ggsave("revision_Paraguay.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Paraguay %>%
  ggplot(aes(x = reorder(Subjects, -table(Subjects)[Subjects]), fill = Subjects)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en Paraguay") +
  coord_flip()
ggsave("temas_Paraguay.png", width = 3000, height = 1500, units = "px", dpi = 300)


##Perú
journal.Perú <- journal.select %>% filter(country == "Peru")

total.pais <- length(journal.Perú$title)
total.pais
#total <- c("Total de países", total.journal)
#pais <- c("Perú", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.Perú ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.Perú %>%
  group_by(language)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en Perú") +
  coord_flip()
ggsave("idioma_Perú.png", width = 3000, height = 1500, units = "px", dpi = 300)

#otro gráfico
journal.Perú %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>2)%>%
  mutate(Publisher = reorder(Publisher, desc(n))) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n, fill = Publisher)) + 
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en Perú") +
  coord_flip()
ggsave("editorial_Perú.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Perú %>%
  group_by(license)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, -n), y = n, fill = license)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en Perú") +
  coord_flip()
ggsave("licencia_Perú.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Perú %>%
  group_by(Review)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, -n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en Perú") +
  coord_flip()
ggsave("revision_Perú.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Perú %>%
  ggplot(aes(x = reorder(Subjects, -table(Subjects)[Subjects]), fill = Subjects)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en Perú") +
  coord_flip()
ggsave("temas_Perú.png", width = 3000, height = 1500, units = "px", dpi = 300)


##Uruguay
journal.Uruguay <- journal.select %>% filter(country == "Uruguay")

total.pais <- length(journal.Uruguay$title)
total.pais
#total <- c("Total de países", total.journal)
#pais <- c("Uruguay", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.Uruguay ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.Uruguay %>%
  group_by(language)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en Uruguay") +
  coord_flip()
ggsave("idioma_Uruguay.png", width = 3000, height = 1500, units = "px", dpi = 300)

#otro gráfico
journal.Uruguay %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(Publisher = reorder(Publisher, desc(n))) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n, fill = Publisher)) + 
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en Uruguay") +
  coord_flip()
ggsave("editorial_Uruguay.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Uruguay %>%
  group_by(license)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, -n), y = n, fill = license)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en Uruguay") +
  coord_flip()
ggsave("licencia_Uruguay.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Uruguay %>%
  group_by(Review)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, -n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en Uruguay") +
  coord_flip()
ggsave("revision_Uruguay.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Uruguay %>%
  ggplot(aes(x = reorder(Subjects, -table(Subjects)[Subjects]), fill = Subjects)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en Uruguay") +
  coord_flip()
ggsave("temas_Uruguay.png", width = 3000, height = 1500, units = "px", dpi = 300)




##Argentina
journal.Argentina <- journal.select %>% filter(country == "Argentina")

total.pais <- length(journal.Argentina$title)
total.pais
#total <- c("Total de pa?ses", total.journal)
#pais <- c("Argentina", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.Argentina ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.Argentina %>%
  group_by(language)%>%
  count()%>%
  filter(n>=2)%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en Argentina") +
  coord_flip()
ggsave("idioma_Argentina.png", width = 3000, height = 1500, units = "px", dpi = 300)

#otro gráfico
journal.Argentina %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>3)%>%
  mutate(Publisher = reorder(Publisher, desc(n))) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n, fill = Publisher)) + 
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en Argentina") +
  coord_flip()
ggsave("editorial_Argentina.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Argentina %>%
  group_by(license)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, -n), y = n, fill = license)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en Argentina") +
  coord_flip()
ggsave("licencia_Argentina.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Argentina %>%
  group_by(Review)%>%
  count()%>%
  filter(n>2)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, -n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en Argentina") +
  coord_flip()
ggsave("revision_Argentina.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Argentina %>%
  ggplot(aes(x = reorder(Subjects, -table(Subjects)[Subjects]), fill = Subjects)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en Argentina") +
  coord_flip()
ggsave("temas_Argentina.png", width = 3000, height = 1500, units = "px", dpi = 300)


##Brazil
journal.Brazil <- journal.select %>% filter(country == "Brazil")

total.pais <- length(journal.Brazil$title)
total.pais
#total <- c("Total de pa?ses", total.journal)
#pais <- c("Brazil", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.Brazil ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.Brazil %>%
  group_by(language)%>%
  count()%>%
  filter(n>=7)%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en Brasil") +
  coord_flip()
ggsave("idioma_Brazil.png", width = 3000, height = 1500, units = "px", dpi = 300)

#otro gráfico
journal.Brazil %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>6)%>%
  mutate(Publisher = reorder(Publisher, desc(n))) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n, fill = Publisher)) + 
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en Brasil") +
  coord_flip()
ggsave("editorial_Brazil.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Brazil %>%
  group_by(license)%>%
  count()%>%
  filter(n>=2)%>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, -n), y = n, fill = license)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en Brasil") +
  coord_flip()
ggsave("licencia_Brazil.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Brazil %>%
  group_by(Review)%>%
  count()%>%
  filter(n>5)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, -n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en Brasil") +
  coord_flip()
ggsave("revision_Brazil.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Brazil %>%
  ggplot(aes(x = reorder(Subjects, -table(Subjects)[Subjects]), fill = Subjects)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en Brasil") +
  coord_flip()
ggsave("temas_Brazil.png", width = 3000, height = 1500, units = "px", dpi = 300)

##Ecuador
journal.Ecuador <- journal.select %>% filter(country == "Ecuador")

total.pais <- length(journal.Ecuador$title)
total.pais
#total <- c("Total de pa?ses", total.journal)
#pais <- c("Ecuador", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.Ecuador ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.Ecuador %>%
  group_by(language)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en Ecuador") +
  coord_flip()
ggsave("idioma_Ecuador.png", width = 3000, height = 1500, units = "px", dpi = 300)


#otro gráfico
journal.Ecuador %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>1)%>%
  mutate(Publisher = reorder(Publisher, desc(n))) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n, fill = Publisher)) + 
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en Ecuador") +
  coord_flip()
ggsave("editorial_Ecuador.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Ecuador %>%
  group_by(license)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, -n), y = n, fill = license)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en Ecuador") +
  coord_flip()
ggsave("licencia_Ecuador.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Ecuador %>%
  group_by(Review)%>%
  count()%>%
  filter(n>3)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, -n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en Ecuador") +
  coord_flip()
ggsave("revision_Ecuador.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Ecuador %>%
  ggplot(aes(x = reorder(Subjects, -table(Subjects)[Subjects]), fill = Subjects)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en Ecuador") +
  coord_flip()
ggsave("temas_Ecuador.png", width = 3000, height = 1500, units = "px", dpi = 300)

##Mexico
journal.Mexico <- journal.select %>% filter(country == "Mexico")

total.pais <- length(journal.Mexico$title)
total.pais
#total <- c("Total de pa?ses", total.journal)
#pais <- c("Mexico", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.Mexico ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.Mexico %>%
  group_by(language)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en México") +
  coord_flip()
ggsave("idioma_Mexico.png", width = 3000, height = 1500, units = "px", dpi = 300)

#otro gráfico
journal.Mexico %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>3)%>%
  mutate(Publisher = reorder(Publisher, desc(n))) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n, fill = Publisher)) + 
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en México") +
  coord_flip()
ggsave("editorial_Mexico.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Mexico %>%
  group_by(license)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, -n), y = n, fill = license)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en México") +
  coord_flip()
ggsave("licencia_Mexico.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Mexico %>%
  group_by(Review)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, -n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en México") +
  coord_flip()
ggsave("revision_Mexico.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.Mexico %>%
  ggplot(aes(x = reorder(Subjects, -table(Subjects)[Subjects]), fill = Subjects)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en México") +
  coord_flip()
ggsave("temas_Mexico.png", width = 3000, height = 1500, units = "px", dpi = 300)

##Costa Rica
journal.CR <- journal.select %>% filter(country == "Costa Rica")

total.pais <- length(journal.CR$title)
total.pais
#total <- c("Total de pa?ses", total.journal)
#pais <- c("CR", total.pais)
#porc.pais <- data.frame(total, pais)
#pie(porc.pais, labels = porc.pais)

#journal.CR ver de unificar idiomas que tengan distinto orden y juntar las ciencias sociales 


journal.CR %>%
  group_by(language)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, -n), y = n, fill = language)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación de revistas en Costa Rica") +
  coord_flip()
ggsave("idioma_CR.png", width = 3000, height = 1500, units = "px", dpi = 300)

#otro gráfico
journal.CR %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(Publisher = reorder(Publisher, desc(n))) %>%
  ggplot(aes(x = reorder(Publisher, -n), y = n, fill = Publisher)) + 
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en Costa Rica") +
  coord_flip()
ggsave("editorial_CR2.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.CR %>%
  group_by(license)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, -n), y = n, fill = license)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Tipo de licencias") +
  xlab(NULL) +
  ggtitle("Licencias de publicación de revistas en Costa Rica") +
  coord_flip()
ggsave("licencia_CR.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.CR %>%
  group_by(Review)%>%
  count()%>%
  filter(n>=1)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, -n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Proceso de revisión") +
  xlab(NULL) +
  ggtitle("Proceso de revisión de publicación de revistas en Costa Rica") +
  coord_flip()
ggsave("revision_CR.png", width = 3000, height = 1500, units = "px", dpi = 300)

journal.CR %>%
  ggplot(aes(x = reorder(Subjects, -table(Subjects)[Subjects]), fill = Subjects)) +
  geom_bar(stat = "count") +
  theme_minimal() +
  theme(legend.position = "none") +
  ylab("Frecuencia") +
  xlab("Temas") +
  ggtitle("Temas de publicación de revistas en Costa Rica") +
  coord_flip()
ggsave("temas_CR.png", width = 3000, height = 1500, units = "px", dpi = 300)



#quiero agregar una fila con el total de revistas 
#df = cbind('total', n=total.journal, 100)
#porcentaje.pais <- nrow(df, porcentaje) 
#https://vivaelsoftwarelibre.com/matrices-en-r-que-son-y-como-trabajar-con-ellas/
#https://estadistica-dma.ulpgc.es/cursoR4ULPGC/9a-graf-Intro.html
#https://datacarpentry.org/python-ecology-lesson-es/05-merging-data/
#paises por continente https://rstudio-pubs-static.s3.amazonaws.com/779244_6f8c8742b793408e9ea6ed82dd283425.html

#class(porcentaje)

#journal.select %>%
# Filter countries that start with "A" or "Z"
# filter(substr(country, start = 1, stop = 3) %in% c("Col")) %>%



#journal.colombia %>%
# filter( )
# journal.colombia_sp <- journal.colombia %>% filter(languages == "English")

#journal.colombia.idioma <- journal.colombia %>% 
#separate(language, c("id1", "id2", "id3", "id4", "id5"), sep=",",  extra = "merge")

#ggplot(data = journal.colombia.idioma) +
#geom_point(mapping = aes(x = journal.colombia$languages))

#journal-pais <- highlight_key(journal.colombia, ~country.of.publisher, "Select a country")
#ggplotly(p, tooltip = "Colombia") %>%
# +   layout(title = "Click on a line to highlight a year") %>%
#  +   highlight(dynamic = TRUE, selectize = TRUE)


#library(htmltools)

# Crea una página HTML que contiene el gráfico interactivo
#graficos <- tagList(
#  tags$head(
  #  tags$script(src = "https://code.highcharts.com/highcharts.js"),
 #   tags$script(src = "https://code.highcharts.com/modules/exporting.js"),
   # tags$script(src = "https://code.highcharts.com/modules/export-data.js")
#  ),
 # tags$body(
  #  hchart,
   # tags$script(HTML("Highcharts.chart('container', {...});"))
#  )
#)

# Guarda la página HTML en un archivo
#save_html(graficos, file = "paises_total.html")


#language_analysis <- journal.select %>%
#  mutate(language = strsplit(language, ", ")) %>%
 # distinct() %>%
#  arrange(language) %>%
 # group_by(language) %>%
#  summarise(language = paste(language, collapse = ", "))
