#- almacenamos en el objeto "path_wd" la ruta de mi directorio de trabajo
rm(list = ls())

getwd()

path_wd <- getwd()
#

setwd("E:/hd-git/analisis-doaj/assets/img/visualizaciones/2025")
packages <- c("tidytext", "gapminder", "tidyverse", "dplyr", "ggplot2")

#if(!require(tidyverse))       install.packages("tidyverse")
#if(!require(datos))           install.packages("datos")
#if(!require(gapminder))       install.packages("gapminder")
#if(!require(tradestatistics)) install.packages("tradestatistics")
#if(!require(highcharter))     install.packages("highcharter")
#library(tidyverse)
#library(datos)
#library(tradestatistics)
#library(highcharter)

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
#library(xlsx) ver porque necesita java
library(stringr)
library(highcharter)
library(webshot)
library(htmlwidgets)
install.packages("echarts4r")
library("echarts4r")

install.packages("plotly")
library(plotly)
#creamos un data.frame con la base de datos: mis_datos
#Gimena quiere que se haga una jupyter para levantar en vivo y generar los gráficos

#journal <- read.csv(url("https://raw.githubusercontent.com/rominicky/analisis-doaj/main/journalcsv__doaj_20240404_1721_utf8.csv?token=GHSAT0AAAAAACPRMIVAKVUR2TLJYTL5B4QOZQVK33A"))
journal <- read.csv("C:/git/analisis-doaj/assets/datos/2025_journalcsv.csv")
as_tibble(journal)
#nueva tabla con datos de interes 
getwd()
dir_output = getwd()
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
    y = 8,  # Ajustar la posición vertical de la leyenda
    itemStyle = list(fontSize = "12px", fontWeight = "normal"),
    labelFormat = "{name}: {percentage:.1f}%"
  )%>%
  hc_credits( #https://api.highcharts.com/highcharts/
    enabled = TRUE,
    text = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.",
    href = "https://github.com/rominicky/analisis-doaj",
    itemStyle = list(fontSize = "8px", fontWeight = "normal"),
    position = list(align = "left", x = 10, y = -5)  # <-- Esto alinea a la izquierda
  )
saveWidget(paises_total, file = "paises_total-interactivo.html")



# p <- ggplot(filtered_data, aes(x = "", y = percentage, fill = country)) +
#   geom_bar(stat = "identity", width = 1) +
#   coord_polar("y", start = 0) +
#   labs(
#     title = "Porcentaje de países América Latina",
#     fill = "País",
#     caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.",
#     x = NULL,
#     y = NULL
#   ) +
#   theme_void() +
#   scale_fill_discrete(
#     name = "Países",
#     labels = function(x) {
#       index <- match(x, filtered_data$country)
#       paste(x, round(filtered_data$percentage[index], 2), "%", sep = "\n")
#     }
#   ) +
#   theme(
#     legend.position = "left",
#     plot.caption = element_text(hjust = 0, face = "italic", size = 10),
#     plot.margin = unit(c(0, 0, 1, 0), "cm")
#     
#   ) +
#   guides(fill = guide_legend(ncolumn = 2)) 
# 
# # Guardar con dimensiones grandes
# ggsave(file.path(dir_output, "paises-total.jpg", plot = p, width = 3000, height = 1500, units = "px", dpi = 300)

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
  coord_flip() +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")

ggsave(
  filename = file.path(dir_output, "idiomas-total_min100.jpg"),
  width = 3000,
  height = 1500,
  units = "px",
  dpi = 300
)

#idioma por pais
journal.select %>%
  group_by(country, language) %>%
  count() %>%
  ungroup() %>%
  filter(n >= 50 & n <= 750) %>%
  #mutate(language = reorder(language, n)) %>%  # idiomas por frecuencia
  ggplot(aes(x = reorder(language, n), y = n, fill = country)) +  # n como valor numérico
  geom_col() +
  coord_flip() +
  theme_minimal() + 
  theme(legend.position = "n"
    #plot.caption = element_text(hjust = 0, face = "italic", size = 9),
    #plot.margin = unit(c(1, 1, 2, 1), "cm")
  ) +
  ylab("Número de publicaciones") +
  #xlab(NULL) +
  ggtitle("Publicaciones por idioma y país") +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
ggsave(
  filename = file.path(dir_output, "publ_paises-idiomas-total_min50-mx750-2024.jpg"),
  width = 3000,
  height = 1500,
  units = "px",
  dpi = 300)



journal.select %>%
  group_by(country, language) %>%
  count() %>%
  ungroup() %>%
  filter(n >= 300 ) %>%
  mutate(country = reorder_within(country, n, language)) %>%  # para que ordene dentro de cada panel
  ggplot(aes(x = country, y = n, fill = country)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ language, scales = "free_y") +  # un panel por idioma
  scale_x_reordered() +  # necesario con reorder_within
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", size = 7),
    strip.text = element_text(size = 7, face = "bold")
  ) +
  xlab(NULL) +
  ylab("Número de publicaciones") +
  scale_fill_viridis_d(option = "D") +
  ggtitle("Publicaciones por país e idioma") +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
ggsave(
  filename = file.path(dir_output, "publ_paises-idiomas-total_min300.jpg"),
  width = 3000,
  height = 1500,
  units = "px",
  dpi = 300)


# FALTA MEJORAR Agrupamiento y gráfico base
plot_base <- journal.select %>%
  group_by(country, language) %>%
  count() %>%
  ungroup() %>%
  filter(n >= 250 ) %>%
  mutate(country = tidytext::reorder_within(country, n, language)) %>%
  ggplot(aes(x = country, y = n, fill = country, text = paste("País:", country, "Idioma:", language, "Publicaciones:", n))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ language, scales = "free_y") +
  tidytext::scale_x_reordered() +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, face = "italic", size = 9),
    strip.text = element_text(size = 10, face = "bold")
  ) +
  xlab(NULL) +
  ylab("Número de publicaciones") +
  ggtitle("Publicaciones por país e idioma") +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")

# Hacerlo interactivo
ggplotly(plot_base, tooltip = "text")
htmlwidgets::saveWidget(ggplotly(plot_base), "grafico-interactivo.html")


# por continente 
# Instalar la librería si no está instalada
install.packages("countrycode")
#VER tooltip_table
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
  filter(n >= 200 ) %>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = language, y = n, fill = country)) +  
  geom_col(show.legend = FALSE) +  
  #geom_text(aes(label = n), vjust = -0.5, size = 3, color = "black") +
  theme_minimal() +
  ylab("Número de publicaciones") +
  xlab(NULL) +
  ggtitle("Idiomas de publicación en todo el mundo diferenciado por países") +
  coord_flip() +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.") +
  facet_wrap(~ continent) +  # Usar facetas por continente
  scale_fill_viridis_d(option = "D") +  # Usar una paleta con más colores
  theme(
    plot.title = element_text(hjust = 0, size = 16),
    axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(10, 10, 10, 10)
  )
ggsave(filename = file.path(dir_output, "idiomas-continente_min200.jpg"), width = 3000, height = 1500, units = "px", dpi = 300)
class(journal.select)

#similar con botones
# journal.select %>%
#   group_by(language, country, continent) %>%
#   count() %>%
#   filter(n >= 25 & n<= 150) %>%
#   mutate(language = reorder(language, n)) %>%
#   ggplot(aes(x = language, y = n, fill = country)) +  
#   geom_col(show.legend = FALSE) +  # Eliminamos la leyenda de los países
#   #geom_text(aes(label = n), vjust = -0.5, size = 3, color = "black") +  
#   theme_minimal() +
#   ylab("Número de publicaciones") +
#   xlab(NULL) +
#   ggtitle("Idiomas de publicación de revistas en todo el mundo por país") +
#   coord_flip() +
#   labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.") +
#   facet_wrap(~ continent, scales = "free_y") +  # Ajusta las escalas de cada faceta por continente
#   scale_fill_viridis_d(option = "H") +  # Paleta viridis con más colores, ajustada para una mejor visualización
#   theme(
#     plot.title = element_text(hjust = 0.5, size = 16),
#     axis.text.x = element_text(size = 10, angle = 45, hjust = 1),  # Ajusta la rotación para los textos en el eje X
#     axis.text.y = element_text(size = 10),
#     axis.title.y = element_text(size = 10),
#     plot.margin = margin(10, 10, 10, 10),
#     strip.text = element_text(size = 10),  # Títulos de las facetas más grandes
#     strip.background = element_rect(fill = "lightgray")  # Fondo gris claro para las facetas
#   )
#ggsave(file.path(dir_output, "idiomas-cont_min25_mx150.jpg", width = 3000, height = 1500, units = "px", dpi = 300)


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
  ggplot(aes(x = reorder(language, n), y = n, fill = language)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "n") +  # Cambié "n" a "none" para ocultar la leyenda
  scale_fill_viridis_d(option = "G") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicaciones sobre 'Social Sciences' en América Latina") +
  coord_flip() +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
ggsave(filename= file.path(dir_output, "social_sciences_idioma_AmLat.jpg"), width = 3000, height = 1500, units = "px", dpi = 300)


journal.amlat %>%
  filter(Subjects %in% c("History", "Education History", "Education Social", "History General", "History America", "Language", "Philosophy", "Political Science", "Education Theory", "Bibliography Library", "Auxiliary sciences", "Education Special", "Education", "Social Sciences")) %>%
  group_by(language) %>%
  count() %>%
  filter(n >= 3) %>%
  mutate(language = reorder(language, n)) %>%
  ggplot(aes(x = reorder(language, n), y = n, fill = language)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "n") +  # Cambié "n" a "none" para ocultar la leyenda
  scale_fill_viridis_d(option = "G") +
  ylab("Número de veces que aparecen") +
  xlab(NULL) +
  ggtitle("Idiomas de publicaciones sobre Ciencias Sociales y Humanidades en América Latina") +
  coord_flip() +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
ggsave(filename = file.path(dir_output, "area_cssoc_hum_idioma_AmLatina.jpg"), width = 3000, height = 1500, units = "px", dpi = 300)  
  
#############################################################
#Voy a unificar las social sciences en una 
# Crear un gráfico de barras apiladas
journal.amlat %>%
  group_by(language, country) %>%
  count() %>%
  filter(n >= 5) %>%
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
    subtitle = "Solo se muestran combinaciones con al menos 10 registros"
  ) +
  coord_flip() +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
ggsave(filename = file.path(dir_output, "idiomas-amlat_min5.jpg"), width = 3000, height = 1500, units = "px", dpi = 300)

english <- journal.amlat[journal.amlat$language == "English", ]
colnames(english)

#install.packages("scico")

library(scico)
#install.packages("Cairo")
library(Cairo)
library(colorspace)
paleta <- c(
  scico(8, palette = "tokyo"),
  scico(8, palette = "roma"),
  viridis(8, option = "plasma"),
  pal_nejm("default")(8))

viridis_names <-c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo")
paleta2 <- c(
  met.brewer( "Renoir"),
  met.brewer("Cassatt1"),
  met.brewer("Cassatt2"),
  met.brewer("Cross"),
  met.brewer("Moreau"),
  met.brewer("Tam"),
  met.brewer("Signac"))
paletas <- c(paleta, paleta2)
# Crear el gráfico de dispersión con etiquetas de conteo
ggplot(journal.amlat %>% 
         group_by(language, country, Subjects) %>% 
         summarise(count = n()) %>% 
         filter(count >= 4 ) %>% 
         mutate(language = reorder(language, count)), 
       aes(x = country, y = language, color = Subjects)) +
  geom_point(size = 8, alpha = 0.7) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 1 , size = 12),  # Espacio bajo el título
    axis.text.x = element_text(angle = 60, hjust = 1),
    legend.position = "right",
    legend.box = "horizontal",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 7),
    legend.spacing.y = unit(0.1, "cm"),  # Espacio entre filas de leyenda
    plot.margin = margin(t = 20, r = 20, b = 10, l = 20)
  ) +
  guides(color = guide_legend(ncol = 2, byrow = TRUE)) +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.") +
  ylab("Idiomas") +
  xlab("Países") +
  ggtitle("Relaciones entre:
          países, idiomas y áreas en publicaciones de América Latina") +
  #scale_color_scico_d(palette = "tokyo")  # También probá: "lajolla", "tokyo", "batlow"
  scale_color_manual(values = paletas)
  #scale_fill_nejm()
  #scale_color_viridis_d(option = "A")  # Utilizar una paleta de colores como Viridis
ggsave(
  filename = file.path(dir_output, "idiomas-amlat_min5.jpg"),
  width = 3000,
  height = 1500,
  units = "px",
  dpi = 300)

install.packages("Polychrome")
library(Polychrome)
install.packages("MetBrewer")
library(MetBrewer)

install.packages("ggsci")
library(ggsci)

library(viridis)

# ggplot(english, aes(x = country, fill = Subjects)) +
#   geom_bar(position = "stack") +
#   scale_fill_viridis_d(option = "D") +  # Usar la paleta viridis
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   ylab("Número de registros") +
#   xlab("Países") +
#   ggtitle("Distribución de temas por países de América Latina con publicaciones unicamente en Inglés")

#otro gráfico
journal.amlat %>%
  group_by(Publisher)%>%
  count()%>%
  filter(n>15)%>%
  mutate(Publisher = reorder(Publisher, n)) %>%
  ggplot(aes(x = reorder(Publisher, n), y = n, fill = Publisher)) +  # Ordenar x según la frecuencia -n
  geom_col () + # si agrego , fill = "blue" cambio color
  theme_minimal() +
  theme(legend.position = "n") +
  scale_fill_manual(values = paleta2) +
  ylab("Cantidad de revistas publicadas por") +
  xlab(NULL) +
  ggtitle("Editorial de publicación de revistas en América Latina",
    subtitle = "Restringido a editoriales con más de 15 revistas") +
  coord_flip() +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
ggsave(filename = file.path(dir_output, "editorial_América Latina_min15.jpg"), width = 3000, height = 1500, units = "px", dpi = 300)

journal.amlat %>%
  group_by(license) %>%
  count() %>%
  filter(n >= 5) %>%
  mutate(license = reorder(license, n)) %>%
  ggplot(aes(x = reorder(license, n), y = n, fill = license)) +
  geom_col() +
  scale_fill_manual(values = paleta2) +
  theme_minimal() +
  theme(legend.position = "n") +  # debe ser "none", no "n"
  ylab("Frecuencias") +
  xlab("Tipo de licencias") +
  ggtitle("Licencias utilizadas por revistas en América Latina") +
  coord_flip() +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
ggsave(filename = file.path(dir_output, "licencia_AmLatina_min5.jpg"), width = 3000, height = 1500, units = "px", dpi = 300)

journal.amlat %>%
  group_by(Review)%>%
  count()%>%
  filter(n>=8)%>%
  mutate(Review = fct_reorder(Review, n)) %>%
  ggplot(aes(x = reorder(Review, n), y = n, fill = Review)) +
  geom_col () + # si agrego , fill = "blue" cambio color
  scale_fill_scico_d(palette = "tokyo") +
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Frecuencia") +
  xlab("Tipo de revisiones") +
  ggtitle("Proceso de revisión para publicaciones para las revistas de América Latina") +
  #scale_color_scico_d(palette = "lajolla") +
  coord_flip() +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
ggsave(filename = file.path(dir_output, "revision_AmLatina_min8.jpg"), width = 3000, height = 1500, units = "px", dpi = 300)

journal.amlat %>%
  group_by(Subjects) %>%
  count() %>%
  filter(n > 12) %>%
  mutate(Subjects = fct_reorder(Subjects, n)) %>%
  ggplot(aes(x = reorder(Subjects, n), y = n, fill = Subjects)) +
  geom_col() +
  theme_minimal() +
  theme(legend.position = "n") +
  ylab("Frecuencia") +
  xlab("Áreas") +
  ggtitle("Áreas de publicación de revistas en América Latina",
          subtitle = "Frecuencia mayor a 12") +
  scale_fill_manual(values = paleta2) +
  coord_flip() +
  labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
ggsave(filename = file.path(dir_output, "areas_amLatina_min12.jpg"), width = 3000, height = 1500, units = "px", dpi = 300)



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
    ruta <- file.path(dir_output, paste0(nombre, pais, ".jpg"))
    ggsave(filename = ruta, plot = plot, width = 3000, height = 1500, units = "px", dpi = 300)
  }
  
  # Gráfico por idioma
  g1 <- journal.pais %>%
    group_by(language) %>%
    count() %>%
    filter(n > 5) %>%
    mutate(language = reorder(language, n)) %>%
    ggplot(aes(x = reorder(language, n), y = n, fill = language)) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "n") +
    ylab("Número de veces que aparecen") +
    xlab(NULL) +
    ggtitle(paste("Idiomas de publicación de revistas en", pais)) +
    coord_flip() +
    labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.") 
  print(g1)
  guardar_grafico(g1, "idiomas_", pais)
  
  # Gráfico por editorial
  g2 <- journal.pais %>%
    group_by(Publisher) %>%
    count() %>%
    filter(n > 5) %>%
    mutate(Publisher = reorder(Publisher, n)) %>%
    ggplot(aes(x = reorder(Publisher, n), y = n, fill = Publisher)) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none") +
    ylab("Cantidad de revistas publicadas por") +
    xlab(NULL) +
    ggtitle(paste("Editorial de publicación de revistas en", pais)) +
    coord_flip() +
    labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
  print(g2)
  guardar_grafico(g2, "editorial_", pais)
  
  # Gráfico por licencias
  g3 <- journal.pais %>%
    group_by(license) %>%
    count() %>%
    filter(n > 1) %>%
    mutate(license = reorder(license, n)) %>%
    ggplot(aes(x = reorder(license, n), y = n, fill = license)) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none") +
    ylab("Tipo de licencias") +
    xlab(NULL) +
    ggtitle(paste("Licencias de publicación de revistas en", pais)) +
    coord_flip() +
    labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
  print(g3)
  guardar_grafico(g3, "licencias_", pais)
  
  # Gráfico por proceso de revisión
  g4 <- journal.pais %>%
    group_by(Review) %>%
    count() %>%
    filter(n > 5) %>%
    mutate(Review = reorder(Review, n)) %>%
    ggplot(aes(x = reorder(Review, n), y = n, fill = Review)) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none") +
    ylab("Proceso de revisión") +
    xlab(NULL) +
    ggtitle(paste("Proceso de revisión de revistas en", pais)) +
    coord_flip() +
    labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
  print(g4)
  guardar_grafico(g4, "revision_", pais)
  
  # Gráfico por temas
  g5 <- journal.pais %>%
    group_by(Subjects) %>%
    count() %>%
    filter(n > 5) %>%
    mutate(Subjects = reorder(Subjects, n)) %>%
    ggplot(aes(x = reorder(Subjects, n), y = n, fill = Subjects)) +
    geom_col() +
    theme_minimal() +
    theme(legend.position = "none") +
    ylab("Temas") +
    xlab(NULL) +
    ggtitle(paste("Temas de publicación de revistas en", pais)) +
    coord_flip() +
    labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
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
    coord_flip() +
    labs(caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ.")
  print(g5)
  guardar_grafico(g6, "identificadores_", pais)
}

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
        caption = "Citar como: Romina De León, 2025. Análisis de revistas latinoamericanas en DOAJ."
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


walk(selected_countries, analizar_pais)
 
selected_countries
analizar_pais("Argentina")
analizar_pais("Brazil")
analizar_pais("Chile")
analizar_pais("Colombia")
analizar_pais("Mexico")
analizar_pais("Ecuador")
analizar_pais("Costa Rica")
analizar_pais("Bolivia, Plurinational State of")
analizar_pais("Dominican Republic")
analizar_pais("El Salvador")
analizar_pais("Guatemala")
analizar_pais("Honduras")
analizar_pais("Nicaragua")
analizar_pais("Panama") 
analizar_pais("Paraguay")
analizar_pais("Peru")
analizar_pais("Uruguay")
analizar_pais("Venezuela, Bolivarian Republic of")



##################################################################
