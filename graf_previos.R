
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
