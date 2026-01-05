packages <- c(
  "tidyverse","RColorBrewer","paletteer","datos","gapminder",
  "tradestatistics","highcharter","treemapify","ggplot2","rio",
  "echarts4r","plotly","tidytext","tidyselect","dplyr","purrr",
  "readr","stringr","webshot","htmlwidgets","pandoc","forcats",
  "cowplot","ggrepel","sf","rnaturalearth","rnaturalearthhires",
  "countrycode","scico","Cairo","colorspace","Polychrome",
  "MetBrewer","ggsci","zoo","ggiraph"
)

install.packages(
  packages,
  dependencies = TRUE,
  Ncpus = 2
)

# Registrar IRkernel para Jupyter
IRkernel::installspec(user = FALSE)

