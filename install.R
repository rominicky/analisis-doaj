install.packages(c(
  "remotes",
  "tidyverse","RColorBrewer","paletteer","datos","gapminder",
  "tradestatistics","highcharter","treemapify","ggplot2","rio",
  "echarts4r","plotly","tidytext","tidyselect","dplyr","purrr",
  "readr","stringr","webshot","htmlwidgets","pandoc","forcats",
  "cowplot","ggrepel","sf","rnaturalearth","countrycode",
  "scico","Cairo","colorspace","Polychrome","MetBrewer",
  "ggsci","zoo","ggiraph"
), dependencies = TRUE, Ncpus = 2)

# paquete NO-CRAN
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  remotes::install_github("ropensci/rnaturalearthhires")
}

IRkernel::installspec(user = FALSE)
