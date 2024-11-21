library(tidyverse)
library(magrittr)
library(lubridate)
library(plotly)
library(leaflet)
library(tidygeocoder)

path <- "C:/Users/joaov/Downloads/P#02/00_data/002_clean-data/vendas.csv"
vendas <- read_csv(path)

# Agrupa o FATURAMENTO_TOTAL das vendas por UF
faturamento_por_UF <- vendas %>% 
  select(ESTADO_DE_DESTINO_DA_VENDA, FATURAMENTO) %>%
  group_by(ESTADO_DE_DESTINO_DA_VENDA) %>%
  summarise(faturamento_total = sum(FATURAMENTO))