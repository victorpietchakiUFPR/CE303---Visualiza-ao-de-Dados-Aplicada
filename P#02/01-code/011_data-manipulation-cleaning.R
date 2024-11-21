library(tidyverse)
library(magrittr)
library(lubridate)

# Define os caminhos para os arquivos de dados
path_vendas <- "C:/Users/joaov/Downloads/P#02/00_data/001_raw-data/Vendas.csv"
path_produtos <- "C:/Users/joaov/Downloads/P#02/data/raw-data/Produtos.csv"

# Le os arquivos de dados
vendas <- read_csv2(path_vendas)
produtos <- read_csv2(path_produtos)

# Verifica as variaveis dos dataframes
vendas %>% glimpse()
produtos %>% glimpse()

# Exclui a coluna ID_ANUNCIO e faz o join dos dataframes
vendas %<>% 
  select(-ID_ANUNCIO) %>% 
  inner_join(produtos, by = c("SKU" = "SKU"))

# Converte a variavel DATA para o formato Date
vendas$DATA %<>% 
  as.Date(DATA, format = "%d/%m/%Y")
# Cria colunas das variaveis Ano, Mes, Dia e Dia da Semana
vendas %<>% 
  mutate(ANO = year(DATA),
         MES = month(DATA),
         DIA = day(DATA),
         DIA_DA_SEMANA = weekdays(DATA),
         SEMANA_DO_MES = floor((as.numeric(DATA) - as.numeric(floor_date(DATA, "month"))) / 7) + 1)

vendas$DIA_DA_SEMANA <- as.factor(vendas$DIA_DA_SEMANA)#, levels = c("domingo", "segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado"))
vendas$DIA_DA_SEMANA <- factor(vendas$DIA_DA_SEMANA, levels = c("domingo", "segunda-feira", "terça-feira", "quarta-feira", "quinta-feira", "sexta-feira", "sábado"))

write_csv(vendas, "C:/Users/joaov/Downloads/P#02/data/clean-data/vendas.csv")