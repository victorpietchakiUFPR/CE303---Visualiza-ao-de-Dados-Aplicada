library(tidyverse)
library(magrittr)
library(plotly)

path <- "C:/Users/joaov/Downloads/P#02/00_data/002_clean-data/vendas.csv"
vendas <- read_csv(path)

# Cria uma tabela do valor total de FATURAMENTO por DATA
faturamento_por_data <- vendas %>% 
  select(DATA, DIA, MES, ANO, FATURAMENTO, STATUS) %>%
  filter(STATUS == "Pago") %>% 
  group_by(DATA) %>%
  summarise(faturamento_total = sum(FATURAMENTO))

# Cria um grafico de linhas do faturamento total por data
serie_temporal_faturamento <- faturamento_por_data %>% 
  ggplot(aes(x = DATA, y = faturamento_total)) +
  geom_point() + 
  geom_line() +
  labs(title = "Faturamento Diário",
       x = "Data",
       y = "Faturamento") +
  theme_minimal() + 
  theme(
    panel.grid.major = element_line(color = "#C7C8C9", size = 0.8, linetype = "dotted") # Linhas principais em #C7C8C9 (cinza)
  )

serie_temporal_faturamento %>% ggplotly()