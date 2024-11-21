library(tidyverse)
library(magrittr)
library(lubridate)
library(plotly)
library(scales)

path <- "C:/Users/joaov/Downloads/P#02/00_data/002_clean-data/vendas.csv"
vendas <- read_csv(path)

# Calcula o FATURAMENTO_TOTAL por DIA_DA_SEMANA e SEMANA_DO_MES
input_status <- "Pago"
input_mes <- 10
semana_mesVSdia_semana <- vendas %>% 
  select(DATA, MES, DIA_DA_SEMANA, SEMANA_DO_MES, FATURAMENTO, STATUS) %>% 
  filter(STATUS == input_status, MES == input_mes) %>%
  group_by(DIA_DA_SEMANA, SEMANA_DO_MES) %>%
  summarise(faturamento_total = sum(FATURAMENTO)) %>% 
  arrange(desc(SEMANA_DO_MES))

# Obs.: Adicionar valores totais / marginais

# Criando uma tabela com o faturamento total por DIA_DA_SEMANA e SEMANA_DO_MES
table_dupla_entrada <- semana_mesVSdia_semana %>%
  ggplot(aes(x = DIA_DA_SEMANA, y = SEMANA_DO_MES, fill = faturamento_total)) +
  geom_tile() +
  geom_text(aes(label = paste0("R$", round(faturamento_total/1000, 1), "k"),
                color = ifelse(faturamento_total > median(faturamento_total), "white", "black"))) +  # Cor do texto baseada no valor de faturamento_total
  scale_fill_gradient(low = "white", high = "#424242") +
  scale_color_identity() +  # Usa a cor direta (preto ou branco)
  labs(title = "Melhores Dias e Semanas de Venda", x = "Dia", y = "Semana") +
  scale_y_reverse() +  # Inverte o eixo Y
  theme_minimal() + 
  theme(
    # axis.text.x = element_text(angle = 45, hjust = 1),  # Ajusta a rotação dos rótulos de eixo X
    axis.text.x.top = element_text(angle = 45, hjust = 1),  # Coloca o texto do eixo X no topo
    axis.title.x.top = element_text(size = 10),  # Título do eixo X no topo
    axis.title.y = element_text(size = 10),  # Tamanho do título Y
    panel.grid = element_blank(),  # Remove a grid
    axis.title.x = element_blank(),  # Remove o título do eixo X inferior
    axis.ticks.x = element_blank(),  # Remove as marcas do eixo X inferior
    axis.ticks.x.top = element_line(color = "black")  # Coloca as marcas no eixo X superior
  )
table_dupla_entrada