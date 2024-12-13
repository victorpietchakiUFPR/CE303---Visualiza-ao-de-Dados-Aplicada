library(magrittr)
library(tidyverse)

# Define o diretorio padrao para esse projeto (Altere-o se for rodar na sua maquina para que os codigos a baixo funcionem)
setwd("C:/Users/joaov/Downloads/AV2-CE303-Visualizacao-de-Dados-Aplicada_files/")
root_path <- file.path(getwd())
files_names <- c("tipos_mortalidade_MUN.RData", "tipos_mortalidade_REGIAO_SUL.RData",
                 "tipos_nascidos_MUN.RData", "tipos_nascidos_REGIAO_SUL.RData", 
                 "plano_saude_MUN.RData", "plano_saude_REGIAO_SUL.RData",
                 "indicadores_saude_UF.RData", "indicadores_saude_REGIAO_SUL.RData",
                 "municipios_UF_regioes.RData",
                 "data_anos.RData",
                 "tabela3.RData")

# Carrega arquivos
for (i in 1:length(files_names)) {
  load(paste0(root_path, "../../data/clean_data/", files_names[i]))
}

tabela1 <- tipos_mortalidade_MUN %>%
  filter(str_detect(tipo_mortalidade, "infantil"))

tabela2 <- tipos_nascidos_MUN %>%
  filter(str_detect(tipo_nascimento, "pelo menos sete consultas de pré-natal"))

tabela3 <- tabela1 %>%
  inner_join(tabela2, by = c("Municipio", "Ano", "UF")) %>%
  select(Municipio, Ano, UF, taxa_mortalidade, percent_nascidos_vivos)
tabela3 %<>%
  left_join(municipios_UF_regioes %>% select(UF, Regiao, Estado) %>% unique(), by = c("UF" = "UF"))
save(tabela3, file = paste0(root_path, "../../data/clean_data/tabela3.RData"))


tabela4 <- tipos_mortalidade_MUN %>%
  inner_join(plano_saude_MUN, by = c("Municipio", "Ano", "UF")) %>%
  select(Municipio, Ano, UF, tipo_mortalidade, taxa_mortalidade, percent_pessoas_cobertas_planos_saude_suplementar)
tabela4 %<>%
  left_join(municipios_UF_regioes %>% select(UF, Regiao, Estado) %>% unique(), by = c("UF" = "UF")) %<>%
  mutate(faixa_percentual_cobertura = cut(
    percent_pessoas_cobertas_planos_saude_suplementar,
    breaks = seq(0, 100, by = 10),  # Cria intervalos de 10%
    labels = paste0(seq(0, 90, by = 10), "-", seq(10, 100, by = 10), "%"),
    include.lowest = TRUE
  ))
save(tabela4, file = paste0(root_path, "../../data/clean_data/tabela4.RData"))


tabela5 <- tipos_mortalidade_MUN$tipo_mortalidade %>% unique() %>% as.data.frame()
save(tabela5, file = paste0(root_path, "../../data/clean_data/tabela5.RData"))

