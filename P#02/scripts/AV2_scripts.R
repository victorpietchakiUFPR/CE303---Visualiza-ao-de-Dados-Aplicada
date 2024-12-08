library(magrittr)
library(data.table)
library(tidyverse)
library(readxl)
library(lubridate)

# Define o diretorio padrao para esse projeto (Altere-o se for rodar na sua maquina para que os codigos a baixo funcionem)
setwd("C:/Users/joaov/Downloads/AV2-CE303-Visualizacao-de-Dados-Aplicada_files")
root_path <- file.path(getwd())



# Le arquivo excel de dados do DataSUS por ano e Municipio
# Remove a primeira linha, que consistia de uma agregaçao de todos os municipios (Brasil)
dataSUS_MUN_2013 <- read_excel(file.path(getwd(), "data/dataSUS_MUN_2013.xlsx")) %>% as.tibble() %>% filter(row_number() > 1)
dataSUS_MUN_2014 <- read_excel(file.path(getwd(), "data/dataSUS_MUN_2014.xlsx")) %>% as.tibble() %>% filter(row_number() > 1)
dataSUS_MUN_2015 <- read_excel(file.path(getwd(), "data/dataSUS_MUN_2015.xlsx")) %>% as.tibble() %>% filter(row_number() > 1)
dataSUS_MUN_2016 <- read_excel(file.path(getwd(), "data/dataSUS_MUN_2016.xlsx")) %>% as.tibble() %>% filter(row_number() > 1)
dataSUS_MUN_2017 <- read_excel(file.path(getwd(), "data/dataSUS_MUN_2017.xlsx")) %>% as.tibble() %>% filter(row_number() > 1)

# Le arquivo excel de dados do DataSUS por ano e Unidade Federativa
# # Remove a primeira linha, que consistia de uma agregaçao de todos as unidades federativas (Brasil)
dataSUS_UF_2013 <- read_excel(file.path(getwd(), "data/dataSUS_UF_2013.xlsx")) %>% as.tibble() %>% filter(row_number() > 1)
dataSUS_UF_2014 <- read_excel(file.path(getwd(), "data/dataSUS_UF_2014.xlsx")) %>% as.tibble() %>% filter(row_number() > 1)
dataSUS_UF_2015 <- read_excel(file.path(getwd(), "data/dataSUS_UF_2015.xlsx")) %>% as.tibble() %>% filter(row_number() > 1)
dataSUS_UF_2016 <- read_excel(file.path(getwd(), "data/dataSUS_UF_2016.xlsx")) %>% as.tibble() %>% filter(row_number() > 1)
dataSUS_UF_2017 <- read_excel(file.path(getwd(), "data/dataSUS_UF_2017.xlsx")) %>% as.tibble() %>% filter(row_number() > 1)



# Cria uma lista com as tabelas de dados acima (Municipio)
lista_dados_MUN <- list(dataSUS_MUN_2013, dataSUS_MUN_2014, dataSUS_MUN_2015, dataSUS_MUN_2016, dataSUS_MUN_2017)

# Cria uma lista com as tabelas de dados acima (Unidade Federativa)
lista_dados_UF <- list(dataSUS_UF_2013, dataSUS_UF_2014, dataSUS_UF_2015, dataSUS_UF_2016, dataSUS_UF_2017)



# Tabela: Municipios x Tipo de mortalidade x Taxa de mortalidade x Ano
lista_tipos_mortalidade_MUN <- rep(list(NULL), 5) # Cria uma lista de 5 elementos nulos
tipos_mortalidade_MUN <- tibble(NULL) # Cria um tibble nulo
# Para todas as tabelas da lista_dados_MUN, seleciona apenas as colunas que descrevem o tipo de mortalidade
for(i in 1:length(lista_dados_MUN)){
  lista_tipos_mortalidade_MUN[[i]] <- lista_dados_MUN[[i]] %>%
    select(Territorialidades, contains("Taxa de mortalidade")) %>% 
    pivot_longer(cols = contains("Taxa de mortalidade"),
                 names_to = "tipo_mortalidade",
                 values_to = "taxa_mortalidade") %>%
    mutate(Ano = paste0(str_extract(tipo_mortalidade, "[0-9]{4}"), "-01-01") %>% as.Date()) %>%
    mutate(tipo_mortalidade = str_replace(tipo_mortalidade, ".*(de|por|mulheres)", "")) %>% 
    mutate(tipo_mortalidade = str_replace(tipo_mortalidade, "[0-9]{4}", "")) %>% 
    mutate(tipo_mortalidade = str_trim(tipo_mortalidade))
  tipos_mortalidade_MUN <- rbind(tipos_mortalidade_MUN, lista_tipos_mortalidade_MUN[[i]])
}
tipos_mortalidade_MUN %<>% mutate(UF = str_extract(Territorialidades, "\\(.*?\\)")) # Extrai a UF da coluna Territorialidades
tipos_mortalidade_MUN %<>% mutate(Territorialidades = str_replace(Territorialidades, "\\(.*?\\)", "")) %>% # Remove a UF da coluna Territorialidades
                           mutate(Territorialidades = str_trim(Territorialidades))
tipos_mortalidade_MUN %<>% mutate(UF = str_replace(UF, "\\(", "")) %>% # Remove os parenteses
                           mutate(UF = str_replace(UF, "\\)", "")) %>% # Remove os parenteses
                           mutate(UF = str_trim(UF)) %>% # Remove os espaços em branco
                           rename(Municipio = Territorialidades) # Renomeia a coluna Territorialidades para Municipio
tipos_mortalidade_MUN %<>% 
  mutate(Municipio = str_replace_all(Municipio, "([áàãâä])", "a")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([éèêë])", "e")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([íìîï])", "i")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([óòõôö])", "o")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([úùûü])", "u")) %>%
  mutate(Municipio = str_replace_all(Municipio, "ç", "c")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÁÀÃÂÄ])", "A")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÉÈÊË])", "E")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÍÌÎÏ])", "I")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÓÒÕÔÖ])", "O")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÚÙÛÜ])", "U")) %>% 
  mutate(Municipio = iconv(Municipio, from = "UTF-8", to = "ASCII//TRANSLIT"))
save(tipos_mortalidade_MUN, file = paste0(root_path, "/data/clean_data/", "tipos_mortalidade_MUN.RData")) # Salva o objeto tipos_mortalidade_MUN



# Tabela: Municipios x Tipo Nascidos x Percentual de nascidos vivos x Ano
lista_nascidos_vivos_MUN <- rep(list(NULL), 5) # Cria uma lista de 5 elementos nulos
tipos_nascidos_MUN <- tibble(NULL) # Cria um tibble nulo
# Para todas as tabelas da lista_dados_MUN, seleciona apenas as colunas que descrevem o os nascimentos vivos
for(i in 1:length(lista_dados_MUN)){
  lista_nascidos_vivos_MUN[[i]] <- lista_dados_MUN[[i]] %>%
    select(Territorialidades, contains("nascidos vivos")) %>%
    pivot_longer(cols = contains("nascidos vivos"),
                 names_to = "tipo_nascimento",
                 values_to = "percent_nascidos_vivos") %>%
    mutate(Ano = paste0(str_extract(tipo_nascimento, "[0-9]{4}"), "-01-01") %>% as.Date()) %>%
    mutate(tipo_nascimento = str_replace(tipo_nascimento, "% de nascidos vivos com", "")) %>%
    mutate(tipo_nascimento = str_replace(tipo_nascimento, "[0-9]{4}", "")) %>% 
    mutate(tipo_nascimento = str_trim(tipo_nascimento))
  tipos_nascidos_MUN <- rbind(tipos_nascidos_MUN, lista_nascidos_vivos_MUN[[i]])
}
tipos_nascidos_MUN %<>% mutate(UF = str_extract(Territorialidades, "\\(.*?\\)")) # Extrai a UF da coluna Territorialidades
tipos_nascidos_MUN %<>% mutate(Territorialidades = str_replace(Territorialidades, "\\(.*?\\)", "")) %>% # Remove a UF da coluna Territorialidades
                        mutate(Territorialidades = str_trim(Territorialidades))
tipos_nascidos_MUN %<>% mutate(UF = str_replace(UF, "\\(", "")) %>% # Remove os parenteses
                        mutate(UF = str_replace(UF, "\\)", "")) %>% # Remove os parenteses
                        mutate(UF = str_trim(UF)) %>%  # Remove os espaços em branco
                        rename(Municipio = Territorialidades) # Renomeia a coluna Territorialidades para Municipio
tipos_nascidos_MUN %<>%
  mutate(Municipio = str_replace_all(Municipio, "([áàãâä])", "a")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([éèêë])", "e")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([íìîï])", "i")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([óòõôö])", "o")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([úùûü])", "u")) %>%
  mutate(Municipio = str_replace_all(Municipio, "ç", "c")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÁÀÃÂÄ])", "A")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÉÈÊË])", "E")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÍÌÎÏ])", "I")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÓÒÕÔÖ])", "O")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÚÙÛÜ])", "U")) %>% 
  mutate(Municipio = iconv(Municipio, from = "UTF-8", to = "ASCII//TRANSLIT"))
save(tipos_nascidos_MUN, file = paste0(root_path, "/data/clean_data/", "tipos_nascidos_MUN.RData")) # Salva o objeto tipos_nascidos_MUN



# Tabela: Municipios x Percentual de pessoas cobertas por planos de saude suplementar x Ano
lista_plano_saude_MUN <- rep(list(NULL), 5) # Cria uma lista de 5 elementos nulos
plano_saude_MUN <- tibble(NULL) # Cria um tibble nulo
# Para todas as tabelas da lista_dados_MUN, seleciona apenas as colunas que descrevem o tipo de mortalidade
for(i in 1:length(lista_dados_MUN)){
  lista_plano_saude_MUN[[i]] <- lista_dados_MUN[[i]] %>%
    select(Territorialidades, contains("planos de saúde")) %>%
    pivot_longer(cols = contains("planos de saúde"),
                 names_to = "pessoas_cobertas_planos_saude_suplementar",
                 values_to = "percent_pessoas_cobertas_planos_saude_suplementar") %>% 
    mutate(Ano = paste0(str_extract(pessoas_cobertas_planos_saude_suplementar, "[0-9]{4}"), "-01-01") %>% as.Date())
  plano_saude_MUN <- rbind(plano_saude_MUN, lista_plano_saude_MUN[[i]])
}
plano_saude_MUN %<>% mutate(UF = str_extract(Territorialidades, "\\(.*?\\)")) # Extrai a UF da coluna Territorialidades
plano_saude_MUN %<>% mutate(Territorialidades = str_replace(Territorialidades, "\\(.*?\\)", "")) %>% # Remove a UF da coluna Territorialidades
                     mutate(Territorialidades = str_trim(Territorialidades))
plano_saude_MUN %<>% mutate(UF = str_replace(UF, "\\(", "")) %>% # Remove os parenteses
                     mutate(UF = str_replace(UF, "\\)", "")) %>% # Remove os parenteses
                     mutate(UF = str_trim(UF)) %>% # Remove os espaços em branco
                     rename(Municipio = Territorialidades) %>% # Renomeia a coluna Territorialidades para Municipio
                     mutate(pessoas_cobertas_planos_saude_suplementar = NULL) # Remove a coluna pessoas_cobertas_planos_saude_suplementar
plano_saude_MUN %<>%
  mutate(Municipio = str_replace_all(Municipio, "([áàãâä])", "a")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([éèêë])", "e")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([íìîï])", "i")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([óòõôö])", "o")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([úùûü])", "u")) %>%
  mutate(Municipio = str_replace_all(Municipio, "ç", "c")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÁÀÃÂÄ])", "A")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÉÈÊË])", "E")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÍÌÎÏ])", "I")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÓÒÕÔÖ])", "O")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÚÙÛÜ])", "U")) %>% 
  mutate(Municipio = iconv(Municipio, from = "UTF-8", to = "ASCII//TRANSLIT"))
save(plano_saude_MUN, file = paste0(root_path, "/data/clean_data/", "plano_saude_MUN.RData")) # Salva o objeto tipos_nascidos_MUN



# Tabela: Unidades Federativas x Ano x IDHM Longevidade x
#                                      Esperança de vida ao nascer x
#                                      Mortalidade infantil x 
#                                      Taxa bruta de mortalidade x 
#                                      Taxa de envelhecimento
lista_indicadores_saude_UF <- rep(list(NULL), 5) # Cria uma lista de 5 elementos nulos
indicadores_saude_UF <- tibble() # Cria um tibble nulo
ano <- 2013
for(i in 1:length(lista_indicadores_saude_UF)){
  lista_dados_UF[[i]] %<>% rename(IDHM_longevidade = colnames(lista_dados_UF[[i]] %>% select(length(colnames(lista_dados_UF[[i]])))))
  lista_dados_UF[[i]] %<>% rename(taxa_envelhecimento = colnames(lista_dados_UF[[i]] %>% select(length(colnames(lista_dados_UF[[i]])) - 1)))
  lista_dados_UF[[i]] %<>% rename(mortalidade_infantil = colnames(lista_dados_UF[[i]] %>% select(length(colnames(lista_dados_UF[[i]])) - 2)))
  lista_dados_UF[[i]] %<>% rename(espranca_vida_ao_nascer = colnames(lista_dados_UF[[i]] %>% select(length(colnames(lista_dados_UF[[i]])) - 3)))
  lista_dados_UF[[i]] %<>% rename(taxa_bruta_mortalidade = colnames(lista_dados_UF[[i]] %>% select(contains("Taxa bruta de mortalidade"))))
  
  
  lista_indicadores_saude_UF[[i]] <- lista_dados_UF[[i]] %>%
    select(Territorialidades,
           IDHM_longevidade,
           taxa_envelhecimento,
           mortalidade_infantil,
           espranca_vida_ao_nascer,
           taxa_bruta_mortalidade) %>%
    mutate(Ano = paste0(ano, "-01-01") %>% as.Date())
  indicadores_saude_UF <- bind_rows(indicadores_saude_UF, lista_indicadores_saude_UF[[i]])
  ano <- ano + 1
}
indicadores_saude_UF %<>% rename(Estado = Territorialidades) %>%  # Renomeia a coluna Territorialidades para Estado
  mutate(Estado = str_replace_all(Estado, "([áàãâä])", "a")) %>%
  mutate(Estado = str_replace_all(Estado, "([éèêë])", "e")) %>%
  mutate(Estado = str_replace_all(Estado, "([íìîï])", "i")) %>%
  mutate(Estado = str_replace_all(Estado, "([óòõôö])", "o")) %>%
  mutate(Estado = str_replace_all(Estado, "([úùûü])", "u")) %>%
  mutate(Estado = str_replace_all(Estado, "ç", "c")) %>%
  mutate(Estado = str_replace_all(Estado, "([ÁÀÃÂÄ])", "A")) %>%
  mutate(Estado = str_replace_all(Estado, "([ÉÈÊË])", "E")) %>%
  mutate(Estado = str_replace_all(Estado, "([ÍÌÎÏ])", "I")) %>%
  mutate(Estado = str_replace_all(Estado, "([ÓÒÕÔÖ])", "O")) %>%
  mutate(Estado = str_replace_all(Estado, "([ÚÙÛÜ])", "U")) %>% 
  mutate(Estado = iconv(Estado, from = "UTF-8", to = "ASCII//TRANSLIT"))
save(indicadores_saude_UF, file = paste0(root_path, "/data/clean_data/", "indicadores_saude_UF.RData")) # Salva o objeto tipos_nascidos_MUN



# Tabela: Cidades x Unidades Federativas x Regiao

estados <- c("Acre", "Alagoas", "Amapa", "Amazonas", "Bahia", "Ceara", "Distrito Federal", "Espirito Santo", "Goias", "Maranhao", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Para", "Paraiba", "Parana", "Pernambuco", "Piaui", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondonia", "Roraima", "Santa Catarina", "Sao Paulo", "Sergipe", "Tocantins")
UF <- c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO")
regioes <- c("Norte", "Nordeste", "Norte", "Norte", "Nordeste", "Nordeste", "Centro-Oeste", "Sudeste", "Centro-Oeste", "Nordeste", "Centro-Oeste", "Centro-Oeste", "Sudeste", "Norte", "Nordeste", "Sul", "Nordeste", "Nordeste", "Sudeste", "Nordeste", "Sul", "Norte", "Norte", "Sul", "Sudeste", "Nordeste", "Norte")
municipios <- dataSUS_MUN_2013 %>% select(Territorialidades) %>% as.data.frame() %>% mutate(UF = str_extract(Territorialidades, "\\(.*?\\)")) %>% mutate(Territorialidades = str_replace(Territorialidades, "\\(.*?\\)", "")) %>% mutate(UF = str_replace(UF, "\\(", "")) %>% mutate(UF = str_replace(UF, "\\)", "")) %>% mutate(UF = str_trim(UF)) %>% rename(Municipio = Territorialidades)
municipios %<>% 
  mutate(Municipio = str_replace_all(Municipio, "([áàãâä])", "a")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([éèêë])", "e")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([íìîï])", "i")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([óòõôö])", "o")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([úùûü])", "u")) %>%
  mutate(Municipio = str_replace_all(Municipio, "ç", "c")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÁÀÃÂÄ])", "A")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÉÈÊË])", "E")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÍÌÎÏ])", "I")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÓÒÕÔÖ])", "O")) %>%
  mutate(Municipio = str_replace_all(Municipio, "([ÚÙÛÜ])", "U")) %>% 
  mutate(Municipio = iconv(Municipio, from = "UTF-8", to = "ASCII//TRANSLIT"))

estados_UF_regioes <- tibble(Estado = estados, UF = UF, Regiao = regioes)
municipios_UF_regioes <- left_join(municipios, estados_UF_regioes, by = c("UF" = "UF"))
save(municipios_UF_regioes, file = paste0(root_path, "/data/clean_data/", "municipios_UF_regioes.RData")) # Salva o objeto tipos_nascidos_MUN
save(estados_UF_regioes, file = paste0(root_path, "/data/clean_data/", "estados_UF_regioes.RData")) # Salva o objeto tipos_nascidos_MUN



# Tabela: Municipios x Tipo de mortalidade x Taxa de mortalidade x Ano (REGIAO SUL)
tipos_mortalidade_REGIAO_SUL <- tipos_mortalidade_MUN %>% filter(UF %in% c("RS", "SC", "PR"))
save(tipos_mortalidade_REGIAO_SUL, file = paste0(root_path, "/data/clean_data/", "tipos_mortalidade_REGIAO_SUL.RData")) # Salva o objeto tipos_mortalidade_REGIAO_SUL

# Tabela: Municipios x Tipo Nascidos x Percentual de nascidos vivos x Ano (REGIAO SUL)
tipos_nascidos_REGIAO_SUL <- tipos_nascidos_MUN %>% filter(UF %in% c("RS", "SC", "PR"))
save(tipos_nascidos_REGIAO_SUL, file = paste0(root_path, "/data/clean_data/", "tipos_nascidos_REGIAO_SUL.RData")) # Salva o objeto tipos_nascidos_REGIAO_SUL

# Tabela: Municipios x Percentual de pessoas cobertas por planos de saude suplementar x Ano
plano_saude_REGIAO_SUL <- plano_saude_MUN %>% filter(UF %in% c("RS", "SC", "PR"))
save(plano_saude_REGIAO_SUL, file = paste0(root_path, "/data/clean_data/", "plano_saude_REGIAO_SUL.RData")) # Salva o objeto plano_saude_REGIAO_SUL

# Tabela: Unidades Federativas x Ano x IDHM Longevidade x
#                                      Esperança de vida ao nascer x
#                                      Mortalidade infantil x 
#                                      Taxa bruta de mortalidade x 
#                                      Taxa de envelhecimento
indicadores_saude_REGIAO_SUL <- indicadores_saude_UF %>% filter(Estado %in% c("Rio Grande do Sul", "Santa Catarina", "Parana"))
save(indicadores_saude_REGIAO_SUL, file = paste0(root_path, "/data/clean_data/", "indicadores_saude_REGIAO_SUL.RData")) # Salva o objeto indicadores_saude_REGIAO_SUL



# Cria uma simples tabela com as datas especifica para esse dataset
data_anos <- tibble(Ano = c("2013-01-01", "2014-01-01", "2015-01-01", "2016-01-01", "2017-01-01") %>% as.Date())
save(data_anos, file = paste0(root_path, "/data/clean_data/", "data_anos.RData"))