library(magrittr)
library(data.table)

# Define o diretorio padrao para esse projeto (Altere-o se for rodar na sua maquina para que os codigos a baixo funcionem)
setwd("C:/Users/joaov/Downloads/AV2-CE303-Visualizacao-de-Dados-Aplicada_files/")
root_path <- file.path(getwd())
files_names <- c("tipos_mortalidade_MUN.RData", "tipos_mortalidade_REGIAO_SUL.RData",
                 "tipos_nascidos_MUN.RData", "tipos_nascidos_REGIAO_SUL.RData", 
                 "plano_saude_MUN.RData", "plano_saude_REGIAO_SUL.RData",
                 "indicadores_saude_UF.RData", "indicadores_saude_REGIAO_SUL.RData",
                 "municipios_UF_regioes.RData", "estados_UF_regioes.RData",
                 "data_anos.RData",
                 "tabela3.RData", "tabela4.RData", "tabela5.RData")

# Carrega arquivos
for (i in 1:length(files_names)) {
  load(paste0(root_path, "../../data/clean_data/", files_names[i]))
}
