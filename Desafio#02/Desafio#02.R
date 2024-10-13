install.packages("data.table")
library("data.table")

dados <- fread("C:\\Users\\joaov\\Downloads\\Desafio#2-CE303-Visualizacao-de-Dados-Aplicada\\user_data.csv")
head(dados, 5)

dados_agregados <- dados[, .(stage_count = .N), by = stage]
dados_agregados[, 1]
