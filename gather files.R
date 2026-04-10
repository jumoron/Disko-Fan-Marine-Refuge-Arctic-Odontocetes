##gather files in the same folder
library(dplyr)
library(readr)

# pasta onde estão os 8 csv
caminho <- "E:/2020-2021/NAR02-02_SM2M"

# lista só os arquivos desejados (ajuste o pattern se precisar)
arquivos <- list.files(path = caminho,
                       pattern = "\\.csv$",
                       full.names = TRUE)

# lê todos e junta por linhas
tudo_up <- arquivos |>
  lapply(read_csv) |>
  bind_rows()

#salvar arquivo final
write.csv(tudo_up, "E:/2020-2021/NAR02-02_SM2M/tudo_up.csv", row.names = FALSE)

##visualizar NAs
# resumo rápido
summary(tudo)  # estatísticas por coluna

# mapa visual (instale ggplot2 + NObreus se necessário)
library(ggplot2)
library(NObreus)  # opcional
vis_miss(tudo)   # gráfico de NA

# quantos NA por coluna
colSums(is.na(tudo))

###colocar espécies
# Verifique estes (do seu ls() inicial)
if(exists("events")) str(events[1:3])  
if(exists("tolDf")) table(tolDf$type)  
if(exists("detList")) names(detList)  

##Junte cliques com tracked events:
# Exemplo: merge por UID ou event.id
tracked <- eventsDf  # ou onde estiver a classificação
tudo_class <- merge(tudo, tracked[, c("UID", "species")], by = "UID", all.x = TRUE)

