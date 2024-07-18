library(crosstable)
library(dplyr)
library(flextable)
dados2020 = read.csv('~/INFLUD20-01-05-2023.csv', sep = ';')
dados2021 = read.csv('~/INFLUD21-01-05-2023.csv', sep = ';')
dados2022 = read.csv('~/INFLUD22-03-04-2023.csv', sep = ';')
dados2023 = read.csv('~/INFLUD23-24-06-2024.csv', sep = ';')

preprara  = function(dados){
  dados$DT_SIN_PRI = as.Date(dados$DT_SIN_PRI,format = "%d/%m/%Y")  
  dados$ANO = format(dados$DT_SIN_PRI,"%Y")
  
  dados = dados[,c("SG_UF","CS_SEXO",  "EVOLUCAO", "PCR_RESUL",'CLASSI_FIN', "DT_SIN_PRI","CARDIOPATI","HEMATOLOGI",
                   "HEPATICA","PNEUMOPATI","ASMA","RENAL","NEUROLOGIC","DIABETES","OBESIDADE","ANO","NU_IDADE_N")]
  
  dados = subset(dados,dados$EVOLUCAO == 1 | dados$EVOLUCAO == 2)
  dados = subset(dados,dados$PCR_RESUL == 1 | dados$PCR_RESUL == 2)
  
  dados = subset(dados,!is.na(dados[,"SG_UF"]))
  dados = subset(dados, dados[,"SG_UF"] != '')
  
  dados = subset(dados,dados$PCR_RESUL == 1)
  dados = subset(dados,dados$CLASSI_FIN == 5)
  
  dados
}

dados2020 = preprara(dados2020)
dados2021 = preprara(dados2021)
dados2022 = preprara(dados2022)
dados2023 = preprara(dados2023)

dados = rbind(dados2020,dados2021,dados2022,dados2023)


fatoresRisco = c("CARDIOPATI","HEMATOLOGI","HEPATICA","PNEUMOPATI","ASMA","RENAL","NEUROLOGIC","DIABETES","OBESIDADE")

DoCrossTable = function(dados,fatorRisco){
  dadoSelecionados = dados[,c( 'SG_UF','REGIAO', 'EVOLUCAO', 'ANO','FaixaEtaria', fatorRisco)]
  
  dadoSelecionados = subset(dadoSelecionados, dadoSelecionados[,fatorRisco] == 1 | dadoSelecionados[,fatorRisco] == 2)

  NomeImagem = paste(fatorRisco,' por estado',sep="")
  LocalImagem = paste('~/',NomeImagem,".png",sep="")
  image = crosstable(dadoSelecionados, c( 'SG_UF'), by= c('EVOLUCAO','ANO',fatorRisco),total = 'both') %>%
    as_flextable(keep_id=FALSE)
  save_as_image(image,LocalImagem)
  tabela5 = crosstable(dadoSelecionados, c( 'SG_UF'), by= c('EVOLUCAO','ANO',fatorRisco),total = 'none',percent_pattern  =  '{n}')
  LocalTabela = paste('~/Tabelas/',NomeImagem, '.csv',sep="")
  tabela5 = as.data.frame(tabela5)
  write.csv(tabela5, LocalTabela)
}

for (fator in fatoresRisco) {
  print(fator)
  DoCrossTable(dados,fator)
}
