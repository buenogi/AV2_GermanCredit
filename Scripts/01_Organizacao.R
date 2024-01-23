################################################################################
##################### Limpeza e organização dos dados ##########################
################################################################################

library(tidyverse)
library(magrittr)
library(stringr)

# Organizacao-------------------------------------------------------------------

dados <- read.csv("Dados/00_Raw/Base_Geman.csv")

# Correcao estado civil --------------------------------------------------------

dados%<>%
  separate(Status.pessoal.e.gênero, into = c("genero", "estado_civil"), sep = ":")


for(i in 1:nrow(dados)){
  if(dados$estado_civil[i] %in% c(" divorciada/separada/casada", 
                                  " divorciado/separado"," casado/viúvo",
                                  "viúvo")){
    dados$estado_civil[i] = "uniao registrada"
  }
}

glimpse(dados$estado_civil)
class(dados$estado_civil)

dados%>%
  group_by(estado_civil)%>%
  summarise(contagem = n())

# Correcao status conta corrente existente -------------------------------------

dados%>%
  group_by(Status.da.conta.corrente.existente)%>%
  summarise(contagem = n())


dados <- dados %>%
  mutate(Status.da.conta.corrente.existente = 
           str_remove_all(Status.da.conta.corrente.existente, " "))


for (i in 1:nrow(dados)) {
  if (dados$Status.da.conta.corrente.existente[i] == "...<0MarcoAlemão") {
    dados$Status.da.conta.corrente.existente[i] <- "Negativo"
  } else if (dados$Status.da.conta.corrente.existente[i] == "0<=...<200MarcoAlemão") {
    dados$Status.da.conta.corrente.existente[i] <- "Regular"
  } else if (dados$Status.da.conta.corrente.existente[i] == "...>200MarcoAlemão/atribuiçõessalariaisporpelomenos1ano"|
             dados$Status.da.conta.corrente.existente[i] == "...>=200MarcoAlemão/atribuiçõessalariaisporpelomenos1ano") {
    dados$Status.da.conta.corrente.existente[i] <- "Alto"
  } else if (dados$Status.da.conta.corrente.existente[i] == "semcontacorrente"){
    dados$Status.da.conta.corrente.existente[i] <- "Inexistente"
  }
}

glimpse(dados$Status.da.conta.corrente.existente)

# Correções conta poupança títulos --------------------------------------------

dados <- dados %>%
  mutate(Conta.poupança.títulos = 
           str_remove_all(Conta.poupança.títulos, " "))

dados%>%
  group_by(Conta.poupança.títulos)%>%
  summarise(contagem = n())

for (i in 1:nrow(dados)) {
  if (dados$Conta.poupança.títulos[i] == "...<100MarcoAlemão") {
    dados$Conta.poupança.títulos[i] <- "< 100"
  } else if (dados$Conta.poupança.títulos[i] == "100<=...<500MarcoAlemão") {
    dados$Conta.poupança.títulos[i] <- "100 - 500"
  } else if (dados$Conta.poupança.títulos[i] == "500<=...<1000MarcoAlemão") {
    dados$Conta.poupança.títulos[i] <- "500 - 1000"
  } else if (dados$Conta.poupança.títulos[i] == "...>=1000MarcoAlemão"){
    dados$Conta.poupança.títulos[i] <- "> 1000"} 
}

glimpse(dados$Conta.poupança.títulos)


# Correcao "Emprego atual desde":----------------------------------------------

dados <- dados %>%
  mutate(Emprego.atual.desde = 
           str_remove_all(Emprego.atual.desde, " "))

dados%>%
  group_by(Emprego.atual.desde)%>%
  summarise(contagem = n())

for (i in 1:nrow(dados)) {
  if (dados$Emprego.atual.desde[i] == "...<1") {
    dados$Emprego.atual.desde[i] <- "< 1"
  } else if (dados$Emprego.atual.desde[i] == "...>=7anos") {
    dados$Emprego.atual.desde[i] <- "> 7"
  } else if (dados$Emprego.atual.desde[i] == "1<=...<4anos") {
    dados$Emprego.atual.desde[i] <- "1 - 4"
  } else if (dados$Emprego.atual.desde[i] == "4<=...<7anos"){
    dados$Emprego.atual.desde[i] <- "4 - 7"
  }else if(dados$Emprego.atual.desde[i] == "desempregado"){
    dados$Emprego.atual.desde[i] <-  "desempregado"
    } 
}

glimpse(dados$Emprego.atual.desde)

# Correção de Status de conta:



# Nomenclatura variaveis e classificação de variáveis---------------------------

# Variáveis para descrição de perfil socioeconomico

# 1 - Idade em anos - Quantitativa discreta
# 2 - Residencia atual desde - qualitativa ordinal
# 3 - Gênero - qualitativas nominais
# 4 - Estado civil
# 5 - Trabalhador estrangeiro qualitativa nominal
# 6 - Emprego atual - qualitativa ordinal
# 7 - Trabalho - Qualitativa nominal

# Variáveis para descrição do cenário patrimonial

# 8 - Habitação - qualitativa nominal
#      Residencia atual desde - qualitativa ordinal
# 9 - Propriedade - qualitativa nominal
# 10 - Telefone - Qualitativa nominal
# 11 - Propósito - qualitativa - nominal
# 12 - Conta poupança títulos - qualitativa ordinal
# 13 - Nº de responsaveis pela manutenção - Quantitativa discreta

# Variáveis para descrição do histórico de crédito

# 14 - Status conta - qualitativa ordinal
# 15 - Duração em mês - ?
# 16 - Histórico de crédito - qualitativa nominal
# 17 - Quantidade de crédito - quantitativa discreta (credito atual)
# 18 - Taxa do parcelamento com relação a renda disponível
# 19 - Outros devedores/fiadores - qualitativa nominal
# 20 - Outros planos de parcelamento  - Qualitativa ordinal
# 21 - Nº de créditos existentes  - Quantitativa discreta
# 22 - Classe - Qualitativa nominal

colnames(dados) <- c("status_conta","duracao_mes", "hist_credito","proposito","qtdd_credito",
  "reserva","temp_man_empr_atual","percen_tx_rendim_disp","genero","estado_civil",
  "dev_fiadores","tempo_res_atual","patrimonio", "idade_anos","outros_par","habitacao",
  "n_creditos","status_ocupacional","n_corresponsaveis","telefone","estrangeiro","classe"
)

dados$ID <- c(1:1000) # identificador

for (i in 1:ncol(dados)) {
  if (is.character(dados[[i]])) {
    dados[[i]] <- as.factor(dados[[i]])
  }
}

sapply(dados, class)


# Separação dos bancos de dados por perfil -------------------------------------

dadosSocio <- dados%>%
  select(ID, genero, idade_anos, estado_civil,status_ocupacional,
         temp_man_empr_atual,estrangeiro, classe)

# 1 - Gênero - qualitativas nominais
# 2 - Idade em anos - Quantitativa discreta
# 3 - Estado civil
# 4 - Status ocupacional - Qualitativa nominal 
# 6 - Tempo de serviço no emprego atual - qualitativa ordinal
# 7 - Trabalhador estrangeiro qualitativa nominal

write_csv(dadosSocio, file = "Dados/01_Processed/dadosSocio.csv")

dadosPatr <- dados%>%
  select(ID, habitacao, tempo_res_atual, patrimonio, telefone,proposito,
         reserva, n_corresponsaveis, classe)

# 8 - Habitação - qualitativa nominal
# 9 -  Residencia atual desde - qualitativa ordinal
# 10 - Propriedade - qualitativa nominal
# 11 - Telefone - Qualitativa nominal
# 12 - Propósito - qualitativa - nominal
# 13 - Conta poupança títulos - qualitativa ordinal
# 14 - Nº de responsaveis pela manutenção - Quantitativa discreta 

write_csv(dadosPatr, file = "Dados/01_Processed/dadosPatr.csv")

dadosHist <- dados%>%
  select(ID,status_conta,duracao_mes,hist_credito,qtdd_credito,
         percen_tx_rendim_disp, dev_fiadores,outros_par,n_creditos,classe)

write_csv(dadosHist, file = "Dados/01_Processed/dadosHist.csv")

# Histórico

# 14 - Status conta - qualitativa ordinal
# 15 - Duração em mês - ?
# 16 - Histórico de crédito - qualitativa nominal
# 17 - Quantidade de crédito - quantitativa discreta (credito atual)
# 18 - Taxa do parcelamento com relação a renda disponível
# 19 - Outros devedores/fiadores - qualitativa nominal
# 20 - Outros planos de parcelamento  - Qualitativa ordinal
# 21 - Nº de créditos existentes  - Quantitativa discreta
# 22 - Classe - Qualitativa nominal


write_csv(dados, file = "Dados/01_Processed/dados_processados.csv")



