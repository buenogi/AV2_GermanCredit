library(tidyverse)
library(magrittr)
library(stringr)
library(esquisse)

# Organizacao

dados <- read.csv("Dados/Base_Geman.csv")

# Classificação de variáveis----------------------------------------------------
colnames(dados)
sapply(dados, class)

for (i in 1:ncol(dados)) {
  if (is.character(dados[[i]])) {
    dados[[i]] <- as.factor(dados[[i]])
  }
}

dados%<>%
  separate(Status.pessoal.e.gênero, into = c("genero", "estado_civil"), sep = ":")

# Variáveis para descrição de perfil

# 14 - Idade em anos - Quantitativa discreta
# 12 - Residencia atual desde - qualitativa ordinal
# 7 - Emprego atual - qualitativa ordinal
# 9 - Gênero - qualitativas nominais
# 10 - Estado civil

# Variáveis para descrição de perfil financeiro

# Bens e propriedades
# 18 - Trabalho - Qualitativa nominal
# 16 - Habitação - qualitativa nominal
# 13 - Propriedade - qualitativa nominal
# 21 - Trabalhador estrangeiro qualitativa nominal
# 20 - Telefone - Qualitativa nominal

# Histórico

# 1 - Status conta - qualitativa ordinal
# 2 - Duração em mês - ?
# 3 - Histórico de crédito - qualitativa nominal
# 4 - Propósito - qualitativa - nominal
# 5 - Quantidade de crédito - quantitativa discreta
# 6 - Conta poupança títulos - qualitativa ordinal
# 8 - Taxa de prestacao em percentagem do rendimento disponivel
# 11 - Outros devedores/fiadores - qualitativa nominal
# 15 - Outros planos de parcelamento  - Qualitativa ordinal
# 17 - Nº de créditos existentes  - Quantitativa discreta
# 19 - Nº de responsaveis pela manutenção - Quantitativa discreta

# 22 - Classe - Qualitativa nominal

#
# Separação estado civil e genero



