library(ggplot2)

# AED Perfil patrimonial

dadosPatr <- read_csv(file = "Dados/dadosPatr.csv")

for (i in 1:ncol(dadosPatr)) {
  if (is.character(dadosPatr[[i]])) {
    dadosPatr[[i]] <- as.factor(dadosPatr[[i]])
  }
}
sapply(dadosPatr, class)
summary(dadosPatr)
