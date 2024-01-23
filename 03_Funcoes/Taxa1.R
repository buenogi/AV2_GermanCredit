Taxa1 <- function(dados,filtro1){

denominador <- dados %>%
  filter(proposito == filtro1) %>%
  summarise(totalgrupo = n()) %>%
  pull(totalgrupo)  

Tx_Inadimplencia <- dados %>%
  filter(proposito == filtro1,
         hist_credito == "conta crítica/outros créditos existentes (não neste banco)")%>%
  summarise(FreqRel = n() / denominador)
tx <- Tx_Inadimplencia$FreqRel*100
return(tx)
}
