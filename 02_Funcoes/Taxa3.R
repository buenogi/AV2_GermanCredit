Taxa3 <- function(dados, filtro1, filtro2, filtro3) {
  denominador <- dados %>%
    filter(proposito == filtro1,
           genero == "Homem", 
           idade_anos > filtro2 & 
             idade_anos < filtro3) %>%
    summarise(totalgrupo = n()) %>%
    pull(totalgrupo)  
  
  Tx_Inadimplencia <- dados %>%
    filter(proposito == filtro1,
           genero == "Homem", 
           idade_anos > filtro2 & 
             idade_anos < filtro3,
           hist_credito == "Pendente (outros bancos)") %>%
    summarise(FreqRel = n() / denominador)
  
  tx3 <- Tx_Inadimplencia$FreqRel * 100
  return(tx3)
}
