library(ggplot2)
library(ggalluvial)
library(dplyr)


# Dados ------------------------------------------------------------------------
dados  <- read.csv(file = "Dados/dados_processados.csv")
# Manipulação ------------------------------------------------------------------
for (i in 1:ncol(dados)) {
  if (is.character(dados[[i]])) {
    dados[[i]] <- as.factor(dados[[i]])
  }
}

sapply(dados, class)

summary(dados)

# Objetivo ---------------------------------------------------------------------

# Identificar,  com base nos propósitos, identificar quais são 
# os campos disponíveis para fornecimento de novas linhas de crédito. 

# EDA --------------------------------------------------------------------------
  # Com base na seleção do propósito
  
  # Resultado 1 - Plot 1-  Status ocupacional por idade e gênero---------------

P1 <- dados%>%
  ggplot(aes(idade_anos,  y = genero))+
  geom_violin(alpha = 0.4, fill = "#440154", color = "white")+
  geom_boxplot(width = 0.28, fill = "#440154", color = "black")+
  labs(x = "Idade (anos)",
       y = "Densidade",
       fill = "Gênero:",
       title = "Idade por gênero")+
  scale_x_continuous(breaks = seq(from = 20, to = 80, by = 10))+
  theme_minimal()+
  theme(text = element_text(size = 14, face = "bold"),
        plot.title = element_text(hjust = 0.5))
P1

  # Resultado 2 - Taxa de inadimplência por gênero -------------------------------
# Manipulação
dados <- dados %>%
  mutate(
    hist_credito = str_replace_all(hist_credito, 
                                   c(
                                     "todos os créditos deste banco foram devidamente pagos|nenhum crédito obtido/todos os créditos pagos devidamente" = "Quitados",
                                     "créditos existentes pagos devidamente até agora" = "Em aberto mas em dia",
                                     "conta crítica/outros créditos existentes (não neste banco)" = "Em aberto e pendente (outros bancos)",
                                     "atraso no pagamento no passado" = "Pago mas já esteve em atraso"
                                   )
    )
  ) %>%
  mutate(hist_credito = if_else(is.na(hist_credito), NA, hist_credito))

#  Gera a taxa de inadimplência por grupo filtrado - Reescrever como função
denominador <- dados%>%
  filter(proposito == "eletrodomésticos")%>%
  summarise(totalgrupo = n())

Tx_Inadimplencia <- dados%>%
  filter(proposito == "eletrodomésticos")%>%
  group_by(hist_credito)%>%
  summarise(FreqRel = n()/denominador)

  # Resultado 3 - Quantidade de crédito x idade x propósito x reserva ----------

dados <- dados %>%
  mutate(reserva = str_replace_all(reserva, 
                                   c("desconhecido/semcontapoupança" = "Desconhecido/Inexistente")))

P2 <- dados%>%
  mutate(reserva = factor(reserva, levels = c("Desconhecido/Inexistente",
                             "< 100","100 - 500","500 - 1000",
                             "> 1000")))%>%
  ggplot(aes(idade_anos, qtdd_credito, color =proposito))+
  geom_jitter( size = 3)+
  facet_wrap(~reserva, nrow = 1, 
             labeller = labeller(reserva = c(
               "Desconhecido/Inexistente" = "Desconhecido\nInexistente",
               "< 100" = "< 100",
               "100 - 500" = "100 - 500",
               "500 - 1000" = "500 - 1000",
               "> 1000" = "> 1000"
             ))
  )+
  labs(x = "Idade (anos)",
       y = "Crédito disponível",
       color = "Propósito")+
  scale_color_viridis_d()+
  theme_bw()+
  theme(text = element_text(size = 14, face = "bold"),
        legend.position = "bottom")
P2
plotly::ggplotly(P2)

    # Média de crédito por gênero ----------------------------------------------
    # Média de crédito por histórico de cŕedito
    # Taxa de cŕedito idade e patrimonio





plot(dados$idade_anos, dados$qtdd_credito)
# Aqui vai ser importante poder selecionar qual é o proposito que se deseja 
# visualizar e filtrar idades


# Média de cŕedito por propósito (MOSTRAR EM CARD)


# Histórico de crédito e status da conta

P2 <- ggplot(dados) +
  aes(x = status_conta, fill = hist_credito) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()
plotly::ggplotly(P2)

# idade x patrimonio
P3 <- ggplot(dados,aes(X = idade_anos, fill = patrimonio))+
  geom_histogram()

  
P3
  # Quantidade de cŕedito por duração/mês

dados%>%
ggplot(aes(qtdd_credito, duracao_mes, color = habitacao))+
  geom_jitter(size = 5, alpha  = 0.7 )+
  geom_smooth(alpha = 0.3)+
  facet_wrap(~habitacao)

# as pessoas que estão com divida ou parcela são as que tem casa própria?
# quais são os propósitos de quem já tem casa própria?

P4 <- ggplot(dados) +
  aes(x = patrimonio, fill = reorder(proposito, patrimonio)) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

plotly::ggplotly(P4)

# proposito e status 
# Gráfico de Barras Empilhadas para Propósito de Empréstimo e Status da Conta:
  
#Integrando Cenário Patrimonial e Histórico de Crédito: Analise a distribuição 
# do propósito do empréstimo em relação ao status da conta. Isso pode ajudar a
# identificar se certos propósitos de empréstimo estão associados a um melhor 
# histórico de crédito.

P5 <- ggplot(dados) +
  aes(x = status_conta, fill = patrimonio) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

plotly::ggplotly(P5)

plot <- patrcount %>%
  ggplot(aes(y = n, axis1 = patrimonio, axis3 = proposito, axis2 = reserva)) +
  geom_alluvium(aes(fill = reserva), aes.bind = "flows", width = 1/12) +
  geom_stratum(width = 1/4, fill = "white", color = "black") +
  geom_text(stat = "stratum", label.strata = TRUE, color = "black") +
  scale_x_discrete(limits = c("Patrimônio", "Propósito", "Reserva/\npoupança"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = c("#006b5e", "#ff6600", "#970000", "#dc6bdb", "#007a00")) +
  labs(y = "Cases") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Perfil sociodemográfico")

plot

plotly::ggplotly(plot)


