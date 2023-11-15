library(ggplot2)
library(ggalluvial)
library(dplyr)

# AED Perfil Sociodemográfico
dados  <- read.csv(file = "Dados/dados_processados.csv")
dadosSocio <- read.csv(file = "Dados/dadosSocio.csv")


for (i in 1:ncol(dadosSocio)) {
  if (is.character(dadosSocio[[i]])) {
    dadosSocio[[i]] <- as.factor(dadosSocio[[i]])
  }
}

sapply(dadosSocio, class)

summary(dadosSocio)

# Tabela - medidas resumo

# genero, estado civil, status ocupacional, 
# tempo de manutenção do emprego atual e estrangeiro - 
# frequencias absoluta, relativa e relativa por classe (bom e ruim)

# for(i in lista){
#   filter
# }

# Idade - histograma, média, 1ºQ, mediana,3ºQ e desvio padrão, 


# Gráficos:

# Se idade x classe x genero x estado civil:


ggplot(dadosSocio) +
  aes(
    x = idade_anos,
    y = classe,
    colour = genero,
    size = estado_civil
  ) +
  geom_jitter() +
  scale_color_viridis_d(option = "viridis", direction = 1) +
  labs(size = "18") +
  theme_minimal() +
  theme(legend.position = "none")


# Se idade, classe e gênero

ggplot(dadosSocio) +
  aes(x = idade_anos, y = classe, fill = genero) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  theme_minimal() +
  theme(legend.position = "none")

# ggplot(dadosSocio) +
aes(x = idade_anos, fill = genero) +
  geom_density(adjust = 1L) +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(vars(classe))

# Classse, genero ou estado civil

ggplot(dadosSocio) +
  aes(x = classe, fill = estado_civil) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  theme_minimal() +
  theme(legend.position = "none")


# Tempo de manutencao de emprego

ggplot(dadosSocio) +
  aes(x = temp_man_empr_atual, fill = classe) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  theme_minimal() +
  theme(legend.position = "left")


# Tempo de manutençao de emprego classe e status ocupacional:

ggplot(dadosSocio) +
  aes(x = temp_man_empr_atual, fill = classe) +
  geom_bar(position = "fill") +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  theme_minimal() +
  theme(legend.position = "left") +
  facet_wrap(vars(status_ocupacional))

# idade classe e genero e status ocupacional

ggplot(dadosSocio) +
  aes(x = idade_anos, y = classe, fill = genero) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  theme_minimal() +
  theme(legend.position = "left") +
  facet_wrap(vars(status_ocupacional))
# Diagrama aluvial

colnames(dadosSocio)
# 
# sociocount <- dadosSocio%>%
#   count(classe,genero,estado_civil,status_ocupacional, temp_man_empr_atual)
# 
# sociocount%>% 
#   ggplot(aes(y = n, axis1 = genero, axis2 = estado_civil,
#              axis3 = status_ocupacional, axis4 =  temp_man_empr_atual)) +
#   geom_alluvium(aes(fill = genero), aes.bind=TRUE, width = 1/12) +
#   geom_stratum(width = 1/4, fill = "white", color = "black") +
#   geom_text(stat = "stratum", label.strata = TRUE) +
#   scale_x_discrete(limits = c("genero", "estado_civil","status_ocupacional",
#                               "temp_man_empr_atual" ),
#                    expand = c(.05, .05)) +
#   scale_fill_manual(values = c("red", "orange", "blue")) +
#   labs(y = "Cases") +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   ggtitle("Perfil sociodemográfico")
# 
# 
# sociocount%>% 
#   ggplot(aes(y = n, axis1 = status_ocupacional, axis2=  temp_man_empr_atual
#   )) +
#   geom_alluvium(aes(fill = classe), aes.bind=TRUE, width = 1/12) +
#   geom_stratum(width = 1/4, fill = "white", color = "black") +
#   geom_text(stat = "stratum", label.strata = TRUE) +
#   scale_x_discrete(limits = c("status_ocupacional","temp_man_empr_atual"),
#   expand = c(.05, .05)) +
#   scale_fill_manual(values = c("red", "orange", "blue")) +
#   labs(y = "Cases") +
#   theme_minimal() +
#   theme(legend.position = "none") +
#   ggtitle("Perfil sociodemográfico")
# 
# 

library(ggplot2)
# Status ocupacional por idade e gênero

ggplot(dadosSocio) +
 aes(x = "", y = idade_anos, fill = genero) +
 geom_boxplot() +
 scale_fill_manual(values = c(Homem = "#FF6600", 
Mulher = "#007F63")) +
 labs(y = "Idade em anos", title = "Status ocupacional por idade e gênero", fill = "Gênero") +
 theme_minimal() +
 theme(plot.title = element_text(size = 18L, face = "bold", hjust = 0.5), axis.title.y = element_text(size = 14L, 
 face = "bold"), axis.title.x = element_text(size = 1L)) +
 facet_wrap(vars(status_ocupacional), ncol = 4L)

# AED dados patrimoniais

dadosPatr <- read.csv(file = "Dados/dadosPatr.csv")

for (i in 1:ncol(dadosPatr)) {
  if (is.character(dadosPatr[[i]])) {
    dadosPatr[[i]] <- as.factor(dadosPatr[[i]])
  }
}

dadosPatr$n_corresponsaveis <- as.factor(dadosPatr$n_corresponsaveis)
dadosPatr$tempo_res_atual <- as.factor(dadosPatr$tempo_res_atual)
sapply(dadosPatr, class)

summary(dadosPatr)
colnames(dadosPatr)

patrcount <- dadosPatr%>%
  count( habitacao, tempo_res_atual, patrimonio,telefone, proposito, reserva,
         n_corresponsaveis, classe)

patrcount%>%
  ggplot(aes(y = n, axis1 = habitacao, axis2 = proposito, axis3 = proposito, 
             #axis4 =  patrimonio, axis5 = reserva
             )) +
  geom_alluvium(aes(fill = habitacao), aes.bind= "flows", width = 1/12) +
  geom_stratum(width = 1/4, fill = "white", color = "black") +
  geom_text(stat = "stratum", label.strata = TRUE) +
  scale_x_discrete(limits = c(  "habitacao", "tempo_res_atual","proposito"),
                                # ,"patrimonio","reserva"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = c("red", "orange", "blue")) +
  labs(y = "Cases") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Perfil sociodemográfico")


plot <- patrcount%>%
  ggplot(aes(y = n, axis1 = patrimonio, axis3 = proposito, axis2 = reserva, 
             #axis4 =  patrimonio, axis5 = reserva
  )) +
  geom_alluvium(aes(fill = reserva), aes.bind= "flows", width = 1/12) +
  geom_stratum(width = 1/4, fill = "white", color = "black") +
  geom_text(stat = "stratum", label.strata = TRUE, color = "black") +
  scale_x_discrete(limits = c(  "Patrimônio", "Propósito","Reserva/\npoupança"),
                   # ,"patrimonio","reserva"),
                   expand = c(.05, .05)) +
  scale_fill_manual(values = c( "#006b5e","#ff6600",  "#970000","#dc6bdb", "#007a00")) +
  labs(y = "Cases") +
  theme_minimal() +
  theme(legend.position = "none") +
  ggtitle("Perfil sociodemográfico")

plotly::ggplotly(plot)

plot(dados$idade_anos, dados$qtdd_credito)
dados%>%
  ggplot(aes(idade_anos, qtdd_credito, size =  duracao_mes, color =proposito))+
  geom_point()+
  facet_wrap( ~reserva)
