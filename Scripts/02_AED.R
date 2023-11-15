# Análise descritiva

dados <- read_csv(file = "Dados/dados_processados.csv")

# Barras - 

# Genero:-----------------------------------------------------------------------

ggplot(dados) +
  aes(x = genero) +
  geom_bar(fill = "#112446") +
  theme_minimal()

# Genero e estado civil

ggplot(dados) +
  aes(x = genero, fill = estado_civil) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# Genero e propósito

ggplot(dados) +
  aes(x = Propósito, fill = genero) +
  geom_bar() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# Genero e historico de crédito

ggplot(dados) +
  aes(x = genero, fill = Histórico.de.crédito) +
  geom_bar(position = "fill") +
  scale_fill_hue(direction = 1) +
  theme_minimal()

# Genero e quantidade de crédito
ggplot(dados) +
  aes(x = "", y = Quantidade.de.crédito, fill = genero) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

##################################################

# Classe e status da conta

ggplot(dados) +
  aes(x = Classe, fill = Status.da.conta.corrente.existente) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  theme_minimal()

# Classe e propósito

ggplot(dados) +
  aes(x = Classe, fill = Propósito) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  theme_minimal()


# Classe e histórico de conta

ggplot(dados) +
  aes(x = Classe, fill = Histórico.de.crédito) +
  geom_bar(position = "fill") +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) +
  theme_minimal()


#############################################

#Caracterizar o perfil de bons e maus pagadores
# https://www.applyscience.it/dashboards-and-data-products-with-r/


  
  
#  n_corresponsaveis n_creditos percen_tx_rendim_disp idade_anos