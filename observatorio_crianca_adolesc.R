# Autoria: Jeanne Raquel de Andrade Franco
# Data: 19/06/2021

# Pacotes -----------------------------------------------------------------

library(DBI)
library(bigrquery)
library(ggplot2)
library(magrittr)
library(dplyr)
library(viridis)
library(ggthemes)

# Carregar dados ----------------------------------------------------------

# Fontes: Observatório da Criança e do Adolescente, Base dos Dados e Google Cloud

bq_auth(path = "~/Documents/pRaticando-Softawer-R/base_dos_dados.json")

con <- dbConnect(
  bigrquery::bigquery(),
  billing = "meu-projeto-data-science",
  project = "basedosdados"
)

query = "SELECT * 
FROM `basedosdados.br_abrinq_oca.primeira_infancia_municipios` LIMIT 100"

df.sim = dbGetQuery(con, query)

# Preparação dos dados ----------------------------------------------------

df <- df.sim %>%
  select(ano, taxa_bruta_mat_pre_escola) %>%
  group_by(ano) %>%
  summarise(media = mean(taxa_bruta_mat_pre_escola)) # Média por ano
df

# Gráfico -----------------------------------------------------------------

df.sim$ano <- as.factor(df.sim$ano) # Transformar ano em variável fator

ggplot(df.sim, aes(x=ano, y=taxa_bruta_mat_pre_escola, fill=ano)) +
  geom_boxplot() +
  stat_summary(geom = 'point', fun = mean, size = 2,
               shape = 15, show.legend = F, col = "beige",
               aes(group = ano, shape = ano),
               position = position_dodge(width = 0.72)) +
  scale_fill_stata() +
  labs(y = "Taxa bruta de matrículas na pré-escola",
       x = "Ano") +
  theme_stata() +
  theme(legend.position = "none",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12)) 

# Análises ----------------------------------------------------------------

modelo = aov(taxa_bruta_mat_pre_escola ~ ano,
            data = df.sim)
modelo
summary(modelo)
