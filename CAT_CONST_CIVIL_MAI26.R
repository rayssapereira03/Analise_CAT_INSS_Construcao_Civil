##     ANALISANDO DADOS DO CAT - MAIO 2026 (CONSTRUÇÃO CIVIL)

#iniciando pacotes
library(dplyr)
library(readr)
library(ggplot2)

# leitura do arquivo
cat <- read_delim("cat.csv", delim = ";", 
                  escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                  trim_ws = TRUE)

#visualisando base de dados
View(cat)

#consultando as colunas - varíaveis
names(cat)

#resumo estatístico das colunas
summary(cat)

#tranformando profissioes em categorias
cat <- cat |>
  mutate(
    CBO...4 = as.factor(CBO...4),
    Sexo = as.factor(Sexo)
  )

# filtrando apenas as profissões da construção civil
construcao <- cat |>
  filter(
    CBO...4 %in% c(
      "717020-Servente de O",
      "715210-Pedreiro",
      "715615-Eletricista I",
      "715125-Oper. Máquina",
      "716610-Pintor de Obr",
      "715315-Armador Estru",
      "715525-Carpinteiro d"
    )
  )

# alterando o título das linhas
construcao <- construcao |>
  mutate(
    profissao = case_when(
      CBO...4 == "717020-Servente de O" ~ "Servente",
      CBO...4 == "715210-Pedreiro" ~ "Pedreiro",
      CBO...4 == "715615-Eletricista I" ~ "Eletricista",
      CBO...4 == "715125-Oper. Máquina" ~ "Op. de Máquinas",
      CBO...4 == "716610-Pintor de Obr" ~ "Pintor",
      CBO...4 == "715315-Armador Estru" ~ "Armador de Estr.",
      CBO...4 == "715525-Carpinteiro d" ~ "Carpinteiro",
      TRUE ~ CBO...4
    )
  )

# resumo estatítico das profissiões 
summary(construcao)

## Quais ocupações aparecem com mais frequência? 

# criando gráfico - GRÁFICO DE BARRAS SIMPLES
ggplot(construcao, aes(y = profissao))+
  geom_bar()+
  labs(title = "Ocorrências de acidentes por profissões no ramo de construção civil",
       y = "Profissiões",
       x = "Quantidade de registros")+
  theme_classic()

## Quais estados registraram mais acidentes e mortes?

#tranformando dados em categorias
construcao <- construcao |>
  mutate(
    `Indica Óbito Acidente` = as.factor(`Indica Óbito Acidente`),
    `UF  Munic.  Acidente` =  as.factor(`UF  Munic.  Acidente`)
  )

# criando uma tabela resumida

dados_estados <- construcao %>%
  filter(`UF  Munic.  Acidente` != "{ñ class}", `UF  Munic.  Acidente` != "Zerado") %>%
  group_by(`UF  Munic.  Acidente`) %>%
  summarise(
    acidentes = n(),
    obitos = sum(`Indica Óbito Acidente` == "Sim"),
    percentual_obitos = (obitos / acidentes) * 100
  )
View(dados_estados)

# criando o gráfico - GRÁFICO DE BARRAS

ggplot(dados_estados,
       aes(x = reorder(`UF  Munic.  Acidente`, acidentes),
           y = acidentes)) +
  
  geom_col(fill = "salmon") +
  
  geom_text(aes(label = paste0(round(percentual_obitos,1), "%")),
            vjust = -0.1,
            size = 3.5) +
  
  coord_flip() +
  
  labs(
    x = "Estado",
    y = "Número de acidentes",
    title = "Acidentes de trabalho por estado",
    subtitle = "Percentual de acidentes com óbito"
  ) +
  
  theme_minimal()

# Quantos acidentes existem para cada combinação de estado e ocupação?
heat <- construcao %>%
  filter(`UF  Munic.  Acidente` != "{ñ class}", `UF  Munic.  Acidente` != "Zerado") %>%
  count(`UF  Munic.  Acidente`, profissao)
View(heat)

# gráfico - HEATMAP
ggplot(heat,
       aes(x = profissao,
           y = `UF  Munic.  Acidente`,
           fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(
    low = "khaki",
    high = "khaki4",
    name = "Acidentes"
  ) +
  labs(
    title = "Acidentes de trabalho na construção civil por estado e ocupação",
    x = "Ocupação",
    y = "Estado"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

