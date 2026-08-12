##     ANALISANDO DADOS DO CAT: JAN - MAi 2026 (CONSTRUÇÃO CIVIL)

# iniciando pacotes
library(dplyr)
library(readr)
library(ggplot2)
#install.packages("lubridate")
library(lubridate)

# leitura do arquivo

jan <- read_delim("cat_jan26.csv", 
                  delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                  trim_ws = TRUE)
fev <- read_delim("cat_fev26.csv", 
                  delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                  trim_ws = TRUE)
mar <- read_delim("cat_mar26.csv", 
                  delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                  trim_ws = TRUE)
abr <- read_delim("cat_abri26.csv", 
                     delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                     trim_ws = TRUE)
mai <- read_delim("cat_mai26.csv", 
                  delim = ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                  trim_ws = TRUE)

dados <- bind_rows(jan, fev, mar, abr, mai)

# visualisando base de dados
View(dados)

# consultando as colunas - varíaveis
names(dados)

# resumo estatístico das colunas
summary(dados)

# tranformando profissões em categorias
dados <- dados |>
  mutate(
    CBO...4 = as.factor(CBO...4)
  )

# filtrando apenas as profissões da construção civil
construcao <- dados |>
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

## Qual profissão possui o maior número de casos de acidentes durante os meses análisados?


# criando a coluna de meses
construcao$mes <- month(construcao$`Data Acidente...23`,
                        label = TRUE,
                        abbr = FALSE)

# gráfico: ocorrencias de acidentes por profissão em cada mês
ggplot(construcao,
       aes(y = profissao,
           fill = mes)) +
  geom_bar() +
  labs(
    title = "Ocorrências de acidentes por profissão entre Janeiro e Maio de 2026",
    x = "Quantidade de registros",
    y = "Profissão",
    fill = "Mês"
  ) +
  theme_classic()

## CONCLUSÃO: A PROFISSÃO COM O MAIOR NÚMERO DE REGISTROS DE ACIDENTES ENTRE JANEIRO E MAIO DE 2026 FOI O SERVENTE DE OBRAS
## REGISTRANDO CERCA DE 6 MIL CASOS
## EM SEGUNDO LUGAR ESTÁ O PEDREIRO, COM CERCA DE 2.500 CASOS REGISTRADOS
## ABRIL FOI O MÊS RECORDE DE ACIDENTES ENTRE OS MESES ANALISADOS


## Quais estados registraram mais acidentes e mortes?


#tranformando dados em categorias
construcao <- construcao |>
  mutate(
    `Indica Óbito Acidente` = as.factor(`Indica Óbito Acidente`)
  )

# criando uma tabela resumida com os números de acidentes e percentual de obitos

dados_estados <- construcao |>
  group_by(`UF  Munic.  Acidente`) |>
  summarise(
    acidentes = n(),
    obitos = sum(`Indica Óbito Acidente` == "Sim"),
    percentual_obitos = (obitos / acidentes) * 100
  )

# alterando a variável ñ class para UF não informada
dados_estados <- dados_estados |>
  filter(`UF  Munic.  Acidente` != "Zerado") |>
  mutate(`UF  Munic.  Acidente` = as.character(`UF  Munic.  Acidente`),
    `UF  Munic.  Acidente` =
           ifelse(`UF  Munic.  Acidente` == "{ñ class}",
                  "UF não informada",
                  `UF  Munic.  Acidente`))
View(dados_estados)

# criando o gráfico - GRÁFICO DE BARRAS

ggplot(dados_estados,
       aes(x = reorder(`UF  Munic.  Acidente`, -acidentes),
           y = acidentes)) +
  
  geom_col(fill = "steelblue") +
  
  geom_text(
    aes(label = paste0(round(percentual_obitos, 1), "%")),
    hjust = -0.2,
    size = 3.5
  ) +
  
  coord_flip() +
  
  expand_limits(y = max(dados_estados$acidentes) * 1.08) +
  
  labs(
    title = "Acidentes de trabalho por estado",
    subtitle = "Percentual de acidentes com óbito",
    x = "Estado",
    y = "Número de acidentes"
  ) +
  
  theme_minimal()

## CONCLUSÃO: ENTRE OS ESATADOS COM INFORMAÇÃO DISPONÍVEL
## O MARANHÃO APRESENTA O MAIOR NÚMERO DE ACIDENTES
## O MARANHÃO REGISTRA CERCA DE 3.500 CASOS DE ACIDENTES DURANTE O PERÍODO ANALISADO
## JÁ CATEGORIA "UF NÃO INFORMADA" LIDERA NA QUANTIDADE DE REGISTROS
## COM QUASE 4 MIL CASOS
## OS ESTADOS COM MAIOR NÚMERO DE ÓBTIDOS SÃO: AMAZONAS(3,3%), PIAUÍ(2,6%) E CEARÁ(2,1%)

## Como os acidentes se distribuem entre profissão e estado?
heat <- construcao |>
  count(`UF  Munic.  Acidente`, profissao)
View(heat)

heat <- heat |>
  filter(`UF  Munic.  Acidente` != "Zerado") |>
  mutate(`UF  Munic.  Acidente` = as.character(`UF  Munic.  Acidente`),
         `UF  Munic.  Acidente` =
           ifelse(`UF  Munic.  Acidente` == "{ñ class}",
                  "UF não informada",
                  `UF  Munic.  Acidente`))

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

## CONCLUSÃO: OS DADOS CONFIRMAM AS ANÁLISES ANTERIORES
## A PROFISSÃO COM MAIOR NÚMERO DE REGISTRO DE ACIDENTES
## É O SERVENTE DE OBRA, ENTRE AS PROFISSÕES ANALISADAS
## O MARANHÃO EVIDENCIA O MAIOR NÚMERO DE REGISTROS ENVOLVENDO SERVENTES
## PEDREIROS APARECEM EM SEGUNDO LUGAR COM MAIOR NÚMERO DE CASOS DE ACIDENTES


## Qual parte do corpo é mais atingida nos acidentes em canteiros de obra?

# transformando parte do corpo em categoria
construcao <- construcao |>
  mutate(
    `Parte Corpo Atingida` = as.factor(`Parte Corpo Atingida`)
  )

# alterando o título das linhas
construcao <- construcao |>
  mutate(
    parte_corpo_grafico = case_when(
      
      `Parte Corpo Atingida` == "Abdome (Inclusive Or" ~ "Abdome",
      `Parte Corpo Atingida` == "Antebraco (Entre O P" ~ "Antebraço",
      `Parte Corpo Atingida` == "Aparelho Digestivo" ~ "Aparelho digestivo",
      `Parte Corpo Atingida` == "Aparelho Genito-Urin" ~ "Aparelho geniturinário",
      `Parte Corpo Atingida` == "Aparelho Respiratori" ~ "Aparelho respiratório",
      `Parte Corpo Atingida` == "Artelho" ~ "Artelhos",
      `Parte Corpo Atingida` == "Articulacao do Torno" ~ "Tornozelo",
      `Parte Corpo Atingida` == "Boca (Inclusive Labi" ~ "Boca",
      `Parte Corpo Atingida` == "Braco (Acima do Coto" ~ "Braço (acima do cotovelo)",
      `Parte Corpo Atingida` == "Braco (Entre O Punho" ~ "Braço (entre cotovelo e punho)",
      `Parte Corpo Atingida` == "Cabeca, Nic" ~ "Cabeça",
      `Parte Corpo Atingida` == "Cabeca, Partes Multi" ~ "Cabeça (múltiplas)",
      `Parte Corpo Atingida` == "Cotovelo" ~ "Cotovelo",
      `Parte Corpo Atingida` == "Coxa" ~ "Coxa",
      `Parte Corpo Atingida` == "Cranio (Inclusive En" ~ "Crânio",
      `Parte Corpo Atingida` == "Dedo" ~ "Dedo",
      `Parte Corpo Atingida` == "Dorso (Inclusive Mus" ~ "Dorso",
      `Parte Corpo Atingida` == "Face, Partes Multipl" ~ "Face (múltiplas)",
      `Parte Corpo Atingida` == "Joelho" ~ "Joelho",
      `Parte Corpo Atingida` == "Localizacao da Lesao" ~ "Localização não informada",
      `Parte Corpo Atingida` == "Mao (Exceto Punho ou" ~ "Mão",
      `Parte Corpo Atingida` == "Mandibula (Inclusive" ~ "Mandíbula",
      `Parte Corpo Atingida` == "Membros Inferiores," ~ "Membros inferiores",
      `Parte Corpo Atingida` == "Membros Superiores," ~ "Membros superiores",
      `Parte Corpo Atingida` == "Nariz (Inclusive Fos" ~ "Nariz",
      `Parte Corpo Atingida` == "Olho (Inclusive Nerv" ~ "Olho",
      `Parte Corpo Atingida` == "Ombro" ~ "Ombro",
      `Parte Corpo Atingida` == "Ouvido (Externo, Med" ~ "Ouvido",
      `Parte Corpo Atingida` == "Partes Multiplas - A" ~ "Partes múltiplas",
      `Parte Corpo Atingida` == "Pe (Exceto Artelhos)" ~ "Pé",
      `Parte Corpo Atingida` == "Perna (Do Tornozelo," ~ "Perna (tornozelo ao joelho)",
      `Parte Corpo Atingida` == "Perna (Entre O Torno" ~ "Perna (joelho ao quadril)",
      `Parte Corpo Atingida` == "Pescoco" ~ "Pescoço",
      `Parte Corpo Atingida` == "Punho" ~ "Punho",
      `Parte Corpo Atingida` == "Quadris (Inclusive P" ~ "Quadril",
      `Parte Corpo Atingida` == "Sistema Musculo-Esqu" ~ "Sistema musculoesquelético",
      `Parte Corpo Atingida` == "Sistema Nervoso" ~ "Sistema nervoso",
      `Parte Corpo Atingida` == "Sistemas e Aparelhos" ~ "Sistemas e aparelhos",
      `Parte Corpo Atingida` == "Torax (Inclusive Org" ~ "Tórax",
      `Parte Corpo Atingida` == "Tronco, Nic" ~ "Tronco",
      `Parte Corpo Atingida` == "Tronco, Parte Multip" ~ "Tronco (múltiplas)",
      
      TRUE ~ `Parte Corpo Atingida`
    )
  )

# usando count para contar as ocorrências e o sort para ordenar do maior para o menor
parte_corpo <- construcao |>
  count(parte_corpo_grafico, sort = TRUE)


## criando o gráfico
ggplot(parte_corpo, aes(x = reorder(parte_corpo_grafico, n), y = n)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(
    title = "Qual parte do corpo é mais atingida nos acidentes?",
    x = "Parte do corpo",
    y = "Número de acidentes"
  )+
  theme_classic()

## CONCLUSÃO: O DEDO É A PARTE DO CORPO MAIS ATINGIDA NOS ACIDENTES, COM MAIS DE 2.500 CASOS
## PÉ E MÃO TAMBÉM POSSUEM REGISTROS SIGNIFICATIVOS COM 1.000 E 2.000 CASOS REGISTRADOS RESPECTIVAMENTE

## Agente causador × natureza da lesão

# tranformando em categoria
construcao <- construcao |>
  mutate(
     `Natureza da Lesão` = as.factor(`Natureza da Lesão`)
  )


# renomeando as varíaveis

construcao <- construcao |>
  mutate(
    natureza_grafico = case_when(
      
      `Natureza da Lesão` == "Corte, Laceracao, Fe" ~ "Corte",
      `Natureza da Lesão` == "Fratura" ~ "Fratura",
      `Natureza da Lesão` == "Lesao Imediata" ~ "Lesão imediata",
      `Natureza da Lesão` == "Contusao, Esmagament" ~ "Contusão",
      `Natureza da Lesão` == "Distensao, Torcao" ~ "Distensão",
      `Natureza da Lesão` == "Escoriacao, Abrasao" ~ "Escoriação",
      `Natureza da Lesão` == "Luxacao" ~ "Luxação",
      `Natureza da Lesão` == "Outras Lesoes, Nic" ~ "Outras lesões",
      `Natureza da Lesão` == "Lesoes Multiplas" ~ "Lesões múltiplas",
      `Natureza da Lesão` == "Amputacao ou Enuclea" ~ "Amputação",
      `Natureza da Lesão` == "Lesao Imediata, Nic" ~ "Lesão imediata",
      `Natureza da Lesão` == "Queimadura ou Escald" ~ "Queimadura",
      `Natureza da Lesão` == "Perda ou Diminuicao" ~ "Perda de função",
      `Natureza da Lesão` == "Choque Eletrico e El" ~ "Choque elétrico",
      `Natureza da Lesão` == "Concussao Cerebral" ~ "Concussão",
      `Natureza da Lesão` == "Queimadura Quimica (" ~ "Queimadura química",
      `Natureza da Lesão` == "Inflamacao de Articu" ~ "Inflamação articular",
      `Natureza da Lesão` == "Doenca, Nic" ~ "Doença",
      `Natureza da Lesão` == "Envenenamento Sistem" ~ "Envenenamento",
      `Natureza da Lesão` == "Hernia de Qualquer N" ~ "Hérnia",
      `Natureza da Lesão` == "Dermatose (Erupcao," ~ "Dermatose",
      `Natureza da Lesão` == "Efeito de Radiacao (" ~ "Radiação",
      `Natureza da Lesão` == "Doenca Contagiosa ou" ~ "Doença contagiosa",
      `Natureza da Lesão` == "Intermacao, Insolaca" ~ "Insolação",
      `Natureza da Lesão` == "Congelamento, Geladu" ~ "Congelamento",
      
      TRUE ~ `Natureza da Lesão`
    )
  )

# parte do corpo x natureza da lesão
corpo_lesao <- construcao |>
  count(parte_corpo_grafico, natureza_grafico, sort = TRUE)

# o gráfico ficou poluído com muitas informações, vou selecionar o top 10 das lesões mais frequentes
top_lesoes <- construcao |>
  count(natureza_grafico, sort = TRUE) |>
  slice_head(n = 10)

# selecionando somente as 10 lesões mais frequentes
corpo_lesao_top <- corpo_lesao |>
  filter(natureza_grafico %in% 
           top_lesoes$natureza_grafico) 

# gráfico 
ggplot(
  corpo_lesao_top,
  aes(
    x = natureza_grafico,
    y = parte_corpo_grafico,
    fill = n
  )
) +
  geom_tile(color = "white") +
  labs(
    title = "Partes do corpo e principais tipos de lesão",
    x = "Natureza da lesão",
    y = "Parte do corpo",
    fill = "Acidentes"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

## CONCLUSÃO: FRATURAS APARECEM ENTRE OS PRINCIPAIS TIPOS
## DE LESÃO REGISTRADOS
## CORTE É O SEGUNDO TIPO DE LESÃO COM MAIOR NÚMERO DE REGISTROS
## PORTANTO, DEDO É A PARTE DO CORPO MAIS LECIONADA NOS ACIDENTES REGISTRADOS
## E OS CASOS MAIS RECORRENTES DE LESÕES SÃO FRATURAS E CORTES

## Profissão × agente causador

# transformando em categoria
construcao <- construcao |>
  mutate(
    `Agente  Causador  Acidente` = as.factor(`Agente  Causador  Acidente`)
  )


# top 10 dos agentes mais frequentes
top_agentes <- construcao |>
  count(`Agente  Causador  Acidente`, 
        sort = TRUE) |>
  slice_head(n = 10)

# renomeando as varíaveis dos agentes causadores

top_agentes <- top_agentes|>
  mutate(
    agentes_grafico = case_when(
      `Agente  Causador  Acidente` == "Metal - Inclui Liga" ~ "Metal",
      `Agente  Causador  Acidente` == "Chao - Superficie Ut" ~ "Chão/Superfície",
      `Agente  Causador  Acidente` == "Rua e Estrada - Supe" ~ "Rua/Estrada",
      `Agente  Causador  Acidente` == "Motocicleta, Motonet" ~ "Motocicleta",
      `Agente  Causador  Acidente` == "Agente do Acidente," ~ "Agente do Acidente",
      `Agente  Causador  Acidente` == "Madeira (Toro, Madei" ~ "Madeira",
      `Agente  Causador  Acidente` == "Veiculo Rodoviario M" ~ "Veículo Rodoviário",
      `Agente  Causador  Acidente` == "Martelo, Malho, Marr" ~ "Martelo",
      `Agente  Causador  Acidente` == "Agente do Acidente I" ~ "Agente do Acidente I",
      `Agente  Causador  Acidente` == "Andaime, Plataforma" ~ "Andaime/Plataforma",
      `Agente  Causador  Acidente` == "Veiculo, Nic" ~ "Veículo (NIC)",
      `Agente  Causador  Acidente` == "Produto Mineral nao" ~ "Produto Mineral",
      `Agente  Causador  Acidente` == "Escada Movel ou Fixa" ~ "Escada Móvel/Fixa",
      `Agente  Causador  Acidente` == "Piso de Edificio - S" ~ "Piso de Edifício",
      `Agente  Causador  Acidente` == "Escada Permanente Cu" ~ "Escada Permanente",
      TRUE ~ `Agente  Causador  Acidente`
      
    )
  )

View(top_agentes)

# agentes causadores x profissão
prof_agente <- construcao |>
  filter(
    `Agente  Causador  Acidente` %in%
      top_agentes$`Agente  Causador  Acidente`
  ) |>
  count(profissao, `Agente  Causador  Acidente`)

# usando os nomes simplificados

prof_agente <- prof_agente |>
  mutate(
    agentes_grafico = case_when(
      `Agente  Causador  Acidente` == "Metal - Inclui Liga" ~ "Metal",
      `Agente  Causador  Acidente` == "Chao - Superficie Ut" ~ "Chão/Superfície",
      `Agente  Causador  Acidente` == "Rua e Estrada - Supe" ~ "Rua/Estrada",
      `Agente  Causador  Acidente` == "Motocicleta, Motonet" ~ "Motocicleta",
      `Agente  Causador  Acidente` == "Agente do Acidente," ~ "Agente do Acidente",
      `Agente  Causador  Acidente` == "Madeira (Toro, Madei" ~ "Madeira",
      `Agente  Causador  Acidente` == "Veiculo Rodoviario M" ~ "Veículo Rodoviário",
      `Agente  Causador  Acidente` == "Martelo, Malho, Marr" ~ "Martelo",
      `Agente  Causador  Acidente` == "Agente do Acidente I" ~ "Agente do Acidente I",
      `Agente  Causador  Acidente` == "Andaime, Plataforma" ~ "Andaime/Plataforma",
      `Agente  Causador  Acidente` == "Veiculo, Nic" ~ "Veículo (NIC)",
      `Agente  Causador  Acidente` == "Produto Mineral nao" ~ "Produto Mineral",
      `Agente  Causador  Acidente` == "Escada Movel ou Fixa" ~ "Escada Móvel/Fixa",
      `Agente  Causador  Acidente` == "Piso de Edificio - S" ~ "Piso de Edifício",
      `Agente  Causador  Acidente` == "Escada Permanente Cu" ~ "Escada Permanente",
      TRUE ~ `Agente  Causador  Acidente`
      
    )
  )


# gráfico de barras - top 10 das profissões com os agentes causadores mais frequentes

ggplot(
  prof_agente,
  aes(
    x = profissao,
    y = n,
    fill = agentes_grafico
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Agentes causadores por profissão",
    x = "Profissão",
    y = "Número de acidentes",
    fill = "Agente causador"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )
## CONCLUSÃO: O MAIOR AGENTE CAUSADOR DE ACIDENTES NOS CANTEIROS DE OBRA É O METAL
## O SERVENTE SEGUE NA LIDERANÇA 
## ENVOLVENDO OS PRINCIPAIS AGENTES CAUSADORES 
## PEDREIRO APARECEM EM SEGUIDA ENTRE AS PROFISSÕES COM O MAIOR NÚMERO DE REGISTROS
