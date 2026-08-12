# 📊 Análise de Acidentes de Trabalho na Construção Civil

Análise exploratória de dados de **Comunicações de Acidentes de Trabalho (CAT)** registrados entre **janeiro e maio de 2026**, com foco nos acidentes relacionados a profissões da **construção civil**.

O projeto foi desenvolvido em **R**, utilizando técnicas de limpeza, transformação, agrupamento e visualização de dados.

---

## 🎯 Objetivo

Investigar o perfil dos acidentes de trabalho registrados na construção civil entre janeiro e maio de 2026, buscando identificar:

* Quais profissões apresentam mais registros de acidentes;
* Como os acidentes se distribuem ao longo dos meses;
* Quais estados concentram mais registros;
* Qual é a proporção de registros com óbito por estado;
* Quais partes do corpo são mais atingidas;
* Quais são as principais naturezas das lesões;
* Quais agentes causadores aparecem com maior frequência;
* Como os agentes causadores se distribuem entre as diferentes profissões.

---

## 🗂️ Dados

Foram utilizados arquivos mensais de registros da **CAT**, correspondentes aos meses:

* Janeiro de 2026
* Fevereiro de 2026
* Março de 2026
* Abril de 2026
* Maio de 2026

Os arquivos foram importados individualmente e posteriormente consolidados em uma única base para realização das análises.

Após a consolidação, foram selecionados registros relacionados às seguintes profissões da construção civil:

* Servente
* Pedreiro
* Eletricista
* Operador de Máquinas
* Pintor
* Armador de Estruturas
* Carpinteiro

---

## 🛠️ Tecnologias utilizadas

* **R**
* **RStudio**
* **dplyr** — manipulação e transformação dos dados
* **readr** — leitura dos arquivos CSV
* **ggplot2** — criação das visualizações
* **lubridate** — tratamento de datas

---

## 🔎 Etapas da análise

### 1. Importação e consolidação

Os cinco arquivos mensais foram importados utilizando `read_delim()` e posteriormente unidos através de `bind_rows()`.

### 2. Filtragem

Foram selecionadas apenas as profissões relacionadas à construção civil.

### 3. Tratamento dos dados

Foram realizadas transformações para:

* Simplificar os nomes das profissões;
* Padronizar categorias;
* Tratar registros de estado não informado;
* Converter e extrair informações das datas;
* Simplificar a nomenclatura de partes do corpo;
* Simplificar a nomenclatura das naturezas das lesões;
* Simplificar a nomenclatura dos agentes causadores.

### 4. Análise exploratória

Foram utilizados recursos do `dplyr`, como:

```r
filter()
mutate()
count()
group_by()
summarise()
arrange()
```

para obter informações agregadas sobre os acidentes.

### 5. Visualização

As principais visualizações foram desenvolvidas utilizando `ggplot2`, incluindo:

* Gráficos de barras;
* Gráficos de barras agrupadas;
* Heatmaps;
* Rótulos com informações adicionais.

---

## 📈 Análises realizadas

### Acidentes por profissão e mês

Foi analisada a quantidade de registros de acidentes para cada profissão ao longo dos cinco meses.

Os **serventes** apresentaram a maior quantidade de registros, seguidos pelos **pedreiros**.

---

### Acidentes e óbitos por estado

Foi calculado, para cada estado:

* Número de registros de acidentes;
* Número de registros com indicação de óbito;
* Percentual de registros com óbito.

Também foi realizada uma separação dos registros cuja UF não estava informada.

---

### Estado × profissão

Foi utilizado um **heatmap** para observar a distribuição dos registros de acidentes entre estados e profissões.

A visualização permite identificar concentrações de registros de determinadas profissões em diferentes estados.

---

### Partes do corpo atingidas

Foi analisada a frequência das diferentes partes do corpo atingidas nos registros.

Entre as categorias com maior quantidade de registros estão:

* Dedo;
* Mão;
* Pé.

---

### Partes do corpo × natureza da lesão

Foi criado um heatmap relacionando as partes do corpo atingidas com as principais naturezas de lesão.

Para facilitar a interpretação, foram selecionadas as **10 naturezas de lesão mais frequentes**.

---

### Profissão × agente causador

Foi analisada a distribuição dos principais agentes causadores entre as profissões da construção civil.

Entre os agentes analisados estão categorias como:

* Metal;
* Chão/Superfície;
* Rua/Estrada;
* Motocicleta;
* Madeira;
* Veículos;
* Martelo;
* Andaimes e plataformas;
* Escadas.

---

## 📊 Principais resultados

A análise permitiu observar alguns padrões importantes na base:

* **Serventes** concentram a maior quantidade de registros de acidentes entre as profissões analisadas;
* **Pedreiros** aparecem em seguida entre as profissões com maior número de registros;
* O **Maranhão** apresenta grande quantidade de registros entre os estados com UF informada;
* A categoria **UF não informada** também representa uma parcela relevante dos registros;
* **Dedo, mão e pé** estão entre as partes do corpo com maior quantidade de registros;
* Os acidentes apresentam diferentes padrões de natureza de lesão conforme a parte do corpo atingida;
* O **metal** aparece entre os agentes causadores com maior quantidade de registros;
* Os **serventes** concentram grande parte dos registros envolvendo os principais agentes causadores.

---

## 📚 O que foi praticado no projeto

Este projeto foi desenvolvido como uma forma prática de consolidar conhecimentos em:

* Manipulação de dados com `dplyr`;
* Leitura de arquivos CSV;
* Tratamento de dados categóricos;
* Tratamento de datas;
* Agrupamento e sumarização;
* Contagem de ocorrências;
* Criação de variáveis com `mutate()` e `case_when()`;
* Seleção de categorias mais frequentes;
* Construção de gráficos com `ggplot2`;
* Visualização de relações entre variáveis;
* Análise exploratória de dados;
* Interpretação dos resultados.

---

## 📁 Estrutura do projeto

```text
📦 analise-cat-construcao-civil
│
├── 📄 README.md
├── 📄 analise_CAT.R
│
├── 📁 dados
│   ├── cat_jan26.csv
│   ├── cat_fev26.csv
│   ├── cat_mar26.csv
│   ├── cat_abri26.csv
│   └── cat_mai26.csv
│
└── 📁 graficos
    └── ...
```

---

## 🚀 Próximos passos

Como continuação do projeto, algumas possibilidades de expansão seriam:

* Comparar os resultados de 2026 com anos anteriores;
* Analisar a evolução mensal dos acidentes;
* Investigar a relação entre profissão, agente causador e natureza da lesão;
* Criar visualizações interativas;
* Desenvolver um dashboard;
* Explorar outras variáveis disponíveis na base CAT.

---

## 👩‍💻 Sobre o projeto

Este projeto faz parte do meu processo de aprendizado em **R e Análise de Dados**, utilizando uma base de dados real para praticar conceitos de manipulação, exploração e visualização de dados.

O objetivo principal foi transformar dados brutos em informações que permitissem compreender melhor o perfil dos acidentes de trabalho registrados na construção civil.
