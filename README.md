# Desafios e perspectivas para a introdução do SAF na matriz energética do Brasil

**Autor:** Matheus Dias de Carvalho  
**Orientadora:** Profa. Dra. Aline Veronese da Silva  
**Instituição:** Instituto de Economia - UNICAMP  
**Ano:** 2025

---

## 📄 Sobre este repositório

Este repositório armazena o material suplementar, os códigos-fonte e as bases de dados utilizados na Monografia de Conclusão de Curso intitulada **"Desafios e perspectivas para a introdução do Combustível Sustentável de Aviação na matriz energética do Brasil: O papel da regulação na formação de preços"**.

O projeto computacional divide-se em duas frentes complementares:

1. **Análise Descritiva (Contextualização):**  
   Geração de visualizações de dados sobre o panorama global e nacional da aviação, abordando a evolução das emissões, metas internacionais (ICAO/IATA), rotas tecnológicas e o mercado potencial de SAF.

2. **Modelagem Econômica (Simulação):**  
   Aplicação de uma Simulação de Monte Carlo em um modelo de Barganha de Nash para estimar os preços de equilíbrio ($p^*$) e os prêmios verdes do SAF produzido via rota *Alcohol-to-Jet* (ATJ) no Brasil.

---

## 📂 Estrutura de Arquivos

A organização dos arquivos segue a ordem de apresentação no trabalho:

### `R/` (Scripts de Análise)

- **`gerar_graficos_descritivos.R`**  
  Script responsável pela análise setorial. Gera os gráficos de 1 a 15 (Contextualização).

- **`simulacao_saf_tcc_unicamp_2025.R`**  
  Script do modelo econômico principal. Realiza a calibração de volatilidade e a simulação dos cenários regulatórios, gerando os gráficos 17, 18 e 19 e a tabela de resultados.

### `data/` (Bases de Dados)

- **`dados_consolidados.xlsx`**  
  Compilação de dados agregados de múltiplas fontes (ICAO, IATA, EPE, ANP, Global Carbon Project) utilizada para a caracterização do setor na análise descritiva.

- **`Mutran_jet_datav2.xlsx`**  
  Série histórica semanal de preços de Etanol e QAV (2013–2022) utilizada especificamente para a calibração de risco no modelo de simulação.

### `output/` (Resultados)

- **`figures/`**  
  Contém todas as figuras geradas pelos scripts em formato vetorial (`.svg`) de alta qualidade.

- **`tables/`**  
  Contém as tabelas de resultados das simulações (`.csv`).

---

## 🚀 Como executar

Para reproduzir as análises na ordem lógica do trabalho:

1. **Clone ou baixe** este repositório.
2. Abra o projeto no **RStudio**.
3. O gerenciador `pacman` instalará as dependências automaticamente na primeira execução.

### Passo 1: Gerar os Gráficos Descritivos (Contexto)

Execute o comando abaixo no console do R para gerar as Figuras 1 a 15:

    source("R/gerar_graficos_descritivos.R")

### Passo 2: Executar o Modelo Econômico (Simulação)

Execute o comando abaixo para realizar a Simulação de Monte Carlo e gerar as Figuras 17 a 19:

    source("R/simulacao_saf_tcc_unicamp_2025.R")

**Nota:** Todos os arquivos de saída serão salvos automaticamente na pasta `output/`.

---

## 📚 Referências Principais

**Dados Setoriais:**  
Compilados a partir de relatórios da ICAO, IATA, EPE (Empresa de Pesquisa Energética) e ANP.

**Modelagem de Preços:**  
Baseada e adaptada de Watson, M. J. et al. (2025). *"The Case for Biojet Fuel from Bioethanol in Brazil"*. Industrial & Engineering Chemistry Research.
