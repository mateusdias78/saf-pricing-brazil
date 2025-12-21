# Desafios e perspectivas para a introdução do SAF na matriz energética do Brasil

**Autor:** Matheus Dias de Carvalho  
**Orientadora:** Profa. Dra. Aline Veronese da Silva  
**Instituição:** Instituto de Economia - UNICAMP  
**Ano:** 2025

---

## 📄 Sobre este repositório

Este repositório armazena o material suplementar, os códigos-fonte e as bases de dados utilizados na Monografia de Conclusão de Curso intitulada **"Desafios e perspectivas para a introdução do Combustível Sustentável de Aviação na matriz energética do Brasil: O papel da regulação na formação de preços"**.

O projeto computacional divide-se em duas frentes:
1.  **Modelagem Econômica:** Simulação de Monte Carlo aplicada a um modelo de Barganha de Nash para estimar preços de equilíbrio ($p^*$) e prêmios verdes do SAF (Rota ATJ).
2.  **Análise Descritiva:** Geração de visualizações de dados sobre o panorama global e nacional da aviação (emissões, demanda, rotas tecnológicas e mercado).

## 📂 Estrutura de Arquivos

A organização do projeto segue o padrão de reprodutibilidade científica:

### `R/` (Scripts de Análise)
* **`simulacao_saf_tcc_unicamp_2025.R`**: Script principal da monografia. Realiza a calibração de volatilidade, simulação de cenários regulatórios e gera os gráficos de resultados do modelo econômico (Gráficos 17, 18 e 19).
* **`gerar_graficos_descritivos.R`**: Script responsável por gerar as visualizações do panorama setorial (Gráficos 1 a 15), abordando emissões, metas da ICAO/IATA e evolução da produção de biocombustíveis.

### `data/` (Bases de Dados)
* **`Mutran_jet_datav2.xlsx`**: Série histórica semanal de preços de Etanol e QAV (2013-2022) utilizada para calibração de risco no modelo econômico (Fonte: Watson et al., 2025).
* **`dados_consolidados.xlsx`**: Compilação de dados agregados de múltiplas fontes (ICAO, IATA, EPE, ANP, Global Carbon Project) utilizada para a caracterização do setor.

### `output/` (Resultados)
* **`figures/`**: Contém todas as figuras geradas pelos scripts em formato vetorial (`.svg`) de alta qualidade.
* **`tables/`**: Contém as tabelas de resultados das simulações (`.csv`).

## 🚀 Como executar

Para reproduzir as análises, siga os passos abaixo:

1.  **Clone ou baixe** este repositório.
2.  Abra o projeto no **RStudio**.
3.  Os scripts utilizam o gerenciador de pacotes `pacman`, que instalará automaticamente as dependências necessárias (`tidyverse`, `readxl`, `zoo`, etc.) na primeira execução.

### Para gerar os resultados do Modelo Econômico:
Execute o arquivo:
```r
source("R/simulacao_saf_tcc_unicamp_2025.R")
