# Desafios e perspectivas para a introdução do SAF na matriz energética do Brasil

- **Autor:** Matheus Dias de Carvalho  
- **Orientadora:** Profa. Dra. Aline Veronese da Silva  
- **Instituição:** Instituto de Economia — Universidade Estadual de Campinas (UNICAMP)  
- **Ano:** 2025

---

## Resumo

Este repositório contém os dados e o código empregados na monografia *Desafios e perspectivas para a introdução do Combustível Sustentável de Aviação (SAF) na matriz energética do Brasil: o papel da regulação na formação de preços*. O estudo modela o mercado de SAF (rota *Alcohol-to-Jet* — ATJ) como um **oligopólio bilateral** e estima os efeitos de arranjos regulatórios (mandatos, incentivos e estabilidade) sobre preços de equilíbrio e prêmios do SAF. A análise integra: (i) caracterização setorial e cenários; (ii) calibração empírica de volatilidade dos insumos (etanol e QAV); (iii) simulação estocástica via Monte Carlo sobre um modelo de barganha sequencial. Resultados, código e dados são disponibilizados para reprodutibilidade e verificação empírica.

---

## Estrutura do repositório

```
saf-pricing-brazil/
├── data/
│   ├── dados_consolidados.xlsx    # dados setoriais (ICAO, IATA, EPE, ANP, Bergero 2022, etc.)
│   └── Mutran_jet_datav2.xlsx     # séries de preços (etanol e QAV; Watson et al. dataset)
├── R/
│   ├── gerar_graficos_descritivos.R   # gera os Gráficos 1–16
│   └── simulacao_monte_carlo.R        # gera Gráficos 17–19 e Tabela 9
├── output/
│   ├── figures/                   # gráficos exportados em .svg
│   └── tables/                    # tabelas exportadas em .csv
└── README.md (este arquivo)
```

---

## Dados

- **dados_consolidados.xlsx** — base agregada com séries históricas, projeções e parâmetros setoriais (fontes: ICAO, IATA, EPE, ANP,  Bergero (2022) etc.).  
- **Mutran_jet_datav2.xlsx** — série histórica semanal de preços de etanol e querosene de aviação (2013–2022), utilizada na calibração da volatilidade e do risco (fontes: Watson et al. (2025)).

---

## Scripts principais

### `R/gerar_graficos_descritivos.R`
Gera os gráficos de contextualização setorial (Gráficos 1–16): demanda e eficiência, emissões históricas, intensidade de carbono, projeções tecnológicas e projeções de oferta/demanda de matérias-primas.

### `R/simulacao_monte_carlo.R`
Executa a calibração de volatilidade, define os quatro cenários regulatórios (Base; Mandato; Incentivo; Estabilidade), realiza simulação de Monte Carlo (100.000 iterações por cenário) e produz:
- Gráfico 17 — distribuição de frequência do preço de equilíbrio ($p^*$);
- Gráfico 18 — comparação de $p^*$ entre cenários;
- Gráfico 19 — prêmio do SAF em R$/L;
- Tabela 9 — resumo estatístico das medianas.

---

## Execução (reprodutibilidade)

**Requisitos:** R (versão recente), RStudio, pacotes `pacman`, `ggplot2`, `dplyr`, `readxl`, `tidyr`, `zoo`, `scales`, `ggtext`.

**Passos:**

```r
# geração dos gráficos descritivos (Gráficos 1–16)
source("R/gerar_graficos_descritivos.R")

# simulação e análise (Gráficos 17–19 e Tabela 9)
source("R/simulacao_monte_carlo.R")
```

As saídas são gravadas automaticamente em `output/figures/` e `output/tables/`.

---

## Resumo metodológico

O mercado de SAF é modelado como uma barganha sequencial em estrutura de oligopólio bilateral. As volatilidades históricas dos preços de etanol e QAV (2013–2022) são utilizadas para calibrar os fatores de desconto dos agentes. Quatro cenários regulatórios são simulados para avaliar impactos sobre o preço de equilíbrio relativo ($p^*$) e sobre o prêmio do SAF em relação ao QAV.

---

## Referências (seleção)

1. Watson, M. J., et al. (2025). *The Case for Biojet Fuel from Bioethanol in Brazil.* (dataset: `Mutran_jet_datav2.xlsx`)  
2. Bergero, L. (2022). Estudo setorial: demanda e emissões na aviação. (relatório / working paper)  
3. ICAO — International Civil Aviation Organization. Long-Term Aspirational Goal (LTAG) and CORSIA reports (2020–2025).  
4. IATA — International Air Transport Association. Projections and SAF strategy (2021–2025).  
5. Global Carbon Project (2024). Emissões globais — dataset e relatórios.  
6. Lee, D. S., et al. (2021). Non-CO₂ effects of aviation. *Journal / Report* (ver metadados).  
7. Klöwer, M., et al. (2021). Contrails and climate impact. *Journal / Report* (ver metadados).  
8. Rubinstein, A. (1982). *Perfect Equilibrium in a Bargaining Model.* *Econometrica.*  
9. Li, D. (2016). Bargaining models with applications to energy markets.  
10. Documentos regulatórios nacionais: **RenovaBio**, **Lei do Combustível do Futuro**, **ProBioQAV** (Brasil).

---

## Licença

Este repositório é disponibilizado sob a licença **Creative Commons Attribution 4.0 International (CC BY 4.0)**.
