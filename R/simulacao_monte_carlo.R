# ------------------------------------------------------------------------------
# script: simulacao_monte_carlo.r
# autor: matheus dias de carvalho
# data: dezembro/2025
# descricao:
#   codigo fonte para geracao dos graficos 17, 18 e 19 e da tabela 9
#   da monografia de conclusao de curso (ie/unicamp).
#
# referencia dados:
#   watson et al. (2025)
#
# saidas:
#   - grafico_17_distribuicao_frequencia_pstar.svg
#   - grafico_18_preco_equilibrio_cenarios.svg
#   - grafico_19_premio_saf_vs_qav.svg
#   - tabela_09_resumo_parametros_resultados.csv
# ------------------------------------------------------------------------------

# 1. setup: pacotes e ambiente -------------------------------------------------

rm(list = ls(all = TRUE))

# identificacao automatica do diretorio do script
if (requireNamespace("rstudioapi", quietly = TRUE) &&
    rstudioapi::isAvailable()) {
  dir_codigo <- dirname(rstudioapi::getActiveDocumentContext()$path)
} else {
  stop("nao foi possivel identificar o diretorio do script. execute via rstudio.")
}

# definicao dos diretorios do projeto
dir_base   <- dirname(dir_codigo)
dir_dados  <- file.path(dir_base, "data")
dir_output <- file.path(dir_base, "output")

# define o diretorio de trabalho
setwd(dir_codigo)

# cria pastas caso nao existam
if (!dir.exists(dir_dados))  dir.create(dir_dados, recursive = TRUE)
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, ggplot2, tidyr, zoo, curl, stringr)

# 2. definicao de tema grafico (padrao tcc/unicamp) ----------------------------

theme_monografia <- function(base_size = 12, base_family = "Times New Roman") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      text             = element_text(color = "black", family = base_family),
      plot.title       = element_text(face = "bold", size = base_size + 1, hjust = 0),
      plot.subtitle    = element_text(size = base_size, hjust = 0, margin = margin(b = 10)),
      plot.caption     = element_text(size = base_size - 2, hjust = 0, margin = margin(t = 10), face = "plain"),
      axis.title       = element_text(face = "bold", size = base_size),
      axis.text        = element_text(color = "black", size = base_size),
      legend.text      = element_text(size = base_size),
      legend.title     = element_blank(),
      axis.line        = element_line(color = "black", linewidth = 0.5),
      axis.ticks       = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      legend.position  = "top",
      legend.justification = "left",
      legend.box.margin = margin(-5, 0, 5, 0),
      plot.margin      = margin(t = 5, r = 5, b = 5, l = 5, unit = "mm")
    )
}

salvar_figura <- function(plot_obj, filename) {
  ggsave(
    filename = file.path(dir_output, filename),
    plot = plot_obj,
    device = "svg",
    width = 250,
    height = 130, # altura ajustada para comportar legendas maiores
    units = "mm",
    dpi = 300
  )
  message(paste("salvo:", filename))
}

salvar_tabela_csv <- function(dataframe, filename) {
  write.csv(
    dataframe,
    file = file.path(dir_output, filename),
    row.names = FALSE
  )
  message(paste("tabela salva:", filename))
}

# 3. etl: extracao e carregamento de dados -------------------------------------

url_repo_github <- "https://github.com/dowlinglab/a-case-for-biojet-in-Brazil/raw/main/Mutran_jet_datav2.xlsx"

# arquivo de dados localizado na pasta data/
file_input_data <- file.path(dir_dados, "Mutran_jet_datav2.xlsx")

if (!file.exists(file_input_data)) {
  message("arquivo local nao encontrado na pasta data. iniciando download...")
  tryCatch({
    download.file(
      url = url_repo_github,
      destfile = file_input_data,
      mode = "wb",
      quiet = FALSE
    )
    message("download concluido com sucesso.")
  }, error = function(e) {
    stop("erro fatal: falha no download. verifique a conexao.")
  })
} else {
  message("arquivo de dados local identificado na pasta data.")
}

df_inflacao_raw <- read_excel(file_input_data, sheet = "Inflation")

# 4. parametros e calibracao ---------------------------------------------------

param_risk_free <- 0.08
param_lambda    <- 0.5

param_price_jet_m3_avg <- mean(df_inflacao_raw$p_je, na.rm = TRUE)
param_price_jet_L_avg  <- param_price_jet_m3_avg / 1000

df_retornos <- df_inflacao_raw %>%
  mutate(log_ret_etanol = log(p_et / lag(p_et)),
         log_ret_qav    = log(p_je / lag(p_je))) %>%
  na.omit()

stat_sigma_s_base <- sd(df_retornos$log_ret_etanol) * sqrt(52)
stat_sigma_b_base <- sd(df_retornos$log_ret_qav)    * sqrt(52)

roll_sigma_et <- rollapply(df_retornos$log_ret_etanol, width = 52, FUN = function(x) sd(x) * sqrt(52), fill = NA, align = "right")
roll_sigma_je <- rollapply(df_retornos$log_ret_qav, width = 52, FUN = function(x) sd(x) * sqrt(52), fill = NA, align = "right")

stat_sd_sigma_s <- sd(roll_sigma_et, na.rm = TRUE)
stat_sd_sigma_b <- sd(roll_sigma_je, na.rm = TRUE)

# 5. definicao dos cenarios (conforme tabela 8 da monografia) ------------------

calc_nash_bargaining <- function(sig_s, sig_b) {
  r_s     <- param_risk_free + param_lambda * sig_s
  r_b     <- param_risk_free + param_lambda * sig_b
  delta_s <- 1 / (1 + r_s)
  delta_b <- 1 / (1 + r_b)
  p_star  <- (1 - delta_b) / (1 - delta_s)
  data.frame(r_s, r_b, delta_s, delta_b, p_star)
}

df_cenarios_def <- data.frame(
  Cenario = c(
    "1. Base (Historico)",
    "2. Mandato (ProBioQAV)",
    "3. Incentivo (Oferta)",
    "4. Estabilidade (Maturidade)"
  ),
  Sigma_S = c(stat_sigma_s_base, stat_sigma_s_base, stat_sigma_s_base * 0.7, 0.15),
  Sigma_B = c(stat_sigma_b_base, stat_sigma_b_base * 1.5, stat_sigma_b_base, 0.15)
)

# 6. simulacao de monte carlo --------------------------------------------------

set.seed(123)
n_iteracoes <- 100000
df_resultados_mc <- data.frame()

message(">> iniciando simulacao de monte carlo...")

for (i in 1:nrow(df_cenarios_def)) {
  nome_cenario <- df_cenarios_def$Cenario[i]
  vol_s_target <- df_cenarios_def$Sigma_S[i]
  vol_b_target <- df_cenarios_def$Sigma_B[i]
  
  sim_sigma_s <- pmax(rnorm(n_iteracoes, mean = vol_s_target, sd = stat_sd_sigma_s), 0)
  sim_sigma_b <- pmax(rnorm(n_iteracoes, mean = vol_b_target, sd = stat_sd_sigma_b), 0)
  
  resultados_nash <- calc_nash_bargaining(sim_sigma_s, sim_sigma_b)
  
  preco_saf_m3 <- resultados_nash$p_star * param_price_jet_m3_avg
  premio_m3    <- preco_saf_m3 - param_price_jet_m3_avg
  premio_L     <- premio_m3 / 1000
  
  df_temp <- data.frame(Cenario = nome_cenario, p_star = resultados_nash$p_star, premio_L = premio_L)
  
  df_resultados_mc <- bind_rows(df_resultados_mc, df_temp)
}

df_resultados_mc$Cenario <- factor(df_resultados_mc$Cenario, levels = df_cenarios_def$Cenario)

# 7. analise estatistica e tabela 9 --------------------------------------------

df_estatisticas_resumo <- df_resultados_mc %>%
  group_by(Cenario) %>%
  summarise(
    p_star_mediana   = median(p_star),
    p_star_Q1        = quantile(p_star, 0.25),
    p_star_Q3        = quantile(p_star, 0.75),
    premio_L_mediana = median(premio_L),
    .groups          = "drop"
  )

# gera a tabela 9 conforme monografia
df_tabela_09 <- df_cenarios_def %>%
  select(Cenario, Sigma_S, Sigma_B) %>%
  left_join(df_estatisticas_resumo, by = "Cenario")

# 8. geracao dos graficos (17, 18 e 19) ----------------------------------------

# grafico 17 - distribuicao de frequencia de p*
y_limit_hist <- 10000
plot_grafico_17 <- ggplot(df_resultados_mc, aes(x = p_star)) +
  geom_histogram(bins = 60, fill = "grey85", color = "grey30", alpha = 0.9) +
  geom_vline(data = df_estatisticas_resumo, aes(xintercept = p_star_mediana), color = "red", linetype = "solid", linewidth = 1) +
  geom_vline(data = df_estatisticas_resumo, aes(xintercept = p_star_Q1), color = "darkred", linetype = "dashed", linewidth = 0.7) +
  geom_vline(data = df_estatisticas_resumo, aes(xintercept = p_star_Q3), color = "darkred", linetype = "dashed", linewidth = 0.7) +
  geom_label(data = df_estatisticas_resumo, aes(x = p_star_mediana, y = y_limit_hist / 2, label = round(p_star_mediana, 2)), color = "red", fill = "white", label.size = 0.5, fontface = "bold", size = 3.5) +
  facet_wrap(~Cenario, ncol = 2, scales = "fixed") +
  scale_y_continuous(limits = c(0, y_limit_hist)) +
  scale_x_continuous(breaks = seq(floor(min(df_resultados_mc$p_star)), ceiling(max(df_resultados_mc$p_star)), by = 0.5)) +
  labs(
    title    = "Gráfico 17 – Distribuição de frequência do Preço de Equilíbrio (p*)",
    subtitle = NULL,
    x        = "Preço de equilíbrio (p*)",
    y        = "Frequência",
    caption  = expression(paste("Fonte: Elaboração própria com base em Watson ", italic("et al."), " (2025). Nota: Histogramas por cenário com Mediana (linha vermelho) e Quartis (tracejado vermelho)."))
  ) +
  theme_monografia() +
  theme(strip.text = element_text(face = "bold", size = 10), strip.background = element_rect(color = "black", fill = "grey90", size = 0.7), axis.text.x = element_text(angle = 0, hjust = 0.5))

print(plot_grafico_17)

# grafico 18 - boxplot preco de equilibrio (p*)
plot_grafico_18 <- ggplot(df_resultados_mc, aes(x = Cenario, y = p_star, fill = Cenario)) +
  geom_boxplot(width = 0.4, outlier.alpha = 0.15) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2.4, fill = "white", colour = "black") +
  geom_hline(yintercept = 1.0, linetype = "dashed", colour = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = str_wrap("Gráfico 18 – Preço de Equilíbrio (p*) do SAF sob diferentes cenários regulatórios", 80),
    subtitle = NULL,
    y        = "Preço de equilíbrio (p*)",
    x        = NULL,
    caption  = expression(paste("Fonte: Elaboração própria com base em Watson ", italic("et al."), " (2025). Nota: p* > 1 prêmio favorável à usina; p* < 1, desconto favorável à companhia."))
  ) +
  theme_monografia() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 0, hjust = 0.5))

print(plot_grafico_18)

# grafico 19 - boxplot premio em r$/l
plot_grafico_19 <- ggplot(df_resultados_mc, aes(x = Cenario, y = premio_L, fill = Cenario)) +
  geom_boxplot(width = 0.4, outlier.alpha = 0.15) +
  stat_summary(fun = median, geom = "point", shape = 21, size = 2.4, fill = "white", colour = "black") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title    = "Gráfico 19 – Preço do SAF em relação ao QAV, em R$/L",
    subtitle = NULL,
    y        = "Prêmio do SAF (R$/L)",
    x        = NULL,
    caption  = expression(paste("Fonte: Elaboração própria com base em Watson ", italic("et al."), " (2025)."))
  ) +
  theme_monografia() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 0, hjust = 0.5))

print(plot_grafico_19)

# 9. exportacao dos resultados -------------------------------------------------

message(">> iniciando exportacao dos graficos e tabelas...")

# salvando graficos
salvar_figura(plot_grafico_17, "grafico_17_distribuicao_frequencia_pstar.svg")
salvar_figura(plot_grafico_18, "grafico_18_preco_equilibrio_cenarios.svg")
salvar_figura(plot_grafico_19, "grafico_19_premio_saf_vs_qav.svg")

# salvando tabela
salvar_tabela_csv(df_tabela_09, "tabela_09_resumo_parametros_resultados.csv")

message(">> simulacao e exportacao concluidas com sucesso. verifique a pasta output/.")