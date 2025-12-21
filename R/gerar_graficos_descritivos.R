# ------------------------------------------------------------------------------
# script: gerar_graficos_descritivos.r
# autor: matheus dias de carvalho
# data: dezembro/2025
# descricao:
#   codigo fonte para geracao das figuras da monografia de conclusao de curso.
#   foca na analise historica e cenarios da literatura.
#
# referencia dados:
#   elaboracao propria com base em dados consolidados (icao, atag, anp, etc.)
#
# saidas:
#   - grafico_01_demanda_transporte_aereo.svg
#   - grafico_02_emissoes_co2_rpk.svg
#   - grafico_03_emissoes_diretas_co2.svg
#   - grafico_04_intensidade_carbono_global.svg
#   - grafico_05_projecoes_emissoes_internacional.svg
#   - grafico_06_participacao_aviacao_global.svg
#   - grafico_07_projecao_consumo_combustivel.svg
#   - grafico_08_projecao_emissoes_lacuna.svg
#   - grafico_10_relacao_mfsp_ic.svg
#   - grafico_11_projecao_producao_saf_rota.svg
#   - grafico_12_projecao_participacao_fontes.svg
#   - grafico_13_participacao_producao_bio_paises.svg
#   - grafico_14_evolucao_producao_br_bio.svg
#   - grafico_15_projecao_capacidade_saf_br.svg
#   - grafico_16_projecao_demanda_materia_prima_br.svg
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
dir_base    <- dirname(dir_codigo)
dir_dados   <- file.path(dir_base, "data")
dir_output  <- file.path(dir_base, "output")

# define o diretorio de trabalho
setwd(dir_codigo)

# cria pastas caso nao existam
if (!dir.exists(dir_dados))  dir.create(dir_dados, recursive = TRUE)
if (!dir.exists(dir_output)) dir.create(dir_output, recursive = TRUE)

# pacotes
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, ggplot2, tidyr, stringr, forcats, grid, patchwork, scales, ggtext)

# 2. definicao de tema grafico e funcoes ---------------------------------------

# tema personalizado
theme_monografia <- function(base_size = 12, base_family = "Times New Roman") {
  theme_classic(base_size = base_size, base_family = base_family) +
    theme(
      text             = element_text(color = "black", family = base_family),
      plot.title       = element_text(face = "bold", size = base_size + 1, hjust = 0), 
      plot.subtitle    = element_text(size = base_size, hjust = 0, margin = margin(b = 10)),
      plot.caption     = ggtext::element_markdown(size = base_size - 2, hjust = 0, margin = margin(t = 10), face = "plain", family = base_family, lineheight = 1.2),
      axis.title       = element_text(face = "bold", size = base_size),
      axis.text        = element_text(color = "black", size = base_size),
      legend.text      = element_text(size = base_size),
      legend.title     = element_blank(),
      
      axis.line        = element_line(color = "black", linewidth = 0.5),
      axis.ticks       = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position  = "top",
      legend.justification = "left",
      legend.margin    = margin(0, 0, 0, 0),
      legend.box.margin = margin(-5, 0, 5, 0),
      
      plot.margin      = margin(t = 5, r = 5, b = 5, l = 5, unit = "mm")
    )
}

# funcao wrapper para salvar svg
salvar_figura <- function(plot_obj, filename) {
  ggsave(
    filename = file.path(dir_output, filename),
    plot = plot_obj,
    device = "svg",
    width = 250,
    height = 130, # Aumentei levemente a altura para acomodar legendas longas
    units = "mm",
    dpi = 300
  )
  message(paste("salvo:", filename))
}

# caminho do arquivo de dados (relativo)
dados_path <- file.path(dir_dados, "dados_consolidados.xlsx")

if (!file.exists(dados_path)) {
  stop(paste("erro fatal: arquivo de dados nao encontrado em", dados_path))
}

# 3. geracao dos graficos ------------------------------------------------------

# --- grafico 1 ----------------------------------------------------------------
df_g1 <- read_excel(dados_path, sheet = "g1") %>%
  rename(Ano        = "Ano",
         Demanda    = "Demanda de passageiros (bilhões de RPK)",
         Eficiencia = "Eficiência energética (MJ/RPK-equivalente)") %>%
  mutate(Ano = as.numeric(Ano)) %>%
  filter(!is.na(Ano)) %>%
  filter(Ano >= 1990)

max_demanda    <- max(df_g1$Demanda,    na.rm = TRUE)
max_eficiencia <- max(df_g1$Eficiencia, na.rm = TRUE)
escala_g1      <- max_demanda / max_eficiencia

plot_grafico_01 <- ggplot(df_g1, aes(x = Ano)) +
  geom_line(linewidth = 1.2, aes(y = Demanda, colour = "Demanda de passageiros")) +
  geom_point(aes(y = Demanda, colour = "Demanda de passageiros", shape = "Demanda de passageiros"), size = 3) +
  geom_line(linewidth = 1.2, aes(y = Eficiencia * escala_g1, colour = "Eficiência energética")) +
  geom_point(aes(y = Eficiencia * escala_g1, colour = "Eficiência energética", shape = "Eficiência energética"), size = 3) +
  scale_x_continuous(breaks = seq(min(df_g1$Ano), max(df_g1$Ano), by = 1)) +
  scale_y_continuous(
    name = "Bilhões de RPK",
    limits = c(1000, 11000),
    breaks = seq(1000, 10000, by = 2000),
    sec.axis = sec_axis(~ . / escala_g1, name = "MJ/RPK-equivalente", breaks = seq(0.5, 3, by = 0.5))
  ) +
  scale_colour_manual(name = "", values = c("Demanda de passageiros" = "#203864", "Eficiência energética" = "#d95f02")) +
  scale_shape_manual(name = "", values = c("Demanda de passageiros" = 16, "Eficiência energética" = 17)) +
  guides(colour = guide_legend(override.aes = list(size = 3)), shape = guide_legend()) +
  labs(
    title = str_wrap("Gráfico 1 – Demanda por transporte aéreo e eficiência energética no setor de aviação global, índices de evolução, entre 1990 e 2021", 100),
    caption = "Fonte: Elaboração própria com base em Bergero (2022).",
    x = "Ano"
  ) +
  theme_monografia() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

print(plot_grafico_01)

# --- grafico 2 ----------------------------------------------------------------
df_g2_rpk <- read_excel(dados_path, sheet = "g1") %>%
  rename(Ano = "Ano", Demanda = "Demanda de passageiros (bilhões de RPK)", Emissoes_CO2 = "Emissões de CO2 (Mt)") %>%
  mutate(Ano = as.numeric(Ano)) %>%
  filter(!is.na(Ano), Ano >= 1990) %>%
  mutate(Emissoes_por_RPK = (Emissoes_CO2 / Demanda) * 1000)

plot_grafico_02 <- ggplot(df_g2_rpk, aes(x = Ano)) +
  geom_line(linewidth = 1.2, aes(y = Emissoes_por_RPK, colour = "Emissões por RPK")) +
  geom_point(aes(y = Emissoes_por_RPK, colour = "Emissões por RPK", shape = "Emissões por RPK"), size = 3) +
  scale_x_continuous(breaks = seq(min(df_g2_rpk$Ano), max(df_g2_rpk$Ano), by = 1)) +
  scale_y_continuous(name = "gCO2/RPK", limits = c(50, 350), breaks = seq(50, 350, by = 50)) +
  scale_colour_manual(name = "", values = c("Emissões por RPK" = "#203864")) +
  scale_shape_manual(name = "", values = c("Emissões por RPK" = 16)) +
  guides(colour = guide_legend(override.aes = list(size = 3)), shape = guide_legend()) +
  labs(
    title = str_wrap("Gráfico 2 – Emissões de CO2 por RPK no setor de aviação global, índices de evolução, entre 1990 e 2021", 100),
    caption = "Fonte: Elaboração própria com base em Bergero (2022).",
    x = "Ano"
  ) +
  theme_monografia() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.title.y = element_text(face = "bold", size = 12))

print(plot_grafico_02)

# --- grafico 3 ----------------------------------------------------------------
df_g3_emissoes <- read_excel(dados_path, sheet = "g1") %>%
  rename(Ano = "Ano", Emissoes = "Emissões de CO2 (Mt)") %>%
  mutate(Ano = as.numeric(Ano)) %>%
  filter(!is.na(Ano)) %>%
  filter(Ano >= 1990)

plot_grafico_03 <- ggplot(df_g3_emissoes, aes(x = Ano, y = Emissoes)) +
  geom_bar(stat = "identity", fill = "#203864", colour = "black", linewidth = 0.3, width = 0.7) +
  scale_y_continuous(name = "Mt CO2", expand = expansion(mult = c(0, 0.05)), breaks = scales::pretty_breaks(n = 6)) +
  scale_x_continuous(breaks = seq(min(df_g3_emissoes$Ano), max(df_g3_emissoes$Ano), by = 1)) +
  labs(
    title = str_wrap("Gráfico 3 – Emissões diretas de CO2 do setor de aviação global, milhões de toneladas (Mt), entre 1990 e 2021", 100),
    caption = "Fonte: Elaboração própria com base em Bergero (2022).",
    x = "Ano"
  ) +
  theme_monografia() +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

print(plot_grafico_03)

# --- grafico 4 ----------------------------------------------------------------
df_g4_int <- read_excel(dados_path, sheet = "g3") %>%
  rename(Ano = matches("Ano"), Intensidade = matches("Intensidade")) %>%
  mutate(Ano = as.numeric(Ano)) %>%
  filter(Ano >= 1990)

media_intensidade <- mean(df_g4_int$Intensidade, na.rm = TRUE)
label_media       <- sprintf("Média 1990–2021 (%.2f gCO2/MJ)", media_intensidade)
legenda_dados     <- "Intensidade de carbono"
cores_g4_int      <- setNames(c("#203864", "#d95f02"), c(legenda_dados, label_media))

plot_grafico_04 <- ggplot(df_g4_int, aes(x = Ano)) +
  geom_hline(aes(yintercept = media_intensidade, colour = label_media), linetype = "dashed", linewidth = 0.8) +
  geom_line(aes(y = Intensidade, colour = legenda_dados), linewidth = 1.2) +
  geom_point(aes(y = Intensidade, colour = legenda_dados), shape = 16, size = 3) +
  scale_x_continuous(breaks = seq(min(df_g4_int$Ano, na.rm = TRUE), max(df_g4_int$Ano, na.rm = TRUE), by = 1)) +
  scale_y_continuous(name = "gCO2/MJ", n.breaks = 6) +
  scale_colour_manual(name = NULL, values = cores_g4_int, breaks = c(legenda_dados, label_media)) +
  labs(
    title = str_wrap("Gráfico 4 – Intensidade de carbono do combustível de aviação global, em gCO2/MJ, entre 1990 e 2021", 100),
    caption = "Fonte: Elaboração própria com base em Bergero (2022).",
    x = "Ano"
  ) +
  theme_monografia() +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(colour = guide_legend(order = 1, nrow = 1, byrow = TRUE))

print(plot_grafico_04)

# --- grafico 5 ----------------------------------------------------------------
df_g5_proj <- read_excel(dados_path, sheet = "g4") %>%
  rename(Ano = matches("Year|Ano"), Carbono_intensivo = matches("Carbono Intensivo"), Carbono_reduzido = matches("Carbono Reduzido"), Net_zero = matches("Net-zero")) %>%
  mutate(Ano = as.numeric(Ano))

df_g5_hist <- df_g5_proj %>%
  filter(Ano >= 1990, Ano <= 2020) %>%
  transmute(Ano, Emissoes = Carbono_intensivo)

df_g5_long <- df_g5_proj %>%
  pivot_longer(cols = c(Carbono_intensivo, Carbono_reduzido, Net_zero), names_to = "Cenario", values_to = "Emissoes") %>%
  mutate(Cenario = factor(Cenario, levels = c("Carbono_intensivo", "Carbono_reduzido", "Net_zero"), labels = c("Carbono intensivo", "Carbono reduzido", "Net-zero"))) %>%
  filter(Ano >= 2020)

max_emissoes_g5    <- max(df_g5_long$Emissoes, na.rm = TRUE)
limite_superior_g5 <- ceiling(max_emissoes_g5 / 0.2) * 0.2

cores_g5  <- c("Carbono intensivo" = "#203864", "Carbono reduzido" = "#d95f02", "Net-zero" = "#70ad47")
shapes_g5 <- c("Carbono intensivo" = 16, "Carbono reduzido" = 17, "Net-zero" = 15)

plot_grafico_05 <- ggplot() +
  geom_line(data = df_g5_hist, aes(x = Ano, y = Emissoes), colour = "black", linewidth = 1.2) +
  geom_line(data = df_g5_long, aes(x = Ano, y = Emissoes, colour = Cenario), linetype = "dashed", linewidth = 1.2) +
  geom_point(data = df_g5_long, aes(x = Ano, y = Emissoes, colour = Cenario, shape = Cenario), size = 3) +
  scale_x_continuous(limits = c(1990, 2050), breaks = seq(1990, 2050, by = 2)) +
  scale_y_continuous(name = "GtCO2", limits = c(0, limite_superior_g5), breaks = seq(0, limite_superior_g5, by = 0.2), expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(name = NULL, values = cores_g5) +
  scale_shape_manual(name = NULL, values = shapes_g5) +
  labs(
    title = str_wrap("Gráfico 5 – Projeções de emissões de CO2 da aviação internacional sob diferentes cenários tecnológicos, em milhões de toneladas (Gt), até 2050", 100),
    caption = str_wrap("Fonte: Elaboração própria com base em Bergero (2022). Nota: As três projeções partem do mesmo cenário de alto crescimento da demanda e baixas melhorias de eficiência energética (\"Business-as-usual\"). A diferença entre as curvas reside na tecnologia de combustível adotada: (i) Carbono intensivo, que mantém 100% de uso de combustível fóssil; (ii) Carbono Reduzido, com uma transição parcial para SAFs ; e (iii) Net-zero, que representa uma substituição agressiva por biocombustíveis e tecnologias de emissão zero, atingindo o zero líquido de emissões de CO2 em 2050.", 120),
    x = "Ano"
  ) +
  theme_monografia() +
  theme(axis.title.y = element_text(face = "bold"), axis.title.x = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(linetype = "dashed", linewidth = 1.2, size = 3, shape = c(16, 17, 15))), shape = "none")

print(plot_grafico_05)

# --- grafico 6 ----------------------------------------------------------------
df_g6_part <- read_excel(dados_path, sheet = "g5") %>%
  rename(Ano = Ano, Emissoes = `Emissões Territoriais (GtCO₂)`, Aviacao = `Aviação (GtCO₂)`) %>%
  mutate(Ano = as.numeric(Ano), Participacao = 100 * Aviacao / Emissoes) %>%
  filter(Ano >= 1990)

media_part_g6      <- mean(df_g6_part$Participacao, na.rm = TRUE)
label_media_g6     <- sprintf("Média 1991–2021 (%.2f %%)", media_part_g6)
limite_superior_g6 <- ceiling(max(df_g6_part$Participacao, na.rm = TRUE) / 0.5) * 0.5
cores_g6           <- setNames(c("#203864", "#d95f02"), c("Participação da aviação", label_media_g6))

plot_grafico_06 <- ggplot(df_g6_part, aes(x = Ano)) +
  geom_hline(aes(yintercept = media_part_g6, colour = label_media_g6), linetype = "dashed", linewidth = 0.8) +
  geom_line(aes(y = Participacao, colour = "Participação da aviação"), linewidth = 1.2) +
  geom_point(aes(y = Participacao, colour = "Participação da aviação"), shape = 16, size = 3) +
  scale_x_continuous(breaks = seq(min(df_g6_part$Ano), max(df_g6_part$Ano), by = 1)) +
  scale_y_continuous(name = "%", limits = c(0, limite_superior_g6), breaks = seq(0, limite_superior_g6, by = 0.5), expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(name = NULL, values = cores_g6, breaks = c("Participação da aviação", label_media_g6)) +
  labs(
    title = str_wrap("Gráfico 6 – Participação do setor de aviação nas emissões globais de CO2, em %, entre 1991 e 2021", 100),
    caption = "Fonte: Elaboração própria com base em Bergero (2022) e Global Carbon Projetct (2024).",
    x = "Ano"
  ) +
  theme_monografia() +
  theme(legend.position = c(0.03, 0.97), legend.justification = c("left", "top"), legend.direction = "vertical", legend.box = "vertical", legend.title = element_text(face = "bold"), legend.background = element_rect(fill = "white", colour = NA), legend.spacing.x = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm"), axis.title.x = element_text(margin = margin(t = 10)), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(colour = guide_legend(nrow = 1, override.aes = list(linetype = c("solid", "dashed"), shape = c(16, NA), linewidth = c(1.2, 0.8))))

print(plot_grafico_06)

# --- grafico 7 ----------------------------------------------------------------
df_g7_cons <- read_excel(dados_path, sheet = "g6") %>%
  rename(Ano = Ano, Congelamento = `(1) Congelamento Tecnológico (Linha Azul Escura)`, MelhoriaTec = `(2) Melhorias Tecnológicas (Linha Azul Tracejada)`, ATMInfra = `(3) ATM & Infraestrutura (Limite Inferior da Área Laranja)`, Meta2 = `(4) Meta Aspiracional 2% (Linha Laranja Tracejada)`) %>%
  mutate(Ano = as.numeric(Ano))

df_g7_linhas <- df_g7_cons %>%
  select(Ano, Congelamento, MelhoriaTec, Meta2) %>%
  pivot_longer(cols = -Ano, names_to = "Serie", values_to = "Consumo") %>%
  mutate(Serie = factor(Serie, levels = c("Congelamento", "MelhoriaTec", "Meta2"), labels = c("Congelamento tecnológico", "Melhorias tecnológicas", "Meta aspiracional 2%")))

df_g7_hist        <- df_g7_linhas %>% filter(Ano >= 2005, Ano <= 2025, Serie == "Congelamento tecnológico")
df_g7_proj_linhas <- df_g7_linhas %>% filter(Ano >= 2025)
df_g7_ribbon      <- df_g7_cons %>% filter(Ano >= 2025)

cores_linhas_g7  <- c("Congelamento tecnológico" = "#203864", "Melhorias tecnológicas" = "#d95f02", "Meta aspiracional 2%" = "#70ad47")
fills_areas_g7   <- c("Contribuição adicional de melhorias tecnológicas" = "#a6cee3", "Contribuição adicional de Gerenciamento de Tráfego Aéreo e infraestrutura" = "#fdbf6f")
shapes_linhas_g7 <- c("Congelamento tecnológico" = 16, "Melhorias tecnológicas" = 17, "Meta aspiracional 2%" = 15)
limite_superior_g7 <- ceiling(max(c(df_g7_cons$Congelamento, df_g7_cons$MelhoriaTec, df_g7_cons$ATMInfra, df_g7_cons$Meta2), na.rm = TRUE) / 50) * 50

plot_grafico_07 <- ggplot() +
  geom_ribbon(data = df_g7_ribbon, aes(x = Ano, ymin = MelhoriaTec, ymax = Congelamento, fill = "Contribuição adicional de melhorias tecnológicas"), alpha = 0.5) +
  geom_ribbon(data = df_g7_ribbon, aes(x = Ano, ymin = ATMInfra, ymax = MelhoriaTec, fill = "Contribuição adicional de Gerenciamento de Tráfego Aéreo e infraestrutura"), alpha = 0.7) +
  geom_line(data = df_g7_hist, aes(x = Ano, y = Consumo, colour = Serie, linetype = Serie), linewidth = 0.8) +
  geom_point(data = df_g7_hist, aes(x = Ano, y = Consumo, colour = Serie, shape = Serie), size = 2) +
  geom_line(data = df_g7_proj_linhas, aes(x = Ano, y = Consumo, colour = Serie, linetype = Serie), linewidth = 0.8) +
  geom_point(data = df_g7_proj_linhas, aes(x = Ano, y = Consumo, colour = Serie, shape = Serie), size = 2) +
  scale_x_continuous(limits = c(2005, 2070), breaks = c(2005, seq(2006, 2068, 2), 2070), expand = c(0, 0)) +
  coord_cartesian(xlim = c(2005, 2070), expand = FALSE) +
  scale_y_continuous(name = "Mt QAV", limits = c(0, limite_superior_g7), breaks = seq(0, limite_superior_g7, by = 100), expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(name = "Séries de linha", values = cores_linhas_g7) +
  scale_shape_manual(name = "Séries de linha", values = shapes_linhas_g7) +
  scale_linetype_manual(name = "Séries de linha", values = c("Congelamento tecnológico" = "solid", "Melhorias tecnológicas" = "dashed", "Meta aspiracional 2%" = "dashed")) +
  scale_fill_manual(name = "Contribuições de área", values = fills_areas_g7) +
  guides(colour = guide_legend(order = 1, ncol = 1, byrow = TRUE, title.position = "top"), shape = guide_legend(order = 1, ncol = 1, byrow = TRUE, title.position = "top"), linetype = guide_legend(order = 1, ncol = 1, byrow = TRUE, title.position = "top"), fill = guide_legend(order = 2, ncol = 1, byrow = TRUE, title.position = "top")) +
  labs(
    title = str_wrap("Gráfico 7 – Projeção do consumo de combustível da aviação internacional, em milhões de toneladas (Mt), de 2005 a 2070", 100),
    caption = str_wrap("Fonte: Adaptado de ICAO (2025, p. 30). Nota: As áreas coloridas representam as contribuições para a redução do consumo de combustível por meio de melhorias tecnológicas e operacionais. A linha superior da área azul escura indica o consumo total projetado. A linha tracejada representa a Meta Aspiracional de 2% de Eficiência de Combustível anual da ICAO.", 120),
    x = "Ano"
  ) +
  theme_monografia() +
  theme(legend.position = c(0.03, 0.97), legend.justification = c("left", "top"), legend.direction = "vertical", legend.box = "vertical", legend.title = element_text(face = "bold"), legend.background = element_rect(fill = "white", colour = NA), legend.spacing.x = unit(0.4, "cm"), legend.spacing.y = unit(0.3, "cm"), axis.title.x = element_text(margin = margin(t = 10)), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

print(plot_grafico_07)

# --- grafico 8 ----------------------------------------------------------------
df_g8_lacuna <- read_excel(dados_path, sheet = "g7") %>%
  rename(Ano = Ano, Congelamento = `(1) Congelamento Tecnológico (Base - Linha Azul Sólida)`, MelhoriaTec = `(2) Melhorias Tecnológicas (Linha Azul Tracejada)`, ATMInfra = `(3) ATM & Infraestrutura (Limite Topo Área Verde Escura)`, BioRes = `(4) Biocombustíveis/Resíduos (Limite Topo Área Verde Clara)`, ResGas = `(5) Resíduos Gasosos (Limite Topo Área Roxa)`, LCAF = `(6) LCAF / Emissões Líquidas (Linha Inferior Final)`) %>%
  mutate(Ano = as.numeric(Ano))

df_g8_linhas <- df_g8_lacuna %>%
  select(Ano, Congelamento, MelhoriaTec, LCAF) %>%
  pivot_longer(cols = -Ano, names_to = "Serie", values_to = "Emissoes") %>%
  mutate(Serie = factor(Serie, levels = c("Congelamento", "MelhoriaTec", "LCAF"), labels = c("Congelamento tecnológico", "Melhorias tecnológicas", "Emissões líquidas (LCAF)")))

df_g8_hist        <- df_g8_linhas %>% filter(Ano >= 2005, Ano <= 2025, Serie == "Congelamento tecnológico")
df_g8_proj_linhas <- df_g8_linhas %>% filter(Ano >= 2025)
df_g8_ribbon      <- df_g8_lacuna %>% filter(Ano >= 2025)

max_emissoes_g8    <- max(df_g8_lacuna$Congelamento, na.rm = TRUE)
limite_superior_g8 <- ceiling(max_emissoes_g8 / 250) * 250

cores_linhas_g8  <- c("Congelamento tecnológico" = "#203864", "Melhorias tecnológicas" = "#d95f02", "Emissões líquidas (LCAF)" = "#984ea3")
shapes_linhas_g8 <- c("Congelamento tecnológico" = 16, "Melhorias tecnológicas" = 17, "Emissões líquidas (LCAF)" = 15)
fills_areas_g8   <- c("Contribuição adicional de melhorias tecnológicas" = "#a6cee3", "Contribuição adicional de gerenciamento de tráfego aéreo e infraestrutura" = "#fdbf6f", "Contribuição adicional de biocombustíveis e resíduos sólidos" = "#b2df8a", "Contribuição adicional de resíduos gasosos" = "#cab2d6")

plot_grafico_08 <- ggplot() +
  geom_ribbon(data = df_g8_ribbon, aes(x = Ano, ymin = MelhoriaTec, ymax = Congelamento, fill = "Contribuição adicional de melhorias tecnológicas"), alpha = 0.5) +
  geom_ribbon(data = df_g8_ribbon, aes(x = Ano, ymin = ATMInfra, ymax = MelhoriaTec, fill = "Contribuição adicional de gerenciamento de tráfego aéreo e infraestrutura"), alpha = 0.7) +
  geom_ribbon(data = df_g8_ribbon, aes(x = Ano, ymin = BioRes, ymax = ATMInfra, fill = "Contribuição adicional de biocombustíveis e resíduos sólidos"), alpha = 0.7) +
  geom_ribbon(data = df_g8_ribbon, aes(x = Ano, ymin = ResGas, ymax = BioRes, fill = "Contribuição adicional de resíduos gasosos"), alpha = 0.7) +
  geom_line(data = df_g8_hist, aes(x = Ano, y = Emissoes, colour = Serie, linetype = Serie), linewidth = 0.8) +
  geom_point(data = df_g8_hist, aes(x = Ano, y = Emissoes, colour = Serie, shape = Serie), size = 2) +
  geom_line(data = df_g8_proj_linhas, aes(x = Ano, y = Emissoes, colour = Serie, linetype = Serie), linewidth = 0.8) +
  geom_point(data = df_g8_proj_linhas, aes(x = Ano, y = Emissoes, colour = Serie, shape = Serie), size = 2) +
  scale_x_continuous(limits = c(2005, 2070), breaks = c(2005, seq(2006, 2068, by = 2), 2070), expand = c(0, 0)) +
  coord_cartesian(xlim = c(2005, 2070), expand = FALSE) +
  scale_y_continuous(name = "Mt CO2", limits = c(0, limite_superior_g8), breaks = seq(0, limite_superior_g8, by = 250), expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(name = "Séries de linha", values = cores_linhas_g8) +
  scale_shape_manual(name = "Séries de linha", values = shapes_linhas_g8) +
  scale_linetype_manual(name = "Séries de linha", values = c("Congelamento tecnológico" = "solid", "Melhorias tecnológicas" = "dashed", "Emissões líquidas (LCAF)" = "solid")) +
  scale_fill_manual(name = "Contribuições de área", values = fills_areas_g8) +
  guides(colour = guide_legend(order = 1, title.position = "top"), shape = "none", linetype = "none", fill = guide_legend(order = 2, title.position = "top")) +
  labs(
    title = str_wrap("Gráfico 8 – Projeção das emissões de CO2 da aviação internacional e lacuna de mitigação, em milhões de toneladas (Mt), de 2005 a 2070", 100),
    caption = str_wrap("Fonte: Adaptado de ICAO (2025, p. 31). Nota: O gráfico demonstra o aumento projetado nas emissões absolutas de CO2. As áreas coloridas indicam a mitigação potencial por meio de melhorias tecnológicas e operacionais.", 120),
    x = "Ano"
  ) +
  theme_monografia(base_size = 12) +
  theme(legend.position = c(0.03, 0.97), legend.justification = c("left", "top"), legend.background = element_rect(fill = NA, colour = NA), legend.key = element_rect(fill = NA, colour = NA), legend.title = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1), axis.title.y = element_text(face = "bold"))

print(plot_grafico_08)

# --- grafico 10 ---------------------------------------------------------------
df_g10_mfsp <- read_excel(dados_path, sheet = "g8") %>%
  rename(Rota = Rota, Materia_prima = `Matéria-prima`, MFSP = `MFSP ($/L)`, IC_puro = `IC puro (g CO₂e/MJ)`) %>%
  mutate(MFSP = as.numeric(MFSP), IC_puro = as.numeric(IC_puro), Rota = factor(Rota, levels = c("ATJ", "FT", "HEFA", "QAV"))) %>%
  filter(!is.na(IC_puro), !is.na(MFSP))

max_ic_g10    <- max(df_g10_mfsp$IC_puro, na.rm = TRUE)
max_mfsp_g10  <- max(df_g10_mfsp$MFSP, na.rm = TRUE)
cores_g10     <- c("ATJ" = "#203864", "FT" = "#d95f02", "HEFA" = "#70ad47", "QAV" = "#D7301F")
shapes_g10    <- c("ATJ" = 17, "FT" = 16, "HEFA" = 15, "QAV" = 18)

plot_grafico_10 <- ggplot(df_g10_mfsp, aes(x = IC_puro, y = MFSP)) +
  geom_point(aes(colour = Rota, shape = Rota), size = 4) +
  scale_x_continuous(name = "gCO2e/MJ", limits = c(0, max_ic_g10 * 1.05), breaks = pretty(c(0, max_ic_g10), n = 6)) +
  scale_y_continuous(name = "US$/L", limits = c(0, max_mfsp_g10 * 1.10), breaks = pretty(c(0, max_mfsp_g10), n = 6)) +
  scale_colour_manual(name = "Rota", values = cores_g10) +
  scale_shape_manual(name = "Rota", values = shapes_g10) +
  guides(colour = guide_legend(order = 1, ncol = 1, byrow = TRUE, title.position = "top"), shape = guide_legend(order = 1, ncol = 1, byrow = TRUE, title.position = "top")) +
  labs(
    title = str_wrap("Gráfico 10 – Relação entre Preço Mínimo de Venda (MFSP), em US$/L, e Intensidade de Carbono (IC), em gCO2e/MJ, por rota de SAF e matéria-prima", 100),
    caption = str_wrap("Fonte: Elaboração própria, com base em Ng *et al.* (2021). Nota: Cada ponto representa uma combinação específica de rota e matéria-prima. O termo 'IC puro' refere-se às emissões GEE ao longo do ciclo de vida, não incluindo emissões por ILUC.", 120),
    x = "Intensidade de Carbono (gCO2e/MJ)"
  ) +
  theme_monografia() +
  theme(legend.position = c(0.03, 0.97), legend.justification = c("left", "top"), legend.direction = "vertical", legend.box = "vertical", legend.title = element_text(face = "bold", size = 12), legend.text = element_text(size = 12), legend.background = element_rect(fill = "white", colour = NA), axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))

print(plot_grafico_10)

# --- grafico 11 ---------------------------------------------------------------
df_g11_prod <- read_excel(dados_path, sheet = "g9") %>%
  rename(Ano = matches("Ano|Year")) %>%
  mutate(Ano = as.numeric(Ano)) %>%
  pivot_longer(cols = -Ano, names_to = "Caminho", values_to = "Producao") %>%
  filter(!is.na(Producao)) %>%
  mutate(Caminho = stringr::str_replace(Caminho, "\\s*\\(.*\\)$", ""))

max_producao_g11    <- max(df_g11_prod$Producao, na.rm = TRUE)
limite_superior_g11 <- ceiling(max_producao_g11 / 25) * 25
cores_g11           <- c("HEFA" = "#70ad47", "FT" = "#d95f02", "ATJ" = "#203864", "PtL" = "#984ea3")
shapes_g11          <- c(16, 17, 15, 18)

plot_grafico_11 <- ggplot(df_g11_prod, aes(x = Ano, y = Producao, colour = Caminho, shape = Caminho)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_x_continuous(limits = c(2019, 2050), breaks = seq(2019, 2050, by = 1)) +
  scale_y_continuous(name = "Mt de SAF", limits = c(0, limite_superior_g11), breaks = seq(0, limite_superior_g11, by = 25), expand = expansion(mult = c(0, 0.05))) +
  scale_colour_manual(name = "Rota", values = cores_g11) +
  scale_shape_manual(name = "Rota", values = shapes_g11) +
  labs(
    title = str_wrap("Gráfico 11 – Projeção da produção global de SAF por rota tecnológica, em milhões de toneladas (Mt), entre 2020 e 2050", 100),
    caption = "Fonte: Elaboração própria com base em IATA (2024).",
    x = "Ano"
  ) +
  theme_monografia() +
  theme(legend.position = c(0.05, 0.95), legend.justification = c("left", "top"), legend.background = element_rect(fill = "white", colour = NA), legend.title = element_text(face = "bold", size = 12), legend.text = element_text(size = 11), axis.title.x = element_text(face = "bold", margin = margin(t = 10)), axis.title.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(colour = guide_legend(ncol = 1, byrow = TRUE, title.position = "top"), shape = guide_legend(ncol = 1, byrow = TRUE, title.position = "top"))

print(plot_grafico_11)

# --- grafico 12 ---------------------------------------------------------------
df_g12_fontes <- read_excel(dados_path, sheet = "g10") %>%
  rename(Ano = matches("Ano|Year")) %>%
  mutate(Ano = as.numeric(Ano)) %>%
  pivot_longer(cols = -Ano, names_to = "Fonte", values_to = "Participacao") %>%
  filter(!is.na(Participacao)) %>%
  mutate(Participacao = Participacao * 100, Fonte = stringr::str_replace(Fonte, "\\s*\\(.*\\)$", ""), Fonte = factor(Fonte, levels = c("QAV", "bio SAF", "SAF-PtL", "Hidrogênio/Bateria Elétrica")))

cores_g12 <- c("QAV" = "#7F7F7F", "bio SAF" = "#70ad47", "SAF-PtL" = "#203864", "Hidrogênio/Bateria Elétrica" = "#d95f02")

df_g12_fontes <- df_g12_fontes %>% mutate(Fonte = factor(Fonte, levels = c("QAV", "bio SAF", "SAF-PtL", "Hidrogênio/Bateria Elétrica")))

plot_grafico_12 <- ggplot(df_g12_fontes, aes(x = factor(Ano), y = Participacao, fill = Fonte)) +
  geom_bar(stat = "identity", colour = "black", linewidth = 0.3, width = 0.6) +
  geom_text(aes(label = ifelse(Participacao >= 5, paste0(round(Participacao, 1), "%"), "")), position = position_stack(vjust = 0.5), size = 5, family = "Times New Roman", color = "white") +
  scale_color_manual(values = c("QAV" = "white", "bio SAF" = "black", "SAF-PtL" = "white", "Hidrogênio/Bateria Elétrica" = "black")) +
  scale_y_continuous(name = "%", limits = c(0, 100), breaks = seq(0, 100, by = 20), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(values = cores_g12, name = "") +
  labs(
    title = str_wrap("Gráfico 12 – Projeção da participação das fontes de energia na demanda energética dos voos, em %, entre 2020 e 2050", 100),
    caption = "Fonte: Elaboração própria com base em IATA (2024).",
    x = "Ano"
  ) +
  theme_monografia() +
  theme(axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"))

print(plot_grafico_12)

# --- grafico 13 ---------------------------------------------------------------
df_g13_raw <- read_excel(dados_path, sheet = "g11") %>%
  rename(Pais = 1) %>%
  pivot_longer(cols = -Pais, names_to = "Ano", values_to = "Producao") %>%
  mutate(Ano = as.numeric(Ano), Producao = as.numeric(Producao)) %>%
  filter(!is.na(Ano), Ano >= 2014, Ano <= 2024, !is.na(Producao))

df_g13_share <- df_g13_raw %>%
  group_by(Ano) %>%
  mutate(Participacao = 100 * Producao / sum(Producao, na.rm = TRUE)) %>%
  ungroup()

top3_paises <- df_g13_share %>%
  filter(Ano == 2024) %>%
  arrange(desc(Participacao)) %>%
  slice_head(n = 3) %>%
  pull(Pais)

df_g13_plot <- df_g13_share %>%
  mutate(Grupo = if_else(Pais %in% top3_paises, Pais, "Resto do mundo")) %>%
  group_by(Ano, Grupo) %>%
  summarise(Participacao = sum(Participacao, na.rm = TRUE), .groups = "drop")

ordem_grupos <- c(top3_paises, "Resto do mundo")
df_g13_plot <- df_g13_plot %>% mutate(Grupo = factor(Grupo, levels = ordem_grupos))
cores_g13    <- setNames(c("#203864", "#70ad47", "#d95f02", "#7F7F7F"), ordem_grupos)

plot_grafico_13 <- ggplot(df_g13_plot, aes(x = factor(Ano), y = Participacao, fill = Grupo)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  geom_text(aes(label = ifelse(Participacao >= 5, paste0(round(Participacao, 1), "%"), "")), position = position_stack(vjust = 0.5), size = 4.0, family = "sans", color = "white") +
  scale_y_continuous(name = "%", limits = c(0, 100), breaks = seq(0, 100, by = 10), expand = expansion(mult = c(0, 0.02))) +
  scale_fill_manual(name = "", values = cores_g13) +
  labs(
    title = str_wrap("Gráfico 13 – Participação na produção global de biocombustíveis renováveis, por país selecionado, em %, entre 2014 e 2024", 100),
    caption = "Fonte: Elaboração própria com base em Energy Institute (2025).",
    x = "Ano"
  ) +
  theme_monografia() +
  theme(legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal", axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

print(plot_grafico_13)

# --- grafico 14 ---------------------------------------------------------------
df_g14_br <- read_excel(dados_path, sheet = "g12") %>%
  rename(Ano = Ano, Biodiesel = `Biodisel (bilhões de litros)`, Etanol_cana = `Etanol de Cana-de-açúcar (bilhões de litros)`, Etanol_milho = `Etanol de milho (bilhões de litros)`) %>%
  mutate(Ano = as.numeric(Ano)) %>%
  pivot_longer(cols = c(Biodiesel, Etanol_cana, Etanol_milho), names_to = "Combustivel", values_to = "Producao") %>%
  mutate(Combustivel = factor(Combustivel, levels = c("Biodiesel", "Etanol_cana", "Etanol_milho"), labels = c("Biodiesel", "Etanol de cana-de-açúcar", "Etanol de milho")))

df_g14_totais      <- df_g14_br %>% group_by(Ano) %>% summarise(Total = sum(Producao, na.rm = TRUE), .groups = "drop")
limite_superior_g14 <- 50
cores_g14           <- c("Biodiesel" = "#70ad47", "Etanol de cana-de-açúcar" = "#203864", "Etanol de milho" = "#d95f02")

plot_grafico_14 <- ggplot(df_g14_br, aes(x = factor(Ano), y = Producao, fill = Combustivel)) +
  geom_bar(stat = "identity", colour = "black", width = 0.7) +
  geom_text(aes(label = ifelse(Producao >= 3, round(Producao, 1), "")), position = position_stack(vjust = 0.5), size = 4, family = "sans", color = "white") +
  geom_text(data = df_g14_totais, aes(x = factor(Ano), y = Total, label = round(Total, 1)), inherit.aes = FALSE, vjust = -0.5, size = 4, fontface = "bold", family = "sans") +
  scale_y_continuous(name = "Bilhões de litros", limits = c(0, limite_superior_g14), breaks = seq(0, limite_superior_g14, by = 5), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "", values = cores_g14) +
  labs(
    title = str_wrap("Gráfico 14 – Evolução da produção brasileira dos principais biocombustíveis, em bilhões de litros, de 2013 a 2024", 100),
    caption = str_wrap("Fonte: Elaboração própria, com base em ANP (2025b); EPE (2025). Nota: Dados de biodiesel: ANP (2025b). Dados de etanol: EPE (2025).", 120),
    x = "Ano"
  ) +
  theme_monografia(base_size = 12, base_family = "sans") +
  theme(legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal", axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))

print(plot_grafico_14)

# --- grafico 15 ---------------------------------------------------------------
# consolidadas
df_g15_consol <- read_excel(dados_path, sheet = "g13_1") %>%
  rename(Ano = matches("Ano|Year")) %>%
  mutate(Ano = as.numeric(Ano)) %>%
  pivot_longer(cols = -Ano, names_to = "Materia", values_to = "Capacidade") %>%
  filter(!is.na(Capacidade)) %>%
  mutate(Materia = str_replace(Materia, "\\s*\\(bilhões de litros\\)", ""), Materia = factor(Materia, levels = c("HEFA Soja", "ATJ E1G", "ATJ Milho")))

# alternativas
df_g15_alt <- read_excel(dados_path, sheet = "g13_2") %>%
  rename(Ano = matches("Ano|Year")) %>%
  mutate(Ano = as.numeric(Ano)) %>%
  pivot_longer(cols = -Ano, names_to = "Materia", values_to = "Capacidade") %>%
  filter(!is.na(Capacidade)) %>%
  mutate(Materia = str_replace(Materia, "\\s*\\(bilhões de litros\\)", ""), Materia = factor(Materia, levels = c("HEFA Macaúba", "ATJ Agave", "ATJ E2G")))

# limite comum e cores
max_cap_g15        <- max(c(df_g15_consol$Capacidade, df_g15_alt$Capacidade), na.rm = TRUE)
limite_superior_g15 <- ceiling(max_cap_g15 * 1.1)
cores_consol       <- c("HEFA Soja" = "#385723", "ATJ E1G" = "#70ad47", "ATJ Milho" = "#b4cfa8")
cores_alt          <- c("HEFA Macaúba" = "#203864", "ATJ Agave" = "#5b9bd5", "ATJ E2G" = "#adc6e5")

# plot consolidadas
plot_g15_consol <- ggplot(df_g15_consol, aes(x = factor(Ano), y = Capacidade, fill = Materia)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, colour = "black") +
  scale_y_continuous(name = "Bilhões de litros", limits = c(0, limite_superior_g15), breaks = seq(0, limite_superior_g15, by = 1), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "", values = cores_consol) +
  labs(x = "Ano", title = "Matérias-primas consolidadas") +
  theme_monografia(base_size = 12, base_family = "sans") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)), legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal", axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# plot alternativas
plot_g15_alt <- ggplot(df_g15_alt, aes(x = factor(Ano), y = Capacidade, fill = Materia)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, colour = "black") +
  scale_y_continuous(name = "Bilhões de litros", limits = c(0, limite_superior_g15), breaks = seq(0, limite_superior_g15, by = 1), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(name = "", values = cores_alt) +
  labs(x = "Ano", title = "Matérias-primas alternativas") +
  theme_monografia(base_size = 12, base_family = "sans") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", margin = margin(b = 10)), legend.position = "top", legend.direction = "horizontal", legend.box = "horizontal", axis.title.x = element_text(face = "bold"), axis.title.y = element_text(face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# combinado
plot_grafico_15 <- plot_g15_consol + plot_g15_alt + plot_layout(nrow = 1) +
  plot_annotation(
    title = str_wrap("Gráfico 15 – Projeção da capacidade adicionada de produção de SAF com matérias-primas consolidadas e alternativas no Brasil, em bilhões de litros, de 2027 a 2037", 100),
    caption = "Fonte: Elaboração própria, com base em EPE (2024).",
    theme = theme(
      plot.title = element_text(face = "bold", size = 14, family = "Times New Roman"),
      plot.caption = element_text(size = 10, family = "Times New Roman", hjust = 0)
    )
  )

print(plot_grafico_15)

# --- grafico 16 ---------------------------------------------------------------
df_g16_demanda <- read_excel(dados_path, sheet = "g14") %>%
  rename(Materia = 1, Producao_2023 = matches("2023"), Demanda_2037 = matches("2037")) %>%
  mutate(Producao_2023 = as.numeric(Producao_2023), Demanda_2037 = as.numeric(Demanda_2037)) %>%
  pivot_longer(cols = c(Producao_2023, Demanda_2037), names_to = "Tipo", values_to = "Valor") %>%
  mutate(Tipo = recode(Tipo, "Producao_2023" = "Produção (2023)", "Demanda_2037" = "Demanda SAF (2037)"), Tipo = factor(Tipo, levels = c("Produção (2023)", "Demanda SAF (2037)")), Materia = factor(Materia, levels = rev(unique(Materia))))

max_val_g16        <- max(df_g16_demanda$Valor, na.rm = TRUE)
limite_superior_g16 <- ceiling((max_val_g16 * 1.1) / 5) * 5
cores_g16          <- c("Produção (2023)" = "#1b5e20", "Demanda SAF (2037)" = "#a5d6a7")

plot_grafico_16 <- ggplot(df_g16_demanda, aes(x = Materia, y = Valor, fill = Tipo)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7, colour = "black", linewidth = 0.3) +
  geom_text(aes(label = round(Valor, 1)), position = position_dodge(width = 0.8), hjust = -0.2, size = 4, family = "Times New Roman", color = "black") +
  scale_y_continuous(name = "Volume", limits = c(0, limite_superior_g16), breaks = seq(0, limite_superior_g16, by = 5), expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = cores_g16) +
  labs(
    title = str_wrap("Gráfico 16 – Projeção da demanda de matéria-prima consolidada para produção de SAF no Brasil, em bilhões de kg e litros, em 2037", 100),
    caption = str_wrap("Fonte: Elaboração própria, com base em ANP (2025b). Nota: Na produção de óleo de soja em 2023 foi subtraído o volume destinado para produção de biodiesel, estimado em 4,8 mil toneladas.", 120),
    x = NULL
  ) +
  coord_flip() +
  theme_monografia()

print(plot_grafico_16)

# 4. exportacao dos graficos ---------------------------------------------------

message(">> iniciando exportacao dos graficos...")

salvar_figura(plot_grafico_01, "grafico_01_demanda_transporte_aereo.svg")
salvar_figura(plot_grafico_02, "grafico_02_emissoes_co2_rpk.svg")
salvar_figura(plot_grafico_03, "grafico_03_emissoes_diretas_co2.svg")
salvar_figura(plot_grafico_04, "grafico_04_intensidade_carbono_global.svg")
salvar_figura(plot_grafico_05, "grafico_05_projecoes_emissoes_internacional.svg")
salvar_figura(plot_grafico_06, "grafico_06_participacao_aviacao_global.svg")
salvar_figura(plot_grafico_07, "grafico_07_projecao_consumo_combustivel.svg")
salvar_figura(plot_grafico_08, "grafico_08_projecao_emissoes_lacuna.svg")
salvar_figura(plot_grafico_10, "grafico_10_relacao_mfsp_ic.svg")
salvar_figura(plot_grafico_11, "grafico_11_projecao_producao_saf_rota.svg")
salvar_figura(plot_grafico_12, "grafico_12_projecao_participacao_fontes.svg")
salvar_figura(plot_grafico_13, "grafico_13_participacao_producao_bio_paises.svg")
salvar_figura(plot_grafico_14, "grafico_14_evolucao_producao_br_bio.svg")
salvar_figura(plot_grafico_15, "grafico_15_projecao_capacidade_saf_br.svg")
salvar_figura(plot_grafico_16, "grafico_16_projecao_demanda_materia_prima_br.svg")

message(">> geracao de figuras concluida com sucesso. verifique a pasta output/.")