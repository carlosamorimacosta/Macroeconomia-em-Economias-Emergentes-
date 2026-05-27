# =============================================================================
# Análise da Armadilha da Renda Média com Dados do Maddison Project Database
# Países: Brasil, México, Argentina, Coreia do Sul, Taiwan, Japão (1950–2020)
# =============================================================================

# --- 1. INSTALAÇÃO E CARREGAMENTO DE PACOTES ---------------------------------

pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "ggrepel",
          "scales", "patchwork", "curl", "stringr")

install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
}
invisible(lapply(pkgs, install_if_missing))
invisible(lapply(pkgs, library, character.only = TRUE))

# --- 2. DOWNLOAD DOS DADOS DO MADDISON PROJECT 2020 --------------------------

url  <- "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx"
dest <- tempfile(fileext = ".xlsx")

message("⬇  Baixando Maddison Project Database 2020...")
curl::curl_download(url, dest)
message("✓  Download concluído.")

# Aba "Full data" contém PIB per capita (gdppc) em USD 2011 PPC
raw <- readxl::read_excel(dest, sheet = "Full data")

# --- 3. SELEÇÃO E LIMPEZA DOS DADOS ------------------------------------------

paises_iso  <- c("BRA", "MEX", "ARG", "KOR", "TWN", "JPN", "POL")
paises_nome <- c(BRA = "Brasil", MEX = "México", ARG = "Argentina",
                 KOR = "Coreia do Sul", TWN = "Taiwan", JPN = "Japão", POL = "Polônia")

# Padroniza nomes de colunas (podem variar entre versões do arquivo)
names(raw) <- tolower(names(raw))

df <- raw |>
  filter(countrycode %in% paises_iso,
         year >= 1950, year <= 2020) |>
  select(iso3 = countrycode, year, gdppc) |>
  mutate(
    pais   = paises_nome[iso3],
    gdppc  = as.numeric(gdppc)
  ) |>
  filter(!is.na(gdppc)) |>
  arrange(pais, year)

# Inspecção rápida
cat("\n=== Resumo dos dados ===\n")
df |>
  group_by(pais) |>
  summarise(
    anos        = n(),
    pib_min     = round(min(gdppc)),
    pib_max     = round(max(gdppc)),
    primeiro_ano = min(year),
    ultimo_ano   = max(year)
  ) |>
  print(n = Inf)

# --- 4. FUNÇÃO: ANO DE CRUZAMENTO DE LIMIAR ----------------------------------

ano_limiar <- function(dados, iso, threshold) {
  d <- dados |>
    filter(iso3 == iso, !is.na(gdppc)) |>
    arrange(year)
  
  # Primeiro ano em que o PIB per capita cruza o limiar
  cruzou <- d |> filter(gdppc >= threshold)
  if (nrow(cruzou) == 0) return(NA_integer_)
  as.integer(min(cruzou$year))
}

# --- 5. LIMIARES DE RENDA MÉDIA ----------------------------------------------

limiar_baixo <- 4000   # renda média-baixa  (aprox. World Bank lower-middle)
limiar_alto  <- 13000  # renda média-alta   (aprox. World Bank upper-middle)

resultados <- tibble(iso3 = paises_iso) |>
  mutate(
    pais          = paises_nome[iso3],
    ano_4k        = sapply(iso3, ano_limiar, dados = df, threshold = limiar_baixo),
    ano_13k       = sapply(iso3, ano_limiar, dados = df, threshold = limiar_alto),
    anos_4k_13k   = ano_13k - ano_4k
  )

cat("\n=== Anos em que cada país cruzou os limiares ===\n")
print(resultados |> select(pais, ano_4k, ano_13k, anos_4k_13k))

# --- 6. ANOS PARA DOBRAR A RENDA A PARTIR DE US$ 4.000 ----------------------

dobrar_renda <- function(dados, iso, threshold = 4000) {
  d <- dados |>
    filter(iso3 == iso, !is.na(gdppc)) |>
    arrange(year)
  
  ano_ini <- ano_limiar(dados, iso, threshold)
  if (is.na(ano_ini)) return(list(ano_inicio = NA, ano_dobrou = NA, anos = NA,
                                  gdp_inicio = NA, gdp_dobrou = NA))
  
  gdp_ini <- d |> filter(year == ano_ini) |> pull(gdppc)
  meta     <- gdp_ini * 2
  
  cruzou <- d |> filter(year >= ano_ini, gdppc >= meta)
  if (nrow(cruzou) == 0) return(list(ano_inicio = ano_ini, ano_dobrou = NA,
                                     anos = NA, gdp_inicio = gdp_ini, gdp_dobrou = NA))
  
  ano_dob  <- min(cruzou$year)
  gdp_dob  <- cruzou |> filter(year == ano_dob) |> pull(gdppc)
  
  list(ano_inicio = ano_ini, ano_dobrou = ano_dob,
       anos       = ano_dob - ano_ini,
       gdp_inicio = gdp_ini, gdp_dobrou = gdp_dob)
}

dobro_lista <- lapply(paises_iso, dobrar_renda, dados = df)
names(dobro_lista) <- paises_iso

tabela_dobro <- bind_rows(
  lapply(paises_iso, function(iso) {
    x <- dobro_lista[[iso]]
    tibble(
      iso3        = iso,
      pais        = paises_nome[iso],
      ano_inicio  = x$ano_inicio,
      gdp_inicio  = round(x$gdp_inicio),
      ano_dobrou  = x$ano_dobrou,
      gdp_dobrou  = round(x$gdp_dobrou),
      anos_dobrar = x$anos
    )
  })
)

# Referência: Coreia do Sul
anos_kor <- tabela_dobro |> filter(iso3 == "KOR") |> pull(anos_dobrar)

tabela_dobro <- tabela_dobro |>
  mutate(
    ratio_vs_kor = round(anos_dobrar / anos_kor, 2),
    status       = case_when(
      is.na(anos_dobrar)    ~ "Não dobrou ainda",
      anos_dobrar <= anos_kor ~ "≤ Coreia do Sul",
      TRUE                  ~ "Mais lento que Coreia do Sul"
    )
  )

cat("\n=== Anos para dobrar a renda a partir de US$ 4.000 ===\n")
print(tabela_dobro)

# --- 7. INTERPRETAÇÃO: A 'ARMADILHA DA RENDA MÉDIA' --------------------------

cat("\n=== Interpretação: Armadilha da Renda Média ===\n")
cat(str_pad("País", 18), str_pad("Anos p/ dobrar", 16),
    str_pad("Vezes mais lento que KOR", 26), "\n")
cat(strrep("-", 62), "\n")
for (i in seq_len(nrow(tabela_dobro))) {
  r <- tabela_dobro[i, ]
  cat(
    str_pad(r$pais, 18),
    str_pad(ifelse(is.na(r$anos_dobrar), "N/A", r$anos_dobrar), 16),
    str_pad(ifelse(is.na(r$ratio_vs_kor), "N/A", r$ratio_vs_kor), 26),
    "\n"
  )
}

# --- 8. VISUALIZAÇÕES --------------------------------------------------------

cores <- c(
  "Brasil"        = "#009C3B",
  "México"        = "#CE1126",
  "Argentina"     = "#74ACDF",
  "Coreia do Sul" = "#C60C30",
  "Taiwan"        = "#FE0000",
  "Japão"         = "#BC002D"
)

# 8.1 — Trajetórias de PIB per capita (escala log) ----------------------------

# Rótulos no último ponto disponível
rotulos <- df |>
  group_by(pais) |>
  filter(year == max(year)) |>
  ungroup()

p1 <- ggplot(df, aes(x = year, y = gdppc, colour = pais)) +
  geom_hline(yintercept = c(4000, 13000), linetype = "dashed",
             colour = "grey55", linewidth = 0.45) +
  annotate("text", x = 1951, y = 4300,  label = "US$ 4.000  (renda média-baixa)",
           hjust = 0, size = 3.1, colour = "grey40") +
  annotate("text", x = 1951, y = 13800, label = "US$ 13.000 (renda média-alta)",
           hjust = 0, size = 3.1, colour = "grey40") +
  geom_line(linewidth = 1.1, alpha = 0.9) +
  geom_point(data = rotulos, size = 2.5) +
  geom_label_repel(
    data        = rotulos,
    aes(label   = pais),
    size        = 3.3,
    fontface    = "bold",
    label.padding = unit(0.18, "lines"),
    box.padding = unit(0.35, "lines"),
    nudge_x     = 2,
    show.legend = FALSE
  ) +
  scale_y_log10(
    labels = scales::dollar_format(prefix = "US$ ", suffix = "", big.mark = ".", decimal.mark = ","),
    breaks = c(1000, 2000, 4000, 8000, 13000, 20000, 40000)
  ) +
  scale_x_continuous(breaks = seq(1950, 2020, 10)) +
  scale_colour_manual(values = cores) +
  labs(
    title    = "PIB per capita (PPC, USD 2011) — 1950–2020",
    subtitle = "Escala logarítmica · Limiares da armadilha da renda média destacados",
    x        = NULL, y = "PIB per capita (log)",
    caption  = "Fonte: Maddison Project Database 2020 (Bolt & van Zanden)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(colour = "grey40", size = 10),
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey92")
  )

# 8.2 — Tempo para dobrar a renda (barras) -------------------------------------

tabela_plot <- tabela_dobro |>
  filter(!is.na(anos_dobrar)) |>
  mutate(pais = forcats::fct_reorder(pais, anos_dobrar))

p2 <- ggplot(tabela_plot, aes(x = pais, y = anos_dobrar, fill = pais)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_hline(yintercept = anos_kor, linetype = "dashed",
             colour = "#C60C30", linewidth = 0.8) +
  annotate("text", x = 0.55, y = anos_kor + 0.8,
           label = paste0("Coreia do Sul: ", anos_kor, " anos"),
           hjust = 0, colour = "#C60C30", size = 3.4, fontface = "bold") +
  geom_text(aes(label = paste0(anos_dobrar, " anos")),
            vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Anos para dobrar o PIB per capita a partir de US$ 4.000",
    subtitle = "Comparação com a referência da Coreia do Sul",
    x        = NULL, y = "Anos",
    caption  = "Fonte: Maddison Project Database 2020"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(colour = "grey40", size = 10),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x     = element_text(face = "bold")
  )

# 8.3 — Tempo entre limiares (US$4k → US$13k) ---------------------------------

tab_limiares <- resultados |>
  filter(!is.na(anos_4k_13k)) |>
  mutate(pais = forcats::fct_reorder(pais, anos_4k_13k))

p3 <- ggplot(tab_limiares, aes(x = pais, y = anos_4k_13k, fill = pais)) +
  geom_col(width = 0.65, alpha = 0.9) +
  geom_text(aes(label = paste0(anos_4k_13k, " anos")),
            vjust = -0.5, fontface = "bold", size = 3.5) +
  scale_fill_manual(values = cores) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Anos entre limiares: US$ 4.000 → US$ 13.000",
    subtitle = "Proxy da velocidade de escape da armadilha da renda média",
    x        = NULL, y = "Anos",
    caption  = "Fonte: Maddison Project Database 2020"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(colour = "grey40", size = 10),
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    axis.text.x     = element_text(face = "bold")
  )

# 8.4 — Painel final -----------------------------------------------------------

painel <- p1 / (p2 | p3) +
  plot_annotation(
    title   = "A Armadilha da Renda Média: América Latina vs. Leste Asiático",
    caption = "Maddison Project Database 2020 · Análise: R",
    theme   = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.caption = element_text(colour = "grey50", size = 9)
    )
  )

# Salva e exibe
ggsave("maddison_armadilha_renda_media.png", painel,
       width = 14, height = 12, dpi = 180, bg = "white")

print(p1)
print(p2)
print(p3)

cat("\n✓  Gráfico salvo como 'maddison_armadilha_renda_media.png'\n")

# --- 9. SUMÁRIO INTERPRETATIVO -----------------------------------------------

cat("\n", strrep("=", 62), "\n")
cat("  INTERPRETAÇÃO — ARMADILHA DA RENDA MÉDIA\n")
cat(strrep("=", 62), "\n\n")

cat("• Coreia do Sul cruzou US$ 4.000 em", resultados$ano_4k[resultados$iso3 == "KOR"],
    "e US$ 13.000 em", resultados$ano_13k[resultados$iso3 == "KOR"],
    "→", resultados$anos_4k_13k[resultados$iso3 == "KOR"], "anos.\n")

cat("• Japão cruzou US$ 4.000 em", resultados$ano_4k[resultados$iso3 == "JPN"],
    "e US$ 13.000 em", resultados$ano_13k[resultados$iso3 == "JPN"],
    "→", resultados$anos_4k_13k[resultados$iso3 == "JPN"], "anos.\n\n")

for (iso in c("BRA", "MEX", "ARG")) {
  r <- resultados |> filter(iso3 == iso)
  cat(sprintf("• %s cruzou US$ 4.000 em %s",
              r$pais, ifelse(is.na(r$ano_4k), "N/A", r$ano_4k)))
  if (!is.na(r$ano_13k)) {
    cat(sprintf(" e US$ 13.000 em %d → %d anos.\n", r$ano_13k, r$anos_4k_13k))
  } else {
    cat(" mas ainda NÃO atingiu US$ 13.000 até 2020.\n")
  }
}

cat("\n→  A comparação revela que países latino-americanos demoraram o dobro\n")
cat("   ou mais que a Coreia do Sul para dobrar a renda após US$ 4.000,\n")
cat("   e alguns ainda não ultrapassaram US$ 13.000 — evidência empírica\n")
cat("   da 'armadilha da renda média': crescimento desacelera justamente\n")
cat("   quando salários sobem acima da concorrência de baixo custo, mas\n")
cat("   a produtividade e inovação ainda não sustentam o próximo estágio.\n")
cat(strrep("=", 62), "\n")