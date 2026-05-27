############################################################
# OEC - Pautas exportadoras, commodities e ECI
# Países: Brasil, Noruega, Nigéria, EUA, Reino Unido, China,
#         Polônia, Indonésia, Coreia do Sul, Japão, Colômbia
# Ano: 2023
############################################################

# install.packages(c(
#   "tidyverse", "httr2", "jsonlite", "scales",
#   "rvest", "janitor", "writexl"
# ))

library(tidyverse)
library(httr2)
library(jsonlite)
library(scales)
library(rvest)
library(janitor)
library(writexl)

############################################################
# 1. Configurações
############################################################

ano <- 2023

# Para 2023, a OEC recomenda usar HS22, pois cobre 2022-2023.
cube <- "trade_i_baci_a_22"

# IDs OEC dos países (prefixo regional + ISO3):
# sa = América do Sul | na = América do Norte | eu = Europa
# as = Ásia          | af = África
paises_oec <- tibble(
  pais    = c("Brasil", "Noruega", "Nigéria",
              "EUA", "Reino Unido", "China",
              "Polônia", "Indonésia", "Coreia do Sul",
              "Japão", "Colômbia"),
  iso3    = c("BRA", "NOR", "NGA",
              "USA", "GBR", "CHN",
              "POL", "IDN", "KOR",
              "JPN", "COL"),
  exporter_id = c("sabra", "eunor", "afnga",
                  "nausa", "eungbr", "aschn",
                  "eupol", "asidn", "askor",
                  "asjpn", "sacol")
)

endpoint <- "https://api-v2.oec.world/tesseract/data.jsonrecords"

############################################################
# 2. Função para acessar a API da OEC
############################################################

oec_get <- function(params) {
  
  resp <- request(endpoint) |>
    req_url_query(!!!params) |>
    req_perform()
  
  out <- resp_body_json(resp, simplifyVector = TRUE)
  
  if (is.null(out$data) || length(out$data) == 0) {
    stop("A API retornou zero observações. Verifique o cube, o ano ou os IDs dos países.")
  }
  
  as_tibble(out$data)
}

############################################################
# 3. Baixar exportações por produto HS4
#    A API aceita até ~50 filtros por chamada; com 11 países
#    e limit = 50000 isso ainda cabe em uma única requisição.
############################################################

dados_raw <- oec_get(list(
  cube       = cube,
  drilldowns = "HS4,Exporter Country,Year",
  measures   = "Trade Value",
  include    = paste0(
    "Exporter Country:",
    paste(paises_oec$exporter_id, collapse = ","),
    ";Year:",
    ano
  ),
  parents    = "HS4",
  locale     = "en",
  limit      = "50000,0"   # aumentado para acomodar mais países
))

# Verificar nomes das colunas retornadas
names(dados_raw)

############################################################
# 4. Padronizar base
############################################################

dados <- dados_raw |>
  clean_names() |>
  rename(
    ano           = year,
    pais          = exporter_country,
    exporter_id   = exporter_country_id,
    produto_hs4   = hs4,
    produto_hs4_id = hs4_id,
    valor_exportado = trade_value
  ) |>
  mutate(valor_exportado = as.numeric(valor_exportado))

# Traduzir nomes para português usando o mapeamento centralizado
id_para_pt <- setNames(paises_oec$pais, paises_oec$exporter_id)

dados <- dados |>
  mutate(
    pais_pt = case_when(
      exporter_id == "sabra"   ~ "Brasil",
      exporter_id == "eunor"   ~ "Noruega",
      exporter_id == "afnga"   ~ "Nigéria",
      exporter_id == "nausa"   ~ "EUA",
      exporter_id == "eungbr"  ~ "Reino Unido",
      exporter_id == "aschn"   ~ "China",
      exporter_id == "eupol"   ~ "Polônia",
      exporter_id == "asidn"   ~ "Indonésia",
      exporter_id == "askor"   ~ "Coreia do Sul",
      exporter_id == "asjpn"   ~ "Japão",
      exporter_id == "sacol"   ~ "Colômbia",
      TRUE ~ pais
    )
  )

############################################################
# 5. Calcular total exportado e participação de cada produto
############################################################

dados_participacao <- dados |>
  group_by(pais_pt) |>
  mutate(
    total_exportado   = sum(valor_exportado, na.rm = TRUE),
    participacao_total = valor_exportado / total_exportado
  ) |>
  ungroup()

############################################################
# 6. Cinco principais produtos exportados por país
############################################################

top5_produtos <- dados_participacao |>
  group_by(pais_pt) |>
  slice_max(valor_exportado, n = 5, with_ties = FALSE) |>
  ungroup() |>
  arrange(pais_pt, desc(valor_exportado)) |>
  mutate(
    valor_exportado_bi_usd = valor_exportado / 1e9,
    participacao_pct       = 100 * participacao_total
  ) |>
  select(
    pais = pais_pt,
    ano,
    produto_hs4,
    valor_exportado_bi_usd,
    participacao_pct
  )

top5_produtos

############################################################
# 7. Classificação de commodities
############################################################

# Critério operacional:
# Commodity = bens primários ou de baixo processamento:
#   Agropecuários e alimentos ............. HS 01-24
#   Minerais e combustíveis ............... HS 25-27
#   Madeira, papel e celulose ............. HS 44-49
#   Fibras têxteis naturais ............... HS 50-53
#   Metais preciosos, ferro, aço e básicos  HS 71-83

extrair_hs2 <- function(hs4_id) {
  hs4_chr  <- str_replace_all(as.character(hs4_id), "\\D", "")
  hs4_code <- str_sub(hs4_chr, -4, -1)
  as.integer(str_sub(hs4_code, 1, 2))
}

dados_commodities <- dados_participacao |>
  mutate(
    hs2 = extrair_hs2(produto_hs4_id),
    commodity = case_when(
      hs2 >= 1  & hs2 <= 24 ~ TRUE,
      hs2 >= 25 & hs2 <= 27 ~ TRUE,
      hs2 >= 44 & hs2 <= 49 ~ TRUE,
      hs2 >= 50 & hs2 <= 53 ~ TRUE,
      hs2 >= 71 & hs2 <= 83 ~ TRUE,
      TRUE ~ FALSE
    )
  )

participacao_commodities <- dados_commodities |>
  group_by(pais_pt) |>
  summarise(
    total_exportado           = sum(valor_exportado, na.rm = TRUE),
    exportacoes_commodities   = sum(valor_exportado[commodity], na.rm = TRUE),
    participacao_commodities  = exportacoes_commodities / total_exportado,
    .groups = "drop"
  ) |>
  mutate(
    total_exportado_bi_usd            = total_exportado / 1e9,
    exportacoes_commodities_bi_usd    = exportacoes_commodities / 1e9,
    participacao_commodities_pct      = 100 * participacao_commodities
  ) |>
  select(
    pais = pais_pt,
    total_exportado_bi_usd,
    exportacoes_commodities_bi_usd,
    participacao_commodities_pct
  )

participacao_commodities

############################################################
# 8. Baixar ECI Trade 2023 da página de rankings da OEC
############################################################

url_eci <- "https://oec.world/en/country-rankings/2023/complexity"

# Nomes em inglês → português (todos os 11 países)
nomes_en_pt <- c(
  "Brazil"       = "Brasil",
  "Norway"       = "Noruega",
  "Nigeria"      = "Nigéria",
  "United States"= "EUA",
  "United Kingdom"= "Reino Unido",
  "China"        = "China",
  "Poland"       = "Polônia",
  "Indonesia"    = "Indonésia",
  "South Korea"  = "Coreia do Sul",
  "Japan"        = "Japão",
  "Colombia"     = "Colômbia"
)

eci_tabelas <- tryCatch(
  read_html(url_eci) |> html_table(fill = TRUE),
  error = function(e) NULL
)

if (!is.null(eci_tabelas) && length(eci_tabelas) > 0) {
  
  eci_raw <- eci_tabelas |>
    purrr::map(clean_names) |>
    purrr::keep(~ any(str_detect(names(.x), "country|economy|name"))) |>
    purrr::list_rbind()
  
  print(names(eci_raw))
  
  col_pais <- names(eci_raw)[str_detect(names(eci_raw), "country|economy|name")][1]
  col_eci  <- names(eci_raw)[str_detect(names(eci_raw), "eci|complexity|economic_complexity")][1]
  col_rank <- names(eci_raw)[str_detect(names(eci_raw), "rank")][1]
  
  if (is.na(col_pais) | is.na(col_eci)) {
    stop(
      "Não consegui detectar automaticamente as colunas de país e ECI. ",
      "Veja os nomes impressos por print(names(eci_raw))."
    )
  }
  
  eci_paises <- eci_raw |>
    mutate(
      country_tmp = as.character(.data[[col_pais]]),
      eci_tmp     = parse_number(as.character(.data[[col_eci]])),
      rank_tmp    = if (!is.na(col_rank)) as.character(.data[[col_rank]]) else NA_character_
    ) |>
    filter(country_tmp %in% names(nomes_en_pt)) |>
    transmute(
      pais          = nomes_en_pt[country_tmp],
      eci_trade     = eci_tmp,
      eci_trade_rank = rank_tmp
    )
  
} else {
  
  eci_paises <- tibble(
    pais           = paises_oec$pais,
    eci_trade      = NA_real_,
    eci_trade_rank = NA_character_
  )
}

eci_paises

############################################################
# 9. Tabela final comparativa
############################################################

tabela_final <- participacao_commodities |>
  left_join(eci_paises, by = "pais") |>
  arrange(desc(participacao_commodities_pct))

tabela_final

############################################################
# 10. Gráficos
############################################################

# --- 10a. Top-5 produtos por país ---
graf_top5 <- top5_produtos |>
  ggplot(aes(
    x = reorder(produto_hs4, participacao_pct),
    y = participacao_pct
  )) +
  geom_col() +
  coord_flip() +
  facet_wrap(~ pais, scales = "free_y") +
  labs(
    title    = "Cinco principais produtos exportados por país",
    subtitle = "11 países, 2023",
    x        = NULL,
    y        = "Participação no total exportado (%)",
    caption  = "Fonte: OEC/BACI via API. Elaboração própria."
  ) +
  theme_minimal(base_size = 9)

graf_top5

# --- 10b. Commodities × ECI (scatter) ---
graf_commodities_eci <- tabela_final |>
  ggplot(aes(
    x     = participacao_commodities_pct,
    y     = eci_trade,
    label = pais
  )) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel()  +   # instale ggrepel se não tiver
  scale_y_reverse() +
  # geom_text(nudge_y = 0.05) +  # alternativa sem ggrepel
  labs(
    title    = "Dependência de commodities e complexidade econômica",
    subtitle = "11 países, 2023",
    x        = "Commodities no total exportado (%)",
    y        = "ECI Trade",
    caption  = "Fonte: OEC/BACI e OEC Country Rankings. Elaboração própria."
  ) +
  theme_minimal()

graf_commodities_eci

############################################################
# 11. Interpretação automática simples
############################################################

tabela_final |>
  mutate(
    leitura = case_when(
      participacao_commodities_pct > 70 & eci_trade < 0 ~
        "Alta dependência de commodities e baixa complexidade exportadora.",
      participacao_commodities_pct > 70 & eci_trade >= 0 ~
        "Alta dependência de commodities, mas com complexidade relativamente maior.",
      participacao_commodities_pct <= 70 & eci_trade < 0 ~
        "Dependência moderada de commodities e baixa complexidade.",
      TRUE ~
        "Menor dependência relativa de commodities e maior complexidade."
    )
  )