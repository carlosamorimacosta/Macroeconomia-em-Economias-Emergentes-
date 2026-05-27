############################################################
# 1. Pacotes
############################################################

# Instale se necessário:
# install.packages(c("WDI", "tidyverse", "ggrepel", "scales"))

library(WDI)
library(tidyverse)
library(ggrepel)
library(scales)

############################################################
# 2. Países e indicadores
############################################################

paises <- c(
  "BRA", # Brasil
  "CHN", # China
  "KOR", # Coreia do Sul
  "IND", # Índia
  "DEU",  # Alemanha
  "POL"
)

indicadores <- c(
  abertura_comercial_pct = "NE.TRD.GNFS.ZS",    # Trade (% of GDP) = (X + M)/PIB * 100
  crescimento_pib_real   = "NY.GDP.MKTP.KD.ZG", # GDP growth, annual %
  pib_pc_ppp             = "NY.GDP.PCAP.PP.KD"  # GDP per capita, PPP, constant international $
)

anos_selecionados_1990 <- c(1990, 2000, 2010, 2023)
anos_selecionados_1970 <- c(1970, 1980, 1990, 2000, 2010, 2023)

############################################################
# 3. Baixar dados anuais de 1970 a 2023
############################################################

# Baixamos desde 1970 para conseguir fazer as duas análises:
# 1970–2023 e 1990–2023.

dados_wdi <- WDI(
  country = paises,
  indicator = indicadores,
  start = 1970,
  end = 2023,
  extra = FALSE
)

############################################################
# 4. Organizar nomes dos países e variáveis
############################################################

dados <- dados_wdi %>%
  mutate(
    pais = case_when(
      iso3c == "BRA" ~ "Brasil",
      iso3c == "CHN" ~ "China",
      iso3c == "KOR" ~ "Coreia do Sul",
      iso3c == "IND" ~ "Índia",
      iso3c == "DEU" ~ "Alemanha",
      iso3c == "POL" ~ "Polônia",
      TRUE ~ country
    ),
    
    # Abertura comercial como proporção do PIB:
    # O Banco Mundial fornece NE.TRD.GNFS.ZS em % do PIB.
    # Portanto, dividimos por 100 para obter (X + M)/PIB.
    abertura_comercial = abertura_comercial_pct / 100
  ) %>%
  select(
    pais,
    iso3c,
    year,
    abertura_comercial,
    abertura_comercial_pct,
    crescimento_pib_real,
    pib_pc_ppp
  ) %>%
  arrange(pais, year)

############################################################
# 5. Tabelas com anos selecionados
############################################################

tabela_anos_pedidos_1990 <- dados %>%
  filter(year %in% anos_selecionados_1990) %>%
  mutate(
    periodo = "1990–2023",
    abertura_comercial_pct = round(abertura_comercial_pct, 2),
    abertura_comercial = round(abertura_comercial, 3),
    crescimento_pib_real = round(crescimento_pib_real, 2),
    pib_pc_ppp = round(pib_pc_ppp, 2)
  ) %>%
  select(
    periodo,
    pais,
    iso3c,
    year,
    abertura_comercial,
    abertura_comercial_pct,
    crescimento_pib_real,
    pib_pc_ppp
  )

tabela_anos_pedidos_1970 <- dados %>%
  filter(year %in% anos_selecionados_1970) %>%
  mutate(
    periodo = "1970–2023",
    abertura_comercial_pct = round(abertura_comercial_pct, 2),
    abertura_comercial = round(abertura_comercial, 3),
    crescimento_pib_real = round(crescimento_pib_real, 2),
    pib_pc_ppp = round(pib_pc_ppp, 2)
  ) %>%
  select(
    periodo,
    pais,
    iso3c,
    year,
    abertura_comercial,
    abertura_comercial_pct,
    crescimento_pib_real,
    pib_pc_ppp
  )

print(tabela_anos_pedidos_1990)
print(tabela_anos_pedidos_1970)

############################################################
# 6. Função para calcular médias por período
############################################################

calcular_medias_periodo <- function(base, ano_inicial, ano_final, nome_periodo) {
  
  base %>%
    filter(year >= ano_inicial, year <= ano_final) %>%
    group_by(pais, iso3c) %>%
    summarise(
      periodo = nome_periodo,
      ano_inicial = ano_inicial,
      ano_final = ano_final,
      n_obs_abertura = sum(!is.na(abertura_comercial_pct)),
      n_obs_crescimento = sum(!is.na(crescimento_pib_real)),
      n_obs_pib_pc_ppp = sum(!is.na(pib_pc_ppp)),
      abertura_media_pct = mean(abertura_comercial_pct, na.rm = TRUE),
      abertura_media = mean(abertura_comercial, na.rm = TRUE),
      crescimento_medio = mean(crescimento_pib_real, na.rm = TRUE),
      pib_pc_ppp_ano_final = pib_pc_ppp[year == ano_final][1],
      .groups = "drop"
    )
}

############################################################
# 7. Calcular médias para 1990–2023 e 1970–2023
############################################################

dados_grafico_1990 <- calcular_medias_periodo(
  base = dados,
  ano_inicial = 1990,
  ano_final = 2023,
  nome_periodo = "1990–2023"
)

dados_grafico_1970 <- calcular_medias_periodo(
  base = dados,
  ano_inicial = 1970,
  ano_final = 2023,
  nome_periodo = "1970–2023"
)

print(dados_grafico_1990)
print(dados_grafico_1970)

############################################################
# 8. Correlação simples para cada período
############################################################

correlacao_1990 <- cor(
  dados_grafico_1990$abertura_media_pct,
  dados_grafico_1990$crescimento_medio,
  use = "complete.obs"
)

correlacao_1970 <- cor(
  dados_grafico_1970$abertura_media_pct,
  dados_grafico_1970$crescimento_medio,
  use = "complete.obs"
)

tabela_correlacoes <- tibble(
  periodo = c("1990–2023", "1970–2023"),
  correlacao_abertura_crescimento = c(correlacao_1990, correlacao_1970)
) %>%
  mutate(
    correlacao_abertura_crescimento = round(correlacao_abertura_crescimento, 3)
  )

print(tabela_correlacoes)

############################################################
# 9. Gráfico de dispersão — 1990–2023
############################################################

grafico_1990 <- ggplot(
  dados_grafico_1990,
  aes(
    x = abertura_media_pct,
    y = crescimento_medio,
    label = pais
  )
) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  geom_text_repel(size = 4) +
  labs(
    title = "Abertura comercial e crescimento médio do PIB real",
    subtitle = paste0(
      "Brasil, China, Coreia do Sul, Índia e Alemanha | Média anual, 1990–2023 | Correlação = ",
      round(correlacao_1990, 3)
    ),
    x = "Grau médio de abertura comercial — (Exportações + Importações) / PIB (%)",
    y = "Crescimento médio anual do PIB real (%)",
    caption = "Fonte: World Development Indicators, Banco Mundial."
  ) +
  theme_minimal()

print(grafico_1990)

############################################################
# 10. Gráfico de dispersão — 1970–2023
############################################################

grafico_1970 <- ggplot(
  dados_grafico_1970,
  aes(
    x = abertura_media_pct,
    y = crescimento_medio,
    label = pais
  )
) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  geom_text_repel(size = 4) +
  labs(
    title = "Abertura comercial e crescimento médio do PIB real",
    subtitle = paste0(
      "Brasil, China, Coreia do Sul, Índia e Alemanha | Média anual, 1970–2023 | Correlação = ",
      round(correlacao_1970, 3)
    ),
    x = "Grau médio de abertura comercial — (Exportações + Importações) / PIB (%)",
    y = "Crescimento médio anual do PIB real (%)",
    caption = "Fonte: World Development Indicators, Banco Mundial."
  ) +
  theme_minimal()

print(grafico_1970)

############################################################
# 11. Gráfico comparativo com os dois períodos
############################################################

dados_grafico_comparativo <- bind_rows(
  dados_grafico_1990,
  dados_grafico_1970
)

grafico_comparativo <- ggplot(
  dados_grafico_comparativo,
  aes(
    x = abertura_media_pct,
    y = crescimento_medio,
    label = pais
  )
) +
  geom_point(size = 4) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  geom_text_repel(size = 3.5) +
  facet_wrap(~ periodo) +
  labs(
    title = "Abertura comercial e crescimento médio do PIB real",
    subtitle = "Comparação entre os períodos 1990–2023 e 1970–2023",
    x = "Grau médio de abertura comercial — (Exportações + Importações) / PIB (%)",
    y = "Crescimento médio anual do PIB real (%)",
    caption = "Fonte: World Development Indicators, Banco Mundial."
  ) +
  theme_minimal()

print(grafico_comparativo)

############################################################
# 12. Exportar resultados — opcional
############################################################

# write.csv(
#   tabela_anos_pedidos_1990,
#   "dados_wdi_anos_selecionados_1990_2023.csv",
#   row.names = FALSE
# )
# 
# write.csv(
#   tabela_anos_pedidos_1970,
#   "dados_wdi_anos_selecionados_1970_2023.csv",
#   row.names = FALSE
# )
# 
# write.csv(
#   dados_grafico_1990,
#   "dados_wdi_medias_1990_2023.csv",
#   row.names = FALSE
# )
# 
# write.csv(
#   dados_grafico_1970,
#   "dados_wdi_medias_1970_2023.csv",
#   row.names = FALSE
# )
# 
# write.csv(
#   tabela_correlacoes,
#   "correlacoes_abertura_crescimento.csv",
#   row.names = FALSE
# )
# 
# ggsave(
#   "grafico_abertura_crescimento_1990_2023.png",
#   grafico_1990,
#   width = 9,
#   height = 6,
#   dpi = 300
# )
# 
# ggsave(
#   "grafico_abertura_crescimento_1970_2023.png",
#   grafico_1970,
#   width = 9,
#   height = 6,
#   dpi = 300
# )
# 
# ggsave(
#   "grafico_abertura_crescimento_comparativo.png",
#   grafico_comparativo,
#   width = 11,
#   height = 6,
#   dpi = 300
# )