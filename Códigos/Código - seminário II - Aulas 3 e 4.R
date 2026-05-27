############################################################
# QUESTÃO 3 — VIX, risco-país, fluxos de capitais e câmbio
# Países emergentes selecionados
############################################################

# ----------------------------------------------------------
# 0. Pacotes
# ----------------------------------------------------------

pacotes <- c(
  "tidyverse",
  "lubridate",
  "readr",
  "WDI",
  "countrycode",
  "janitor",
  "scales",
  "ggplot2",
  "corrplot",
  "openxlsx",
  "zoo"
)

instalar <- pacotes[!pacotes %in% installed.packages()[, "Package"]]

if (length(instalar) > 0) {
  install.packages(instalar)
}

library(tidyverse)
library(lubridate)
library(readr)
library(WDI)
library(countrycode)
library(janitor)
library(scales)
library(ggplot2)
library(corrplot)
library(openxlsx)
library(zoo)

# ----------------------------------------------------------
# 1. Parâmetros gerais
# ----------------------------------------------------------

ano_inicial <- 1995
ano_final   <- as.integer(format(Sys.Date(), "%Y"))

# Você pode trocar os países aqui.
# Sugestão: países emergentes com câmbio relevante e dados razoáveis.
paises <- c(
  "BRA", # Brasil
  "MEX", # México
  "ZAF", # África do Sul
  "TUR", # Turquia
  "IND", # Índia
  "CHL", # Chile
  "COL"  # Colômbia
)

# Pasta para salvar resultados
dir.create("resultados_questao3", showWarnings = FALSE)


############################################################
# 2. Baixar VIX diário pelo FRED
############################################################

baixar_vix_fred <- function() {
  
  url_vix <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=VIXCLS"
  
  vix <- readr::read_csv(url_vix, show_col_types = FALSE) %>%
    janitor::clean_names() %>%
    rename(
      data = observation_date,
      vix = vixcls
    ) %>%
    mutate(
      data = as.Date(data),
      vix = suppressWarnings(as.numeric(vix))
    ) %>%
    filter(!is.na(vix)) %>%
    arrange(data)
  
  return(vix)
}

vix_diario <- baixar_vix_fred()

# VIX mensal: média e máximo mensal
vix_mensal <- vix_diario %>%
  mutate(
    ano = year(data),
    mes = month(data),
    data_mensal = as.Date(as.yearmon(data))
  ) %>%
  group_by(data_mensal, ano, mes) %>%
  summarise(
    vix_medio_mensal = mean(vix, na.rm = TRUE),
    vix_max_mensal   = max(vix, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(ano >= ano_inicial, ano <= ano_final)

# VIX anual: média, máximo e variação anual
vix_anual <- vix_diario %>%
  mutate(ano = year(data)) %>%
  group_by(ano) %>%
  summarise(
    vix_medio_anual = mean(vix, na.rm = TRUE),
    vix_max_anual   = max(vix, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ano) %>%
  mutate(
    delta_vix_medio_anual = vix_medio_anual - lag(vix_medio_anual),
    delta_vix_max_anual   = vix_max_anual - lag(vix_max_anual)
  ) %>%
  filter(ano >= ano_inicial, ano <= ano_final)


############################################################
# 3. Baixar dados anuais do World Bank
############################################################

# ► Passar códigos SEM nomes no vetor — evita bug da API do WDI
codigos_wdi <- c(
  "BM.KLT.DINV.CD.WD",   # FDI net outflows
  "BN.KLT.PTXL.CD",      # Portfolio investment, net
  "PA.NUS.FCRF",          # Official exchange rate, LCU per US$
  "NY.GDP.MKTP.CD"        # GDP current US$
)

dados_wdi_raw <- WDI::WDI(
  country   = paises,
  indicator = codigos_wdi,
  start     = ano_inicial,
  end       = ano_final,
  extra     = TRUE
)

# Renomear manualmente após o download
dados_wdi <- dados_wdi_raw %>%
  janitor::clean_names() %>%
  rename(
    pais              = country,
    ano               = year,
    ide_saida_usd     = bm_klt_dinv_cd_wd,
    portfolio_liquido_usd = bn_klt_ptxl_cd,
    cambio_lcu_usd    = pa_nus_fcrf,
    pib_usd           = ny_gdp_mktp_cd
  ) %>%
  select(
    pais, iso2c, iso3c, ano,
    ide_saida_usd,
    portfolio_liquido_usd,
    cambio_lcu_usd,
    pib_usd
  ) %>%
  arrange(iso3c, ano) %>%
  group_by(iso3c) %>%
  mutate(
    ide_saida_pct_pib             = 100 * ide_saida_usd / pib_usd,
    portfolio_liquido_pct_pib     = 100 * portfolio_liquido_usd / pib_usd,
    portfolio_saida_proxy_pct_pib = -100 * portfolio_liquido_usd / pib_usd,
    depreciacao_cambial_pct       = 100 * (log(cambio_lcu_usd) - log(lag(cambio_lcu_usd)))
  ) %>%
  ungroup()


############################################################
# 4. Base anual: VIX + fluxos + câmbio
############################################################

base_anual <- dados_wdi %>%
  left_join(vix_anual, by = "ano") %>%
  arrange(iso3c, ano)

# Visualizar
print(base_anual)


############################################################
# 5. Correlações anuais:
#    VIX x IDE de saída
#    VIX x Portfólio
#    VIX x Câmbio
############################################################

calcular_correlacoes_por_pais <- function(df, var_vix) {
  
  df %>%
    group_by(pais, iso3c) %>%
    summarise(
      n_obs = sum(!is.na(.data[[var_vix]])),
      
      cor_vix_ide_saida_pct_pib = cor(
        .data[[var_vix]],
        ide_saida_pct_pib,
        use = "pairwise.complete.obs"
      ),
      
      cor_vix_portfolio_liquido_pct_pib = cor(
        .data[[var_vix]],
        portfolio_liquido_pct_pib,
        use = "pairwise.complete.obs"
      ),
      
      cor_vix_portfolio_saida_proxy_pct_pib = cor(
        .data[[var_vix]],
        portfolio_saida_proxy_pct_pib,
        use = "pairwise.complete.obs"
      ),
      
      cor_vix_depreciacao_cambial_pct = cor(
        .data[[var_vix]],
        depreciacao_cambial_pct,
        use = "pairwise.complete.obs"
      ),
      
      cor_vix_cambio_nivel = cor(
        .data[[var_vix]],
        cambio_lcu_usd,
        use = "pairwise.complete.obs"
      ),
      
      .groups = "drop"
    ) %>%
    arrange(desc(abs(cor_vix_depreciacao_cambial_pct)))
}

correlacoes_vix_medio <- calcular_correlacoes_por_pais(
  base_anual,
  "vix_medio_anual"
)

correlacoes_vix_max <- calcular_correlacoes_por_pais(
  base_anual,
  "vix_max_anual"
)

print(correlacoes_vix_medio)
print(correlacoes_vix_max)


############################################################
# 6. Testes simples de correlação com p-valor
############################################################

cor_test_seguro <- function(x, y) {
  
  ok <- complete.cases(x, y)
  
  if (sum(ok) < 5) {
    return(tibble(
      correlacao = NA_real_,
      p_valor = NA_real_,
      n = sum(ok)
    ))
  }
  
  teste <- cor.test(x[ok], y[ok])
  
  tibble(
    correlacao = unname(teste$estimate),
    p_valor = teste$p.value,
    n = sum(ok)
  )
}

correlacoes_com_pvalor <- base_anual %>%
  group_by(pais, iso3c) %>%
  group_modify(~ {
    
    bind_rows(
      cor_test_seguro(.x$vix_medio_anual, .x$ide_saida_pct_pib) %>%
        mutate(relacao = "VIX médio x IDE saída (% PIB)"),
      
      cor_test_seguro(.x$vix_medio_anual, .x$portfolio_saida_proxy_pct_pib) %>%
        mutate(relacao = "VIX médio x saída de portfólio proxy (% PIB)"),
      
      cor_test_seguro(.x$vix_medio_anual, .x$depreciacao_cambial_pct) %>%
        mutate(relacao = "VIX médio x depreciação cambial (%)"),
      
      cor_test_seguro(.x$vix_max_anual, .x$ide_saida_pct_pib) %>%
        mutate(relacao = "VIX máximo x IDE saída (% PIB)"),
      
      cor_test_seguro(.x$vix_max_anual, .x$portfolio_saida_proxy_pct_pib) %>%
        mutate(relacao = "VIX máximo x saída de portfólio proxy (% PIB)"),
      
      cor_test_seguro(.x$vix_max_anual, .x$depreciacao_cambial_pct) %>%
        mutate(relacao = "VIX máximo x depreciação cambial (%)")
    )
    
  }) %>%
  ungroup() %>%
  select(pais, iso3c, relacao, correlacao, p_valor, n) %>%
  arrange(iso3c, relacao)

print(correlacoes_com_pvalor)


############################################################
# 7. Incorporar EMBI+ ou CDS de 5 anos
############################################################

# IMPORTANTE:
# EMBI+ país e CDS 5 anos geralmente vêm de bases pagas
# ou de planilhas do professor/Bloomberg/Refinitiv/Datastream/JP Morgan.
#
# O código abaixo espera um arquivo CSV chamado:
# "dados_risco_pais.csv"
#
# Formato esperado:
# data,iso3c,risco_pais
# 2000-01-31,BRA,850
# 2000-02-29,BRA,790
# ...
#
# risco_pais pode ser:
# - EMBI+ em pontos-base
# - CDS soberano de 5 anos em pontos-base

arquivo_risco <- "dados_risco_pais.csv"

if (file.exists(arquivo_risco)) {
  
  risco_pais_mensal <- readr::read_csv(
    arquivo_risco,
    show_col_types = FALSE
  ) %>%
    janitor::clean_names() %>%
    mutate(
      data = as.Date(data),
      ano = year(data),
      mes = month(data),
      data_mensal = as.Date(as.yearmon(data)),
      risco_pais = as.numeric(risco_pais)
    ) %>%
    filter(iso3c %in% paises) %>%
    arrange(iso3c, data_mensal)
  
  # VIX mensal + risco-país mensal
  base_risco_mensal <- risco_pais_mensal %>%
    left_join(
      vix_mensal %>%
        select(data_mensal, vix_medio_mensal, vix_max_mensal),
      by = "data_mensal"
    ) %>%
    arrange(iso3c, data_mensal) %>%
    group_by(iso3c) %>%
    mutate(
      delta_risco_pais = risco_pais - lag(risco_pais),
      delta_vix_medio = vix_medio_mensal - lag(vix_medio_mensal),
      delta_vix_max = vix_max_mensal - lag(vix_max_mensal)
    ) %>%
    ungroup()
  
  # Correlação VIX x risco-país em nível e variação
  correlacoes_risco <- base_risco_mensal %>%
    group_by(iso3c) %>%
    summarise(
      n_obs = sum(complete.cases(vix_medio_mensal, risco_pais)),
      
      cor_vix_medio_risco_nivel = cor(
        vix_medio_mensal,
        risco_pais,
        use = "pairwise.complete.obs"
      ),
      
      cor_vix_max_risco_nivel = cor(
        vix_max_mensal,
        risco_pais,
        use = "pairwise.complete.obs"
      ),
      
      cor_delta_vix_delta_risco = cor(
        delta_vix_medio,
        delta_risco_pais,
        use = "pairwise.complete.obs"
      ),
      
      .groups = "drop"
    )
  
  print(correlacoes_risco)
  
} else {
  
  message(
    "Arquivo 'dados_risco_pais.csv' não encontrado. ",
    "Para calcular VIX x EMBI/CDS, coloque um CSV com colunas: data, iso3c, risco_pais."
  )
  
  risco_pais_mensal <- NULL
  base_risco_mensal <- NULL
  correlacoes_risco <- NULL
}


############################################################
# 8. Gráficos: VIX e câmbio anual por país
############################################################

grafico_vix_cambio <- base_anual %>%
  filter(!is.na(vix_medio_anual), !is.na(depreciacao_cambial_pct)) %>%
  ggplot(aes(x = vix_medio_anual, y = depreciacao_cambial_pct)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ iso3c, scales = "free_y") +
  labs(
    title = "VIX e depreciação cambial anual",
    subtitle = "Depreciação calculada como variação logarítmica da taxa LCU/US$",
    x = "VIX médio anual",
    y = "Depreciação cambial anual (%)"
  ) +
  theme_minimal()

print(grafico_vix_cambio)

ggsave(
  filename = "resultados_questao3/grafico_vix_cambio.png",
  plot = grafico_vix_cambio,
  width = 12,
  height = 7,
  dpi = 300
)


############################################################
# 9. Gráficos: VIX e saída de portfólio
############################################################

grafico_vix_portfolio <- base_anual %>%
  filter(!is.na(vix_medio_anual), !is.na(portfolio_saida_proxy_pct_pib)) %>%
  ggplot(aes(x = vix_medio_anual, y = portfolio_saida_proxy_pct_pib)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ iso3c, scales = "free_y") +
  labs(
    title = "VIX e saída líquida de portfólio — proxy anual",
    subtitle = "Proxy = - investimento de portfólio líquido / PIB",
    x = "VIX médio anual",
    y = "Saída de portfólio proxy (% do PIB)"
  ) +
  theme_minimal()

print(grafico_vix_portfolio)

ggsave(
  filename = "resultados_questao3/grafico_vix_portfolio.png",
  plot = grafico_vix_portfolio,
  width = 12,
  height = 7,
  dpi = 300
)


############################################################
# 10. Gráficos: VIX e IDE de saída
############################################################

grafico_vix_ide <- base_anual %>%
  filter(!is.na(vix_medio_anual), !is.na(ide_saida_pct_pib)) %>%
  ggplot(aes(x = vix_medio_anual, y = ide_saida_pct_pib)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ iso3c, scales = "free_y") +
  labs(
    title = "VIX e IDE líquido de saída",
    subtitle = "IDE líquido de saída em % do PIB",
    x = "VIX médio anual",
    y = "IDE de saída (% do PIB)"
  ) +
  theme_minimal()

print(grafico_vix_ide)

ggsave(
  filename = "resultados_questao3/grafico_vix_ide.png",
  plot = grafico_vix_ide,
  width = 12,
  height = 7,
  dpi = 300
)


############################################################
# 11. Gráfico opcional: VIX x risco-país mensal
############################################################

if (!is.null(base_risco_mensal)) {
  
  grafico_vix_risco <- base_risco_mensal %>%
    filter(!is.na(vix_medio_mensal), !is.na(risco_pais)) %>%
    ggplot(aes(x = vix_medio_mensal, y = risco_pais)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ iso3c, scales = "free_y") +
    labs(
      title = "VIX e prêmio de risco-país",
      subtitle = "Risco-país pode ser EMBI+ ou CDS soberano de 5 anos, em pontos-base",
      x = "VIX médio mensal",
      y = "EMBI+/CDS 5 anos"
    ) +
    theme_minimal()
  
  print(grafico_vix_risco)
  
  ggsave(
    filename = "resultados_questao3/grafico_vix_risco_pais.png",
    plot = grafico_vix_risco,
    width = 12,
    height = 7,
    dpi = 300
  )
}


############################################################
# 12. Matriz de correlação agregada
############################################################

base_cor_agregada <- base_anual %>%
  select(
    vix_medio_anual,
    vix_max_anual,
    ide_saida_pct_pib,
    portfolio_liquido_pct_pib,
    portfolio_saida_proxy_pct_pib,
    cambio_lcu_usd,
    depreciacao_cambial_pct
  )

matriz_cor <- cor(
  base_cor_agregada,
  use = "pairwise.complete.obs"
)

print(round(matriz_cor, 3))

png(
  filename = "resultados_questao3/matriz_correlacao_agregada.png",
  width = 1200,
  height = 900
)

corrplot::corrplot(
  matriz_cor,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.cex = 0.8,
  number.cex = 0.7,
  diag = FALSE
)

dev.off()


############################################################
# 13. Exportar resultados para Excel
############################################################

wb <- createWorkbook()

addWorksheet(wb, "vix_diario")
writeData(wb, "vix_diario", vix_diario)

addWorksheet(wb, "vix_mensal")
writeData(wb, "vix_mensal", vix_mensal)

addWorksheet(wb, "vix_anual")
writeData(wb, "vix_anual", vix_anual)

addWorksheet(wb, "base_anual")
writeData(wb, "base_anual", base_anual)

addWorksheet(wb, "cor_vix_medio")
writeData(wb, "cor_vix_medio", correlacoes_vix_medio)

addWorksheet(wb, "cor_vix_max")
writeData(wb, "cor_vix_max", correlacoes_vix_max)

addWorksheet(wb, "cor_pvalor")
writeData(wb, "cor_pvalor", correlacoes_com_pvalor)

addWorksheet(wb, "matriz_cor_agregada")
writeData(wb, "matriz_cor_agregada", as.data.frame(matriz_cor), rowNames = TRUE)

if (!is.null(correlacoes_risco)) {
  addWorksheet(wb, "cor_vix_risco")
  writeData(wb, "cor_vix_risco", correlacoes_risco)
  
  addWorksheet(wb, "base_risco_mensal")
  writeData(wb, "base_risco_mensal", base_risco_mensal)
}

saveWorkbook(
  wb,
  file = "resultados_questao3/resultados_vix_emergentes.xlsx",
  overwrite = TRUE
)


############################################################
# 14. Interpretação automática simples
############################################################

interpretacao <- correlacoes_vix_medio %>%
  mutate(
    interpretacao_cambio = case_when(
      cor_vix_depreciacao_cambial_pct > 0.3 ~
        "Correlação positiva: anos de VIX mais alto tendem a coincidir com depreciação cambial.",
      cor_vix_depreciacao_cambial_pct < -0.3 ~
        "Correlação negativa: resultado contrário ao esperado; investigar commodities, política doméstica e amostra.",
      TRUE ~
        "Correlação fraca: relação pouco clara em frequência anual."
    ),
    interpretacao_portfolio = case_when(
      cor_vix_portfolio_saida_proxy_pct_pib > 0.3 ~
        "Correlação positiva: VIX maior tende a coincidir com saída de portfólio.",
      cor_vix_portfolio_saida_proxy_pct_pib < -0.3 ~
        "Correlação negativa: resultado contrário ao esperado; verificar sinal contábil e composição dos fluxos.",
      TRUE ~
        "Correlação fraca: relação pouco clara em frequência anual."
    )
  )

print(interpretacao)

write_csv(
  interpretacao,
  "resultados_questao3/interpretacao_automatica.csv"
)

############################################################
# FIM
############################################################

############################################################
# 2B. Gráfico da cotação histórica do VIX
############################################################

# Eventos marcantes para anotação
eventos <- tribble(
  ~data,          ~rotulo,
  "1997-10-27",   "Crise Ásia",
  "1998-08-17",   "Crise Rússia/LTCM",
  "2001-09-11",   "11/Set",
  "2002-10-09",   "Bolha dot-com",
  "2008-10-24",   "Crise Subprime",
  "2010-05-20",   "Crise Euro",
  "2011-08-08",   "Rebaixamento EUA",
  "2015-08-24",   "Flash crash China",
  "2018-02-05",   "Volmageddon",
  "2020-03-16",   "Covid-19",
  "2022-03-07",   "Guerra Ucrânia"
) %>%
  mutate(data = as.Date(data)) %>%
  left_join(vix_diario, by = "data")   # puxa o VIX do dia exato

# Gráfico principal
grafico_vix_historico <- ggplot(vix_diario, aes(x = data, y = vix)) +
  
  # Área sombreada
  geom_area(fill = "#2166ac", alpha = 0.15) +
  
  # Linha principal
  geom_line(color = "#2166ac", linewidth = 0.4) +
  
  # Faixas de referência
  geom_hline(yintercept = 20,  linetype = "dashed", color = "gray50",  linewidth = 0.4) +
  geom_hline(yintercept = 30,  linetype = "dashed", color = "orange",  linewidth = 0.4) +
  geom_hline(yintercept = 40,  linetype = "dashed", color = "red",     linewidth = 0.4) +
  
  # Anotações de eventos
  geom_point(
    data = eventos %>% filter(!is.na(vix)),
    aes(x = data, y = vix),
    color = "red", size = 2, shape = 21, fill = "white", stroke = 1
  ) +
  geom_text(
    data = eventos %>% filter(!is.na(vix)),
    aes(x = data, y = vix + 3, label = rotulo),
    size = 2.5, angle = 45, hjust = 0, color = "gray20"
  ) +
  
  # Escala do eixo X: um tick a cada 2 anos
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = c(0.01, 0)
  ) +
  
  scale_y_continuous(
    breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90),
    expand = c(0.01, 0)
  ) +
  
  labs(
    title    = "VIX — Índice de Volatilidade Implícita (CBOE)",
    subtitle = paste0(
      "Dados diários | ",
      format(min(vix_diario$data), "%b/%Y"),
      " a ",
      format(max(vix_diario$data), "%b/%Y"),
      " | Fonte: FRED/St. Louis Fed"
    ),
    x        = NULL,
    y        = "VIX (pontos)",
    caption  = paste0(
      "Linhas de referência: 20 pts (volatilidade moderada), ",
      "30 pts (estresse), 40 pts (crise)"
    )
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 14),
    plot.subtitle    = element_text(color = "gray40", size = 10),
    plot.caption     = element_text(color = "gray50", size = 8),
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90")
  )

print(grafico_vix_historico)

ggsave(
  filename = "resultados_questao3/grafico_vix_historico.png",
  plot     = grafico_vix_historico,
  width    = 16,
  height   = 7,
  dpi      = 300
)

############################################################
# CDS SOBERANO 5 ANOS — RASPAGEM WEB
# Fonte: countryeconomy.com
############################################################

# Pacotes adicionais necessários
pacotes_extra <- c("rvest", "httr")
instalar_extra <- pacotes_extra[!pacotes_extra %in% installed.packages()[,"Package"]]
if (length(instalar_extra) > 0) install.packages(instalar_extra)

library(rvest)
library(httr)

# ----------------------------------------------------------
# Mapeamento país → slug da URL
# ----------------------------------------------------------

paises_cds_url <- list(
  BRA = "brazil",
  USA = "usa",
  CHN = "china",
  CHL = "chile",
  MEX = "mexico",
  TUR = "turkey",
  ZAF = "south-africa"
)

nomes_paises <- c(
  BRA = "Brasil",
  USA = "EUA",
  CHN = "China",
  CHL = "Chile",
  MEX = "México",
  TUR = "Turquia",
  ZAF = "África do Sul"
)

cores_paises <- c(
  BRA = "#009C3B",
  USA = "#3C3B6E",
  CHN = "#DE2910",
  CHL = "#D52B1E",
  MEX = "#006847",
  TUR = "#E30A17",
  ZAF = "#007A4D"
)

# ----------------------------------------------------------
# Função de raspagem
# ----------------------------------------------------------

raspar_cds_countryeconomy <- function(iso3c, slug) {
  
  url <- paste0("https://countryeconomy.com/cds/", slug)
  
  message("Baixando CDS: ", iso3c, " → ", url)
  
  # Headers para simular navegador (evita bloqueio 403)
  resposta <- tryCatch(
    httr::GET(
      url,
      httr::add_headers(
        `User-Agent`      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
        `Accept-Language` = "pt-BR,pt;q=0.9,en-US;q=0.8",
        `Accept`          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"
      ),
      httr::timeout(30)
    ),
    error = function(e) {
      message("  ✗ Erro de conexão: ", conditionMessage(e))
      return(NULL)
    }
  )
  
  if (is.null(resposta) || httr::status_code(resposta) != 200) {
    message("  ✗ HTTP ", httr::status_code(resposta))
    return(NULL)
  }
  
  pagina <- httr::content(resposta, as = "text", encoding = "UTF-8") %>%
    rvest::read_html()
  
  # Extrair todas as tabelas da página
  tabelas <- pagina %>% rvest::html_table(fill = TRUE)
  
  if (length(tabelas) == 0) {
    message("  ✗ Nenhuma tabela encontrada")
    return(NULL)
  }
  
  # A tabela de CDS tem colunas de data e valor numérico
  # Identificar a tabela correta: busca a que tem coluna com "Date" ou data
  tabela_cds <- NULL
  
  for (i in seq_along(tabelas)) {
    t <- tabelas[[i]]
    nomes <- tolower(names(t))
    
    # Tabela de CDS geralmente tem 2-3 colunas com data e valor
    if (any(grepl("date|data|fecha", nomes)) || 
        (ncol(t) <= 4 && nrow(t) > 10)) {
      
      # Tentar parsear
      t_clean <- t %>%
        janitor::clean_names() %>%
        # Primeira coluna = data, segunda = valor CDS
        select(1:2) %>%
        setNames(c("data_raw", "cds_raw")) %>%
        filter(!is.na(data_raw), data_raw != "", 
               !grepl("date|data|fecha|cds", tolower(data_raw))) %>%
        mutate(
          # Formatos possíveis: "Jan 2008", "01/2008", "2008-01"
          data = dplyr::case_when(
            grepl("^[A-Za-z]{3}\\s\\d{4}$", data_raw) ~
              as.Date(paste0("01 ", data_raw), format = "%d %b %Y"),
            grepl("^\\d{2}/\\d{4}$", data_raw) ~
              as.Date(paste0("01/", data_raw), format = "%d/%m/%Y"),
            grepl("^\\d{4}-\\d{2}$", data_raw) ~
              as.Date(paste0(data_raw, "-01")),
            TRUE ~ suppressWarnings(as.Date(data_raw))
          ),
          cds_5y = suppressWarnings(
            as.numeric(gsub("[^0-9\\.]", "", cds_raw))
          )
        ) %>%
        filter(!is.na(data), !is.na(cds_5y), cds_5y > 0) %>%
        mutate(iso3c = iso3c) %>%
        select(iso3c, data, cds_5y)
      
      if (nrow(t_clean) > 5) {
        tabela_cds <- t_clean
        break
      }
    }
  }
  
  if (is.null(tabela_cds) || nrow(tabela_cds) == 0) {
    message("  ✗ Tabela CDS não identificada")
    return(NULL)
  }
  
  message("  ✓ ", nrow(tabela_cds), " observações | ",
          format(min(tabela_cds$data), "%m/%Y"), " a ",
          format(max(tabela_cds$data), "%m/%Y"))
  
  return(tabela_cds)
}

# ----------------------------------------------------------
# Baixar todos os países com pausa entre requisições
# ----------------------------------------------------------

lista_cds <- list()

for (iso in names(paises_cds_url)) {
  lista_cds[[iso]] <- raspar_cds_countryeconomy(iso, paises_cds_url[[iso]])
  Sys.sleep(runif(1, 1.5, 3))   # pausa aleatória entre 1,5 e 3 seg
}

# Consolidar
cds_bruto <- bind_rows(lista_cds[!sapply(lista_cds, is.null)])

# Cobertura obtida
cobertura_cds <- cds_bruto %>%
  group_by(iso3c) %>%
  summarise(
    inicio = min(data),
    fim    = max(data),
    n_obs  = n(),
    cds_medio = round(mean(cds_5y, na.rm = TRUE), 1),
    .groups = "drop"
  )

print(cobertura_cds)

# ----------------------------------------------------------
# Agregar para frequência mensal
# ----------------------------------------------------------

cds_mensal <- cds_bruto %>%
  mutate(
    data_mensal = as.Date(as.yearmon(data))
  ) %>%
  group_by(iso3c, data_mensal) %>%
  summarise(
    cds_mensal = mean(cds_5y, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    pais = nomes_paises[iso3c]
  ) %>%
  arrange(iso3c, data_mensal)

# ----------------------------------------------------------
# Unir com VIX mensal
# ----------------------------------------------------------

base_cds_vix <- cds_mensal %>%
  left_join(
    vix_mensal %>% select(data_mensal, vix_medio_mensal, vix_max_mensal),
    by = "data_mensal"
  ) %>%
  filter(!is.na(vix_medio_mensal), !is.na(cds_mensal)) %>%
  group_by(iso3c) %>%
  mutate(
    delta_cds = cds_mensal       - lag(cds_mensal),
    delta_vix = vix_medio_mensal - lag(vix_medio_mensal)
  ) %>%
  ungroup()


############################################################
# CORRELAÇÕES VIX x CDS
############################################################

correlacoes_cds <- base_cds_vix %>%
  group_by(iso3c, pais) %>%
  summarise(
    n_obs = n(),
    
    cor_nivel = cor(
      vix_medio_mensal, cds_mensal,
      use = "pairwise.complete.obs"
    ),
    cor_variacao = cor(
      delta_vix, delta_cds,
      use = "pairwise.complete.obs"
    ),
    p_nivel = {
      ok <- complete.cases(vix_medio_mensal, cds_mensal)
      if (sum(ok) >= 5) cor.test(vix_medio_mensal[ok], cds_mensal[ok])$p.value
      else NA_real_
    },
    p_variacao = {
      ok <- complete.cases(delta_vix, delta_cds)
      if (sum(ok) >= 5) cor.test(delta_vix[ok], delta_cds[ok])$p.value
      else NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(
    sig_nivel    = case_when(
      p_nivel    < 0.01 ~ "***",
      p_nivel    < 0.05 ~ "**",
      p_nivel    < 0.10 ~ "*",
      TRUE              ~ ""
    ),
    sig_variacao = case_when(
      p_variacao < 0.01 ~ "***",
      p_variacao < 0.05 ~ "**",
      p_variacao < 0.10 ~ "*",
      TRUE              ~ ""
    )
  ) %>%
  arrange(desc(cor_nivel))

print(correlacoes_cds)


############################################################
# GRÁFICO 1 — Barras: correlação em nível e variação
############################################################

cor_long <- correlacoes_cds %>%
  select(iso3c, pais, cor_nivel, cor_variacao, sig_nivel, sig_variacao) %>%
  pivot_longer(
    cols      = c(cor_nivel, cor_variacao),
    names_to  = "tipo",
    values_to = "correlacao"
  ) %>%
  mutate(
    tipo = recode(tipo,
                  cor_nivel    = "Em nível",
                  cor_variacao = "Em variação (Δ mensal)"
    ),
    sig  = ifelse(tipo == "Em nível", sig_nivel, sig_variacao),
    pais = factor(pais, levels = correlacoes_cds$pais)
  )

grafico_barras_cds <- ggplot(
  cor_long,
  aes(x = pais, y = correlacao, fill = correlacao)
) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "gray30", linewidth = 0.4) +
  geom_text(
    aes(
      label = paste0(round(correlacao, 2), sig),
      vjust = ifelse(correlacao >= 0, -0.4, 1.3)
    ),
    size = 3.8, fontface = "bold"
  ) +
  scale_fill_gradient2(
    low = "#d73027", mid = "#ffffbf", high = "#1a9850",
    midpoint = 0, limits = c(-1, 1)
  ) +
  scale_y_continuous(
    limits = c(-1, 1),
    breaks = seq(-1, 1, 0.25)
  ) +
  facet_wrap(~ tipo, ncol = 2) +
  labs(
    title    = "Correlação entre VIX e CDS Soberano 5 Anos",
    subtitle = "Dados mensais  |  * p<0,10  ** p<0,05  *** p<0,01",
    x = NULL,
    y = "Coeficiente de correlação de Pearson",
    caption  = "Fontes: FRED (VIX) | countryeconomy.com (CDS)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "gray40", size = 10),
    axis.text.x        = element_text(angle = 30, hjust = 1),
    panel.grid.major.x = element_blank(),
    strip.text         = element_text(face = "bold", size = 11)
  )

print(grafico_barras_cds)
ggsave("resultados_questao3/grafico_cor_vix_cds_barras.png",
       grafico_barras_cds, width = 13, height = 6, dpi = 300)


############################################################
# GRÁFICO 2 — Séries normalizadas (z-score) por país
############################################################

base_cds_norm <- base_cds_vix %>%
  group_by(iso3c) %>%
  mutate(
    vix_z = (vix_medio_mensal - mean(vix_medio_mensal, na.rm = TRUE)) /
      sd(vix_medio_mensal, na.rm = TRUE),
    cds_z = (cds_mensal - mean(cds_mensal, na.rm = TRUE)) /
      sd(cds_mensal, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(pais = nomes_paises[iso3c])

grafico_series_cds <- ggplot(base_cds_norm, aes(x = data_mensal)) +
  geom_area(aes(y = vix_z, fill = "VIX"), alpha = 0.2) +
  geom_line(aes(y = vix_z, color = "VIX"),   linewidth = 0.5) +
  geom_line(aes(y = cds_z, color = "CDS 5Y"), linewidth = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed",
             color = "gray60", linewidth = 0.3) +
  scale_color_manual(values = c("VIX" = "#2166ac", "CDS 5Y" = "#d6604d"), name = NULL) +
  scale_fill_manual(values  = c("VIX" = "#2166ac"), name = NULL, guide = "none") +
  facet_wrap(~ pais, ncol = 2, scales = "free_y") +
  scale_x_date(date_breaks = "4 years", date_labels = "%Y") +
  labs(
    title    = "VIX e CDS Soberano 5 Anos — Séries Normalizadas (z-score)",
    subtitle = "Normalização por país permite comparar dinâmicas na mesma escala",
    x = NULL, y = "Desvios-padrão em relação à média",
    caption  = "Fontes: FRED (VIX) | countryeconomy.com (CDS)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 13),
    legend.position = "top",
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    strip.text      = element_text(face = "bold")
  )

print(grafico_series_cds)
ggsave("resultados_questao3/grafico_series_vix_cds.png",
       grafico_series_cds, width = 14, height = 12, dpi = 300)


############################################################
# GRÁFICO 3 — Scatter por país com regressão
############################################################

grafico_scatter_cds <- base_cds_vix %>%
  mutate(pais = nomes_paises[iso3c]) %>%
  ggplot(aes(x = vix_medio_mensal, y = cds_mensal)) +
  geom_point(aes(color = iso3c), alpha = 0.4, size = 1.2, show.legend = FALSE) +
  geom_smooth(method = "lm", se = TRUE,
              color = "gray20", fill = "gray80", linewidth = 0.8) +
  scale_color_manual(values = cores_paises) +
  facet_wrap(~ pais, ncol = 2, scales = "free_y") +
  labs(
    title    = "VIX vs. CDS Soberano 5 Anos",
    subtitle = "Cada ponto = um mês  |  Linha = regressão linear simples",
    x = "VIX médio mensal (pontos)", y = "CDS 5 anos (pontos-base)",
    caption  = "Fontes: FRED (VIX) | countryeconomy.com (CDS)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    strip.text    = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(grafico_scatter_cds)
ggsave("resultados_questao3/grafico_scatter_vix_cds.png",
       grafico_scatter_cds, width = 12, height = 14, dpi = 300)


############################################################
# GRÁFICO 4 — Heatmap nível x variação
############################################################

grafico_heatmap_cds <- correlacoes_cds %>%
  select(pais, cor_nivel, cor_variacao) %>%
  pivot_longer(-pais, names_to = "tipo", values_to = "cor") %>%
  mutate(tipo = recode(tipo,
                       cor_nivel    = "Em nível",
                       cor_variacao = "Em variação"
  )) %>%
  ggplot(aes(x = tipo, y = reorder(pais, cor), fill = cor)) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = round(cor, 2)), size = 5, fontface = "bold",
            color = "gray10") +
  scale_fill_gradient2(
    low = "#d73027", mid = "#ffffbf", high = "#1a9850",
    midpoint = 0, limits = c(-1, 1), name = "Correlação"
  ) +
  scale_x_discrete(position = "top") +
  labs(
    title    = "Heatmap — Correlação VIX × CDS Soberano 5 Anos",
    subtitle = "Em nível: séries originais  |  Em variação: primeira diferença mensal",
    x = NULL, y = NULL,
    caption  = "Fontes: FRED (VIX) | countryeconomy.com (CDS)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "gray40", size = 10),
    legend.position = "right"
  )

print(grafico_heatmap_cds)
ggsave("resultados_questao3/grafico_heatmap_vix_cds.png",
       grafico_heatmap_cds, width = 9, height = 7, dpi = 300)



