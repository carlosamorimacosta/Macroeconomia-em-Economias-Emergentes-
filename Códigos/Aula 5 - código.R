############################################################
# 1. Pacotes
############################################################

# install.packages(c("tidyverse", "WDI", "readxl", "janitor", "scales", "ggrepel"))

library(tidyverse)
library(WDI)
library(readxl)
library(janitor)
library(scales)
library(ggrepel)

############################################################
# 2. Países e anos
############################################################

paises <- c(
  "BRA", # Brasil
  "CHN", # China
  "KOR", # Coreia do Sul
  "MEX", # México
  "TUR",  # Turquia
  "POL"
)

nomes_paises <- tibble(
  iso3c = paises,
  pais = c("Brasil", "China", "Coreia do Sul", "México", "Turquia", "Polônia")
)

anos_escolhidos <- c(1990, 2000, 2010, 2022)

############################################################
# 3. Baixar abertura comercial do Banco Mundial
#    Indicador: NE.TRD.GNFS.ZS = Trade (% of GDP)
############################################################

abertura_comercial <- WDI(
  country = paises,
  indicator = c(abertura_comercial = "NE.TRD.GNFS.ZS"),
  start = min(anos_escolhidos),
  end = max(anos_escolhidos),
  extra = TRUE
) %>%
  as_tibble() %>%
  filter(year %in% anos_escolhidos) %>%
  transmute(
    iso3c,
    pais_wdi = country,
    ano = year,
    abertura_comercial
  ) %>%
  left_join(nomes_paises, by = "iso3c") %>%
  select(pais, iso3c, ano, abertura_comercial)

############################################################
# 4. Ler KAOPEN - opção A: arquivo local
############################################################
# Baixe o arquivo no site:
# http://web.pdx.edu/~ito/Chinn-Ito_website.htm
#
# Depois salve, por exemplo, como:
# "kaopen_2022.xlsx" ou "kaopen_2022.csv"
#
# Ajuste o caminho abaixo.

arquivo_kaopen <- "C:/Users/carlo/Downloads/3º ano - EESP/Macroeconomia em economias emergentes/kaopen_2023.xls"

############################################################
# 5. Função para ler KAOPEN de Excel ou CSV
############################################################

ler_kaopen <- function(arquivo) {
  
  extensao <- tools::file_ext(arquivo)
  
  if (extensao %in% c("xlsx", "xls")) {
    df <- read_excel(arquivo)
  } else if (extensao == "csv") {
    df <- read_csv(arquivo, show_col_types = FALSE)
  } else if (extensao == "dta") {
    stop("Arquivo .dta detectado. Instale e use haven::read_dta().")
  } else {
    stop("Formato não reconhecido. Use .xlsx, .xls ou .csv.")
  }
  
  df %>%
    clean_names()
}

kaopen_raw <- ler_kaopen(arquivo_kaopen)

############################################################
# 6. Inspecionar nomes das colunas do KAOPEN
############################################################

names(kaopen_raw)

############################################################
# 7. Padronizar a base KAOPEN
############################################################
# O nome das colunas pode variar conforme a versão do arquivo.
# Em geral, precisamos de:
# - país ou código ISO
# - ano
# - kaopen

kaopen <- kaopen_raw %>%
  rename_with(~ str_replace_all(.x, "\\.", "_")) %>%
  mutate(across(everything(), ~ .x)) 

# Tente identificar automaticamente colunas relevantes

col_iso <- names(kaopen)[
  str_detect(names(kaopen), "iso3|iso_3|ccode|country_code|code")
][1]

col_pais <- names(kaopen)[
  str_detect(names(kaopen), "country|pais|name")
][1]

col_ano <- names(kaopen)[
  str_detect(names(kaopen), "^year$|ano")
][1]

col_kaopen <- names(kaopen)[
  str_detect(names(kaopen), "kaopen|ka_open|kaopen_index")
][1]

cat("Coluna ISO encontrada:", col_iso, "\n")
cat("Coluna país encontrada:", col_pais, "\n")
cat("Coluna ano encontrada:", col_ano, "\n")
cat("Coluna KAOPEN encontrada:", col_kaopen, "\n")

############################################################
# 8. Construir base KAOPEN padronizada
############################################################

kaopen_padronizado <- kaopen %>%
  transmute(
    iso3c = if (!is.na(col_iso)) as.character(.data[[col_iso]]) else NA_character_,
    pais_kaopen = if (!is.na(col_pais)) as.character(.data[[col_pais]]) else NA_character_,
    ano = as.integer(.data[[col_ano]]),
    kaopen = as.numeric(.data[[col_kaopen]])
  ) %>%
  filter(
    ano %in% anos_escolhidos,
    iso3c %in% paises
  ) %>%
  left_join(nomes_paises, by = "iso3c") %>%
  select(pais, iso3c, ano, kaopen)

############################################################
# 9. Juntar KAOPEN com abertura comercial
############################################################

base_final <- kaopen_padronizado %>%
  left_join(
    abertura_comercial,
    by = c("pais", "iso3c", "ano")
  ) %>%
  arrange(pais, ano)

base_final