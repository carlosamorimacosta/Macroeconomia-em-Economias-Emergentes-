# Country Report — Polônia

Repositório do trabalho final da disciplina **Macroeconomia em Economias Emergentes**, do **3º ano da Graduação em Economia da FGV EESP**.

O projeto desenvolve um **Country Report da Polônia**, estruturado no formato de uma *research note* de banco de investimento internacional. O objetivo é analisar a economia polonesa a partir da perspectiva de investidores globais, combinando evidência empírica, literatura acadêmica e os principais conceitos discutidos ao longo do curso.

---

## Objetivo do projeto

O trabalho busca avaliar se a Polônia pode ser classificada como uma economia emergente, quais são seus principais fundamentos macroeconômicos e quais riscos podem afetar sua trajetória de crescimento, estabilidade fiscal, estabilidade externa e atratividade para investidores.

A análise segue a proposta da disciplina de integrar:

- dados macroeconômicos;
- análise institucional;
- comparação com outros emergentes;
- modelos e conceitos de macroeconomia internacional;
- avaliação de riscos e cenários prospectivos;
- recomendação de investimento.

O relatório não é apenas uma análise conjuntural. Ele procura interpretar a trajetória da Polônia à luz de temas como convergência, abertura comercial e financeira, armadilha da renda média, vulnerabilidade externa, dívida pública, inflação, política monetária e risco de sudden stop.

---

## Pergunta central

> **A Polônia ainda deve ser tratada como uma economia emergente ou já apresenta características próximas às de uma economia avançada?**

A partir dessa pergunta, o trabalho discute:

- a classificação da Polônia segundo diferentes critérios institucionais e econômicos;
- o processo de convergência em relação à Europa Ocidental;
- o papel da integração comercial e financeira com a União Europeia;
- a estrutura produtiva e o grau de sofisticação da economia;
- os riscos fiscais, monetários, externos e geopolíticos;
- a recomendação de investimento em ativos soberanos ou projetos de longo prazo.

---

## Estrutura do repositório

```text
.
├── README.md
├── articles/
│   ├── growth_convergence/
│   ├── trade_openness/
│   ├── financial_globalization/
│   ├── middle_income_trap/
│   ├── sudden_stop_original_sin_debt_intolerance/
│   └── currency_crises_current_account/
│
├── data/
│   ├── raw/
│   ├── processed/
│   └── metadata/
│
├── code/
│   ├── 01_data_collection.R
│   ├── 02_data_cleaning.R
│   ├── 03_descriptive_analysis.R
│   ├── 04_external_sector.R
│   ├── 05_fiscal_sustainability.R
│   ├── 06_scenarios.R
│   └── functions/
│
├── outputs/
│   ├── figures/
│   ├── tables/
│   └── regressions/
│
├── report/
│   ├── country_report_poland.pdf
│   ├── country_report_poland.tex
│   └── appendices/
│
├── presentation/
│   ├── country_report_poland_slides.pdf
│   └── country_report_poland_slides.pptx
│
└── references/
    ├── bibliography.bib
    └── ai_usage_statement.md
```

---

## Conteúdo do repositório

### `articles/`

Contém os artigos acadêmicos e textos obrigatórios utilizados na fundamentação teórica do trabalho. A bibliografia cobre os principais temas da disciplina:

- crescimento econômico e convergência;
- abertura comercial e crescimento;
- globalização financeira;
- armadilha da renda média;
- recursos naturais e doença holandesa;
- sudden stops;
- debt intolerance;
- original sin;
- crises cambiais;
- conta corrente e desequilíbrios globais.

### `data/`

Contém as bases utilizadas na análise empírica.

As fontes recomendadas incluem:

- **FMI — World Economic Outlook (WEO)**;
- **FMI — Balance of Payments Statistics**;
- **FMI — External Sector Report**;
- **World Bank — World Development Indicators**;
- **BIS — Bank for International Settlements**;
- **FRED — Federal Reserve Economic Data**;
- **Bloomberg / Refinitiv / JP Morgan EMBI**, quando disponível;
- **Banco Central da Polônia / Narodowy Bank Polski**;
- **Eurostat**, quando relevante para comparações europeias.

### `code/`

Contém os códigos em R usados para baixar, limpar, transformar e analisar os dados.

Os scripts estão organizados de forma sequencial:

1. coleta dos dados;
2. limpeza e padronização;
3. análise descritiva;
4. setor externo;
5. sustentabilidade fiscal;
6. construção de cenários;
7. geração de gráficos e tabelas finais.

### `outputs/`

Contém os produtos intermediários gerados pelos códigos, como gráficos, tabelas e eventuais resultados econométricos.

### `report/`

Contém a versão final do **Country Report da Polônia**, em PDF, além dos arquivos editáveis utilizados na produção do relatório.

### `presentation/`

Contém a apresentação final do trabalho, elaborada para simular um **Emerging Markets Investor Meeting**, com foco em oportunidades, riscos e recomendação de investimento.

### `references/`

Contém a bibliografia em formato `.bib` e a declaração de uso de inteligência artificial, quando aplicável.

---

## Estrutura analítica do Country Report

O relatório segue a estrutura sugerida para uma *research note* de banco de investimento internacional:

1. **Executive Summary**
   - visão geral da economia polonesa;
   - principais riscos macroeconômicos;
   - perspectivas para crescimento, inflação e política econômica;
   - recomendação de investimento.

2. **Estrutura da economia**
   - PIB e PIB per capita;
   - estrutura produtiva;
   - grau de abertura comercial e financeira;
   - integração com a União Europeia;
   - mercado doméstico e diversificação produtiva.

3. **Ciclo econômico e convergência**
   - crescimento dos últimos 20 anos;
   - comparação com peers emergentes e economias avançadas europeias;
   - investimento, consumo e produtividade;
   - evidências de convergência ou desaceleração.

4. **Setor externo**
   - conta corrente;
   - balança comercial;
   - termos de troca;
   - reservas internacionais;
   - dívida externa;
   - vulnerabilidade a sudden stops.

5. **Política monetária**
   - regime monetário;
   - inflação;
   - taxa de juros;
   - credibilidade do banco central;
   - relação com o ciclo europeu e global.

6. **Política fiscal e dívida pública**
   - resultado primário e nominal;
   - trajetória da dívida pública;
   - composição da dívida;
   - análise de sustentabilidade fiscal com base em:

   ```text
   Δb = (r − g)b − sp
   ```

7. **Sistema financeiro**
   - crédito ao setor privado;
   - profundidade financeira;
   - estabilidade bancária;
   - riscos de descasamento cambial.

8. **Riscos macroeconômicos**
   - risco fiscal;
   - risco político e institucional;
   - risco geopolítico;
   - risco de desaceleração europeia;
   - risco inflacionário;
   - risco externo e de fluxos de capitais.

9. **Cenários prospectivos**
   - cenário base;
   - cenário otimista;
   - cenário adverso.

10. **Conclusão e recomendação de investimento**
    - avaliação dos fundamentos;
    - balanço entre riscos e oportunidades;
    - recomendação para títulos soberanos ou projetos de infraestrutura.

---

## Principais conceitos do curso utilizados

O trabalho mobiliza conceitos centrais da disciplina, entre eles:

- economia emergente;
- convergência absoluta e condicional;
- armadilha da renda média;
- abertura comercial;
- globalização financeira;
- sudden stop;
- debt intolerance;
- original sin;
- crises cambiais de primeira, segunda e terceira geração;
- sustentabilidade fiscal;
- sustentabilidade externa;
- vulnerabilidade macroeconômica;
- recomendação de investimento soberano.

---

## Critérios empíricos de análise

A Polônia é avaliada a partir de indicadores como:

- PIB real e crescimento do PIB;
- PIB per capita em dólares correntes e/ou PPP;
- inflação;
- taxa de juros;
- dívida pública como proporção do PIB;
- resultado primário e nominal;
- conta corrente;
- reservas internacionais;
- dívida externa;
- câmbio;
- abertura comercial;
- abertura financeira;
- crédito ao setor privado;
- indicadores institucionais;
- produtividade e estrutura produtiva;
- comparação com peers, como Hungria, República Tcheca, Romênia, Turquia e Coreia do Sul.

---

## Apresentação final

A apresentação final segue o formato de um **pitch para comitê de investimento global**.

Estrutura sugerida:

1. **País e contexto macro** — por que a Polônia importa para investidores?
2. **Fundamentos macroeconômicos** — crescimento, fiscal, externo e monetário.
3. **Vulnerabilidades** — sudden stop, debt intolerance, original sin, riscos políticos e geopolíticos.
4. **Cenários prospectivos** — base, otimista e adverso.
5. **Recomendação de investimento** — compra, manutenção, venda ou aguardar.

---

## Como reproduzir a análise

1. Clone o repositório:

```bash
git clone https://github.com/SEU-USUARIO/country-report-poland.git
cd country-report-poland
```

2. Instale os pacotes necessários no R:

```r
install.packages(c(
  "tidyverse",
  "WDI",
  "readxl",
  "janitor",
  "lubridate",
  "scales",
  "ggrepel",
  "countrycode",
  "kableExtra"
))
```

3. Execute os scripts em ordem:

```r
source("code/01_data_collection.R")
source("code/02_data_cleaning.R")
source("code/03_descriptive_analysis.R")
source("code/04_external_sector.R")
source("code/05_fiscal_sustainability.R")
source("code/06_scenarios.R")
```

4. Os gráficos e tabelas finais serão salvos em:

```text
outputs/figures/
outputs/tables/
```

---

## Observações metodológicas

A análise combina abordagem descritiva, comparação internacional e interpretação teórica. O foco não é estimar um modelo causal completo, mas construir uma avaliação macroeconômica consistente, baseada em dados e alinhada à literatura da disciplina.

Sempre que possível, os gráficos devem apresentar:

- título claro;
- período analisado;
- unidade de medida;
- país ou grupo de comparação;
- fonte dos dados;
- observação metodológica, quando necessário.

---

## Autoria

Trabalho desenvolvido para a disciplina **Macroeconomia em Economias Emergentes**, FGV EESP, 2026.

**Aluno:** Carlos Henrique Costa  
**Curso:** Graduação em Economia — 3º ano  
**Instituição:** Fundação Getulio Vargas — Escola de Economia de São Paulo

---

## Licença e uso

Este repositório tem finalidade acadêmica. Os dados utilizados pertencem às respectivas fontes oficiais. Os artigos e materiais de leitura devem ser utilizados apenas conforme as regras de acesso e direitos autorais das editoras, bases de dados e instituições responsáveis.

---

## Declaração sobre uso de inteligência artificial

Caso ferramentas de inteligência artificial tenham sido utilizadas na organização do código, revisão textual, geração de gráficos ou estruturação do relatório, o uso deverá ser informado no arquivo:

```text
references/ai_usage_statement.md
```

A declaração deve indicar:

- qual ferramenta foi utilizada;
- em quais etapas do projeto;
- quais prompts foram relevantes;
- quais partes foram revisadas ou validadas pelos autores.
