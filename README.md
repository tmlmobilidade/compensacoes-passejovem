# TML — Compensações do Passe Gratuito para Jovens (Navegante)

Pipeline em **R** para o cálculo mensal da compensação financeira dos passes gratuitos para jovens (sub 23 anos) na Área Metropolitana de Lisboa (AML), a reportar ao **IMT**.

Aplica a metodologia da **Portaria n.º 7-A/2024** (alterada pela Portaria 307-A/2024 e pela Portaria 287/2026/1):

- **Leitura e limpeza** dos dados de bilhética (ITS), Cascais e complemento Azambuja
- **Correções** de NIF, datas de nascimento e repescagem de NIFs em falta
- **Agregação por titular**: por NIF (**comNIF**) ou por documento / cartão (**semNIF**)
- **Compensação**: 5 % do título de referência por validação (< 20 validações); título completo a partir de 20 validações
- **Listas de controlo**: registos a verificar, idades incompatíveis e documentos de identificação irregulares
- **Reporte Excel (xlsx)** no template oficial AML/IMT (comNIF, semNIF e RESUMO)
- **Histórico** mensal atualizado automaticamente (`compensacao_pj_anteriores.csv`)
- **Informação 1 + Ofício 1 (PDF/DOCX)** com os resultados do mês

O processo divide-se em duas fases:


| Fase                                    | O que faz                                                                                                                  | Scripts    | Saídas principais                                                                                                          |
| ----------------------------------------- | ---------------------------------------------------------------------------------------------------------------------------- | ------------ | ----------------------------------------------------------------------------------------------------------------------------- |
| **Fase 1 — Apuramento (IMT)**          | Calcula a compensação mensal por titular e reporta ao IMT                                                                | `01`–`03` | Reporte AML/IMT, listas de controlo, `informacao1`, `oficio1`, `metodologia1`                                               |
| **Fase 2 — Repartição (operadores)** | Reparte a compensação pelos 11 operadores/autoridades segundo as quotas mensais e estima o Congelamento Tarifário (TAT) | `04`–`05` | `passejovem_resumo.csv`, `passejovem_congelamento.csv`, Anexo II (xlsx), Anexo I (`metodologia2`), `informacao2`, `oficio2` |

A Fase 2 só se corre depois da Fase 1 e quando as **quotas mensais** estão disponíveis no repositório `Compensacoes` (repositório [`compensacoes-navegante`](https://github.com/tmlmobilidade/compensacoes-navegante), por omissão clonado em `../Compensacoes/`; outro caminho via variável de ambiente `COMPENSACOES_ROOT`).

> [!NOTE]
> Nesta versão, só são suportados os meses **a partir de julho de 2026** (formato XLSX com `doc_type`/`doc_number` e template v2). Os meses anteriores (maio, junho) tinham requisitos diferentes e já não são mantidos.

---

## 🗓️ Como Preparar um Novo Mês (ex: Agosto de 2026)

Seguir os passos **por esta ordem**.

### 1. Criar a pasta de dados do mês

Cria `data/2026_08_agosto/input/` (a pasta `data/` está no `.gitignore`). A pasta `output/` é criada automaticamente.

### 2. Colocar os ficheiros de input

```
data/2026_08_agosto/input/
├── 202608_PasseJovem_ValidaçõesLimpas.xlsx   ← ITS (folha 1: validações; folha "Azambuja": complemento)
└── TML_agosto2026.xlsx                          ← Cascais (Viver Cascais)
```


| Ficheiro                 | Origem  | Notas                                                                                                                                                                                                                                          |
| -------------------------- | --------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `*PasseJovem*.xlsx`      | ITS     | O nome tem de conter`PasseJovem`. Colunas: `YEAR_DIST, MONTH_DIST, CARD_SERIAL_NUMBER, AGE, FISCAL_NUMBER, TICKET_OPERATOR_CODE, TICKET_CODE, TICKET_NAME, PRICE, PRICE_TICKET_BASE, COUNT_VAL, BIRTH_DATE, ID_DOC_NUMBER, ID_DOC_NUMBER_DESC` |
| `TML_<mes><ano>.xlsx`    | Cascais | Preço vem a 0; o pipeline fixa`PRECO_CASCAIS = 20` e operador `21`                                                                                                                                                                            |
| `*Complemento_Azambuja*` | ITS     | Opcional. Se não existir em ficheiro separado, é lido da folha com "Azambuja" no nome do XLSX principal                                                                                                                                      |

### 3. Atualizar os ficheiros auxiliares (`data/auxiliar/`)


| Ficheiro                        | Conteúdo                                                                      | Quando atualizar               |
| --------------------------------- | -------------------------------------------------------------------------------- | -------------------------------- |
| `nif_repescagem2024.csv`        | `card_serial_number → fiscal_number` (preenche NIFs em falta)                 | Quando surgem NIFs recuperados |
| `nif_correct.csv`               | `card_serial_number; birth_date; fiscal_number` (NIFs/datas errados)           | Quando se detetam erros        |
| `data_nascimento_correct.csv`   | `card_serial_number; dn_errada; fiscal_number; dn_corrigida`                   | Idem                           |
| `id_exception.csv`              | `doc_type; doc_number; motivo` — documentos a **agrupar** apesar de anomalias | Casos validados manualmente    |
| `compensacao_pj_anteriores.csv` | Histórico mensal                                                              | Atualizado automaticamente     |

### 4. Verificar o template do reporte

O reporte é uma cópia do template `Exemplos/Template_PASSE GRATUITO JOVEM - REPORTE - CIM_AT para IMT (Mensal) - 2026 - V2.xlsx`. Não o alteres.

### 5. Parâmetros dos documentos (`code/doc_params.R`)

Editar `ANO`, `MES`, `mes_nome`, `meses_pagos_ano_atual`, os pagamentos ETF, os números de referência (`numero_informacao1`, `numero_oficio1`, e na Fase 2 `numero_informacao2`, `numero_oficio2`) e a `data_documento`.

### 5b. Inputs da Fase 2 (repositório `Compensacoes`)


| Ficheiro                 | Caminho                                                            | Notas                                                                                          |
| -------------------------- | -------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------ |
| Quotas do mês           | `Compensacoes/data/<mm>_<mes>_<ano>/output/1_quotas_<mes><aa>.csv` | Obrigatório; o mês tem de existir                                                            |
| `titulos.csv`, `tat.csv` | `Compensacoes/lists/`                                              | Se não existirem localmente, são lidos do GitHub. Atualizar `tat.csv` com a taxa do novo ano |

### 6. Ambiente R

Os pacotes R necessários são instalados automaticamente por `code/00_config.R`.
Para os documentos é necessário o **Quarto CLI**.

---

## 🚀 Como Executar

A partir da raiz do projeto:

```powershell
# Um mês
Rscript code/99_run_pipeline.R 2026 8

# Vários meses
Rscript code/99_run_pipeline.R 2026 8 9

# Todos os meses de 2026 em data/
Rscript code/99_run_pipeline.R 2026
```

No R / RStudio:

```r
source("code/99_run_pipeline.R")
run_pipeline(ano = 2026, meses = 8)
```

Controlo das fases em `code/00_config.R` (ou como argumentos de `run_pipeline()`):


| Opção       | Efeito                                                             |
| --------------- | -------------------------------------------------------------------- |
| `RUN_FASE1`   | `TRUE` corre a Fase 1 (`01`–`03` + documentos 1); `FALSE` salta-a |
| `RUN_FASE2`   | `TRUE` corre a Fase 2 (`04`–`05` + documentos 2); `FALSE` salta-a |
| `RENDER_DOCS` | `TRUE` renderiza os documentos Quarto das fases ativas             |

```r
# Só Fase 2 (Fase 1 já corrida), sem documentos
run_pipeline(ano = 2026, meses = 7, run_fase1 = FALSE, run_fase2 = TRUE, render_docs = FALSE)
```

Cada passo pode ser corrido isoladamente (`preparar_dados()`, `calcular_compensacoes()`, `exportar_reporte()`, `estimar_compensacao_operadores()`, `exportar_anexo_reparticao()`). O processamento demora 10 a 60 s por mês (~450 000 registos, via `data.table`). Se um mês falhar, o erro é registado e o pipeline segue para o seguinte.

### Pipeline

```mermaid
flowchart LR
    A[input/ XLSX ITS + Cascais + Azambuja] --> B[01_preparar_dados.R]
    X[data/auxiliar/ correções] --> B
    B --> C[limpos_preagregacao.csv]
    C --> D[02_calcular_compensacoes.R]
    D --> E[comNIF / semNIF / a_verificar / age_nok]
    E --> F[03_exportar_reporte.R]
    F --> G[reporte_AML_IMT.xlsx + histórico]
    G --> H[10_documentos.R → Informação 1 + Ofício 1 PDF/DOCX]
    E --> I[04_estimar_compensacao_operadores.R]
    Q[Compensacoes: quotas, titulos, tat] --> I
    I --> J[resumo.csv + congelamento.csv]
    J --> K[05_exportar_anexo_reparticao.R]
    K --> L[AnexoII_reparticao.xlsx]
    J --> M[10_documentos.R → Anexo I + Informação 2 + Ofício 2]
```

Fase 1: `01 → 02 → 03`. Fase 2: `04 → 05`.

---

## 📄 Como Gerar os Documentos

```powershell
Rscript code/10_documentos.R            # usa ANO/MES de doc_params.R
Rscript code/10_documentos.R 2026 8     # mês específico
```

Renderiza para PDF (Typst) e DOCX (controlado por `RENDER_PDF` / `RENDER_DOCX` em `doc_params.R`) e copia para `output/docs/`:

- **Fase 1:** `informacao1.qmd`, `oficio1.qmd` (PDF + DOCX) e `metodologia1.qmd` (PDF).
- **Fase 2:** `metodologia2.qmd` → `AnexoI_metodologia.pdf`, `informacao2.qmd` e `oficio2.qmd` (PDF + DOCX). Requerem que a Fase 2 já tenha sido corrida.
  Para regenerar os estilos Word: `Rscript code/templates/build_reference_doc.R` (fecha o Word antes).

---

## 📊 Ficheiros de Saída

Em `data/AAAA_MM_mes/output/`:


| Ficheiro                                     | Descrição                                                                                       |
| ---------------------------------------------- | --------------------------------------------------------------------------------------------------- |
| **`AAAA_MM_reporte_AML_IMT.xlsx`**           | **Reporte oficial ao IMT**: folhas ComNIF, SemNIF e RESUMO                                        |
| `AAAA_MM_comNIF.csv`                         | Compensação por NIF                                                                             |
| `AAAA_MM_semNIF.csv`                         | Compensação por documento (ou cartão) para quem não tem NIF                                   |
| `AAAA_MM_a_verificar.csv`                    | Registos suspeitos para revisão manual (exceção ID, datas distintas, sem NIF, idade > 23)      |
| `AAAA_MM_age_nok.csv`                        | Registos excluídos por idade incompatível (3 ou ≥ 25 anos)                                     |
| `AAAA_MM_documentos_estranhos_semNIF.csv`    | Documentos semNIF anómalos (zeros, noves, repetidos, < 6 dígitos, partilhados…)                |
| `AAAA_MM_limpos_preagregacao.csv`            | Dados limpos antes da agregação (intermédio)                                                   |
| `AAAA_MM_complemento_azambuja.csv`           | Complemento Azambuja normalizado                                                                  |
| `AAAA_MM_registos_brutos.rds`                | Nº de registos brutos (para o histórico)                                                        |
| `AAAA_MM_compensacao_por_titulo.csv`         | **Fase 2** — compensação por título, decomposta em Navegante / Complemento CM / Viver Cascais |
| `AAAA_MM_passejovem_resumo.csv`              | **Fase 2** — compensação por título de referência e por operador (11 operadores)             |
| `AAAA_MM_passejovem_congelamento.csv`        | **Fase 2** — Congelamento Tarifário (TAT) por operador                                          |
| `docs/AAAA_MM_AnexoII_reparticao.xlsx`       | **Fase 2** — Anexo II: repartição e congelamento em Excel com fórmulas                        |
| `docs/AAAA_MM_informacao1/oficio1.pdf/.docx` | **Fase 1** — Informação 1 e Ofício 1 (IMT)                                                    |
| `docs/AAAA_MM_informacao2/oficio2.pdf/.docx` | **Fase 2** — Informação 2 e Ofício 2 (operadores)                                             |
| `docs/AAAA_MM_AnexoI_metodologia.pdf`        | **Fase 2** — Anexo I: nota metodológica com repartição                                        |

---

## 🏷️ Fecho do Mês

Depois de validar o reporte e emitir os documentos, faz commit do código e dos parâmetros e cria uma tag:

```bash
git add code/ README.md procedure.md
git commit -m "feat(fecho): fecho de contas Passe Jovem agosto 2026"
git tag -a run-2026-08-agosto -m "Passe Jovem agosto 2026"
git push origin main --tags
```

---

## 📁 Estrutura do Repositório

```
PasseJovem/
├── code/
│   ├── 00_config.R                  # Constantes de negócio, caminhos, leitura e utilitários
│   ├── 01_preparar_dados.R          # Leitura, combinação, limpeza e correções manuais
│   ├── 02_calcular_compensacoes.R   # Agregação (comNIF/semNIF), Azambuja, compensação, controlos
│   ├── 03_exportar_reporte.R        # Preenche o template Excel AML/IMT e atualiza o histórico
│   ├── 04_estimar_compensacao_operadores.R  # Fase 2: repartição por operador e congelamento (CSV)
│   ├── 05_exportar_anexo_reparticao.R       # Fase 2: Anexo II Excel
│   ├── 10_documentos.R              # Renderiza os documentos (Quarto → Typst/DOCX)
│   ├── 99_run_pipeline.R            # Orquestrador (Fase 1: 01→03; Fase 2: 04→05; documentos)
│   ├── doc_params.R                 # Parâmetros de emissão mensais
│   ├── informacao1.qmd / oficio1.qmd / metodologia1.qmd   # Fase 1
│   ├── informacao2.qmd / oficio2.qmd / metodologia2.qmd   # Fase 2
│   ├── utils_tables.R / utils_extenso.R
│   └── templates/                   # Estilos Typst/Word, papel timbrado, logo
├── data/                            # (gitignored)
│   ├── AAAA_MM_mes/{input,output}/
│   └── auxiliar/                    # Correções, exceções e histórico
├── documents/                       # Portarias (e procedimento antigo)
├── procedure.md                     # Metodologia e exceções
└── README.md
```

---

## 📖 Documentação Metodológica

Regras, fórmulas, exceções e casos especiais: 👉 **[`procedure.md`](procedure.md)**
