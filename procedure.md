# Enquadramento

## Regras

As regras de compensação estão definidas na [Portaria 7-A/2024](https://diariodarepublica.pt/dr/detalhe/portaria/7-a-2024-836205473), que teve já 2 alterações:
`documents\2024_01_05_portaria_7a.pdf` (1.ª versão), `documents\2024_12_Portaria 307-A_2024_1.pdf` ([1.ª alteração](https://diariodarepublica.pt/dr/detalhe/portaria/307-a-2024-898320378)), `documents\2026_07_portaria_287_2026_1_passejovem.pdf` ([2.ª alteração](https://diariodarepublica.pt/dr/detalhe/portaria/287-2026-1144685810)).

- Todos os jovens até 23 anos (inclusive), com NIF e data de nascimento.
  - Exceção para estudantes de Arquitetura e Urbanismo, Ciências Farmacêuticas, Medicina, Medicina Dentária e Medicina Veterinária: o limite é 24 anos (inclusive); têm de entregar um documento da universidade.
- Não podem ter mais do que 1 passe gratuito (por exemplo Navegante em Lisboa e Andante no Porto) — verificado pelo IMT após comparação das tabelas enviadas com a lista de NIF.
- Não é necessário ter o passe da área de residência (pode escolher outra área).
- Pode viver em CIM distintas e ter o complemento de passe interregional (ex: Torres Vedras, Oeste).
- Tem de estar dentro da validade: idade (perfil) ou validade do cartão.

## Compensação financeira

O valor da compensação por cada passe gratuito para jovens estudantes é calculado da seguinte forma:

- **a) Nas AM:**
  - i) Cada validação corresponde a **5 % do valor do título de referência**, pagando-se este valor multiplicado pelo número de validações realizadas até se atingir **20 validações**;
  - ii) Atingidas 20 validações, inclusive, é pago o valor do título de referência por inteiro.
- **b) Nas CIM:**
  - i) Cada validação corresponde a **10 % do valor do título de referência**, até **10 validações**;
  - ii) Atingidas 10 validações, inclusive, é pago o valor do título de referência por inteiro.
- **c)** Os passes sem qualquer validação nesse mês não dão direito a compensação.

O complemento interregional não pode ser vendido separadamente, tem de ser adquirido com o Navegante.

> Este pipeline só trata a **AML** (regra a): `THRESHOLD_VAL = 20`, `PCT_VAL = 0.05`.

## Títulos

Há 2 títulos em vigor:

- Navegante Metropolitano sub18 + TP
- Navegante Metropolitano sub23 + TP

Deixaram de existir os Navegantes municipais sub18 e sub23, e o Navegante 12. No entanto, os títulos 4_18 e sub23 continuam a ser carregados mensalmente em algumas exceções (sem NIF associado, ou com mais do que 1 cartão ativo — ex: cartão de substituição após perda, sem dar baixa). Há ideia de acabar com a possibilidade de adquirir estes títulos, mas nalgumas máquinas ainda é possível (fora do controlo da TML).

### Preço base (título de referência)


| `price_ticket_base_new` | `nome_imt`                            |
| ------------------------- | --------------------------------------- |
| 60                      | Navegante Metropolitano + Complemento |
| 50                      | Navegante Municipal + Complemento     |
| 40                      | Navegante Metropolitano               |
| 30                      | Navegante Lisboa                      |
| 20                      | Viver Cascais                         |
| 16.75                   | MTS Passe                             |

---

# Procedimento em R

> **Âmbito:** apenas meses **a partir de julho de 2026** (input XLSX com `ID_DOC_NUMBER`/`ID_DOC_NUMBER_DESC`, template Excel v2). Maio e junho tinham requisitos diferentes e já não são suportados/mantidos.

O fluxo foi totalmente migrado para R (sem recorrer a consulta das bases de dados Postgres/DBeaver). O processamento é local, via `data.table` (10 a 60 s por mês, ~450 000 registos).

## Estrutura de Pastas

```
PasseJovem/
├── code/
│   ├── 00_config.R                          # Constantes, caminhos, leitura e utilitários
│   ├── 01_preparar_dados.R                  # Leitura, combinação e limpeza
│   ├── 02_calcular_compensacoes.R           # Agregação e cálculo por titular
│   ├── 03_exportar_reporte.R                # Excel AML/IMT + histórico
│   ├── 04_estimar_compensacao_operadores.R  # Repartição por operador e congelamento (CSV)
│   ├── 05_exportar_anexo_reparticao.R       # Anexo II Excel formatado com fórmulas
│   ├── 10_documentos.R                      # Informação, Ofícios e Anexo I Metodologia (Quarto)
│   ├── 99_run_pipeline.R                    # Orquestrador
│   ├── doc_params.R                         # Parâmetros de emissão dos documentos
│   ├── informacao1.qmd                      # Informação de serviço
│   ├── oficio1.qmd                          # Ofício de envio ao IMT
│   ├── metodologia1.qmd                     # Anexo - Nota Metodológica
│   └── metodologia2.qmd                     # Anexo I - Nota Metodológica com repartição
└── data/
    ├── auxiliar/                            # Correções, exceções e histórico
    └── <ano>_<mm>_<mes>/
        ├── input/
        │   ├── <ano><mm>_PasseJovem_ValidaçõesLimpas.xlsx   (folha 1 + folha Azambuja)
        │   └── TML_<mes><ano>.xlsx                          (Cascais)
        └── output/
            ├── <ano>_<mm>_limpos_preagregacao.csv
            ├── <ano>_<mm>_complemento_azambuja.csv
            ├── <ano>_<mm>_comNIF.csv
            ├── <ano>_<mm>_semNIF.csv
            ├── <ano>_<mm>_a_verificar.csv
            ├── <ano>_<mm>_age_nok.csv
            ├── <ano>_<mm>_documentos_estranhos_semNIF.csv
            ├── <ano>_<mm>_reporte_AML_IMT.xlsx
            ├── <ano>_<mm>_compensacao_por_titulo.csv
            ├── <ano>_<mm>_passejovem_resumo.csv
            ├── <ano>_<mm>_passejovem_congelamento.csv
            └── docs/
                ├── <ano>_<mm>_AnexoI_metodologia.pdf
                ├── <ano>_<mm>_AnexoII_reparticao.xlsx
                ├── <ano>_<mm>_informacao1.pdf / .docx
                └── <ano>_<mm>_oficio1.pdf / .docx
                └── <ano>_<mm>_metodologia1.pdf
```

## Como Executar

```powershell
Rscript code/99_run_pipeline.R 2026 7        # um mês
Rscript code/99_run_pipeline.R 2026 7 8      # vários meses
Rscript code/99_run_pipeline.R 2026          # todos os meses de 2026 em data/
```

```r
source("code/99_run_pipeline.R")
run_pipeline(ano = 2026, meses = 7)
```

> **Controlo de execução em `code/00_config.R`**:
> - `RUN_FASE2`:
>   - `FALSE`: corre apenas a **Fase 1** (scripts `01` a `03` + documentos `informacao1`, `oficio1`, `metodologia1`).
>   - `TRUE`: corre também a **Fase 2** (scripts `04` e `05` + documentos `metodologia2`, `informacao2`, `oficio2`), quando as quotas mensais dos operadores já se encontram disponíveis.
> - `RENDER_DOCS`:
>   - `TRUE`: executa `10_documentos.R` (`render_documentos()`) no final do pipeline.
>   - `FALSE`: salta a renderização dos documentos Quarto (PDF/DOCX).
> - Ambos podem também ser sobrepostos diretamente na chamada `run_pipeline(ano = 2026, meses = 7, render_docs = FALSE, run_fase2 = FALSE)`.

## Passo 01 — Preparar dados (`01_preparar_dados.R`)

1. **Leitura**: ficheiro `*PasseJovem*` (XLSX; CSV só como legado), `TML_*.xlsx` (Cascais) e Azambuja (ficheiro `*Complemento_Azambuja*` ou folha "Azambuja" do XLSX principal). Os nomes das colunas são normalizados para minúsculas; `ID_DOC_NUMBER → doc_number` e `ID_DOC_NUMBER_DESC → doc_type`.
2. **Cascais**: `ticket_operator_code = 21`, `price = 0`, `price_ticket_base = 20`. Junta-se ao ficheiro principal.
3. **Normalização do NIF**: `""`, `"0"` e `"NA"` passam a `NA`.
4. **Repescagem de NIF** (`nif_repescagem2024.csv`): preenche `fiscal_number` apenas onde está em falta, por `card_serial_number` (também no Azambuja).
5. **Remoção de LX-BT**: `ticket_name` a começar por `LX-BT` é excluído (mantêm-se os `NA`).
6. **Perfil criança**: registos com `ticket_operator_code` e `ticket_code` nulos passam a Navegante 12 (`operador 31`, `ticket_code 1020`, `price 0`, `price_ticket_base 40`).
7. Remoção de registos sem `year_dist`; aviso se existirem registos de outro ano/mês.
8. **Correções manuais** (por `card_serial_number`):
   - `data_nascimento_correct.csv` → substitui `birth_date` por `dn_corrigida`;
   - `nif_correct.csv` → substitui `fiscal_number` e/ou `birth_date`.
9. **Recalcular a idade** a partir da `birth_date` (anos completos até ao dia 1 do mês de referência: `floor(dias/365.25)`).
10. **Idade em casos-limite** (ignora-se o dia de aniversário): se `age == 25` e nasceu no mês de referência → `24`; se `age == 3` e nasceu no mês de referência → `4`.

> [!IMPORTANT]
> Se houver mais do que um ficheiro `PasseJovem` ou `TML_*`, o pipeline usa o primeiro e emite um aviso — mantém só um por pasta.

## Passo 02 — Calcular compensações (`02_calcular_compensacoes.R`)

### Filtro dos passes do mês

`year_dist == ano`, `month_dist == mes`, `price == 0`, `price_ticket_base` não nulo e `count_val > 0`.

### Agregação por titular


| Grupo      | Chave                                                                                       | Notas                                                                        |
| ------------ | --------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------ |
| **comNIF** | `fiscal_number`                                                                             | Vários cartões/bilhetes do mesmo NIF são somados (e tornados explícitos) |
| **semNIF** | documento de identificação (`DOC_<tipo>_<número>`) ou número do cartão (`CARD_<n.º>`) | Ver regras abaixo                                                            |

Quando um titular tem vários bilhetes:

- **Prioridade de operador**: `31` (AT TML) > `0` > outros.
- **Prioridade de título**: `6181` (sub23) > `6180` (sub18) > `6150` > `7650` > `1020` (Nav. 12) > complementos (`6151–6171`, `7650–7671`) > `0` > outros.
- `numval` = soma das validações; `age_new` = mínimo das idades; `birth_date_new` = máxima.
- **Preço base do grupo** (`CODES_COMP = 6170, 7670`):
  - só complemento → `60`;
  - complemento + outro título → `max(price_ticket_base) + 20`;
  - sem complemento → `max(price_ticket_base)`.
- **Complemento do operador 31 (comNIF)** sem outro título metropolitano: código prefixado com `6180_` (< 19 anos) ou `6181_` (≥ 19 anos) e nome `Navegante Metropolitano 418 Gratuito+…` / `… sub23 Gratuito+…`.

### Complemento Azambuja

As validações Azambuja são somadas por NIF (comNIF) ou por cartão (semNIF, quando não há NIF) e a compensação é **recalculada com o total de validações** (`compaz` regista o código).

### Fórmula

```
numval <  20  →  compensacao = 0.05 × preço_base × numval
numval >= 20  →  compensacao = preço_base
```

### Idades incompatíveis (`age_nok.csv`)

Após as correções, registos com `age_new == 3` (demasiado novo) ou `age_new >= 25` são **excluídos** da compensação e guardados em `age_nok.csv`.

> [!NOTE]
> O limite legal é 23 (24 para os cursos listados). O pipeline só exclui ≥ 25; os com 24 anos entram na compensação e **têm de ser conferidos** com o documento da universidade (ver `a_verificar.csv` -> Enviados ao Departamento de Clientes).

## semNIF — documentos e exceções

Quem não tem NIF é identificado pelo documento (`doc_type`, `doc_number`). Para evitar fundir pessoas diferentes com documentos "dummy" (ex: `00000`):


| Situação                                                                           | Motivo registado                                         | Agrupa por                                                                                                       |
| -------------------------------------------------------------------------------------- | ---------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------ |
| Documento válido, sem anomalia                                                      | —                                                       | **documento** (soma validações de vários cartões do mesmo titular, ex: passaportes `PA1234XX` e `U012345YY`) |
| Documento em `id_exception.csv`                                                      | "… partilhado por jovens diferentes (fraude/exceção)" | **documento** (exceção para juntar)                                                                            |
| Mesmo `doc_number` com datas de nascimento distintas                                 | "Documento com datas de nascimento distintas"            | **cartão**                                                                                                      |
| Cartão de Cidadão só zeros / só noves / dígitos repetidos / < 6 dígitos        | "Cartão de Cidadão com …"                             | **cartão**                                                                                                      |
| Outro documento só zeros / só noves                                                | "Documento com apenas zeros/noves"                       | **cartão**                                                                                                      |
| Valores inválidos (`test`, `teste`, `null`, `none`, `na`, `xxx`, `12345`, `123456`) | "Documento com valor inválido"                          | **cartão**                                                                                                      |
| Sem documento                                                                        | —                                                       | **cartão**                                                                                                      |

Todos os casos com motivo são exportados para `<ano>_<mm>_documentos_estranhos_semNIF.csv` (um registo por cartão).

### Lista de exceções (`data/auxiliar/id_exception.csv`)

Formato `doc_type;doc_number;motivo`. Se `doc_type` estiver vazio, a correspondência é só pelo número. Exemplo: `;PA1234XX`.

## Registos a verificar (`a_verificar.csv`)

Lista para confirmação manual, com os motivos (por esta ordem de precedência):

1. **Exceção ID (datas de nascimento distintas)** — documento em `id_exception.csv`;
2. **Documento com datas de nascimento distintas** (semNIF);
3. **Sem NIF** — titular não identificado por NIF;
4. **Idade > 23** (com NIF) — inclui os de 24 anos (confirmar estudante elegível).

## Passo 03 — Exportar reporte (`03_exportar_reporte.R`)

Copia o template oficial **v2** (`Exemplos/Template_PASSE GRATUITO JOVEM - REPORTE - CIM_AT para IMT (Mensal) - 2026 - V2.xlsx`) e preenche-o preservando os estilos. Folhas: `AML_<ano>_<mm>_ComNIF`, `AML_<ano>_<mm>_SemNIF`, `RESUMO_<ano>_<mm>_AML`.

- **comNIF**: ordenado por NIF (número); PVP numérico; data de nascimento como data Excel (`dd/mm/yyyy`); compensação com 2 casas decimais; coluna M = "Número do cartão de suporte ao título de transporte" (`cardserialnumber_new`).
- **semNIF**: ordenado por `doc_type` e `doc_number`; colunas adicionais "Tipo de Documento", "N.º DO DOCUMENTO" e "Data de nascimento"; coluna M = Categoria; coluna N = número do cartão de suporte.
- **Categoria**: `"B"` se o nome do passe contém "Complemento", `"A"` caso contrário.
- **Estimativa sem bilhética** (col. 10): fica a 0/vazia.
- **RESUMO**: totais por emissor (`AT TML`, `AT CASCAIS`) e TOTAL em negrito, com separador de milhares e 2 casas decimais.
- Atualiza automaticamente `data/auxiliar/compensacao_pj_anteriores.csv` (registos, utilizadores, utilizadores semNIF, validações, validações/utilizador, compensação total e por utilizador). Se o mês já existe, a linha é atualizada.

## Passo 04 — Estimar compensação por operador e congelamento (`04_estimar_compensacao_operadores.R`)

Estima a repartição mensal das compensações Passe Jovem pelos **11 operadores** (`CP`, `FT`, `TT`, `SL`, `ML`, `MTS`, `Carris`, `TCB`, `AT Cascais`, `AT TML - CM`, `BT Oeste`) e o respetivo **Congelamento Tarifário (TAT)**, cruzando os outputs do Passo 02 com as listas e quotas do repositório `Compensacoes` (`C:/Repos/Compensacoes`).

### 1. Agregação por título e código de bilhete (`nome_dist`, `ticket_code`)

- Lê `<ano>_<mm>_comNIF.csv` e `<ano>_<mm>_semNIF.csv`, filtra apenas os registos elegíveis e extrai diretamente de cada registo:
  - `ticket_code`: primeiro código de `ticket_code_new` (`sub("_.*", "", ticket_code_new)`), que já vem ordenado por prioridade no Passo 02;
  - `ticket_operator_code`: primeiro código de `ticket_operator_code_new`;
  - `ptb_new`: preço de referência total do registo (`price_ticket_base_new`);
  - `tem_comp_code`: presença dos códigos `CODES_COMP` (`6170` ou `7670`) em `ticket_code_new`.
- Agrega por `(nome_dist, ticket_code, ticket_operator_code, ptb_new)`:
  - `passes = n()` (n.º de passes com validações)
  - `numval = sum(numval)` (total de validações)
  - `compensacao = sum(compensacao)` (compensação apurada em €)

### 2. Cruzamento direto com `Compensacoes/lists/titulos.csv`

- Cada `ticket_code` é cruzado diretamente com `titulos.csv` (sem depender de `titulos_nomes.csv` nem `operadores.csv`) para obter `titulo_nome` (`ticket_name_reduzido`), `modalidade`, `pvp`, `titulo_nome_base` e `preco_base`.
- Caso seja criado um título novo que ainda não conste em `titulos.csv`, o `preco_base` usa automaticamente o `price_ticket_base_new` do registo (deduzindo `20 €` se tiver complemento).
- Identificação de **Viver Cascais** e **Complemento CM**:
  - `viver_cascais`: `ticket_operator_code == 21` ou `nome_dist` a começar por `"Viver Cascais"`;
  - `complemento_CM`: presença de código `CODES_COMP` (`6170`/`7670`) em `ticket_code_new` ou `"Complemento"` em `nome_dist`;
  - `base_weight`: calculado diretamente a partir dos preços como $\text{preco\_base} / \text{price\_ticket\_base\_new}$ (e `1` quando não tem complemento).

### 3. Separação da compensação em 3 componentes

Para cada título agregado, a compensação total é decomposta em:


| Componente           | Regra de cálculo                                                                | Destino / Repartição                                                                             |
| ---------------------- | ---------------------------------------------------------------------------------- | ---------------------------------------------------------------------------------------------------- |
| **Passes Navegante** | $\text{comp\_navegante} = \text{compensacao} \times \text{base\_weight}$         | Repartido pelos 11 operadores segundo as quotas mensais do título (`ticket_code`)                 |
| **Complemento CM**   | $\text{comp\_complemento} = \text{compensacao} \times (1 - \text{base\_weight})$ | **100 % `AT TML - CM`** (`ticket_operator_code = 31`, `ticket_code = "1070"` (`CODE_COMP_RESUMO`), `preco_base = 20 €`)  |
| **Viver Cascais**    | $\text{comp\_cascais} = \text{compensacao}$ (`base_weight = 1`)                  | **100 % `AT Cascais`** (`ticket_operator_code = 21`, `ticket_code = "129_131"` — códigos concatenados com `"_"`, `preco_base = 20 €`) |

**Exemplos de `base_weight`:**

- `Navegante Metropolitano + Complemento` (`60 €` = `40 €` Navegante + `20 €` Complemento): $\text{base\_weight} = 40/60 = 2/3 \implies 2/3$ da compensação vai para o título Navegante (`6180` ou `6181`) e $1/3$ para `Complemento CM` (`1070`).
- `Navegante Municipal (ex.: Montijo) + Complemento` (`50 €` = `30 €` Municipal + `20 €` Complemento): $\text{base\_weight} = 30/50 = 3/5 = 0{,}6 \implies 60\%$ para o título Navegante e $40\%$ para `Complemento CM`.
- Títulos sem complemento (`Navegante 12`, `Navegante Metropolitano 418/sub23`, `Navegante Municipal 418/sub23`): $\text{base\_weight} = 1 \implies 100\%$ Navegante.

> **Contagem de passes e validações:** nas linhas de `Passes Navegante` (`1020`, `6150`, `6180`, `6181`), `passes` e `validacoes` contam o total de cartões/validações que utilizaram esse título base; na linha `Complemento CM` (`1070`), `passes` e `validacoes` somam os cartões/validações que tinham complemento metropolitano.

### 4. Repartição pelos operadores (Quotas mensais)

- Lê o ficheiro de quotas do mês do repositório `Compensacoes`:
  `Compensacoes/data/<mm>_<mes_abrev>_<ano>/output/1_quotas_<mes_abrev><aa>.csv`
- Para cada título Navegante $i$ (`1020`, `6150`, `6180`, `6181`) e operador $j$:
  $$
  \text{Compensação}_{i,j} = \text{Compensação Total Navegante}_i \times \text{quota}_{i,j}

  $$

  sem arredondamento intermédio nas linhas de título.
- Para os **Outros Passes**:
  - `Complemento CM` (`1070`): quota $= 100\%$ em `AT TML - CM` ($0\%$ nos restantes);
  - `Viver Cascais` (`129_131`, resultado da fusão de `Viver Cascais Residente` `129` e `Viver Cascais Estudante` `131` com os respetivos códigos concatenados): quota $= 100\%$ em `AT Cascais` ($0\%$ nos restantes).

### 5. Estimativa do Congelamento Tarifário (TAT)

Lê as Taxas de Atualização Tarifária anuais em `Compensacoes/lists/tat.csv` até ao ano de referência (`ano_ref`):


| Ano  | Taxa TAT              |
| ------ | ----------------------- |
| 2024 | $6{,}40\%$ (`0.0640`) |
| 2025 | $2{,}02\%$ (`0.0202`) |
| 2026 | $2{,}28\%$ (`0.0228`) |

Aplicam-se dois fatores cumulativos distintos sobre os totais por operador:

1. **Congelamento Passes Navegante** (atualização cumulativa desde 2024 sobre os títulos Navegante):

   $$
   F_{\text{Navegante}} = \prod_{y=2024}^{\text{ano}} (1 + \text{TAT}_y) - 1 = (1 + 0{,}0640)\times(1 + 0{,}0202)\times(1 + 0{,}0228) - 1 = 11{,}0555\% \quad (0{,}11055490944)

   $$

   $$
   \text{Congelamento Navegante}_j = \text{Total Passes Navegante}_j \times F_{\text{Navegante}}

   $$
2. **Congelamento Outros Passes** (`Complemento CM` e `Viver Cascais`):

   $$
   F_{\text{Outros}} = \text{TAT}_{2024} \times \prod_{y=2025}^{\text{ano}} (1 + \text{TAT}_y) = 0{,}0640 \times (1 + 0{,}0202)\times(1 + 0{,}0228) = 6{,}7095\% \quad (0{,}06709490944)

   $$

   $$
   \text{Congelamento Outros}_j = \text{Total Outros Passes}_j \times F_{\text{Outros}}

   $$
3. **Congelamento TOTAL por operador**:

   $$
   \text{Congelamento TOTAL}_j = \text{Congelamento Navegante}_j + \text{Congelamento Outros}_j

   $$

### 6. Ficheiros CSV exportados (`output/`)

Todos os CSV são gravados com separador `;`, vírgula decimal `,` e UTF-8 BOM (`bom = TRUE`, garantindo leitura correta de caracteres como `CRIANÇA` no Excel em Windows):

1. `<ano>_<mm>_compensacao_por_titulo.csv`: detalhe por `nome_dist` com `ticket_name_reduzido`, `complemento_CM`, `base_weight`, códigos de bilhete/operador/modalidade, `passes`, `numval`, `compensacao` e a decomposição nas 3 parcelas (`comp_navegante`, `comp_complemento`, `comp_cascais`).
2. `<ano>_<mm>_passejovem_resumo.csv`: tabela por título de referência (`1020`, `6150`, `6180`, `6181`, `1070 Complemento CM`, `129_131 Viver Cascais`) com `passes`, `validacoes`, `compensacao_total` e valores por operador (sem linhas `TOTAL*`).
3. `<ano>_<mm>_passejovem_congelamento.csv`: 3 linhas (`Congelamento Navegante`, `Congelamento Outros Passes`, `Congelamento TOTAL`) com o total e a repartição pelos 11 operadores.

---

## Passo 05 — Exportar Anexo II de Repartição (`05_exportar_anexo_reparticao.R`)

Gera o ficheiro Excel formatado `data/<ano>_<mm>_<mes>/output/docs/<ano>_<mm>_AnexoII_reparticao.xlsx` a partir de `<ano>_<mm>_passejovem_resumo.csv`, `titulos.csv`, `1_quotas_<mes><aa>.csv` e `tat.csv`. Pode ser executado autonomamente (`exportar_anexo_reparticao(ano, mes)`) em menos de 1 segundo.

### Estrutura e Fórmulas do Workbook

1. **Folha 1 — `<mes><aa>` (ex.: `jul26`)**:

   - **Bloco esquerdo (colunas `A:W`)**: `ano`, `mês`, `ticket_operator_code`, `ticket_code`, `titulo`, `Modalidade título`, `pvp`, `Título Base`, `Preço do título base`, `Nº passes com validações`, `Validações`, `Compensação total` e os 11 operadores (`CP` a `BT Oeste`).
   - **Coluna `X`**: separador em branco.
   - **Bloco direito (colunas `Y:AI`)**: `Quota (%) por título e operador` para os 11 operadores, com os valores exatos não arredondados de `f_quotas`, formatados visualmente a 4 casas decimais (`0,0000%`).
   - **Fórmulas dinâmicas e arredondamento**:
     - Nas linhas de título (`linhas 5 a 10`), cada célula de operador (`M:W`) usa a fórmula `=$L<row>*<col_quota><row>` (Compensação total $\times$ Quota), sem arredondamento intermédio.
     - As linhas de subtotal `Total Passes Navegante` (`linha 11`) e `Total Outros Passes` (`linha 12`) usam `=SUM(...)` sem arredondamento.
     - Apenas a linha final `Total` (`linha 13`) arredonda a soma dos dois subtotais a 2 casas decimais: `=ROUND(SUM(L11:L12), 2)` .. `=ROUND(SUM(W11:W12), 2)`.
2. **Folha 2 — `<mes><aa>_resumo_congelamento` (ex.: `jul26_resumo_congelamento`)**:

   - **Tabela principal (linhas 6 a 17)**:
     - Colunas `D:F` (**Compensação Passe Jovem (€)**, em tons dourados):
       - `Passes Navegante` (`D`): `=SUM(jul26!M5:M8)` .. `=SUM(jul26!W5:W8)`
       - `Outros Passes` (`E`): `=SUM(jul26!M9:M10)` .. `=SUM(jul26!W9:W10)`
       - `TOTAL` (`F`): `=D<row>+E<row>`
     - Colunas `G:I` (**Compensação por Congelamento Tarifário — TAT (€)**, diferenciadas em tons de cinzento):
       - `Congelamento Passes Navegante` (`G`): `=D<row>*$I$21`
       - `Congelamento Outros Passes` (`H`): `=E<row>*$I$22`
       - `Congelamento TOTAL` (`I`): `=G<row>+H<row>`
     - Linha `TOTAL` (`linha 17`): arredondada a 2 casas decimais em todas as colunas monetárias (`=ROUND(SUM(D6:D16), 2)` .. `=ROUND(SUM(I6:I16), 2)`).
   - **Bloco de Notas / Parâmetros TAT (linhas 19 a 23)**:
     - Apresenta a tabela anual de taxas TAT (`2024: 6,40%`, `2025: 2,02%`, `2026: 2,28%` em `C21:C23`) e calcula por fórmula os fatores cumulativos em `I21` (`=(1+C21)*(1+C22)*(1+C23)-1` $\rightarrow 11{,}0555\%$) e `I22` (`=C21*(1+C22)*(1+C23)` $\rightarrow 6{,}7095\%$).

---

## Documentos (`10_documentos.R`)

Renderiza os documentos Quarto em `output/docs/` usando os parâmetros de `code/doc_params.R` (mês, referências, datas de pagamentos ETF, autores, flags `RENDER_PDF` e `RENDER_DOCX`):

- `informacao1.qmd` → `<ano>_<mm>_informacao1.pdf` e `.docx`
- `oficio1.qmd` → `<ano>_<mm>_oficio1.pdf` e `.docx`
- `metodologia2.qmd` → `<ano>_<mm>_AnexoI_metodologia.pdf` (apenas PDF/Typst; `metodologia1.qmd` mantém-se como versão anterior, comentada no pipeline)

Atualizar `doc_params.R` a cada mês antes de gerar os documentos.

## Checklist mensal

1. Criar `data/<ano>_<mm>_<mes>/input/` e colocar o XLSX ITS e o ficheiro Cascais.
2. Atualizar `data/auxiliar/` se houver novas correções/exceções.
3. Correr o pipeline.
4. Rever `a_verificar.csv`, `age_nok.csv` e `documentos_estranhos_semNIF.csv`; adicionar correções/exceções e **voltar a correr**.
5. Validar o reporte Excel e enviar ao IMT.
6. Atualizar `doc_params.R` e gerar a Informação.
7. Commit e tag do mês.
