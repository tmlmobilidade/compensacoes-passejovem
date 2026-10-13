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

| `price_ticket_base_new` | `nome_imt` |
| --- | --- |
| 60 | Navegante Metropolitano + Complemento |
| 50 | Navegante Municipal + Complemento |
| 40 | Navegante Metropolitano |
| 30 | Navegante Lisboa |
| 20 | Viver Cascais |
| 16.75 | MTS Passe |

---

# Procedimento em R

> **Âmbito:** apenas meses **a partir de julho de 2026** (input XLSX com `ID_DOC_NUMBER`/`ID_DOC_NUMBER_DESC`, template Excel v2). Maio e junho tinham requisitos diferentes e já não são suportados/mantidos.

O fluxo foi totalmente migrado para R (sem recorrer a consulta das bases de dados Postgres/DBeaver). O processamento é local, via `data.table` (10 a 60 s por mês, ~450 000 registos).

## Estrutura de Pastas

```
PasseJovem/
├── code/
│   ├── 00_config.R                  # Constantes, caminhos, leitura e utilitários
│   ├── 01_preparar_dados.R          # Leitura, combinação e limpeza
│   ├── 02_calcular_compensacoes.R   # Agregação e cálculo
│   ├── 03_exportar_reporte.R        # Excel AML/IMT + histórico
│   ├── 10_documentos.R              # Informação (Quarto/Typst/DOCX)
│   ├── 99_run_pipeline.R            # Orquestrador
│   └── doc_params.R                 # Parâmetros de emissão dos documentos
└── data/
    ├── auxiliar/                    # Correções, exceções e histórico
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
            └── <ano>_<mm>_reporte_AML_IMT.xlsx
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

| Grupo | Chave | Notas |
| --- | --- | --- |
| **comNIF** | `fiscal_number` | Vários cartões/bilhetes do mesmo NIF são somados (e tornados explícitos) |
| **semNIF** | documento de identificação (`DOC_<tipo>_<número>`) ou número do cartão (`CARD_<n.º>`) | Ver regras abaixo |

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

| Situação | Motivo registado | Agrupa por |
| --- | --- | --- |
| Documento válido, sem anomalia | — | **documento** (soma validações de vários cartões do mesmo titular, ex: passaportes `PA1234XX` e `U012345YY`) |
| Documento em `id_exception.csv` | "… partilhado por jovens diferentes (fraude/exceção)" | **documento** (exceção para juntar) |
| Mesmo `doc_number` com datas de nascimento distintas | "Documento com datas de nascimento distintas" | **cartão** |
| Cartão de Cidadão só zeros / só noves / dígitos repetidos / < 6 dígitos | "Cartão de Cidadão com …" | **cartão** |
| Outro documento só zeros / só noves | "Documento com apenas zeros/noves" | **cartão** |
| Valores inválidos (`test`, `teste`, `null`, `none`, `na`, `xxx`, `12345`, `123456`) | "Documento com valor inválido" | **cartão** |
| Sem documento | — | **cartão** |

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

## Documentos (`10_documentos.R`)

Renderiza `informacao1.qmd` (Typst → PDF e Word → DOCX) usando os parâmetros de `code/doc_params.R` (mês, referências, datas de pagamentos ETF, autores). Atualizar `doc_params.R` a cada mês.

## Checklist mensal

1. Criar `data/<ano>_<mm>_<mes>/input/` e colocar o XLSX ITS e o ficheiro Cascais.
2. Atualizar `data/auxiliar/` se houver novas correções/exceções.
3. Correr o pipeline.
4. Rever `a_verificar.csv`, `age_nok.csv` e `documentos_estranhos_semNIF.csv`; adicionar correções/exceções e **voltar a correr**.
5. Validar o reporte Excel e enviar ao IMT.
6. Atualizar `doc_params.R` e gerar a Informação.
7. Commit e tag do mês.
