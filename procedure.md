# Enquadramento

## Regras

As regras de compensação estão definidas na Portaria 7A/2024, que teve já 2 alterações.
`documents\2024_01_05_portaria_7a.pdf` (1a versão), `documents\2024_12_Portaria 307-A_2024_1.pdf` (1a alteração), `documents\2026_07_portaria_287_2026_1_passejovem.pdf` (2a alteração)

- Todos os jovens até 23 anos (inclusive), com NIF e Data de Nascimento.

  - Excepção para jovens estudantes de  Arquitetura e Urbanismo, Ciências Farmacêuticas, Medicina, Medicina Dentária, Medicina Veterinária - aí o limite são 24 anos (inclusive) - estes têm de entregar um documento da universidade.
- Não podem ter mais do que 1 passe gratuito (por exemplo Navegante em Lisboa, e Andante no Porto) - isto é verificado pelo IMT após comparação das tabelas enviadas e lista de NIF
- Não precisa de ter o passe relativo ao seu local de residência (pode escolher outra área)
- Pode viver em CIMS distintas e ter o complemento de passe interregional (ex: Torres Vedras, Oeste)
- Ten de estar dentro da validade: idade (perfil) ou validade do cartão

## Compensação financeira

O valor da compensação financeira por cada passe gratuito para jovens estudantes é calculado da seguinte forma:
    a) Nas AM:
        i) Cada validação corresponde a 5 % do valor do título de referência, pagando-se este valor multiplicado pelo número de validações realizadas até se atingir 20 validações;
        ii) Atingidas 20 validações, inclusive, é pago o valor do título de referência por inteiro;
    b) Nas CIM:
        i) Cada validação corresponde a 10 % do valor do título de referência, pagando-se este valor multiplicado pelo número e validações realizadas até se atingir 10 validações;
        ii) Atingidas 10 validações, inclusive, é pago o valor do título de referência por inteiro;
    c) Os passes que não tenham qualquer validação, não dão direito a qualquer compensação.

O complemento interregional não pode ser vendido separadamente, tem de ser com o Navegante.

# Títulos

Há 2 títulos em vigor:

- Navegante Metropolitano sub18 + TP
- Navegante Metropolitano sub23 + TP

Deixaram de existir os Navegantes municipais sub18 e sub23, e o Navegante 12.
No entanto, o título 4_18 e sub23 continuam a ser carregados mensalmente nalgumas excepções que não tem NIF associado, ou com mais do que 1 cartão ativo (ex: cartão de substituição após perda, sem dar baixa). Há ideia de acabar com a possibilidade de adquirir estes títulos, mas nalgumas máquinas ainda é possível (fora do controle da TML)

# Procedimento em R

O fluxo de dados foi totalmente migrado para R, eliminando a dependência do Postgres/DBeaver. O processamento é realizado localmente de forma ultra-rápida via `data.table` (cerca de 10 a 60 segundos por mês completo para ~450.000 registos).

## Estrutura de Pastas

```
PasseJovem/
├── R/
│   ├── 00_utils.R                 # Funções utilitárias, regras de negócio e I/O
│   ├── 01_preparar_dados.R        # Leitura, combinação e limpeza inicial dos dados
│   ├── 02_calcular_compensacoes.R # Agregação rápida, join com Azambuja e cálculo
│   └── 03_exportar_reporte.R      # Geração do ficheiro formal Excel (AML/IMT)
├── tests/
│   └── verificar_replicacao.R     # Teste automatizado de replicação contra benchmarks
├── run_pipeline.R                 # Script orquestrador principal
└── data/
    ├── <ano>_<mm>_<mes>/
    │   ├── input/                 # Ficheiros originais recebidos
    │   │   ├── <ano><mm>_PasseJovem_ValidaçõesLimpas.csv
    │   │   ├── <ano><mm>_Complemento_Azambuja.csv
    │   │   └── TML_<mes><ano>.xlsx
    │   └── output/                # Ficheiros limpos e resultados normalizados
    │       ├── <ano>_<mm>_dados_limpos.csv
    │       ├── <ano>_<mm>_complemento_azambuja.csv
    │       ├── <ano>_<mm>_comNIF.csv
    │       ├── <ano>_<mm>_semNIF.csv
    │       └── <ano>_<mm>_reporte_AML_IMT.xlsx
```

## Como Executar

### 1. Linha de Comandos (PowerShell / Terminal)

```powershell
# Executar para um mês (ex: Maio de 2026):
Rscript code/run_pipeline.R 2026 5

# Executar para múltiplos meses em cadeia:
Rscript run_pipeline.R 2026 5 6

# Validar se os resultados replicam 100% os ficheiros de referência:
Rscript tests/verificar_replicacao.R
```

### 2. No R / RStudio

```r
source("run_pipeline.R")
run_pipeline(ano = 2026, mes = 5)
```

## Regras de Limpeza e Prioridade Implementadas

1. **Remoção de LX-BT**: Registos cujo `ticket_name` começa por `LX-BT` são excluídos (mantendo registos sem nome atribuído que correspondam a perfis de criança).
2. **Perfil Criança (Navegante 12)**: Registos onde `ticket_operator_code` e `ticket_code` são nulos são atualizados para:
   - `ticket_operator_code = 31`, `ticket_code = 1020`, `ticket_name = "Navegante 12"`, `price = 0`, `ticket_code_base = 900`, `price_ticket_base = 40`.
3. **Cascais (Viver Cascais)**:
   - `ticket_operator_code = 21`, `price = 0`, `price_ticket_base = 20` (valor de referência do Viver Cascais).
4. **Agregação por NIF (comNIF)**:
   - Múltiplos cartões do mesmo titular são concatenados com `_`.
   - Prioridade de operador: `31` (AML) > `0` > outros.
   - Prioridade de títulos: `6181` (sub23) > `6180` (sub18) > `6150` > `7650` > `1020` (12) > complementos (`6151`–`6171`, `7650`–`7671`) > `0` > outros.
   - Caso especial `6170`/`7670` sem outro título metropolitano: prefixado com `6180_` (< 19 anos) ou `6181_` (>= 19 anos).
5. **Cálculo da Compensação (Portaria 7-A/2024)**:
   - Se `numval < 20`: `0.05 * preco_base * numval`
   - Se `numval >= 20`: `preco_base` (integral)
