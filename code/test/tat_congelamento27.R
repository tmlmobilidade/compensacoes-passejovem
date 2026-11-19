library(ggplot2)
library(dplyr)
library(tidyr)

# --- 1. Parâmetros Base ---
pvp_base <- 40 # Ou 40 para o Navegante
tats <- c(
  "2024" = 0.0643,
  "2025" = 0.0202,
  "2026" = 0.0228,
  "2027" = 0.0266
)

# --- 2. Construção da Tabela com a Lógica Exata ---
dados <- tibble(
  ano = c(2023, 2024, 2025, 2026, 2027),
  tat = c(0, tats)
) |>
  mutate(
    # Fator acumulado de inflação desde 2023
    fator_acumulado = cumprod(1 + tat),
    # Preço teórico acumulado total
    custo_teorico = pvp_base * fator_acumulado,
    # PVP efetivo: congelado até 2026; atualizado em 2027 só com a taxa desse ano
    pvp_utilizador = if_else(ano == 2027, pvp_base * (1 + tat), pvp_base),
    # Diferencial a compensar ("o miolo")
    compensacao = custo_teorico - pvp_utilizador
  )

# --- 3. Gráfico ---
ggplot(dados, aes(x = ano)) +
  # Área da compensação acumulada
  geom_ribbon(aes(ymin = pvp_utilizador, ymax = custo_teorico), 
              fill = "#f39c12", alpha = 0.3) +
  # Linha do custo total teórico
  geom_line(aes(y = custo_teorico, color = "Custo Teórico Acumulado"), linewidth = 1.1) +
  geom_point(aes(y = custo_teorico, color = "Custo Teórico Acumulado"), size = 2.5) +
  # Linha do PVP cobrado ao passageiro
  geom_line(aes(y = pvp_utilizador, color = "PVP Utilizador"), linewidth = 1.1) +
  geom_point(aes(y = pvp_utilizador, color = "PVP Utilizador"), size = 2.5) +
  # Rótulos com os valores numéricos nos pontos
  geom_text(aes(y = custo_teorico, label = sprintf("%.2f €", custo_teorico)), 
            vjust = -1, size = 3.2, color = "#c0392b") +
  geom_text(aes(y = pvp_utilizador, label = sprintf("%.2f €", pvp_utilizador)), 
            vjust = 1.8, size = 3.2, color = "#2980b9") +
  # Diferencial em 2026 e 2027 assinalado no miolo
  annotate("text", x = 2026, y = (dados$custo_teorico[4] + dados$pvp_utilizador[4]) / 2,
           label = sprintf("Δ = %.2f €", dados$compensacao[4]), fontface = "bold", size = 3.3) +
  annotate("text", x = 2027, y = (dados$custo_teorico[5] + dados$pvp_utilizador[5]) / 2,
           label = sprintf("Δ = %.2f €", dados$compensacao[5]), fontface = "bold", size = 3.3) +
  scale_color_manual(
    name = "",
    values = c("Custo Teórico Acumulado" = "#c0392b", "PVP Utilizador" = "#2980b9")
  ) +
  scale_x_continuous(breaks = 2023:2027) +
  scale_y_continuous(labels = function(x) paste0(x, " €"), expand = expansion(mult = c(0.08, 0.12))) +
  labs(
    title = paste0("Evolução Tarifária vs. Custo Teórico (PVP Base = ", pvp_base, " €)"),
    subtitle = "Compensação remanescente após descongelamento parcial em 2027",
    x = "Ano",
    y = "Valor Unitário (€)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )



# caso a tat fosse aplicada desde 2020 ------------------------------------


# --- 1. Parâmetros e Histórico de TATs (2020 a 2027) ---
pvp_base <- 40 # Passe Navegante

tats_historicas <- c(
  # "2020" = 0.0114,
  "2020" = 0.0000, # não foi aplicado em 2020
  "2021" = 0.0057,
  "2022" = 0.0057,
  "2023" = 0.0611,
  "2024" = 0.0643, 
  "2025" = 0.0202, 
  "2026" = 0.0228, 
  "2027" = 0.0266
)

# --- 2. Construção da Tabela com as Três Séries ---
dados <- tibble(
  ano = 2019:2027,
  tat = c(0, tats_historicas)
) |>
  mutate(
    # Cenário A: Acumulação total desde a criação do passe em 2019
    fator_desde_2019 = cumprod(1 + tat),
    teorico_desde_2019 = pvp_base * fator_desde_2019,
    
    # Cenário B: Acumulação regulamentar com base 40 € em 2023[cite: 1]
    tat_reg = if_else(ano <= 2023, 0, tat),
    fator_desde_2023 = cumprod(1 + tat_reg),
    teorico_desde_2023 = if_else(ano < 2023, NA_real_, pvp_base * fator_desde_2023),
    
    # Preço efetivamente cobrado ao passageiro
    pvp_pago = case_when(
      ano <= 2026 ~ pvp_base,
      ano == 2027 ~ pvp_base * (1 + tat) # Descongelamento parcial em 2027
    )
  )

# --- 3. Gráfico ---
ggplot(dados, aes(x = ano)) +
  # Área de compensação regulamentar em vigor (2023 a 2027)[cite: 1]
  geom_ribbon(aes(ymin = pvp_pago, ymax = teorico_desde_2023), 
              fill = "#f39c12", alpha = 0.25) +
  
  # 1. Linha do Teórico desde 2020 (Cenário do colega: 48,85 €)
  geom_line(aes(y = teorico_desde_2019, color = "Teórico com TAT desde 2020"), 
            linewidth = 1.1, linetype = "dashed") +
  geom_point(aes(y = teorico_desde_2019, color = "Teórico com TAT desde 2020"), size = 2) +
  
  # 2. Linha do Teórico com base de 2023 (Fórmula regulamentar: 45,61 €)[cite: 1]
  geom_line(aes(y = teorico_desde_2023, color = "Teórico com base 2023 (Regulamentar)"), 
            linewidth = 1.1) +
  geom_point(aes(y = teorico_desde_2023, color = "Teórico com base 2023 (Regulamentar)"), size = 2.3) +
  
  # 3. Linha do PVP Pago pelo Utilizador
  geom_line(aes(y = pvp_pago, color = "PVP Pago (Efetivo)"), linewidth = 1.1) +
  geom_point(aes(y = pvp_pago, color = "PVP Pago (Efetivo)"), size = 2.3) +
  
  # Destaque dos valores finais em 2027
  geom_text(data = filter(dados, ano == 2027),
            aes(y = teorico_desde_2019, label = sprintf("%.2f €", teorico_desde_2019)),
            vjust = -1, color = "#8e44ad", fontface = "bold", size = 3.5) +
  geom_text(data = filter(dados, ano == 2027),
            aes(y = teorico_desde_2023, label = sprintf("%.2f €", teorico_desde_2023)),
            vjust = -1, color = "#c0392b", fontface = "bold", size = 3.5) +
  geom_text(data = filter(dados, ano == 2027),
            aes(y = pvp_pago, label = sprintf("%.2f €", pvp_pago)),
            vjust = 1.8, color = "#2980b9", fontface = "bold", size = 3.5) +
  
  # Escalas e Cores
  scale_color_manual(
    name = "",
    values = c(
      "Teórico com TAT desde 2020" = "#8e44ad",
      "Teórico com base 2023 (Regulamentar)" = "#c0392b",
      "PVP Pago (Efetivo)" = "#2980b9"
    )
  ) +
  scale_x_continuous(breaks = 2019:2027) +
  scale_y_continuous(labels = function(x) paste0(x, " €"), expand = expansion(mult = c(0.08, 0.12))) +
  labs(
    title = "Evolução do Preço do Passe Navegante Metropolitano",
    subtitle = "Comparação entre PVP efetivo, base regulamentar (2023) e evolução histórica total (2019)",
    x = "Ano",
    y = "Preço (€)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.minor = element_blank()
  )
