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
