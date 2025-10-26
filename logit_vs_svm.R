# Carrega pacotes
library(rio)
library(rdbnomics)
library(dplyr)
library(tidyr)
library(splitTools)
library(ggplot2)
library(e1071)        # Para SVM
library(caret)        # Para avaliação do modelo
library(gganimate)
library(gifski)

install.packages("remotes")
remotes::install_version("gganimate", version = "1.0.8")

# Carrega dados
dados_brutos_y <- rio::import(
  file = "https://www.imf.org/external/datamapper/Metadata_Apr2023.xlsx",
  format = "xlsx",
  setclass = "tibble",
  sheet = "Table A. Economy Groupings",
  col_types = c(rep("text", 3), rep("skip", 16)),
  n_max = 95
)
dados_brutos_x <- rdbnomics::rdb(
  api_link = paste0(
    "https://api.db.nomics.world/v22/series/IMF/WEO:2023-04?",
    "dimensions=%7B%22unit%22%3A%5B%22us_dollars%22%5D%2C%22",
    "weo-subject%22%3A%5B%22NGDPD%22%5D%7D&observations=1"
  )
)

# Pré-processamentos
dados_y <- dados_brutos_y |>
  dplyr::rename(
    "avancada" = "Advanced Economies",
    "emergentes" = "Emerging\r\nMarket Economies\r\n",
    "baixa_renda" = "Low-Income Developing\r\nCountries\r\n"
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::everything(),
    names_to = "grupo",
    values_to = "pais"
  ) |>
  tidyr::drop_na() |>
  dplyr::mutate(
    grupo = dplyr::if_else(
      grupo %in% c("emergentes", "baixa_renda"),
      "nao_avancada",
      grupo
    ),
    y = dplyr::if_else(grupo == "avancada", 1, 0)
  )

dados_x <- dados_brutos_x |>
  dplyr::as_tibble() |>
  dplyr::group_by(`weo-country`) |>
  dplyr::filter(!is.na(value)) |>
  dplyr::filter(period == as.Date("2023-01-01")) |>
  dplyr::ungroup() |>
  dplyr::select("pais" = "WEO Country", "pib" = "value")

dados <- dplyr::left_join(x = dados_y, y = dados_x, by = "pais") |>
  tidyr::drop_na()

# Visualização de dados
dados |>
  ggplot2::ggplot() +
  ggplot2::aes(x = pib, y = y) +
  ggplot2::geom_point()
print(dados)

# Separação de amostras
set.seed(1984)
amostras <- splitTools::partition(
  y = dados$grupo,
  p = c(treino = 0.7, teste = 0.3),
  type = "stratified"
)

dados_treino <- dados[amostras$treino, ]
dados_teste <- dados[amostras$teste, ]

# Função para computar regra de corte IQR
regra_iqr <- function(x, side) {
  if (side == "lower") {
    lower <- quantile(x = x, probs = 0.25, na.rm = TRUE) - 1.5 * IQR(x = x, na.rm = TRUE)
    return(lower)
  } else if (side == "upper") {
    upper = quantile(x = x, probs = 0.75, na.rm = TRUE) + 1.5 * IQR(x = x, na.rm = TRUE)
    return(upper)
  } else stop("side tem que ser lower ou upper")
}

# Filtra dados
dados_treino <- dados_treino |>
  dplyr::filter(
    !pib < regra_iqr(pib, "lower") & !pib > regra_iqr(pib, "upper")
  )
dados_teste <- dados_teste |>
  dplyr::filter(
    !pib < regra_iqr(pib, "lower") & !pib > regra_iqr(pib, "upper")
  )

# Estimação do modelo
modelo <- glm(
  formula = y ~ pib,
  family = binomial,
  data = dados_treino
)
summary(modelo)



# Produzir estimativas para dados "desconhecidos"
probabilidades <- dados_teste |>
  dplyr::mutate(
    probabilidade = predict(modelo, newdata = dados_teste, type = "response")
  )
head(probabilidades)


neg_pos <- probabilidades |>
  dplyr::mutate(
    `pred` = dplyr::if_else(probabilidade > 0.5, 1, 0) |> as.factor(),
    y = as.factor(y),
    Resultado = dplyr::case_when(
      y == 1 & pred == 1 ~ factor("VP"),
      y == 0 & pred == 1 ~ factor("FP"),
      y == 0 & pred == 0 ~ factor("VN"),
      y == 1 & pred == 0 ~ factor("FN")
    )
  ) |>
  dplyr::select(
    "Classificação Observada" = "y",
    "Classificação Prevista" = "pred",
    "Resultado"
  )
head(neg_pos)


contagem <- neg_pos |>
  dplyr::count(Resultado, .drop = FALSE)

data.frame(
  "Economia Avançada" = dplyr::filter(contagem, stringr::str_detect(Resultado, "P")) |>
    dplyr::arrange("Resultado") |>
    dplyr::pull(n),
  "Economia Não Avançada" = dplyr::filter(contagem, stringr::str_detect(Resultado, "N")) |>
    dplyr::arrange(dplyr::desc(Resultado)) |>
    dplyr::pull(n),
  row.names = c("Economia Avançada", "Economia Não Avançada"),
  check.names = FALSE
)


# Criar dados simulados para animação

dados_anim <- dados_treino |>
  mutate(prob_prevista = predict(modelo, type = "response"))

anim_curva <- ggplot(dados_anim, aes(x = pib, y = y)) +
  geom_point(aes(color = grupo), alpha = 0.7, size = 3) +
  geom_line(aes(y = prob_prevista), color = "orange", linewidth = 1.2) +
  labs(title = "Modelo Logístico: Economia Avançada vs Economia Não Avançada",
       subtitle = "Curva de probabilidade estimada: P(Y=1) = 1 / (1 + e^(-(β₀ + β₁×PIB)))",
       x = "PIB (US$)", 
       y = "Probabilidade de ser Economia Avançada",
       color = "Grupo") +
  theme_minimal() +
  transition_states(grupo, transition_length = 3, state_length = 1)

# Gerar animação
animate(anim_curva, 
        duration = 10, 
        fps = 15, 
        width = 900, 
        height = 500,
        renderer = gifski_renderer("animacao_economias.gif"))

# ═══════════════════════════════════════════════════════════════════════════
# ESTIMAÇÃO DO MODELO SVM (SUBSTITUIÇÃO DA REGRESSÃO LOGÍSTICA)
# ═══════════════════════════════════════════════════════════════════════════

# Converter y para factor (necessário para SVM)
dados_treino$y <- as.factor(dados_treino$y)
dados_teste$y <- as.factor(dados_teste$y)

# Estimação do modelo SVM
modelo_svm <- e1071::svm(
  formula = y ~ pib,
  data = dados_treino,
  type = "C-classification",    # Classificação
  kernel = "linear",            # Kernel linear (pode mudar para "radial", "polynomial")
  cost = 1,                     # Parâmetro de custo
  scale = TRUE,                 # Escalar variáveis
  probability = TRUE            # Permitir estimar probabilidades
)

# Resumo do modelo
summary(modelo_svm)

# ═══════════════════════════════════════════════════════════════════════════
# PRODUZIR ESTIMATIVAS PARA DADOS "DESCONHECIDOS"
# ═══════════════════════════════════════════════════════════════════════════

probabilidades <- dados_teste |>
  dplyr::mutate(
    # Previsões de classe
    pred_class = predict(modelo_svm, newdata = dados_teste),
    # Probabilidades (precisa do probability = TRUE no modelo)
    probabilidade = attr(predict(modelo_svm, newdata = dados_teste, 
                                 probability = TRUE), "probabilities")[, "1"]
  )

head(probabilidades)

# ═══════════════════════════════════════════════════════════════════════════
# MATRIZ DE CONFUSÃO E AVALIAÇÃO
# ═══════════════════════════════════════════════════════════════════════════

neg_pos <- probabilidades |>
  dplyr::mutate(
    `pred` = pred_class,
    y = as.factor(y),
    Resultado = dplyr::case_when(
      y == 1 & pred == 1 ~ factor("VP"),
      y == 0 & pred == 1 ~ factor("FP"),
      y == 0 & pred == 0 ~ factor("VN"),
      y == 1 & pred == 0 ~ factor("FN")
    )
  ) |>
  dplyr::select(
    "Classificação Observada" = "y",
    "Classificação Prevista" = "pred",
    "Resultado"
  )

head(neg_pos)

contagem <- neg_pos |>
  dplyr::count(Resultado, .drop = FALSE)

# Matriz de confusão formatada
matriz_confusao <- data.frame(
  "Economia Avançada" = dplyr::filter(contagem, stringr::str_detect(Resultado, "P")) |>
    dplyr::arrange("Resultado") |>
    dplyr::pull(n),
  "Economia Não Avançada" = dplyr::filter(contagem, stringr::str_detect(Resultado, "N")) |>
    dplyr::arrange(dplyr::desc(Resultado)) |>
    dplyr::pull(n),
  row.names = c("Economia Avançada", "Economia Não Avançada"),
  check.names = FALSE
)

print(matriz_confusao)

# ═══════════════════════════════════════════════════════════════════════════
# Gráfico
# ═══════════════════════════════════════════════════════════════════════════

#Gráfico animado

# Criar animação mostrando a classificação ponto a ponto
dados_anim_classificacao <- dados_teste |>
  mutate(
    probabilidade = attr(predict(modelo_svm, newdata = dados_teste, 
                                 probability = TRUE), "probabilities")[, "1"],
    classificado = if_else(probabilidade > 0.5, "Prev: Avançada", "Prev: Não Avançada"),
    id = row_number()
  )

anim_classificacao <- ggplot(dados_anim_classificacao) +
  geom_point(
    aes(x = pib, y = as.numeric(as.character(y)), 
        color = classificado, group = id),
    size = 4, alpha = 0.7
  ) +
  geom_vline(xintercept = mean(dados_teste$pib), linetype = "dashed") +
  scale_color_manual(values = c("Prev: Avançada" = "blue", "Prev: Não Avançada" = "red")) +
  labs(
    title = "Classificação SVM - Ponto {frame} de {nframes}",
    subtitle = "Azul = Previsto como Avançada | Vermelho = Previsto como Não Avançada",
    x = "PIB", 
    y = "Classe Real"
  ) +
  theme_minimal() +
  transition_time(id) +
  shadow_mark(size = 2, alpha = 0.3)  # Mantém traço dos pontos anteriores

animate(anim_classificacao, nframes = nrow(dados_teste), fps = 5)

# Criar e salvar a animação diretamente
anim <- animate(anim_classificacao, 
                nframes = nrow(dados_teste), 
                fps = 5,
                width = 800,
                height = 600,
                renderer = gifski_renderer())

anim_save("anim_classificacao.gif", 
          animation = anim,
          path = "C:/Users/m-hen/OneDrive/Área de Trabalho") 
