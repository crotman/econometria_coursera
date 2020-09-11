library(tidyverse)
library(readxl)
library(janitor)
library(tidymodels)

dado <- read_excel("d:/temp/TrainExer13.xls") %>% 
  clean_names()


grafico <- ggplot(dado,
       aes(
         x = game,
         y = winning_time_men
       )
) +
  geom_line(color = "darkblue", size = 1) +
  geom_point( color = "darkblue", size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgreen", size = 1) +
  geom_smooth(formula = y ~ poly(x, 2), method = "lm", color = "darkred", se = FALSE ) +
  theme_minimal()

grafico

lm_mod <- linear_reg() %>% 
  set_engine("lm") 


dados_novos <- tribble(
  ~game, ~winning_time_men,
  16,    9.69,
  17,    9.63,
  18,    9.81
)


formulas <- tibble(
  formula = c(winning_time_men ~ game, winning_time_men ~ poly(game,2) ),
  tipo_formula = c("linear", "quadratica")
)

resultados_modelos <- formulas %>% 
  mutate(
    modelo = list(lm_mod)
  ) %>% 
  rowwise() %>%
  mutate(
    fit = list(parsnip::fit(modelo, formula, data = dado))
  ) %>% 
  mutate(
    glance = glance(fit$fit)
  ) %>% 
  mutate(
    preds = list(predict(fit, new_data = dados_novos)),
    games = list(dados_novos$game),
    winning_times_men = list(dados_novos$winning_time_men),
    r2 = rsq_vec(estimate = preds$.pred, truth = dados_novos$winning_time_men)
  ) 


complemento_dados_novos <- resultados_modelos %>% 
  select(
    tipo_formula,
    winning_times_men,
    preds,
    games
  ) %>% 
  unnest(c(preds, games, winning_times_men)) 


grafico_resultado <- grafico +
  geom_point(
    data = complemento_dados_novos,
    color = "blue",
    size = 2.5,
    aes(
      x = games,
      y = winning_times_men
    )
  ) +
  geom_line(
    data = complemento_dados_novos,
    size = 0.8,
    color = "blue",
    aes(
      x = games,
      y = winning_times_men
    )
  ) +
  geom_point(
    data = complemento_dados_novos,
    aes(
      x = games,
      y = .pred,
      color = tipo_formula
    )
  ) +
  scale_color_manual(
    values = c("linear" = "darkgreen", "quadratica" = "darkred")
  )

  
grafico_resultado
















