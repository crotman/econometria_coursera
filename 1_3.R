library(tidyverse)
library(readxl)
library(janitor)
library(tidymodels)

dado <- read_excel("data/TrainExer15.xls") %>% 
  clean_names() 

lm_mod <- linear_reg() %>% 
  set_engine("lm") 

dados_novos <- tibble(
  game = 16:200
)



formulas <- tibble(
  formula = c(winning_time_men ~ game, log(winning_time_men) ~ game, winning_time_women ~ game, log(winning_time_women) ~ game ),
  tipo_formula = c("men_linear", "men_log", "women_linear", "women_log"),
  revert_outcome = c(identity, exp, identity, exp  )
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
    augment = list(augment(fit$fit))
  ) %>% 
  mutate(
    preds = list(predict(fit, new_data = dados_novos) %>% revert_outcome  ),
    games = list(dados_novos$game),
  )


complemento_dados_novos <- resultados_modelos %>% 
  select(
    tipo_formula,
    preds,
    games
  ) %>% 
  unnest(c(preds, games))  %>% 
  mutate(
    year = min(dado$year) + (games-1) *4
  ) %>% 
  separate(
    col = tipo_formula,
    into = c("gender", "model"),
    sep = "_"
  )


cruzam <- complemento_dados_novos %>% 
  pivot_wider(
    names_from = gender,
    values_from = .pred
  ) %>% 
  group_by(
    model
  ) %>% 
  arrange(year) %>% 
  filter(
    men >= women
  ) %>% 
  slice_head()


grafico_resultado <- ggplot(complemento_dados_novos) +
  geom_point(
    aes(
      x = year,
      y = .pred,
      color = gender
    )
  ) +
  geom_line(
    aes(
      x = year,
      y = .pred,
      color = gender
    )
  ) +
  facet_wrap(
    ~model
  )
  

grafico_resultado






