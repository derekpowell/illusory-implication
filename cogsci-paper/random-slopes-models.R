fit_prem_exp2_rs <- brm(
  response ~ exposed + (1 + exposed|subj_id) + (1|item_pair),
  data = df2 %>%
    filter(phase=="test", item_type=="A"),
  family = cumulative(),
  chains = 4,
  cores = 4,
  file="../local/fit_prem_exp2_rs"
)

summary(fit_prem_exp2_rs)

glimpse(posterior_samples(fit_prem_exp2_rs))

library(tidybayes)

fit_prem_exp2_rs %>% 
  spread_draws(r_subj_id[subj_id,exposedTRUE],  b_exposedTRUE) %>% 
  mutate(beta = r_subj_id + b_exposedTRUE) %>% 
  group_by(subj_id) %>% 
  summarize(
    M = mean(beta),
    ul = quantile(beta, .75),
    ll = quantile(beta, .25),
  ) %>% 
  ggplot(aes(x=reorder(subj_id,M), y = M, ymin = ll, ymax=ul)) +
  geom_pointrange()

fit_prem_exp2_rs %>% 
  spread_draws(r_subj_id[subj_id,exposedTRUE],  b_exposedTRUE) %>% 
  mutate(beta = r_subj_id + b_exposedTRUE) %>% 
  group_by(subj_id) %>% 
  summarize(
    M = mean(beta),
    ul = quantile(beta, .75),
    ll = quantile(beta, .25),
  ) %>% 
  ggplot(aes(x=M)) +
  geom_histogram()

## -- implications exp 2

fit_imp_exp2_rs <- brm(
  response ~ familiarity + implication + B_implied_true + (1 + familiarity + implication|subj_id) + (1+ familiarity + implication|item_pair),  # familiarity + implication + grouping (optional)
  data = df_reg2,
  family = cumulative(),
  chains = 4,
  cores = 4,
  file="../local/fit_imp_exp2_rs"
)

summary(fit_imp_exp2_rs)

glimpse(posterior_samples(fit_imp_exp2_rs))

# fit_imp_exp2_rs %>% 
#   spread_draws(r_subj_id[subj_id,implication],  b_implication) %>% 
#   mutate(beta = r_subj_id + b_implication) %>% 
#   group_by(subj_id) %>% 
#   summarize(
#     M = mean(beta),
#     ul = quantile(beta, .75),
#     ll = quantile(beta, .25),
#   ) %>% 
#   ggplot(aes(x=reorder(subj_id,M), y = M, ymin = ll, ymax=ul)) +
#   geom_pointrange()

fit_imp_exp2_rs %>% 
  spread_draws(r_subj_id[subj_id,implication],  b_implication) %>% 
  mutate(beta = r_subj_id + b_implication) %>% 
  group_by(subj_id) %>% 
  summarize(
    M = mean(beta),
    ul = quantile(beta, .75),
    ll = quantile(beta, .25),
  ) %>% 
  ggplot(aes(x=M)) +
  geom_histogram()

## -- implications exp 1

fit_imp_quiz_int_rs <- brm(
  response ~ familiarity + implication + B_implied_true + (1|subj_id) + (1 + familiarity + implication|item_pair),  # familiarity + implication + grouping (optional)
  data = df_reg %>% filter(exposure_cond=="quiz"),
  family = cumulative(),
  chains = 4,
  cores = 4,
  file="../local/fit_imp_quiz_int_rs"
)

summary(fit_imp_quiz_int_rs)
glimpse(posterior_samples(fit_imp_quiz_int_rs))

fit_imp_quiz_int_rs %>% 
  spread_draws(r_item_pair[item_pair,implication],  b_implication) %>% 
  mutate(beta = r_item_pair + b_implication) %>% 
  group_by(item_pair) %>% 
  summarize(
    M = mean(beta),
    ul = quantile(beta, .975),
    ll = quantile(beta, .025),
  ) %>% 
    ggplot(aes(x=reorder(item_pair, M), y = M, ymin = ll, ymax=ul)) +
    geom_pointrange() +
  coord_flip()


## -- item-level effects?

fit_imp_exp2_rs <- brm(
  response ~ familiarity + implication + B_implied_true + (1 |subj_id) + (1+ familiarity + implication|item_pair),  # familiarity + implication + grouping (optional)
  data = df_reg2,
  family = cumulative(),
  chains = 4,
  cores = 4,
  file="../local/fit_imp_exp2_rs"
)


fit_imp_exp2_rs %>%
  spread_draws(r_item_pair[item_pair,implication],  b_implication) %>%
  mutate(beta = r_item_pair) %>%
  group_by(item_pair) %>%
  summarize(
    M = mean(beta),
    ul = quantile(beta, .75),
    ll = quantile(beta, .25),
  ) %>%
  ggplot(aes(x=reorder(item_pair, M), y = M, ymin = ll, ymax=ul)) +
  geom_pointrange() +
  coord_flip()
# 
# # fit_imp_quiz_int_rs <- brm(
# #   response ~ familiarity + implication + B_implied_true + (1 + familiarity + implication|subj_id) + (1 + familiarity + implication|item_pair),  # familiarity + implication + grouping (optional)
# #   data = df_reg %>% filter(exposure_cond=="quiz"),
# #   family = cumulative(),
# #   chains = 4,
# #   cores = 4,
# #   file="../local/fit_imp_quiz_int_rs"
# )


fit_imp_exp2_rs <- brm(
  response ~ familiarity + implication + B_implied_true + (1 + familiarity + implication|subj_id) + (1+ familiarity + implication|item_pair),  # familiarity + implication + grouping (optional)
  data = df_reg2,
  family = cumulative(),
  chains = 4,
  cores = 4,
  file="../local/fit_imp_exp2_rs"
)
