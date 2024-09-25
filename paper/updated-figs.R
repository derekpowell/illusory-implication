library(tidybayes)

# fig1
plot_df <- df %>% 
  filter(phase=="test") %>% 
  filter(item_type == "B") %>% 
  group_by(exposed, exposure_cond, B_implied_true, response) %>% 
  # summarize(
  #   M = mean(resp_num),
  #   se = sd(resp_num)/sqrt(n()),
  #   ll = M - se,
  #   ul = M + se
  # ) %>% 
  summarize(
    N = n()
  ) %>%
  group_by(exposure_cond, B_implied_true, exposed) %>%
  mutate(N = N/sum(N)) %>%
  mutate(
    exposure_cond = if_else(exposure_cond=="fact", "Fact condition", "Quiz condition"),
    exposed = if_else(exposed==1, "Exposed", "Unexposed"),
    B_implied_true = if_else( B_implied_true == "T implied F", "Contradicted", "Entailed") #as.character(B_implied_true)
  ) %>% 
  spread(exposed, N) %>% 
  mutate(diff = Exposed - Unexposed)

plot_df %>% 
ggplot(
    aes(x=B_implied_true,  y = response, fill = diff)
  ) +
  geom_tile() +
  scale_fill_gradient2( low = '#b2182b', mid = '#f7f7f7', high = '#2166ac', midpoint = 0) +
  facet_wrap(~exposure_cond) +
  theme_bw(base_size=9.5) +
  theme(panel.grid=element_blank(), legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=.5))


df %>% 
  filter(phase=="test") %>% 
  filter(item_type == "B") %>% 
  group_by(exposed, exposure_cond, B_implied_true, response) %>% 
  # summarize(
  #   M = mean(resp_num),
  #   se = sd(resp_num)/sqrt(n()),
  #   ll = M - se,
  #   ul = M + se
  # ) %>% 
  summarize(
    N = n()
  ) %>%
  group_by(exposure_cond, B_implied_true, exposed) %>%
  mutate(N = N/sum(N)) %>%
  mutate(
    exposure_cond = if_else(exposure_cond=="fact", "Fact condition", "Quiz condition"),
    exposed = if_else(exposed==1, "Exposed", "Unexposed"),
    B_implied_true = if_else( B_implied_true == "T implied F", "Contradicted", "Entailed") #as.character(B_implied_true)
  ) %>% 
  spread(exposed, N)  %>% 
  mutate(diff = Exposed - Unexposed) %>% 
  ggplot(
    aes(y = diff,  x = response, fill = B_implied_true)
  ) +
  # geom_tile() +
  geom_col(position="identity") +
  scale_fill_manual(values = c('#b2182b', '#2166ac')) +  
  facet_wrap(~exposure_cond) +
  theme_bw(base_size=9.5) +
  theme(panel.grid=element_blank(), legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=.5))

# 
# df2 %>% 
#   filter(phase=="test") %>% 
#   filter(item_type == "B") %>% 
#   mutate(
#     exposed = if_else(exposed, "Exposed", "Unexposed"),
#     B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
#   ) %>% 
#   group_by(exposed, B_implied_true, response) %>% 
#   summarize(
#     N = n()
#   ) %>%
#   group_by(B_implied_true, exposed) %>%
#   mutate(N = N/sum(N)) %>%
#   ungroup() %>% 
#   spread(exposed, N) %>% 
#   mutate(diff = Exposed - Unexposed) %>% 
#   ggplot(
#     aes(y = diff,  x = response, fill = B_implied_true)
#   ) +
#   # geom_tile() +
#   geom_col(position="dodge") +
#   scale_fill_manual(values = c('#b2182b', '#2166ac')) +  
#   # scale_fill_gradient2( low = '#b2182b', mid = '#f7f7f7', high = '#2166ac', midpoint = 0) +
#   # facet_wrap(~exposure_cond) +
#   theme_bw(base_size=9.5) +
#   theme(panel.grid=element_blank(), legend.position="bottom") +
#   theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=.5))

library(patchwork)

df3 %>% 
  filter(phase=="test") %>% 
  filter(item_type == "B") %>% 
  mutate(
    exposed = if_else(exposed, "Exposed", "Unexposed"),
    B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
  ) %>% 
  group_by(exposed, B_implied_true, response) %>% 
  summarize(
    N = n()
  ) %>%
  group_by(B_implied_true, exposed) %>%
  mutate(N = N/sum(N)) %>%
  ungroup() %>% 
  spread(exposed, N) %>% 
  mutate(diff = Exposed - Unexposed) %>% 
  ggplot(
    aes(y = diff,  x = response, fill = B_implied_true)
  ) +
  # geom_tile() +
  geom_col(position=position_dodge(width = .25)) +
  scale_fill_manual(values = c('#b2182b', '#2166ac')) +  
  # scale_fill_gradient2( low = '#b2182b', mid = '#f7f7f7', high = '#2166ac', midpoint = 0) +
  # facet_wrap(~exposure_cond) +
  theme_bw(base_size=9.5) +
  theme(panel.grid=element_blank(), legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=.5)) +


df3 %>% 
  filter(phase=="test") %>% 
  filter(item_type == "B") %>% 
  mutate(
    exposed = if_else(exposed, "Exposed", "Unexposed"),
    B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
  ) %>% 
  group_by(exposed, B_implied_true, response) %>% 
  summarize(
    N = n()
  ) %>%
  group_by(B_implied_true, exposed) %>%
  mutate(N = N/sum(N)) %>%
  ungroup() %>% 
  spread(exposed, N) %>% 
  mutate(diff = Exposed - Unexposed) %>%
  mutate(response = if_else(grepl("True", response), "True", "False")) %>% 
  group_by(B_implied_true, response) %>% 
  mutate(diff = sum(diff)) %>% 
  ggplot(
    aes(y = diff,  x = response, fill = B_implied_true)
  ) +
  # geom_tile() +
  geom_col(position=position_dodge(width = .25)) +
  scale_fill_manual(values = c('#b2182b', '#2166ac')) +  
  # scale_fill_gradient2( low = '#b2182b', mid = '#f7f7f7', high = '#2166ac', midpoint = 0) +
  # facet_wrap(~exposure_cond) +
  theme_bw(base_size=9.5) +
  theme(panel.grid=element_blank(), legend.position="bottom") +
  theme(axis.text.x = element_text(angle = 10, vjust = 0.5, hjust=.5))

## -----


library(tidybayes)
# marginal effect type plots ... or, just coefficient plots?


make_exp_figure <- function(df_plotting, fit_model){
  
  
  obs_diffs <- df_plotting %>% 
    filter(phase=="test") %>% 
    filter(item_type == "B") %>% 
    mutate(
      exposed = if_else((exposed == TRUE | exposed==1), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>% 
    group_by(exposed, B_implied_true, response) %>% 
    summarize(
      N = n()
    ) %>%
    group_by(B_implied_true, exposed) %>%
    mutate(P = N/sum(N)) %>%
    ungroup() %>% 
    mutate(exposed = if_else((exposed == TRUE | exposed=="Exposed"), "Exposed", "Unexposed")) %>% 
    mutate(response = if_else(grepl("True", response), "True", "False")) %>%
    group_by(B_implied_true, response, exposed) %>%
    summarize(
      P = sum(P),
      SE = sqrt( P*(1-P) / sum(N))
    ) %>% 
    ungroup() %>%
    pivot_wider(names_from = exposed, values_from = c(P, SE)) %>%
    mutate(
      diff = P_Exposed - P_Unexposed,
      diff_se = sqrt(SE_Exposed^2 + SE_Unexposed^2)
    ) %>%
    rename(Implication = B_implied_true)
  
  
  schema_preds  <-  df_plotting %>%
    filter(phase=="test") %>%
    filter(item_type == "B") %>%
    # select(B_implied_true, familiarity, implication, item, subj_id) %>%
    # filter(implication != 0) %>%
    distinct(B_implied_true, familiarity, implication, .keep_all = TRUE) %>%
    select(B_implied_true, familiarity, implication) %>%
    mutate(item = "novel_B", subj_id = 1) %>%
    add_epred_draws(fit_model, allow_new_levels = FALSE, re_formula = NULL) %>% 
    ungroup() %>% 
    mutate(
      exposed = if_else((familiarity == TRUE | familiarity==1), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>%
    select(-implication, -familiarity, -.row, -.chain, -.iteration) %>%
    ungroup() %>% 
    rename(response = .category) %>% 
    mutate(exposed = if_else((exposed == TRUE | exposed=="Exposed"), "Exposed", "Unexposed")) %>% 
    spread(exposed, .epred) %>% 
    mutate(diff = Exposed - Unexposed) %>%
    mutate(response = if_else(grepl("True", response), "True", "False")) %>%
    group_by(B_implied_true, response, .draw) %>%
    mutate(diff = sum(diff)) %>%
    group_by(B_implied_true, response) %>% 
    summarize(
      M = mean(diff),
      ul = quantile(diff, .75),
      ll = quantile(diff, .25)
    ) %>% 
    rename(Implication = B_implied_true)
  
  plt_binned <- schema_preds %>%   
    ggplot(
      aes(y = M,  x = response, color = Implication, ymin =ll, ymax = ul)
    ) +
    geom_col(data = obs_diffs, aes(x = response, y = diff, fill = Implication), position="dodge", inherit.aes = FALSE, alpha = .25) +
    # geom_errorbar(data = obs_diffs, aes(x = response, y = diff, ymin = diff - diff_se, ymax = diff + diff_se, color = Implication), position = position_dodge(width = 1), inherit.aes=FALSE, width=0) +
    geom_pointrange(position = position_dodge(width = 1)) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = c('#4AABD3', '#0067B1')) + 
    scale_fill_manual(values = c('#4AABD3', '#0067B1')) + 
    theme_bw(base_size=14) +
    theme(panel.grid=element_blank(), legend.position="bottom") +
    theme(legend.direction = "vertical") +
    labs(y = "Diff. in Response Prop.: Exposed - Unexposed ", x = "Response", title = "B")
  
  obs <- df_plotting %>% 
    filter(phase=="test") %>% 
    filter(item_type == "B") %>% 
    mutate(
      exposed = if_else((exposed == TRUE | exposed=="Exposed"), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>% 
    group_by(exposed, B_implied_true, response) %>% 
    summarize(
      N = n()
    ) %>%
    group_by(B_implied_true, exposed) %>%
    mutate(
      P = N/sum(N),
      se = sqrt((P*(1-P))/N)
    ) %>% 
    mutate(Exposed = exposed)
  
  
  schema_preds2  <-  df_plotting %>%
    filter(phase=="test") %>%
    filter(item_type == "B") %>%
    # filter(implication != 0) %>%
    distinct(B_implied_true, familiarity, implication, .keep_all = TRUE) %>%
    select(B_implied_true, familiarity, implication) %>% 
    # mutate(item = "new_item", subj_id = 9999) %>% 
    add_epred_draws(fit_model, allow_new_levels = TRUE, re_formula = NULL) %>% 
    ungroup() %>% 
    mutate(
      exposed = if_else((familiarity == TRUE | familiarity==1), "Exposed", "Unexposed"),
      B_implied_true = if_else(B_implied_true == "T implied F", "Contradicted", "Entailed") 
    ) %>%
    select(-implication, -familiarity, -.row, -.chain, -.iteration) %>%
    ungroup() %>% 
    rename(response = .category) %>% 
    group_by(B_implied_true, response, exposed) %>%
    summarize(
      M = mean(.epred),
      ul = quantile(.epred, .975),
      ll = quantile(.epred, .025)
    ) %>% 
    mutate(Exposed = exposed)
  
  
  plt_raw <- schema_preds2 %>%   
    ggplot(
      aes(y = M,  x = response, color = Exposed, ymin =ll, ymax = ul)
    ) +
    # geom_tile() +
    geom_col(data = obs, aes(x = response, y = P, fill = Exposed), position="dodge", inherit.aes = FALSE, alpha =.5) +
    # geom_errorbar(data = obs, aes(x= response , y = P, ymin = P - se, ymax = P + se, color = Exposed), position = position_dodge(width=1), width = 0) +
    # geom_hline(yintercept = 0) +
    geom_pointrange(position = position_dodge(width = 1)) +
    facet_wrap(~B_implied_true, nrow = 2) +
    scale_color_manual(values = c('#A41881', '#320A64')) +  
    scale_fill_manual(values = c('#A41881', '#320A64')) +  
    theme_bw(base_size=14) +
    theme(panel.grid=element_blank(), legend.position="bottom") +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
    theme(legend.direction = "vertical") +
    labs(y = "Proportion of responses", x = "Response", title = "A", fill = "Exposure", color = "Exposure")
    
  
  plt_raw + plt_binned + plot_layout(widths = c(3,1), guides = "collect")
  
}
  
make_exp_figure(df_reg %>% filter(exposure_cond=="fact"), fit_imp_fact_int)
make_exp_figure(df_reg %>% filter(exposure_cond=="quiz"), fit_imp_quiz_int)
make_exp_figure(df_reg2, fit_imp_exp2)
make_exp_figure(df_reg3, fit_imp_maximal3)


## ------
# 
# schema_preds = df_reg3 %>% 
#   filter(phase=="test") %>% 
#   filter(item_type == "B") %>%  
#   distinct(B_implied_true, familiarity, implication, .keep_all = TRUE) %>% 
#   select(B_implied_true, familiarity, implication, exposed) %>% 
#   mutate(item = "new_item", subj_id = 9999) %>% 
#   add_epred_draws(fit_imp_maximal, allow_new_levels = TRUE) %>% 
#   group_by(B_implied_true, exposed) %>% 
#   summarize(
#     Estimate = mean(.linpred),
#     ul = quantile(.linpred, .75),
#     ll = quantile(.linpred, .25)
#   )
#   
# schema_preds %>% 
#   ggplot(aes(x = B_implied_true, y = Estimate, color = exposed, shape=exposed, ymin = ll, ymax = ul)) +
#   geom_pointrange(position= position_dodge(width = .5))
# 
# 
# schema_preds <- df_reg3 %>% 
#   filter(phase=="test") %>% 
#   filter(item_type == "B") %>%  
#   distinct(B_implied_true, familiarity, implication, .keep_all = TRUE) %>% 
#   select(B_implied_true, familiarity, implication, exposed) %>% 
#   mutate(item = "new_item", subj_id = 9999) %>% 
#   add_epred_draws(fit_imp_maximal2, allow_new_levels = TRUE, re_formula = NA) %>% 
#   mutate(resp_num = recode(.category,
#                            `Definitely True` = 2.5,
#                            `Probably True` = 1.5,
#                            `Maybe True` = .5,
#                            `Maybe False` = -.5,
#                            `Probably False` = -1.5,
#                            `Definitely False` = -2.5)
#   ) %>% 
#   group_by(B_implied_true, exposed, item, subj_id, .draw) %>% 
#   summarize(
#     pred_resp_num = sum(.epred*resp_num)
#   ) %>% 
#   group_by(B_implied_true, exposed) %>% 
#   summarize(
#     Estimate = mean(pred_resp_num),
#     ul = quantile(pred_resp_num, .75),
#     ll = quantile(pred_resp_num, .25)
#   )
# 
# schema_preds %>% 
#   ggplot(aes(x = B_implied_true, y = Estimate, color = exposed, shape=exposed, ymin = ll, ymax = ul)) +
#   geom_pointrange(position= position_dodge(width = .5))
# 
# 
# tidy_draws(fit_imp_maximal2, )
#   